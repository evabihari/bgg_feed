
%% bgg_feed_parse_hackney - stream parse feed over HTTP using hackney

-module(bgg_feed_parse_hackney).

-export([start_link/1]).
-export([resume/1]).

-behaviour(gen_fsm).

-export([code_change/4]).
-export([handle_event/3]).
-export([handle_info/3]).
-export([handle_sync_event/4]).
-export([init/1]).
-export([ready/3]).
-export([request/1]).
-export([terminate/3]).

-define(TIMEOUT, 30000).

-include("../include/record.hrl").

-record(state, {
  entries=[],
  feed,
  httpcPid,
  parsing=false,
  ref,
  reqId,
  url
}).

init(Url) ->
  {ok, ready, #state{url=Url}}.

%% Create gen_fsm as part of a supervision tree.
start_link(Url) ->
    gen_fsm:start_link(?MODULE, Url, []).

%% Begin stream parsing over HTTP.
resume(FsmRef) ->
  case inet:gethostbyname(?BGG_HOST_NAME) of
      {ok,_} ->
	  gen_fsm:sync_send_event(FsmRef, executing,?TIMEOUT);
      {error,Reason} ->
	  io:format("~p not available due to ~p~n",[?BGG_URL,inet:format_error(Reason)]),
	  gen_fsm:sync_send_event(FsmRef, stop,?TIMEOUT)
  end.

opts(http) -> [
  {autoredirect, true}];
opts(req) -> [
  {body_format, binary},
  {stream, {self, once}},
  {sync, false}].

request(State=#state{url=Url}) ->
%  case httpc:request(get, {Url, []}, opts(http), opts(req)) of
  Opts = [{async, once}, {stream_to,self()}],
  case hackney:get(Url,[], <<>>, Opts) of
      {ok, Ref} ->
	  receive
	      %% {http, {ReqId, stream_start, _Headers, Pid}} ->
	      %% 	  stream(State#state{reqId=ReqId, httpcPid=Pid});
	      %% {http, {error, Reason}} ->
	      %% 	  {error, Reason};
	      %% {http, {ReqId, {error, Reason}}} ->
	      %% 	  {error, Reason};
	      %% {error, Reason} ->
	      %% 	  {error, Reason};
	      {hackney_response, Ref, {status, StatusInt, Reason}} ->
		  io:format("got status: ~p with reason ~p~n~n", [StatusInt,
                                                          Reason]),
		  stream(State#state{ref=Ref});
	      Other ->
		  io:format("~p~n",[Other])
	  after
	      ?TIMEOUT ->
		  {error, timeout}
	  end;
      {error, {failed_connect,_}}->
	   % seems to be problem with netwrok connection give back empty result and terminate normally?
	  io:format("http request failed due to failed_connect ~n",[]),	  
	  {stop,normal, {ok,[],[]},State};
      {error, Reason} ->
	  io:format("http request failed due to Reason=~p ~n",[Reason]),	  
	  {stop,normal, {ok,[],[]},State}
    end.

result({ok, State, _Rest}) ->
  Entries = lists:reverse(State#state.entries),
  {stop, normal, {ok, State#state.feed, Entries}, State};
result({error, Reason}) ->
  {stop, error, Reason};
result({fatal_error, _, Reason,_ ,_State}) ->
  {stop, error, Reason}.

ready(executing, _, State) ->
  R = request(State),
   result(R);
ready(stop,_,State) ->
    {stop, normal, {ok, [],[]}, State}.


terminate(_Reason, _StateName, #state{reqId=ReqId}) ->
  httpc:cancel_request(ReqId);
terminate(_Reason, _StateName, _StateData) ->
  io:format("terminate with Reason=~p, StateData=~p~n",[_Reason, _StateData]),
  ok.

event_fun({entry, Entry}, State) ->
  State#state{entries=[Entry|State#state.entries]};
event_fun({feed, Feed}, State) ->
  State#state{feed=Feed};
event_fun(endFeed, State) ->
  State.

parser_opts(State) ->
  [{event_state, State}, {event_fun, fun event_fun/2},
    {continuation_state, State}, {continuation_fun, fun stream/1}].

parse(Chunk, State) when State#state.parsing ->
  {Chunk, State};
parse(Chunk, State) ->
  feeder:stream(Chunk, parser_opts(State#state{parsing=true})).

stream(State=#state{ref=Ref, httpcPid=_Pid}) ->
  hackney:stream_next(Ref),
  receive
   {hackney_response, Ref, {headers, Headers}} ->
          io:format("got headers: ~p~n", [Headers]),
	  stream(State);
    {hackney_response, Ref,  done} ->
          io:format("got done: ~n", []),
	  {<<>>, State};
   {hackney_response, Ref, {error, Reason}} ->
          io:format("got error: ~p~n", [Reason]),      
	  {error, Reason};
   {hackney_response, Ref, Bin} ->
	  io:format("got chunk: ~n", []),	  
	  parse(Bin, State)
  end.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    io:format("handle_sync_event, StateName=~p, StateData=~p~n",[_StateName, StateData]),
    {stop, error, StateData}.

handle_event(_Event, _StateName, StateData) ->
  {stop, error, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

handle_info({'EXIT', _Pid, _Reason}, _StateName, StateData) ->
  {stop, normal, StateData}.
