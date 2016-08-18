%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 17 Aug 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(airtable).
-define(API_URL,"https://api.airtable.com/v").
-define(API_VERSION,"0").
-define(BASE_ID, "apps6C5ZrvEBCsx9I").
-define(API_KEY, "keyIsTQjg9qICigb5").
-record(airtable, {
	  airtable_url=?API_URL ++ ?API_VERSION ++ "/",
	  base_url,
	  headers
	 }).
%% API
-export([init/0,init/2,
	 request/2,request/3,request/4,request/5,
	 get/2,get/3,filter/3,
	 create/3,delete/3,
	 update/4,update_all/4,
	 find/4]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    init(?BASE_ID,?API_KEY).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
init(Base_id, API_key) ->
    Default=#airtable{},
    Base_url=Default#airtable.airtable_url ++ Base_id,
						%    Headers = hackney_headers:make_header( <<"Authorization">> , "Bearer " ++ API_key),
    Headers = { <<"Authorization">> , list_to_binary("Bearer " ++ API_key)},
    #airtable{base_url = Base_url,
	      headers = [Headers]}.
request(AT, Url) ->
    request(AT,"GET",Url).
request(AT, Method, Url) ->
    request(AT, Method, Url, [], []).
request(AT, Method, Url, Params) ->
    request(AT, Method, Url, Params,[]).
request(AT, Method, Url, Params, Payload) ->
    Headers = case Method of
		  "POST" -> add_content_type(AT, "application/json");
		  "PUT"  -> add_content_type(AT, "application/json");
		  "PATCH" ->
		      add_content_type(AT, "application/json");
		  _ ->
		      AT#airtable.headers
	      end,
    ReqUrl=AT#airtable.base_url++"/"++Url,
    case hackney:request(Method, ReqUrl, Headers, Payload, Params) of
	{ok, 200, ResponseHeaders} ->
	    {ok, 200, ResponseHeaders};
	{ok, ResponseStatus, ResponseHeaders} ->
	    {error,{ResponseStatus, ResponseHeaders}};
	{ok, 200, _ResponseHeaders,Ref} ->
	    {ok,ResponseBody}=hackney:body(Ref),
	    {ok,ResponseBody};
	{ok, ResponseStatus,_ResponseHeaders,Ref} ->
	    {ok,ResponseBody}=hackney:body(Ref),
	    {error,{ResponseStatus,ResponseBody}};
	{error, {closed, PartialBody}} ->
	    io:format("request answered with closed, partial body was: ~p~n",[PartialBody]),
	    {error, {closed, PartialBody}};
	{error, Reason} ->
	    io:format("request resulted in error, Reason was: ~p~n",[Reason]),
	    {error,Reason}				   
    end.


add_content_type(AT, Value) ->
    AT#airtable.headers++[{<<"Content-Type">>, list_to_binary(Value)}]. 

get(AT,Table_name) ->
    get(AT, Table_name, []).
get(AT,Table_name, Params) ->
    Record_id=get_param(record_id,Params),
    Limit=get_param(limit,Params),
    Offset=get_param(offset,Params),
    Url=case Record_id of
	    [] ->
		Table_name;
	    Record_name ->
		Table_name ++ "/" ++ Record_name
	end,
    Url1=case {Limit,Offset} of
	     {[],[]} ->
		 Url;
	     {LV,[]} when is_integer(LV) ->
		 Url ++ "?limit="++integer_to_list(LV);		 
	     {LValue,[]} ->
		 Url ++ "?limit="++LValue;
	     {[],OValue} ->
		 Url++ "?offset="++OValue;
	     {LV,OV} when is_integer(LV)->
		 Url ++"?limit="++integer_to_list(LV)++"&offset="++OV;
	     {LV,OV} ->
		 Url ++"?limit="++LV++"&offset="++OV
	 end,
    case request(AT,Url1) of
	{ok,Result} ->
	    R=jsx:decode(Result),
	    Records=proplists:get_value(list_to_binary("records"),R),
	    {ok,[airtable_record:decode(X) || X <- Records]};
	{error,Reason} ->
	    {error,Reason}
    end.

get_param(_Name,[]) ->
    [];
get_param(Name,[{Name,Value}|_ParamList]) ->
    Value;
get_param(Name,[_|ParamList]) ->
    get_param(Name,ParamList).

create(AT, Table_name, Data) ->
    Payload=create_payload(Data,false),
    case request(AT, "POST", Table_name, [], Payload) of
	{ok,Result} -> {ok,airtable_record:decode(Result)};
	{error,Reason} ->
	    {error,Reason}
    end.

update(AT, Table_name, Record_id, Data) ->
    Payload=create_payload(Data,true),
    Url=Table_name ++ "/" ++ Record_id,
    case request(AT, "PATCH", Url, [], Payload) of
	{ok,Result} ->
	    {ok,airtable_record:decode(Result)};
	{error,Reason} ->
	    {error,Reason}
    end.

update_all(AT, Table_name, Record_name, Data) ->
    Payload=create_payload(Data,true),
    Url=Table_name ++ "/" ++ Record_name,
    case request(AT, "PUT", Url, [], Payload) of
	{ok,Result} ->
	    {ok,airtable_record:decode(Result)};
	{error,Reason} ->
	    {error,Reason}
    end.

create_payload(Data,true) ->
    D1=Data -- "}",
    D1 ++ " , \"typecast\": true}";
create_payload(Data,_) ->
    Data.

delete(AT,Table_name, Record_name) ->
    Url=Table_name ++ "/" ++ Record_name,
    Expected="{\"deleted\":true",
    case request(AT, "DELETE", Url) of
	{ok,Result} ->
	    case string:str(binary_to_list(Result),Expected) of
		0 ->
		    {error, "problem occured during deleting the record"};
		_ ->
		    {ok,"deleted"}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.

find(AT,Table_name, Field, Value) ->
    filter(AT,Table_name,{Field,"=",Value}).

filter(AT,Table_name, {A,Rel,B}) ->
    Query=A++Rel++"\""++B++"\"",
    filter(AT,Table_name, Query);
filter(AT,Table_name, Query) ->
    Url=Table_name ++ "?filterByFormula="++Query,
    case request(AT, "GET", Url) of
	{ok,Result} ->
	    R=jsx:decode(Result),
	    Records=proplists:get_value(list_to_binary("records"),R),
	    {ok,[airtable_record:decode(X) || X <- Records]};
	{error,Reason} ->
	    {error,Reason}
    end.

