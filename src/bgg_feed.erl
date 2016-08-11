-module(bgg_feed).

-export([print_links/0]).
-export([print_titles/0]).
-export([start/0, start/1]).
-export([stop/0]).
-export([run/0, run/1]).

-export([get_new_items/1]).
-export([export_games_to_file/1]).

-export([loop/1]).

-include("../include/record.hrl").
start() ->
    start(?BGG_RSS_FEED),
    run(?BGG_RSS_FEED).
start(_Url) ->
    ok = application:ensure_started(inets),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    hackney:start(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    %% create_feed_table(),
    bgg_feed_utils:create_tables(),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

print_titles() ->
    spawn(bgg_feed_utils,print_titles,[?BGG_URL]).

print_links() ->
    bgg_feed_utils:print_links(?BGG_URL).

get_new_items(Type) ->
    MatchHead=#entries{link='$1', title='$2',type=Type,_='_'},
    Results=mnesia:dirty_select(entries,[{MatchHead,[],['$_']}]),
    lists:foreach(
      fun(X) ->
	      _T=X#entries.title,
	      _L=X#entries.link,
	      bgg_feed_utils:handle_item(Type, X)
      end,
      Results).

run() ->
    run(?BGG_RSS_FEED).

export_games_to_file(File) ->
    bgg_feed_utils:do_logging_async(File, ets:tab2list(games)).

%%%%%%%%%%%%%% Internal functions %%%%%%%%%%%%%%%

run(Url) ->
    bgg_feed_utils:all(Url),
    Pid=spawn(?MODULE,loop,[Url]),
    register(run,Pid).

loop(Url) ->
    io:format("loop() at ~p ~n",[calendar:local_time()]),
    receive
	stop -> io:format("run loop has been stopped",[])
    after
	?FREQ ->
	    io:format("Timeout, games table size=~p~n",
		      [mnesia:table_info(games,size)]),
	    bgg_feed_utils:store_entries(Url),
	    get_new_items(boardgame),
	    loop(Url)
    end.

