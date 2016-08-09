-module(game).
%% -compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-json({game,   
       {string,"id"},
       {string,"name"},
       {string,"family"},
       {string,"mechanics"},
       {string,"yearpublished"},
       {string,"minplayers"},
       {string,"maxplayers"},
       {string,"publishers"},
       {string,"gamedesigners"},
       {string,"categories"},
       {string,"types"},
       {string,"lang_dependence"},
       {string,"price"},
       {string,"updated"}}).

-export([start/0,
	 encode/1,
	 decode/1]).

encode(Term) ->
    to_json(Term).
decode(Term) ->
    from_json(Term,game).

start() ->
    bgg_feed_utils:start_apps(),
    mnesia:create_schema([node()]),
    mnesia:start(),
						% create_booth_table(),
    bgg_feed_utils:create_games_table(),
    {ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
    io:format("riakc_pb_socket:start ok",[]),
						% {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017),
    MyBucket = <<"bgg">>,
    Obj1=riakc_obj:new(MyBucket,<<"one">>,1),
    riakc_pb_socket:put(Pid,Obj1),
    {ok, Fetched1} = riakc_pb_socket:get(Pid, MyBucket, "one"),
    io:format("Read from Riak =~p~n",[Fetched1]),
						% Caylus
    Id="18602",
    io:format("Url=~s~n",[string:concat(?BGG_GAME_URL , Id)]),
    Game=bgg_feed_utils:new_game(Id),
    io:format("~p~n",[Game]),
    io:format("family=~p~n",[Game#game.family]),
    {ok, Json} = encode(Game),
    io:format("~s~n",[Json]),
    GameObj=riakc_obj:new(MyBucket,list_to_binary(Id),Json,<<"application/json">>),
    riakc_pb_socket:put(Pid,GameObj),
    io:format("keys: ~p~n",[riakc_pb_socket:list_keys(Pid, <<"bgg">>)]),
    riakc_pb_socket:stop(Pid).



