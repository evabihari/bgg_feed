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
	 decode/1,
	 convert_to_airtable/1]).

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


convert_to_airtable(Game) when is_record(Game,game) ->
    {ok,Game_Json}=encode(Game),
    %% {"id":"184459","name":"Ave Roma","family":"Crowdfunding: Kickstarter","mechanics":"Set Collection , Trading , Worker Placement",
    %%  "yearpublished":"2016","minplayers":"2","maxplayers":"5","publishers":"A-games (Board Game Publisher)",
    %%  "gamedesigners":"Attila SzÅgyi","categories":"Ancient , Civilization , Economic","types":"","lang_dependence":"1",
    %%  "price":"Unknown","updated":"1471254961000000"}

    %% airtable Games item:
%% {"records":[{"id":"recN1OCI8WwO6HLaj",
%% "fields":{"id":"133731",
    %% "attachments":[{"id":"att6Nvmsn7HCdgudD","url":"https://dl.airtable.com/pMhZ1X5jSPmYOJHaiMhg_pic2786349_md.jpg","filename":"pic2786349_md.jpg",
        %% "size":170971,"type":"image/jpeg","thumbnails":{"small":{"url":"https://dl.airtable.com/jk1yjXSATDiBlrwN9vBJ_small_pic2786349_md.jpg","width":36,"height":36},
        %% "large":{"url":"https://dl.airtable.com/JkYGzkfZSz8Ct2NR8fKA_large_pic2786349_md.jpg","width":500,"height":500}}}],
    %% "Name":"Brasil","gamedesigners":["recBjBCRLqI9BGXqo","rec1cBapeWK70R394","recmIv3X861tm06z9"],
    %% "publishers":["rec2YQGfod19TdZO8"],
    %% "yearpublished":2016,
    %% "mechanics":["recFtGEfz28wjHdim","rect6v86VnmURGoJX","rec9320uVGusBvVAS","recbPABBNuXz2DULf"],
    %% "artists":["rec2kUaLUR5QU4Hdu"],"minplayers":"2","maxplayers":"4","min_age":"12",
    %% "family":["recJ2ZOCatSo1Gm1X","recdbVBVB6twWlgyf"],
    %% "link_to_BGG":"https://boardgamegeek.com/boardgame/133731/brasil",
    %% "categories":["recbKiE6fEhWbpFvO","recnGGVwVih3eToNc","rec6OmOvoRd1IDu7A","recahNKWEGlVbl9kn"]},
%% "createdTime":"2016-08-16T11:09:27.000Z"}]}


%% PL= "{\"fields\":{\"id\":\"134157\",\"name\":\"Guilds of London\",\"family\":\"Admin - Unreleased Games , Cities - London\",\"mechanics\":\"Area Control / Area Influence , Hand Management\",\"yearpublished\":\"2016\",\"minplayers\":\"1\",\"maxplayers\":\"4\",\"publishers\":\"Surprised Stare Games Ltd , Tasty Minstrel Games\",\"gamedesigners\":\"Tony Boydell\",\"categories\":\"Medieval\",\"types\":\" \",\"lang_dependence\":\"1\",\"price\":\" EUR 45, as noted above; to preorder, head to [url=https://docs.google.com/forms/d/e/1FAIpQLSectFFzhLRJnSzMBULtjxe1xKrcI3IEW2Qf3HCr8zBeGkoeuA/viewform]this page[/url] to drop your deets.\",\"updated\":\"1471341574000000\"}}".

    PayLoad= "{\"fields\":" ++ binary_to_list(Game_Json) ++ "}",
    list_to_binary(PayLoad).

    


    
