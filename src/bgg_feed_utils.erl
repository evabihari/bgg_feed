-module(bgg_feed_utils).

-export([all/1]).
-export([titles/1]).
-export([feed/1]).
-export([entries/1]).
-export([links/1]).
-export([collect_types/1]).
-export([store/1]).
-export([get_type/1]).
-export([handle_item/2]).


-include("../include/record.hrl").

all(Url) ->
    {ok, Pid} = supervisor:start_child(bgg_feed_sup, [Url]),
    bgg_feed_parse:resume(Pid).

titles(Url) ->
    [feeder_entries:get(title, Entry) || Entry <-  entries(Url)].

feed(Url) ->
    {ok, Feed, _} = bgg_feed_utils:all(Url),
    Feed.

entries(Url) ->
    {ok, _, Entries} = bgg_feed_utils:all(Url),
    Entries.

links(Url) ->
    [feeder_entries:get(link, Entry) || Entry <-  entries(Url)].

collect_types(Url) ->
    collect_types(links(Url),[]).

collect_types([],TypeList) ->
    TypeList;
collect_types([undefined|BLinks],Types) ->
    collect_types(BLinks,Types);
collect_types([BinLink|BLinks],Types)->
    Url=binary_to_list(BinLink),
    NewType=case Url--?BGG_URL of
		Url -> [];
		PostFix1 -> PostFix=PostFix1--"/",
			    Length=string:str(PostFix,"/"),
			    list_to_atom(string:sub_string(PostFix,1,Length-1))
	    end,
    NTypes=case lists:member(NewType,Types) of
	       true ->  Types;
	       _ -> [NewType|Types]
	   end,
    collect_types(BLinks,NTypes).

get_type([undefined]) ->
    undefined;
get_type(undefined) ->
    undefined;
get_type(BinLink)->
    Url=binary_to_list(BinLink),
    case Url--?BGG_URL of
	Url -> undefined;
	PostFix1 -> PostFix=PostFix1--"/",
		    Length=string:str(PostFix,"/"),
		    list_to_atom(string:sub_string(PostFix,1,Length-1))
    end.

store(Url) ->
    {ok, Feeds, Entries} = bgg_feed_utils:all(Url),
    store(Feeds, Entries).

store(_,[]) ->
    ok;
store(Feeds,[Entry|Entries]) ->
    Type=get_type(Entry#entry.link),
    mnesia:dirty_write(entries,#entries{
				  author = Entry#entry.author,
				  duration = Entry#entry.duration,
				  enclosure = Entry#entry.enclosure,
				  id = Entry#entry.id,
				  image = Entry#entry.image,
				  link = Entry#entry.link,
				  subtitle = Entry#entry.subtitle,
				  summary = Entry#entry.summary,
				  title = Entry#entry.title,
				  updated = Entry#entry.updated,
				  type = Type}),
    store(Feeds,Entries).

handle_item(boardgame,EntriesRecord) ->
    Link=binary_to_list(EntriesRecord#entries.link),
    GameUrlPostFix=Link--?BGG_URL,
    [_,Id|_]=string:tokens(GameUrlPostFix,"/"),
						% TODO: check that the item is not added to teh games table yet!
    Url=?BGG_URL++"/xmlapi"++GameUrlPostFix,
    case ets:lookup(games,Id) of
	[] -> new_game(Id, Url, EntriesRecord);
	_ -> ok
    end;
handle_item(_Type,_EntriesRecord) ->
    ok.

new_game(Id, Url, EntriesRecord) ->
    case httpc:request(Url) of
	{error, socket_closed_remotely} ->
	    io:format("socket closed remotely, Url=~p let's wait and try again~n",[Url]),
	    timer:sleep(10000),
	    handle_item(boardgame,EntriesRecord);
	{error,Reason} ->
	    io:format("hhtpc:request returened error with Reason: ~p~n let's wait and try again~n",[Reason]),
	    timer:sleep(10000),
	    handle_item(boardgame,EntriesRecord);
	{ok, {{"HTTP/1.1",200,"OK"},_Options,ResponseBody}} ->
						% {{"HTTP/1.1",200,"OK"},_Options,ResponseBody}=Result,
	    io:format("http request towards ~p got OK ~n",[Url]),
	    {Xml,_}=xmerl_scan:string(ResponseBody),
	    {boardgames,_,[_,Data|_]}=xmerl_lib:simplify_element(Xml),
	    {boardgame,_,Properties}=Data,
	    Mechanics=find_tupples(Properties,boardgamemechanic),
	    Family=find_tupples(Properties,boardgamefamily),
	    Name=find_tupples(Properties,name),
	    YearPublished=find_tupples(Properties,yearpublished),
	    MinPlayers=find_tupples(Properties,minplayers),
	    MaxPlayers=find_tupples(Properties,maxplayers),
	    Publishers=find_tupples(Properties,boardgamepublisher),
	    GameDesigners=find_tupples(Properties,boardgamedesigner),
	    Categories=find_tupples(Properties,boardgamecategory),
	    Types=find_tupples(Properties,boardgamesubdomain),
	    Game=#game{
		    id=Id,
		    name=extractvalue(Name),
		    mechanics=extractvalues(Mechanics),
		    family=extractvalues(Family),
		    yearpublished=extractvalue(YearPublished),
		    minplayers=extractvalue(MinPlayers),
		    maxplayers=extractvalue(MaxPlayers),
		    publishers=extractvalues(Publishers),
		    gamedesigners=extractvalues(GameDesigners),
		    categories=extractvalues(Categories),
		    types=extractvalues(Types)
		   },
	    mnesia:dirty_write(games,Game);
	{ok, {{_,ErrorCode,ErrorReason},_,_}} ->
	    io:format("request towards ~p failed with ErrorCode=~p, ErrorReason=~p~n",
		      [Url,ErrorCode,ErrorReason])
    end.

find_tupples([],_)->
    [];
find_tupples([{Type,OID,Value}|List],Type)  ->
    [{Type,OID,Value}|find_tupples(List,Type)];
find_tupples([_T|List],Type) ->
    find_tupples(List,Type).

extractvalue({_Type,_OID,Value}) ->
    lists:flatten(Value);
extractvalue([]) ->
    undefined;
extractvalue([{Type,OID,Value}|_]) ->
    extractvalue({Type,OID,Value}).
extractvalues([]) ->
    [];
extractvalues([{_Type,_OID,Value}|List]) ->
    [lists:flatten(Value)|extractvalues(List)].

