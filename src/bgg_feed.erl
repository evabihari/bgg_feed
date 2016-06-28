-module(bgg_feed).

-export([print_links/1]).
-export([print_titles/1]).
-export([start/0]).
-export([stop/0]).
-export([new_items_with_type/2]).
-export([get_new_items/1]).
-export([loop/1, run/1, run/0]).
-export([export_games_to_file/1]).

-include("../include/record.hrl").

start() ->
    ok = application:ensure_started(inets),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    mnesia:create_schema([node()]),
    mnesia:start(),
    %% create_feed_table(),
    create_entries_table(),
    create_games_table(),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

print(L) ->
    lists:foreach(fun (Title) ->
			  io:format("~ts~n", [Title])
		  end, L),
    ok.

print_titles(Url) ->
    print(bgg_feed_utils:titles(Url)).

print_links(Url) ->
    print(bgg_feed_utils:links(Url)).

new_items_with_type(Type,Url) ->
    Entries=bgg_feed_utils:entries(Url),
    lists:flatten(lists:map(fun ( Entry) ->
				    filter_by_prefix(Type,feeder_entries:get(link, Entry))
			    end, Entries)).


filter_by_prefix(Type,Link) ->
    Prefix=prefix(Type),
    case string:str(binary_to_list(Link), Prefix) of
	0 -> [];
	_ -> Link
    end.

prefix(boardgame) ->
    ?BGG_URL++"/boardgame/";
prefix(video) ->
    ?BGG_URL++"/video/";
prefix(article) ->
    ?BGG_URL++"/article/";
prefix(Atom) ->
    ?BGG_URL++"/"++atom_to_list(Atom)++"/".


create_entries_table() ->
    case mnesia:create_table(entries,
			     [{disc_copies,[node()]},
			      {type, ordered_set},
			      {index, [id,updated]},
			      {attributes, record_info(fields,entries)},
			      {record_name,entries}]) of
	{atomic, ok} -> ok;
	{aborted,{already_exists,entries}} ->
	    error_logger:info_msg("entries table already_exists");
	Other ->
	    error_logger:error_msg("entries table creation failed , reason = ~p~n",[Other])
    end.

create_games_table() ->
    case mnesia:create_table(games,
			     [{disc_copies,[node()]},
			      {type, ordered_set},
						%{index, [id]},
			      {attributes, record_info(fields,game)},
			      {record_name,game}]) of
	{atomic, ok} -> ok;
	{aborted,{already_exists,games}} ->
	    error_logger:info_msg("games table already_exists");
	Other ->
	    error_logger:error_msg("games table creation failed , reason = ~p~n",[Other])
    end.

get_new_items(Type) ->
    MatchHead=#entries{link='$1', title='$2',type=Type,_='_'},
						%Title='$2',
    Results=mnesia:dirty_select(entries,[{MatchHead,[],['$_']}]),
    lists:foreach(
      fun(X) ->
	      _T=X#entries.title,
	      _L=X#entries.link,
						% io:format("name:~25s ,   link:~s~n",[T,L]),
	      handle_item(Type, X)
      end,
      Results).

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

run() ->
    run(?BGG_RSS_FEED).
run(Url) ->
    bgg_feed_utils:all(Url),
    Pid=spawn(?MODULE,loop,[Url]),
    register(run,Pid).

loop(Url) ->
    receive
	stop -> io:format("run loop has been stopped",[])
    after
	300000 ->
	    io:format("Timeout, games table size=~p~n",[mnesia:table_info(games,size)]),
	    bgg_feed_utils:store(Url),
	    get_new_items(boardgame),
	    loop(Url)
    end.

do_logging_async(File, EtsAsList) ->
    Fields=record_info(fields,game),
    write_header(File,Fields),
    F = fun(Record) ->
		write_record(File,Record,length(Fields))
	end,
    lists:foreach(F,EtsAsList).

export_games_to_file(File) ->
    do_logging_async(File, ets:tab2list(games)).

write_header(_File,[]) ->
    ok;
write_header(File,[H|T]) ->
    file:write_file(File,io_lib:format("~p ",[H]),[append]),
    case T of
        [] ->  file:write_file(File,io_lib:format("~n ",[]),[append]);
        _ ->  file:write_file(File,io_lib:format(", ",[]),[append]),
              write_header(File, T)
    end.

write_record(File,Record,Size) ->
    write_field(File,Record,1,Size).

write_field(File,Record,Size,Size) ->
    file:write_file(File,io_lib:format("~p~n",[erlang:element(Size+1,Record)]),[append]);
write_field(File,Record,N,Size) ->
    file:write_file(File,io_lib:format("~p, ",[erlang:element(N+1,Record)]),[append]),
    write_field(File,Record,N+1,Size).
