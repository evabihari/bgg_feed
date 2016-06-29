-module(bgg_feed_utils).

-export([all/1]).
-export([titles/1]).
-export([feed/1]).
-export([entries/1]).
-export([links/1]).
-export([collect_types/1]).
-export([store/1]).
-export([handle_item/2]).
-export([new_items_with_type/2]).
-export([do_logging_async/2]).
-export([create_tables/0]).


-include("../include/record.hrl").

all(Url) ->
    {ok, Pid} = supervisor:start_child(bgg_feed_sup, [Url]),
    bgg_feed_parse:resume(Pid).

titles(Url) ->
    [feeder_entries:get(title, Entry) || Entry <-  entries(Url)].

feed(Url) ->
    {ok, Feed, _} = all(Url),
    Feed.

entries(Url) ->
    {ok, _, Entries} = all(Url),
    Entries.

links(Url) ->
    [feeder_entries:get(link, Entry) || Entry <-  entries(Url)].

collect_types(Url) ->
    collect_types(links(Url),[]).

store(Url) ->
    {ok, Feeds, Entries} = all(Url),
    store(Feeds, Entries).

new_items_with_type(Type,Url) ->
    Entries=entries(Url),
    lists:flatten(lists:map(fun ( Entry) ->
				    filter_by_prefix(Type,feeder_entries:get(link, Entry))
			    end, Entries)).

create_tables() ->
    create_entries_table(),
    create_games_table().

do_logging_async(File, EtsAsList) ->
    Fields=record_info(fields,game),
    write_header(File,Fields),
    F = fun(Record) ->
		write_record(File,Record,length(Fields))
	end,
    lists:foreach(F,EtsAsList).

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
	    io:format("socket closed remotely, Url=~p let's wait and try again~n",
		      [Url]),
	    timer:sleep(10000),
	    handle_item(boardgame,EntriesRecord);
	{error,Reason} ->
	    io:format("hhtpc:request returened error with Reason: ~p~n let's
 wait and try again~n",[Reason]),
	    timer:sleep(10000),
	    handle_item(boardgame,EntriesRecord);
	{ok, {{"HTTP/1.1",200,"OK"},_Options,ResponseBody}} ->
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
	    Lang_dependence=categorize(find_poll(Properties,language_dependence)),
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
		    types=extractvalues(Types),
		    lang_dependence=Lang_dependence
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

find_poll([],_) ->
    [];
find_poll([{poll,[{name,Type},{title,_Title},{totalvotes,TotalVotes}],Result}|_List],
	  Type) ->
    {{totalvotes,TotalVotes},Result};
find_poll([T|List],Type) when is_atom(Type) ->
    find_poll([T|List],atom_to_list(Type));
find_poll([_T|List],Type) ->
    find_poll(List,Type).

categorize({{totalvotes,0},_ResultList}) -> 0;
categorize({{totalvotes,TotalVotes},ResultList}) ->
    [_,R2|_R3]=ResultList,
    {results,[],SubRList}=R2,
    VoteList=extract(SubRList,list_to_integer(TotalVotes)),
    {MaxLevel,_MaxText,_MaxValue}=find_maximum(VoteList,{0,"N/A",0}),
    MaxLevel. 

extract([],_) ->
    [];
extract([{result,Result,_}|List],TotalVotes) ->
    [extractResult(Result,TotalVotes)|extract(List,TotalVotes)];
extract([H|List],TotalVotes) ->
    extract(List,TotalVotes).

extractResult([{level,Level},{value,Value},{numvotes,Num}],TotalVotes)->
    {Level, Value,list_to_integer(Num)/TotalVotes*100}.

find_maximum([],{MaxLevel,MaxText,MaxValue}) ->
    {MaxLevel,MaxText,MaxValue};
find_maximum([{Level,Text,Percent}|List],{MaxLevel,MaxText,MaxValue}) 
  when Percent > MaxValue ->
    find_maximum(List,{Level,Text,Percent});  
find_maximum([H|List],{MaxLevel,MaxText,MaxValue}) ->
    find_maximum(List,{MaxLevel,MaxText,MaxValue}).
    
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
    write_field(File,Record,2,Size).

write_field(File,Record,Size,Size) ->
    file:write_file(File,io_lib:format("~p~n",[erlang:element(Size,Record)]),[append]);
write_field(File,Record,N,Size) ->
    file:write_file(File,io_lib:format("~p, ",[erlang:element(N,Record)]),[append]),
    write_field(File,Record,N+1,Size).
