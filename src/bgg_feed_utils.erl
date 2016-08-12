-module(bgg_feed_utils).

-export([start_apps/0]).
-export([all/1]).
-export([titles/1]).
-export([feed/1]).
-export([entries/1]).
-export([links/1]).
-export([collect_types/1]).
-export([store_entries/1]).
-export([handle_item/2]).
-export([new_items_with_type/2]).
-export([do_logging_async/2]).
-export([create_tables/0,create_games_table/0]).
-export([new_game/1, update_game/1]).
-export([add_price/2]).
-export([replace_strange_characters/1]).
-export([reset_booths_table/0,convert_to_timestamp/1]).
-export([reset_games_table/0]).
-export([print_links/1, print_titles/1]).


-include("../include/record.hrl").

start_apps() ->
    ok = application:ensure_started(inets),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    hackney:start().

all(Url) ->
    {ok, Pid} = supervisor:start_child(bgg_feed_sup, [Url]),
    bgg_feed_parse_hackney:resume(Pid).

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

store_entries(Url) ->
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
    file:delete(File),
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

new_game(Id) ->
    Url= string:concat(?BGG_GAME_URL , Id),
    new_game(Id, Url, []).
new_game(Id, Url, EntriesRecord) ->
    case hackney:request(Url) of
	{error, socket_closed_remotely} ->
	    io:format("socket closed remotely, Url=~p let's wait and try again~n",
		      [Url]),
	    timer:sleep(10000),
	    new_game(Id, Url, EntriesRecord);
	{error,Reason} ->
	    io:format("httpc:request returned error with Reason: ~p~n ",
		      [Reason]);
	{ok, 200,_Header,ClientRef} ->
	    %% io:format("http request towards ~p got OK ~n",[Url]),
	    {ok,ResponseBody}=hackney:body(ClientRef),
	    {Xml,_}=xmerl_scan:string(binary_to_list(ResponseBody)),
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
	    OldGame=case mnesia:dirty_read(games,Id) of
			[] -> #game{id=Id};
			[GameRec|_] -> GameRec
		    end,
	    Game=OldGame#game{
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
		   lang_dependence=Lang_dependence,
		   updated=get_date()
		  },
	    riak_handler:store(Id,Game),
	    mnesia:dirty_write(games,Game),
	    Game;
	{ok,ErrorCode ,Reason, ClientRef} ->
	    io:format("request towards ~p failed with ErrorCode=~p, ErrorReason=~p let's wait and try again~n",
		      [Url,ErrorCode,Reason]),
	    timer:sleep(10000),
	    new_game(Id, Url, EntriesRecord)
    end.

get_date() ->
    integer_to_list(timestamp(erlang:timestamp())).

timestamp({Mega, Secs, Micro}) ->
    Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.

convert_to_timestamp(DateString) ->
    % DateString="Mon, 26 Oct 2015 16:14:32 +0000"
    [_,Day,Month,Year,H,Min,Sec,_]=string:tokens(DateString,", :+"),
    to_timestamp({{list_to_integer(Year),to_month(Month),list_to_integer(Day)},
		  {list_to_integer(H),list_to_integer(Min),list_to_integer(Sec)}}).
    
to_timestamp({{Year,Month,Day},{Hours,Minutes,Seconds}}) ->
    (calendar:datetime_to_gregorian_seconds({{Year,Month,Day},{Hours,Minutes,Seconds}})
	- 62167219200)*1000000.

to_month("Jan")->
    1;
to_month("Feb") ->
    2;
to_month("Mar") ->
    3;
to_month("Apr") ->
    4;
to_month("May") ->
    5;
to_month("Jun") ->
    6;
to_month("Jul") ->
    7;
to_month("Aug") ->
    8;
to_month("Sep") ->
    9;
to_month("Oct") ->
    10;
to_month("Nov") ->
    11;
to_month("Dec") ->
    12.

update_game(Game) ->
    Id=Game#game.id,
    riak_handler:store(Id,Game),
    mnesia:dirty_write(games,Game).

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
    case List of
	[] -> lists:flatten(Value);
	_ ->
	    [lists:flatten(Value)," , "|extractvalues(List)]
    end.

find_poll([],_) ->
    [];
find_poll([{poll,[{name,Type},{title,_Title},{totalvotes,TotalVotes}],Result}|_List],
	  Type) ->
    {{totalvotes,list_to_integer(TotalVotes)},Result};
find_poll([T|List],Type) when is_atom(Type) ->
    find_poll([T|List],atom_to_list(Type));
find_poll([_T|List],Type) ->
    find_poll(List,Type).

categorize({{totalvotes,0},_ResultList}) -> "0";
categorize({{totalvotes,TotalVotes},ResultList}) when is_list(TotalVotes) ->
    categorize({{totalvotes,list_to_integer(TotalVotes)},ResultList});
categorize({{totalvotes,TotalVotes},ResultList}) ->
    [_,R2|_R3]=ResultList,
    {results,[],SubRList}=R2,
    VoteList=extract(SubRList,TotalVotes),
    {MaxLevel,_MaxText,_MaxValue}=find_maximum(VoteList,{0,"N/A",0}),
    MaxLevel. 

extract([],_) ->
    [];
extract([{result,Result,_}|List],TotalVotes) ->
    [extractResult(Result,TotalVotes)|extract(List,TotalVotes)];
extract([_H|List],TotalVotes) ->
    extract(List,TotalVotes).

extractResult([{level,Level},{value,Value},{numvotes,Num}],TotalVotes)->
    {Level, Value,list_to_integer(Num)/TotalVotes*100}.

find_maximum([],{MaxLevel,MaxText,MaxValue}) ->
    {MaxLevel,MaxText,MaxValue};
find_maximum([{Level,Text,Percent}|List],{_MaxLevel,_MaxText,MaxValue}) 
  when Percent > MaxValue ->
    find_maximum(List,{Level,Text,Percent});  
find_maximum([_H|List],{MaxLevel,MaxText,MaxValue}) ->
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
	    error_logger:info_msg("entries table already_exists~n");
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
	    error_logger:info_msg("games table already_exists~n");
	Other ->
	    error_logger:error_msg("games table creation failed , reason = ~p~n",[Other])
    end.

write_header(_File,[]) ->
    ok;
write_header(File,[H|T]) ->
    file:write_file(File,io_lib:format("~90.90p ",[H]),[append]),
    case T of
        [] ->  file:write_file(File,io_lib:format("~n ",[]),[append]);
        _ ->  file:write_file(File,io_lib:format("; ",[]),[append]),
              write_header(File, T)
    end.

write_record(File,Record,Size) ->
    write_field(File,Record,2,Size).

write_field(File,Record,Size,Size) ->
    Value=erlang:element(Size,Record),
    write_element(File,Value),
    file:write_file(File,io_lib:format("~n",[]),[append]);
write_field(File,Record,N,Size) ->
    Value=erlang:element(N,Record),
    write_element(File,Value),
    file:write_file(File,io_lib:format("; ",[]),[append]),
    write_field(File,Record,N+1,Size).

write_element(File,Element) ->
    Value=case string:to_integer(Element) of
	      {V,[]} -> V;
	      _ -> lists:flatten(Element)
	  end,
    file:write_file(File,io_lib:format("~p",[Value]),[append]).

add_price(Game_id,PriceStr) when is_integer(Game_id)->
    add_price(integer_to_list(Game_id),PriceStr);
add_price(Game_id,PriceStr) ->
    case read_game(Game_id) of
	[] ->
	    io:format("Game not found Game_id=~p~n",[Game_id]);
	[Game|_] ->
	    NewPrice = case get_price_string(Game#game.price) of
			   "" ->
			       PriceStr;
			   Other ->
			       Other ++ " , "++PriceStr
		 end,
             update_game(Game#game{price=NewPrice})
     end.

get_price_string([]) ->
    "";
get_price_string("Unknown") ->
    "";
get_price_string("not known yet") ->
    "";
get_price_string("undefined") ->
    "";
get_price_string(Other) ->
    Other.

read_game(Game_id) when is_integer(Game_id) ->
    read_game(integer_to_list(Game_id));
read_game(Game_id) ->
    case mnesia:dirty_read(games,Game_id) of 
	    [] -> case riak_handler:read(game,Game_id) of
		      [] -> [];
		      {ok,GameR} -> [GameR]
		  end;
	    [Game|_] -> Game
    end.
   
replace_strange_characters(Text) ->
   Bullet={[226,130,172]," -.- "},
   Bullet2={[226,128,162]," -.- "},
   EM_Dash={[226,128,148], " - "}, 
   Latin_small={[195,175], "A"},
   Euro={[226,130,172],"EUR "},
  replacement(Text,[Bullet,EM_Dash,Latin_small,Euro, Bullet2]).

replacement(Text,[]) ->
    Text;
replacement(Text,[{In,Out}|Patterns]) ->
    T1=case string:rstr(Text,In) of
	N when N>0 ->
	    re:replace(Text, In, Out, [global, {return, list}]);
	_ ->
	    Text
       end,
    replacement(T1,Patterns).

		    
reset_booths_table()->
    reset_table(booth,mnesia:dirty_all_keys(booths)).

reset_table(_Type,[])->
    ok;
reset_table(Type,[Key|Keys]) ->
    riak_handler:delete(Type,Key),
    mnesia:dirty_delete(get_table_name(Type),Key),
    reset_table(Type,Keys).

get_table_name(booth) ->
    booths;
get_table_name(game) ->
    games.

reset_games_table()->
    BinKeys=riak_handler:list_keys(game),
    Keys=lists:map(fun(X) -> binary_to_list(X) end, BinKeys),
    reset_table(game,Keys).

print(L) ->
    lists:foreach(fun (Title) ->
			  io:format("~ts~n", [Title])
		  end, L),
    ok.

print_titles(Url) ->
    Titles= titles(Url),
    print(Titles).

print_links(Url) ->
    print(links(Url)).

