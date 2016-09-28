-module(math_trade).
-export([read/1, read/0]).
-export([find_info/2]).
-export([dump_to_file/1]).
-include("../include/record.hrl").

%-define(ESSEN_MT_2016,"https://boardgamegeek.com/xmlapi/geeklist/214426/essen-spiel-2016-no-shipping-leftovers-math-trade").
-define(ESSEN_MT_2016,"https://boardgamegeek.com/xmlapi/geeklist/214726/essen-spiel-2016-no-shipping-leftovers-leftovers-m").

-define(OUT,"LeftOvers_LeftOver.csv").
-define(DROPBOX_PUBLIC,"~/Dropbox/Public/").
-define(STRANGE_CHR_REPLACEMENTS,[{[226,130,172]," EUR"},{[195,175],"i"}, {[195,188],"ü"},{[195,132],"A"},
				  {[195,136],"E"},{[195,32],"a "},{[195,164],"a"},{[195,169],"é"},{[195,182],"ö"},
				  {[195,150],"O"}]).

% {[226,128,156],""},{[226,128,157],""},{"[/size]",""}
% {[226,128,153],""},{[226,128,147],""}

-define(REMOVE_STRANGE_CHARS,[":","[b]","[/b]","\r","[u]","[/u]","[B]","[/B]",";",[239,187,191] ] ).
read() ->
    read(?ESSEN_MT_2016).
read(Url) ->
    bgg_feed_utils:start_apps(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_math_trade_table(),
    bgg_feed_utils:create_games_table(),
    io:format(" hackney:request, Url=~p~n",[Url]),
    case hackney:request(Url) of
	{ok,200,_Head,Ref} ->
	    {ok,Body} = hackney:body(Ref),
	    {ok,F} = file:open("eric2016.txt",[read,write]),
	    file:write(F,Body),
	    Struct=mochiweb_html:parse(Body),
	    {ok,F2} = file:open("structs.txt",[read,write]),
	    file:write(F2,io_lib:print(Struct)),
	    Path="geeklist/item",
	    ItemList=mochiweb_xpath:execute(Path,Struct),
	    {ok,F3} = file:open("itemss.txt",[read,write]),
	    file:write(F3,io_lib:print(ItemList)),
	    store_items(ItemList,1),
	    dump_to_file(?OUT),
	    Cmd="cp "++ ?OUT ++ " "++ ?DROPBOX_PUBLIC ++ ".",
	    os:cmd(Cmd);
	{ok, 202,_Head,_Ref} ->
	    io:format("request accepted, wait a bit,",[]),
	    timer:sleep(10000),
	    read(Url);
	{error, closed} ->
	    %let'ss try agin ?
	    io:format("request failed, let's try again~n",[]),
	    timer:sleep(10000),
	    read(Url);	    
	Result -> io:format("Http request failed, Result=~p~n",[Result])
    end.

store_items([],_Nr) ->
    [];
store_items([{<<"item">>,Properties,Body}|ItemList],Nr) ->
    case checkType(Properties, [<<"objecttype">>,<<"subtype">>]) of
	["thing","boardgame"] ->
	    % check whether game already stored and this is a fresh update or not:
	    [Id,Name] =  checkType(Properties,[<<"objectid">>,<<"objectname">>]),
	    case mnesia:dirty_read(math_trade_items,Nr) of
		[] ->
	       
		    MTI=fill_other_item_info(#math_trade_item{item_no=Nr,
						  id=Id,
						  title=Name},
					     Properties,
					     Body),
		    mnesia:dirty_write(math_trade_items,MTI),
		    %% io:format("MTI=~p~n",[MTI]),
		    riak_handler:store(integer_to_list(Nr),MTI);
		[_MTI|_] ->
		    % already checked, don't need to repeat 
		    ok
	    end;
	["thing",Other] ->
	    % out_of_scope_for_now
	    io:format("not a boardgame, rather ~p~n",[Other]);
	_ -> io:format("will_be_find_later, Properties = ~p~n",[Properties]), 
	    will_be_find_later
    end,
    store_items(ItemList,Nr+1).

create_math_trade_table()->
    case mnesia:create_table(math_trade_items, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, 
                               record_info(fields, math_trade_item)},
                              {record_name,  math_trade_item}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,math_trade_items}} -> 
	    mnesia:delete_table(math_trade_items),
	    create_math_trade_table();
        Other ->
            error_logger:error_msg("math_trade_items table creation failed , 
                                    reason = ~p~n",[Other])
    end.

checkType(_Properties,[]) ->
    [];
checkType(Properties,[Key|List]) ->
    case lists:keyfind(Key, 1, Properties) of
	{Key, Result} ->
	    [binary_to_list(Result)|checkType(Properties,List)];
	_ ->[not_found|checkType(Properties,List)]
    end.

find_info_opts(_S,[]) ->
    "";
find_info_opts(String,[Pattern|PList]) ->
    case find_info(String,Pattern) of
	"" ->
	    find_info_opts(String,PList);
	Result ->
	    Result
    end.

find_info(String,Pattern) ->
    RegExp=string:concat(string:concat("^.*(",Pattern),").*$"),
    Res=case re:run(String,RegExp,[multiline,{capture,all,list},caseless]) of
	nomatch ->
	    "";
	{match,[Result|_Other]} ->
	    R1=re:replace(Result,Pattern,"", [{return, list}, global]),
	    re:replace(R1,[226,128,147],"-", [{return, list}, global]);
	_ ->
	    ""
	end,
    %Res1=replace_strange_chars(Res , ?STRANGE_CHR_REPLACEMENTS),
    Res1=Res,
    remove_special_str(Res1,?REMOVE_STRANGE_CHARS).
    
remove_strs(String,[]) ->
    String;
remove_strs(String,[SubStr|List]) ->
    remove_strs(remove_str(String,SubStr),List).

remove_str(String,SubStr) ->
    re:replace(String,SubStr,"", [{return, list}, global]).

get_player_no("23953") ->
    % Outside the scope of BGG
    {"0","0"};
get_player_no(Id) ->
    case is_it_known(Id) of
	dontknow ->
	    % ask_bgg(Id);
	    % add the game to teh Games table -> next time no http request will be needed!
	    Game=bgg_feed_utils:new_game(Id),
	    {Game#game.minplayers,Game#game.maxplayers};
	Other ->
	    Other
     end.

fill_other_item_info(MTI,Properties,[{<<"body">>,[],[]}]) ->
    %% there were no body in the geeklist item, 
    %% let's set the body to a binary containing and empty string
    fill_other_item_info(MTI,Properties,[{<<"body">>,[],[<<"">>]}]);
fill_other_item_info(MTI,_Properties,[{<<"body">>,_,[Body]}|_]) ->
    Text=binary_to_list(Body),
    Publisher=find_info(Text,"Publisher:"),
    Language=find_info(Text,"Language:"),
    Condition=find_info(Text,"Condition:"),
    Attendance1=find_info_opts(Text,["Attendance","At Spiel","att. Spiel","Attending","Presence in Essen","Available in Essen","In Essen",
				     "Presence at Essen","At the fair","Presence","Essen:","Att. Spiel"]),
    Attendance=remove_strs(Attendance1,["at SPIEL'16 ","at SPIEL ","Presence ","at SPIEL'16"," '16","at SPIEL'15","Attendence in Essen ",
					"on the fair ","at Spiel'16 ","at SPIEL16","Att. Spiel "," at Spiel","  in Essen "]),
    {Min_player,Max_player}=case  MTI#math_trade_item.item_no of
	N when N>0 ->
	    get_player_no(MTI#math_trade_item.id);
	_ -> {"0","0"}
    end,
    Title=MTI#math_trade_item.title,
    Title1 = case MTI#math_trade_item.id of
	"23953" -> % Outside the Scope of BGG
	    AltName=remove_special_str(find_info(Text,"altname"),["altname","++","--","[/size]"]),
	    replace_strange_chars(AltName,?STRANGE_CHR_REPLACEMENTS);
	 _ -> replace_strange_chars(Title,?STRANGE_CHR_REPLACEMENTS)
    end,
    MTI#math_trade_item{title=Title1,
                        publisher=replace_strange_chars(Publisher,?STRANGE_CHR_REPLACEMENTS),
			language=Language,
			attendance=Attendance,
			min_player=Min_player,
			max_player=Max_player,
			condition=Condition}.

replace_strange_chars(String,[]) ->
    String;
replace_strange_chars(String,[{K,V}|List]) ->
    Res= re:replace(String,K,V,[{return, list}, global]),
    replace_strange_chars(Res,List).

remove_non_ascii(List) ->
    lists:filter(fun(X) -> X<128 end, List).
				 


is_it_known(Id) ->
    case mnesia:dirty_match_object(games,#game{_='_', id=Id}) of
	[]
	   ->
	    dontknow;
	[Game|_]->
	    {Game#game.minplayers,Game#game.maxplayers}
     end.

remove_special_str(Text,List) when is_binary(Text) ->
    remove_special_str(binary_to_list(Text),List);
remove_special_str(Text,[]) ->
    Text;
remove_special_str(Text,[SubStr|StrList]) ->
    Pos=string:str(Text,SubStr),
    T1 = case Pos of
	   0 -> Text;
	   N -> L=string:left(Text,N-1),
		R=string:substr(Text,N+length(SubStr)),	
		L++R
       end,
    remove_special_str(T1,StrList).
    
dump_to_file(FileName) ->
    bgg_feed_utils:dump_to_file(math_trade_items,FileName).
