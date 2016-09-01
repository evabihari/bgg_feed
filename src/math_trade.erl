-module(math_trade).
-export([read/1, read/0]).
-include("../include/record.hrl").

-define(ESSEN_MT_2016,"https://boardgamegeek.com/xmlapi/geeklist/212782/essen-spiel-2016-no-shipping-math-trade").
read() ->
    read(?ESSEN_MT_2016).
read(Url) ->
    bgg_feed_utils:start_apps(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_math_trade_table(),
%    create_booth_table(),
%    bgg_feed_utils:create_games_table(),
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
	    store_items(ItemList,1);
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
		    io:format("id=~p title=~p~n",
			      [MTI#math_trade_item.id,MTI#math_trade_item.title]),
		    mnesia:dirty_write(math_trade_items,MTI),
		    io:format("MTI=~p~n",[MTI]),
		    riak_handler:store(integer_to_list(Nr),MTI);
		[MTI|_] ->
		    % already checked, don't need to repeat 
		    ok
	    end;
	["thing",Other] ->
	    % out_of_scope_for_now
	    io:format("not a boardgame, rather ~p",[Other]);
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
        {aborted,{already_exists,daily_values}} -> 
            error_logger:info_msg("math_trade_items table already_exists");
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

find_info(String,Pattern) ->
    RegExp=string:concat(string:concat("^.*(",Pattern),").*$"),
    case re:run(String,RegExp,[multiline,{capture,all,list},caseless]) of
	nomatch ->
	    "";
	{match,[Result|_Other]} ->
	    Result;
	_ ->
	    ""
    end.

ask_bgg(Id) ->
    Url= string:concat(?BGG_GAME_URL , Id),
    case hackney:request(Url) of
	{error, socket_closed_remotely} ->
	    io:format("socket closed remotely, Url=~p let's wait and try again~n",
		      [Url]),
	    timer:sleep(10000),
	    ask_bgg(Id);
	{error, closed} ->
	    %REST API overloaded, let's wait a bit
	    io:format("hackney returned closed, Url=~p let's wait and try again~n",
		      [Url]),
	    timer:sleep(50000),
	    ask_bgg(Id);
	{error,bad_request} ->
	    %REST API overloaded, let's wait a bit
	    io:format("httpc:request returned error with bad_request, url: ~p~n ",
		      [Url]),
	    timer:sleep(10000),
	    ask_bgg(Id);
	{error,Reason} ->
	    io:format("httpc:request returned error with Reason: ~p, url: ~p~n ",
		      [Reason,Url]);
	{ok, 200,_Header,ClientRef} ->
	    {ok,ResponseBody}=hackney:body(ClientRef),
	    {Xml,_}=xmerl_scan:string(binary_to_list(ResponseBody)),
	    {boardgames,_,[_,Data|_]}=xmerl_lib:simplify_element(Xml),
	    {boardgame,_,Properties}=Data,
	    bgg_feed_utils:extractvalue(bgg_feed_utils:find_tupples(Properties,minplayers));
	{ok,ErrorCode ,Reason, _ClientRef} ->
	    io:format("request towards ~p failed with ErrorCode=~p, ErrorReason=~p let's wait 
                       and try again~n",
		      [Url,ErrorCode,Reason]),
	    timer:sleep(10500),
	    ask_bgg(Id)
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
    Attendance=find_info(Text,"Attendance"),
    Min_player=ask_bgg(MTI#math_trade_item.id),
    MTI#math_trade_item{publisher=Publisher,
			language=Language,
			attendance=Attendance,
			min_player=Min_player,
			condition=Condition}.


remove_non_ascii(List) ->
    lists:filter(fun(X) -> X<128 end, List).
				 



