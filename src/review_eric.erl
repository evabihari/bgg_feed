-module(review_eric).
-export([read/1, read/0]).
-include("../include/record.hrl").

-define(ERIC_2015,"https://boardgamegeek.com/xmlapi/geeklist/174654/speil-2015-review").
%% last year's URL: https://boardgamegeek.com/xmlapi/geeklist/174654/speil-2015-review
-define(ERIC_2016,"https://boardgamegeek.com/xmlapi/geeklist/198728/gen-con-2016-preview").
-define(ESSEN_2016,"https://boardgamegeek.com/xmlapi/geeklist/193588/spiel-2016-preview").
read() ->
    read(?ESSEN_2016).
read(2015)->
    read(?ERIC_2015);
read(2016) ->
    read(?ERIC_2016);
read(Url) ->
    bgg_feed_utils:start_apps(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_booth_table(),
    bgg_feed_utils:create_games_table(),
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
	    store_items(ItemList);
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

store_items([]) ->
    [];
store_items([{<<"item">>,Properties,Body}|ItemList]) ->
    case checkType(Properties, [<<"objecttype">>,<<"subtype">>]) of
	["company","boardgamepublisher"] -> 
	    find_and_store_booth(Properties,Body);
	["thing","boardgame"] ->
	    % check whether game already stored and this is a fresh update or not:
	    [Id,_Name,Update] =  checkType(Properties,[<<"objectid">>,<<"objectname">>,<<"editdate">>]),
	     Last_updated = case mnesia:dirty_read(games,Id) of
		    [] -> 
			"0";	      
		    [GameRec|_] -> 
			GameRec#game.updated	 
		end, 
	    Item_update=bgg_feed_utils:convert_to_timestamp(Update),
	    Game_update=list_to_integer(Last_updated),
	    if ( Item_update > Game_update) -> 
		    io:format("Games table update is needed for ~p~n",[_Name]),
		    find_and_store_game_price(Properties,Body);
	       true -> ok
	    end;
			
	_ -> io:format("will_be_find_later, Properties = ~p~n",[Properties]), 
	    will_be_find_later
    end,
    store_items(ItemList).

create_booth_table()->
    case mnesia:create_table(booths, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, 
                               record_info(fields, booth)},
                              {record_name, booth}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,daily_values}} -> 
            error_logger:info_msg("booths table already_exists");
        Other ->
            error_logger:error_msg("booths table creation failed , 
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

find_and_store_booth(_Properties,[{<<"body">>,[],[]}]) ->
    %% there were no body in the geeklist item 
    ok;
find_and_store_booth(Properties,[{<<"body">>,_,[Body]}|_]) ->
    [Id,Publisher] = checkType(Properties,[<<"objectid">>,<<"objectname">>]),
    Text=binary_to_list(Body),
    Pattern=[226,128,162]++" Booth",
    Location = case string:rstr(Text,Pattern) of
		   0 ->
		       "Not provided";
		   StartingPos -> 
		       St=StartingPos+4,
						%_End=St+length(" X-1234 and Y-45678"),
		       SubStr=string:sub_string(Text,St), 
		       [Booth|_]=string:tokens(SubStr,","),
		       bgg_feed_utils:replace_strange_characters(Booth)
	       end,
    Key=remove_non_ascii(Publisher),
    PubRecord=#booth{key=Key,
		     publisher = Publisher,
		     id = Id,
		     booth = Location},
    mnesia:dirty_write(booths,PubRecord),
    riak_handler:store(Key,PubRecord).

find_and_store_game_price(Properties,[{<<"body">>,[],[]}]) ->
    %% there were no body in the geeklist item, let's set the body to a binary containing and empty string
    find_and_store_game_price(Properties,[{<<"body">>,[],[<<"">>]}]);
find_and_store_game_price(Properties,[{<<"body">>,_,[Body]}|_]) ->
    [Id,_Name] =  checkType(Properties,[<<"objectid">>,<<"objectname">>]),
    Text=binary_to_list(Body),
    Pattern1=[226,128,162]++" Price",
    Pattern2=[226,128,162]++" MSRP",
    Pattern3=[226,128,162]++" Preorder price",

    St1=string:rstr(Text,Pattern1),
    St2=string:rstr(Text,Pattern2),
    St3=string:rstr(Text,Pattern3),
    StartPos = maxvalue([St1,St2,St3],0),
    Offset = case StartPos of
		 St1 -> 5;
		 St3 -> 14;
		 _ -> 0
	     end,
    Price = case StartPos of
		0 ->
		    "Unknown";
		StartingPos ->    
		    St=StartingPos+4,
		    SubStr=string:sub_string(Text,St+Offset), 
		    [PriceText|_]=string:tokens(SubStr,[10,13]),
		    replace_eur(PriceText)
	    end,
    OldRecord = case mnesia:dirty_read(games,Id) of
		    [] -> 
			bgg_feed_utils:new_game(Id),
			case mnesia:dirty_read(games,Id) of
			    [G|_] ->	 G;
			    _ -> #game{id=Id}
			end;	      
		    [GameRec|_] -> 
			GameRec
		end,
    Date=integer_to_list(bgg_feed_utils:to_timestamp(calendar:local_time())),
    bgg_feed_utils:update_game(OldRecord#game{price = Price,
					      updated=Date}).

maxvalue([],Value) ->
    Value;
maxvalue([H|T],Value) when Value>H ->
    maxvalue(T,Value);
maxvalue([H|T],_V) ->
    maxvalue(T,H).


replace_eur(Text) ->
    Pattern=[226,130,172],
    case string:rstr(Text,Pattern) of
	N when N>0 ->
	    re:replace(Text, Pattern, "EUR ", [global, {return, list}]);
	_ ->
	    Text
	end.

remove_non_ascii(List) ->
    lists:filter(fun(X) -> X<128 end, List).
				 
