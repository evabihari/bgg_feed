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
    case httpc:request(Url) of 
	{ok, {{"HTTP/1.1",200,"OK"},
	      _Head,
	      Body}} ->
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
	{ok, {{"HTTP/1.1",202,"Accepted"},
	      _Head,
	      _Body}} ->
	    io:format("request accepted, wait a bit,",[]),
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
	    find_and_store_game_price(Properties,Body);
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
		       Booth
	       end,
    PubRecord=#booth{publisher = Publisher,
		     id = Id,
		     booth = Location},
    mnesia:dirty_write(booths,PubRecord),
    riak_handler:store(Publisher,PubRecord).

find_and_store_game_price(Properties,[{<<"body">>,[],[]}]) ->
    %% there were no body in the geeklist item, let's set the body to a binary containing and empty string
    find_and_store_game_price(Properties,[{<<"body">>,[],[<<"">>]}]);
find_and_store_game_price(Properties,[{<<"body">>,_,[Body]}|_]) ->
    [Id,Name] =  checkType(Properties,[<<"objectid">>,<<"objectname">>]),
    Text=binary_to_list(Body),
    Pattern1=[226,128,162]++" Price",
    Pattern2=[226,128,162]++" MSRP",

    St1=string:rstr(Text,Pattern1),
    St2=string:rstr(Text,Pattern2),
    StartPos = max(St1,St2),
    Offset = case StartPos of
		 St1 -> 5;
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
    bgg_feed_utils:update_game(OldRecord#game{price = Price}).


replace_eur(Text) ->
    Pattern=[226,130,172],
    case string:rstr(Text,Pattern) of
	N when N>0 ->
	    re:replace(Text, Pattern, "EUR ", [global, {return, list}]);
	_ ->
	    Text
	end.
