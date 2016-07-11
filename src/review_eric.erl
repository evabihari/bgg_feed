-module(review_eric).
-export([read/1, read/0]).

-record(booth, {
  publisher :: undefined | binary(),
  id :: undefined | binary(),
  booth :: undefined | list()
}).

-define(ERIC_2015,"https://boardgamegeek.com/xmlapi/geeklist/174654/speil-2015-review").
%% last year's URL: https://boardgamegeek.com/xmlapi/geeklist/174654/speil-2015-review

read() ->
    read(?ERIC_2015).
read(Url) ->
    ok = application:ensure_started(inets),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_booth_table(),
    case httpc:request(Url) of 
	{ok, {{"HTTP/1.1",200,"OK"},
	      _Head,
	      Body}} ->
		{ok,F} = file:open("eric2015.txt",[read,write]),
		file:write(F,Body),
		Struct=mochiweb_html:parse(Body),
		{ok,F2} = file:open("structs.txt",[read,write]),
		file:write(F2,io_lib:print(Struct)),
	        Path="geeklist/item",
	        ItemList=mochiweb_xpath:execute(Path,Struct),
	        {ok,F3} = file:open("itemss.txt",[read,write]),
		file:write(F3,io_lib:print(ItemList)),
	        store_booths(ItemList);
	Result -> io:format("Http request failed, Result=~p~n",[Result])
    end.

store_booths([]) ->
    [];
store_booths([{<<"item">>,Properties,Body}|ItemList]) ->
    case checkType(Properties, [<<"objecttype">>,<<"subtype">>]) of
	[<<"company">>,<<"boardgamepublisher">>] -> find_and_store_booth(Properties,Body);
	_ -> will_be_find_later
    end,
    store_booths(ItemList).

		
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
	    [Result|checkType(Properties,List)];
	_ ->[not_found|checkType(Properties,List)]
    end.

find_and_store_booth(Properties,[{<<"body">>,_,[Body]}|_]) ->
    [Id,Publisher] = checkType(Properties,[<<"objectid">>,<<"objectname">>]),
    Text=binary_to_list(Body),
    Pattern="Booth ",
    Location = case string:rstr(Text,Pattern) of
		   0 ->
		       "Not provided";
		   StartingPos -> 
		       St=StartingPos+length(Pattern),
		       End=St+length(" X-1234"),
		       SubStr=string:sub_string(Text,St,End), 
		       [Booth|_]=string:tokens(SubStr,","),
		       Booth
	       end,
    mnesia:dirty_write(booths,#booth{publisher = Publisher,
				     id = Id,
				     booth = Location}).
