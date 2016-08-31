%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 19 Aug 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(bgg_to_airtable).

%% API
-export([game_to_airtable/1]).
-export([publisher_to_airtable/1]).
-export([update_publishers/0]).
-export([find_pictures/1]).
-export([replace_8211/1]).


-include("../include/record.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% Get Game data from BGG and store them to the Airtable Table
game_to_airtable(Id) ->
    Game = case mnesia:dirty_read(games,Id) of
	       [] ->
		   G=bgg_feed_utils:new_game(Id),
		   riak_handler:store(Id,G),
		   mnesia:dirty_write(games,G),
		   G;
	       [G|_] -> G
	   end,
    AT=airtable:init(),
    io:format("game_to_airtable Id=~p, Game=~p ~n",[Id,Game]),
    Payload=create_payload(game,AT,Game),
    case airtable:find(AT,"Games","id",Id) of 
	{ok,[]} ->
	    {ok,Result}=airtable:create(AT,"Games",Payload),
	    {ok, {added,Result}};
	{ok,[ATR|_]} ->
	    AT_id=ATR#airtable_record.id,
	    {ok,Result}=airtable:update(AT,"Games",AT_id,Payload),
	    {ok, {updated,Result}}

    end.

publisher_to_airtable(Name) ->
    Key=remove_non_ascii(Name),
    case mnesia:dirty_read(booths,Key) of
		[] ->
		    ok;
		[BR|_Tail] ->
	            AT=airtable:init(),
		    Payload=create_payload(booth,AT,BR),
		    case airtable:find(AT,"Publishers","Name",Name) of 
			  {ok,[]} ->
			    {ok,Result}=airtable:create(AT,"Publishers",Payload),
			    {ok, {added,Result}};
			{ok,[ATR|_]} ->
			    AT_id=ATR#airtable_record.id,
			    {ok,Result}=airtable:update(AT,"Publishers",AT_id,Payload),
			    {ok, {updated,Result}}
		    end
    end.
		    
update_publishers()->
    PList=ets:tab2list(booths),
    update_publishers(PList).

update_publishers([]) ->
    ok;
update_publishers([BR|List]) ->
    io:format("Let's update ~p~n",[BR#booth.publisher]),
    publisher_to_airtable(BR#booth.publisher),
    update_publishers(List).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_payload(game,AT,Games) when is_record(Games, game) ->
    Publishers =find_links(AT,"Publishers", Games#game.publishers),    
    Designers = find_links(AT,"Designers", Games#game.gamedesigners),
    Artists = find_links(AT,"Artists", Games#game.artists),
    Mechanics = find_links(AT,"Mechanics", Games#game.mechanics),
    Families = find_links(AT,"Families", Games#game.family),
    Categories = find_links(AT,"Categories", Games#game.categories),
    Url = find_pictures(Games#game.id),

    PL= [{"id",Games#game.id},
	 {"name",replace_8211(Games#game.name)},
	 {"yearpublished",Games#game.yearpublished}, 
	 {"price",Games#game.price},
	 {"minplayers",Games#game.minplayers},
	 {"maxplayers",Games#game.maxplayers},
	 {"lang_dependence",Games#game.lang_dependence},
	 {"updated",Games#game.updated},
	 {"link_to_BGG",string:concat("https://boardgamegeek.com/boardgame/" , 
				      Games#game.id)}]++
	add_attachment(Url)++
	add_if_exist([{"publishers",Publishers},{"gamedesigners",Designers},
		     {"mechanics",Mechanics},{"family",Families},
		     {"categories",Categories},{"artists",Artists}]),

%% "attachments": [
%%       {
%%         "url": "https://dl.airtable.com/pMhZ1X5jSPmYOJHaiMhg_pic2786349_md.jpg"
%%       }
%%     ],
    airtable_record:encode(PL);
create_payload(linked,_AT,Name) ->
    PL=[{"Name",Name}],
    airtable_record:encode(PL);
create_payload(booth,_AT,BR) ->
    PL1=[{"Name",BR#booth.publisher},
	{"Publisher_id",BR#booth.id}],
    PL2=case string:tokens(BR#booth.booth,"\r\n") of
	   [] -> [];
	   [S|_] ->[{"Booth",S}];
	   _O -> []
     end,
     airtable_record:encode(PL1++PL2).


find_links(AT,Table,Values) ->
    S=lists:flatten(Values),
    List=string:tokens(S,","),
    Links=find_link_values(AT,Table,List),
    case Links of
	[] ->
	    [];
	Link_list ->
	    sets:to_list(sets:from_list(Link_list))
    end.

find_link_values(_AT,_Table,[]) ->
    [];
find_link_values(AT,Table,[H|Tail]) ->
    Value=string:strip(escape_what_must(H)),
    case airtable:find(AT,Table,"Name",edoc_lib:escape_uri(Value)) of
	{ok,[]} ->
	    case linked_item_to_create(AT,Table,Value) of
		[] ->
		    find_link_values(AT,Table,Tail);
		Id ->
		    [Id|find_link_values(AT,Table,Tail)]

	    end;
	{ok,[ATR|_]} ->
	    ATR_id=ATR#airtable_record.id,
	    [ATR_id|find_link_values(AT,Table,Tail)]
    end.

linked_item_to_create(AT,Table,Name) ->
%    PL=create_payload(linked,AT,escape_what_must(Name)),
    PL=create_payload(linked,AT,Name),
    case airtable:create(AT,Table,PL) of
	{ok,ATR} ->
	    ATR#airtable_record.id;
	Other ->
	    io:format(" airtable:create failed ~p~n",[Other]),
	    ""
    end.

add_if_exist([]) ->
    [];
add_if_exist([{_Item,[]}|Tail]) ->
    add_if_exist(Tail);
add_if_exist([{Item,Value}|Tail]) ->
    [{Item,Value}|add_if_exist(Tail)].

escape_what_must(String) ->
    Strong=edoc_lib:escape_uri(String),
    replace_chars(Strong,[{"%20"," "},{"%c1%91","o"},{"%c3%a1","a"},{"%c3%a4","a"},{"%c3%b6","o"},
			  {"%c3%96","O"},{"%c3%84","A"},{"%c3%bc","u"},{"%27","'"},
			  {"%3f","?"},{"%28","("},{"%29",")"},{"%c3%89","E"},{"%c#%9f","S"},
			  {"%26","&"},{"%2b","+"},{"%2f","/"},{"%3a",":"},
			  {"%c3%a9","e"},{"%c3%b3","o"},{"%21","!"},
			  {"%c1%82","l"},{"%c3%b8","o"},{"%c3%ab","e"},{"%22","'"},
			  {"%c3%a7","c"}]).
			  
    %% V=replace_char(Strong,"%20"," "),
    %% V1=replace_char(V,"%c1%91","o"), %
    %% V2=replace_char(V1,"%c3%a1","a"),
    %% V3=replace_char(V2,"%c3%a4","a"), %ä
    %% V4=replace_char(V3,"%c3%b6","o"), %ö
    %% V5=replace_char(V4,"%c3%96","O"), %Ä
    %% V6=replace_char(V5,"%c3%84","A"), %Ö
    %% V7=replace_char(V6,"%c3%bc","u"), %ü
    %% V7.

replace_chars(Input,[]) ->
    Input;
replace_chars(Input,[{InCh,OutCh}|CharList]) ->
    replace_chars(replace_char(Input,InCh,OutCh),CharList).

replace_char(Input,InCh,OutCh) ->
    re:replace(Input,InCh,OutCh,[global,{return,list}]).

replace_8211([])->
    [];
replace_8211([8211|T]) ->
    [45|replace_8211(T)];
replace_8211([H|T]) ->
    [H|replace_8211(T)].
    
remove_non_ascii(List) ->
    lists:filter(fun(X) -> X<128 end, List).

add_attachment([]) ->
    [];
add_attachment(Url) ->
    [{"attachments",[{"url",Url}]}]. 

%% "attachments": [
%%       {
%%         "url": "https://dl.airtable.com/pMhZ1X5jSPmYOJHaiMhg_pic2786349_md.jpg"
%%       }
%%     ],

find_pictures(Id) ->
    case mnesia:dirty_read(pictures,Id) of
	[] ->
	    % let's try to find the URL in BGG
	    Url=string:concat(?BGG_GAME_URL , Id),
	    case hackney:request(Url) of
		   {ok, 200,_Header,ClientRef} ->
		    %% io:format("http request towards ~p got OK ~n",[Url]),
		    {ok,ResponseBody}=hackney:body(ClientRef),
		    {Xml,_}=xmerl_scan:string(binary_to_list(ResponseBody)),
		    {boardgames,_,[_,Data|_]}=xmerl_lib:simplify_element(Xml),
		    {boardgame,_,Properties}=Data,
		    case bgg_feed_utils:find_tupples(Properties,image) of
			[] ->
			    %No photo has been uploaded yet
			    [];
			Image ->
			    %%  [{image,[],
			    %% ["//cf.geekdo-images.com/images/pic2786349.jpg"]}]
			    Url1=string:concat(?HTTPS_PREFIX,
					       string:sub_string(bgg_feed_utils:extractvalue(Image),3)),
			    mnesia:dirty_write(pictures,#picture{id=Id,
								 url=Url1}),
			    Url1
		     end;
		 _ ->
		    []
	    end;
	[PR|_Other] ->
	    PR#picture.url
     end.
