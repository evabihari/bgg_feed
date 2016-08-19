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

-include("../include/record.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% Get Game data from BGG and store them to the Airtable Table
game_to_airtable(Id) ->
    Game = case mnesia:dirty_read(games,Id) of
	       [G|_] -> G;
	       [] ->
		   G=bgg_feed_utils:new_game(Id),
		   riak_handler:store(Id,G),
		   mnesia:dirty_write(games,G)
	   end,
    AT=airtable:init(),
    Payload=create_payload(game,AT,Game),
    case airtable:find(AT,"Games","id",Id) of 
	{ok,[]} ->
	    airtable:create(AT,"Games",Payload),
	    {ok, added};
	{ok,[ATR|_]} ->
	    AT_id=ATR#airtable_record.id,
	    airtable:update(AT,"Games",AT_id,Payload),
	    {ok, updated}

    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_payload(game,AT,Games) ->
    Publishers = find_links(AT,"Publishers", Games#game.publishers),    
    Designers = find_links(AT,"Designers", Games#game.gamedesigners),
    Mechanics = find_links(AT,"Mechanics", Games#game.mechanics),
    Families = find_links(AT,"Families", Games#game.family),
    Categories = find_links(AT,"Categories", Games#game.categories),


    PL= [{"id",Games#game.id},
	 {"name",Games#game.name},
	 {"yearpublished",Games#game.yearpublished}, 
	 {"price",Games#game.price},
	 {"minplayers",Games#game.minplayers},
	 {"maxplayers",Games#game.maxplayers},
	 {"lang_dependence",Games#game.lang_dependence},
	 {"updated",Games#game.updated},
	 {"link_to_BGG",string:concat(?BGG_GAME_URL , Games#game.id)}]++
	add_if_exist([{"publishers",Publishers},{"gamedesigners",Designers},
		     {"mechanics",Mechanics},{"family",Families},
		     {"categories",Categories}]),
    airtable_record:encode(PL);
create_payload(linked,_AT,Name) ->
    PL=[{"Name",Name}],
    airtable_record:encode(PL).

find_links(AT,Table,Values) ->
    S=lists:flatten(Values),
    List=string:tokens(S,","),
    Links=find_link_values(AT,Table,List),
    case Links of
	[] ->
	    [];
	Link_list ->
	    Link_list
    end.

find_link_values(_AT,_Table,[]) ->
    [];
find_link_values(AT,Table,[H|Tail]) ->
    case airtable:find(AT,Table,"Name",edoc_lib:escape_uri(H)) of
	{ok,[]} ->
	    case linked_item_to_create(AT,Table,H) of
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
    PL=create_payload(linked,AT,escape_what_must(Name)),
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
    re:replace(Strong,"%20"," ",[global,{return,list}]).
