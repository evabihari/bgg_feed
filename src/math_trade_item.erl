-module(math_trade_item).
 %-compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-json({math_trade_item,   
       {number, "item_no"},
       {string, "id"},
       {string,"title"},
       {string,"publisher"},
       {string,"language"},
       {string,"attendance"},
       {string,"min_player"},
       {string,"max_player"},
       {string,"interested"},
       {string,"condition"}
       }).

-export([encode/1,
	 decode/1]).

encode(Term) ->
    to_json(Term).
decode(Term) ->
    from_json(Term,math_trade_item).


