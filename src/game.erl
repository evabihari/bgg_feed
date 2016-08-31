-module(game).
%% -compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-json({game,   
       {string,"id"},
       {string,"name"},
       {string,"family"},
       {string,"mechanics"},
       {string,"yearpublished"},
       {string,"minplayers"},
       {string,"maxplayers"},
       {string,"publishers"},
       {string,"gamedesigners"},
       {string,"categories"},
       {string,"types"},
       {string,"lang_dependence"},
       {string,"price"},
       {string,"updated"},
       {string,"artists"}}).

-export([encode/1,
	 decode/1]).

encode(Term) ->
    to_json(Term).
decode(Term) ->
    from_json(Term,game).



    
