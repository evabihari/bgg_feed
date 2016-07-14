-module(booth).
%% -compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-json({booth,   
       {string,"publisher"},
       {string,"id"},
       {string,"booth"}}).

-export([encode/1,
	 decode/1]).

encode(Term) ->
    to_json(Term).
decode(Term) ->
    from_json(Term,game).




