-module(riak_handler).
-include("../include/record.hrl").

-export([store/2]).

store(Id,Data) ->
    case riakc_pb_socket:start("127.0.0.1", 8087) of
	{ok, Pid} -> 
	    MyBucket = <<"bgg">>,
	    {ok,Json}=encode_to_json(Data),
	    Obj=riakc_obj:new(MyBucket,list_to_binary(Id),Json,<<"application/json">>),
	    riakc_pb_socket:put(Pid,Obj),
	    riakc_pb_socket:stop(Pid);
	_ ->
	    io:format("riak is not running, can't store data~n",[])
     end.


encode_to_json(Data)  when is_record(Data,game) ->
    game:encode(Data);
encode_to_json(_Data) -> 
    {error, wrong_type}.
    

