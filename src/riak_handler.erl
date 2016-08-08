-module(riak_handler).
-include("../include/record.hrl").

-export([store/2,
	read/2]).

store(Id,Data)->
    case riakc_pb_socket:start("127.0.0.1", 8087) of
	{ok, Pid} -> 
	    Bucket = assign_bucket_to_record(Data), 
	    {ok,Json}=encode_to_json(Data),
	    Obj=riakc_obj:new(Bucket,list_to_binary(Id),Json,<<"application/json">>),
	    riakc_pb_socket:put(Pid,Obj),
	    riakc_pb_socket:stop(Pid);
	_ ->
	    io:format("riak is not running, can't store data~n",[])
    end.

read(Type,Id) when is_integer(Id) ->
    read(Type,integer_to_list(Id));
read(Type,Id) ->
    case riakc_pb_socket:start("127.0.0.1", 8087) of
	{ok, Pid} -> 
	    Bucket = assign_bucket_to_type(Type), 
	    case riakc_pb_socket:get(Pid, Bucket, Id) of
		{ok, Fetched} ->
		    Json=riakc_obj:get_value(Fetched),
		    riakc_pb_socket:stop(Pid),
		    decode_from_json(game,Json);
		{error,notfound} ->
		    []
	    end;
	_ ->
	    io:format("riak is not running, can't read data~n",[])
    end.    


encode_to_json(Data)  when is_record(Data,game) ->
    game:encode(Data);
encode_to_json(Data)  when is_record(Data,booth) ->
    booth:encode(Data);
encode_to_json(_Data) -> 
    {error, wrong_type}.

decode_from_json(game,Json) ->
    game:decode(Json);
decode_from_json(booth,Json) ->
    booth:decode(Json);
decode_from_json(_,_) ->
    {error,wrong_type}.
    
assign_bucket_to_record(Data) when is_record(Data,game) ->
    <<"bgg">>;
assign_bucket_to_record(Data) when is_record(Data,booth)->
    <<"booth">>.

assign_bucket_to_type(game) ->
    <<"bgg">>;
assign_bucket_to_type(booth)->
    <<"booth">>.