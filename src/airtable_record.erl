-module(airtable_record).
%% -compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-record(airtable_record, {id, fields, createdTime}).
-record(field,{name,value}).
-json({airtable_record,   
       {string, "id"},
       {list,"fields",[{type,field}]},
       {string,"createdTime"}}).
-json({field,
       {string,"name"},
       {string,"value"}}).

-export([encode/1,
	 decode/1,
	 start/0, start1/0]).

-define(R2P(Record), record_to_proplist(#Record{} = Rec) ->
	       lists:zip(record_info(fields, Record), tl(tuple_to_list(Rec)))).


encode(Fields) when is_list(Fields) ->
    F_list=create_field_list(Fields),
    "{\"fields\": {"++create_json_string(F_list)++" }}";
encode(Term) ->
    to_json(Term).


create_field_list([]) ->
    [];
create_field_list([FRecord|Tail]) when  is_record(FRecord,field)->
    ["\""++FRecord#field.name ++ "\": "++encode_value(FRecord#field.value)|create_field_list(Tail)];
create_field_list([{Name,Value}|Tail]) ->
    ["\""++Name++"\": "++encode_value(Value)|create_field_list(Tail)]. 

encode_value(Value) ->
    case lists:flatten(Value) of
	Value ->
	    "\""++Value++"\"";
	_V ->
						%["recN1OCI8WwO6HLaj","recN1OCI8WwO6HLbk"]
	    " [ \""++string_list(Value)++"\" ] "
    end.

create_json_string(InputStringList) ->
    lists:flatten(string:join([[X] || X <- InputStringList],", ")).

string_list(ISL) ->
    lists:flatten(string:join([[X] || X <- ISL],"\",\"")).

decode(Term) when is_binary(Term) ->
    %% ex. {"id":"recs7y11zncGIEADY","fields":{"Publisher_id":4302,"Name":"TestPubl","Booth":"valahol","Games":["recN1OCI8WwO6HLaj"]},
    %% "createdTime":"2016-08-18T11:16:14.481Z"}
    %% problem: fileds content depends on the table
    Decoded = jsx:decode(Term),
    decode(Decoded);

decode(RList) ->
    extract_data(RList).

extract_data(BinList) ->
    %% [{<<"id">>,<<"recs7y11zncGIEADY">>},
    %%  {<<"fields">>,
    %%   [{<<"Publisher_id">>,4302},
    %%    {<<"Name">>,<<"TestPubl">>},
    %%    {<<"Booth">>,<<"valahol">>},
    %%    {<<"Games">>,[<<"recN1OCI8WwO6HLaj">>]}]},
    %%  {<<"createdTime">>,<<"2016-08-18T11:16:14.481Z">>}]  
    Id=binary_to_list(get_value("id",BinList)),
    Created=binary_to_list(get_value("createdTime",BinList)),
    Fields=get_value("fields",BinList),
    #airtable_record{id=Id,
		     createdTime=Created,
		     fields=extract_fields(Fields)}.

extract_fields([]) ->
    [];
extract_fields([{ <<"attachments">>,BValue}|Tail]) ->
						%to be done later,
    F={field,"attachments",handle_attachment(BValue)},
    [F|extract_fields(Tail)];
extract_fields([{BName,BValue}|Tail]) ->
    F=#field{name=binary_to_list(BName),
	     value=convert_to_string(BValue)},
    [F|extract_fields(Tail)].

convert_to_string(V) when is_integer(V) ->
    integer_to_list(V);
convert_to_string(V) when is_list(V) ->
    [binary_to_list(X) || X <- V];
convert_to_string(V) ->
    binary_to_list(V).

get_value(Key,BinList) ->
    case proplists:get_value(list_to_binary(Key),BinList) of
	undefined ->
	    "undefined";
	Value ->
	    Value
    end.

handle_attachment([]) ->
    [];
handle_attachment([Attachment|Rem])->
    Id=get_value("id",Attachment),
    Url=get_value("url",Attachment),
    [{{id,Id},{url,Url}}|handle_attachment(Rem)];
handle_attachment(_) ->
    [].

%% {<<"attachments">>,
%%  [[{<<"id">>,<<"att6Nvmsn7HCdgudD">>},
%%    {<<"url">>,
%%     <<"https://dl.airtable.com/pMhZ1X5jSPmYOJHaiMhg_pic2786349_md.jpg">>},
%%    {<<"filename">>,<<"pic2786349_md.jpg">>},
%%    {<<"size">>,170971},
%%    {<<"type">>,<<"image/jpeg">>},
%%    {<<"thumbnails">>,
%%     [{<<"small">>,
%%       [{<<"url">>,
%%         <<"https://dl.airtable.com/jk1yjXSATDiBlrwN9vBJ_small_pic2786349_md.jpg">>},
%%        {<<"width">>,36},
%%        {<<"height">>,36}]},
%%      {<<"large">>,
%%       [{<<"url">>,
%%         <<"https://dl.airtable.com/JkYGzkfZSz8Ct2NR8fKA_large_pic2786349_md.jpg">>},
%%        {<<"width">>,500},
%%        {<<"height">>,500}]}]}]]},



%% {"records":[{"id":"rec6OmOvoRd1IDu7A","fields":{"Name":"Economic","Games":["recN1OCI8WwO6HLaj"]},"createdTime":"2016-08-16T13:03:44.000Z"},
%% 	       {"id":"recahNKWEGlVbl9kn","fields":{"Name":"Exploration","Games":["recN1OCI8WwO6HLaj"]},"createdTime":"2016-08-16T13:04:58.000Z"},
%% 	       {"id":"recbKiE6fEhWbpFvO","fields":{"Name":"City Building","Games":["recN1OCI8WwO6HLaj"]},"createdTime":"2016-08-16T13:03:44.000Z"},
%% 	       {"id":"recnGGVwVih3eToNc","fields":{"Name":"Civilization","Games":["recN1OCI8WwO6HLaj"]},"createdTime":"2016-08-16T13:03:44.000Z"}]}

start1() ->
    R=#airtable_record{
	 id="rec6OmOvoRd1IDu7A",
	 fields=[{"Name","Economic"},
	 	 {"Games",["recN1OCI8WwO6HLaj"]}],
	 createdTime="2016-08-16T13:03:44.000Z"},
    io:format("~p~n",[R]),
    B=list_to_binary(mochijson2:encode(R)),
    io:format("~s~n", [B]).

start() ->
    R=#airtable_record{
	 id="rec6OmOvoRd1IDu7A",
	 fields=[#field{name="Name",
	 		value="Economic"},
	 	 #field{name="Games",
	 		value=["recN1OCI8WwO6HLaj"]}],
	 createdTime="2016-08-16T13:03:44.000Z"},
    io:format("~p~n",[R]),
    {ok,Json}=to_json(R),
    io:format("~s~n", [Json]),
						% {"id":"rec6OmOvoRd1IDu7A","fields":[{"name":"Name","value":"Economic"},{"name":"Games","value":"recN1OCI8WwO6HLaj"}],"createdTime":"2016-08-16T13:03:44.000Z"}

    {ok,R1}=from_json(Json,airtable_record).
