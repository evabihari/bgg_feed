-module(auction_list).
-export([read/1, read/0]).
-export([find/1]).
-export([what_wins/1]).
-export([traded_by/1]).
-export([remove_special_str/2]).
-include("../include/record.hrl").

-define(ESSEN_NSA_2016,"https://boardgamegeek.com/xmlapi/geeklist/211885?comments=1").
read() ->
    read(?ESSEN_NSA_2016).
read(Url) ->
    bgg_feed_utils:start_apps(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_auction_table(),
    io:format(" hackney:request, Url=~p~n",[Url]),
    case hackney:request(get,Url,[],<<"?comments=1">>) of
	{ok,200,_Head,Ref} ->
	    {ok,Body} = hackney:body(Ref),
	    {ok,F} = file:open("auction2016.txt",[read,write,{encoding,utf8}]),
	    file:write(F,Body),
	    Struct=mochiweb_html:parse(Body),
	    {ok,F2} = file:open("auction_structs.txt",[read,write]),
	    file:write(F2,io_lib:print(Struct)),
	    Path="geeklist/item",
	    ItemList=mochiweb_xpath:execute(Path,Struct),
	    {ok,F3} = file:open("items.txt",[read,write,{encoding,utf8}]),
	    file:write(F3,io_lib:print(ItemList)),
	    store_items(ItemList,1);
	{ok, 202,_Head,_Ref} ->
	    io:format("request accepted, wait a bit,",[]),
	    timer:sleep(10000),
	    read(Url);
	{error, closed} ->
	    %let'ss try agin ?
	    io:format("request failed, let's try again~n",[]),
	    timer:sleep(10000),
	    read(Url);	 
	{error, timeout} ->
	    %let'ss try agin ?
	    io:format("request failed, let's try again~n",[]),
	    timer:sleep(10000),
	    read(Url);	    
	Result -> io:format("Http request failed, Result=~p~n",[Result])
    end.

create_auction_table()->
    case mnesia:create_table(auction_items, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, 
                               record_info(fields, auction_item)},
                              {record_name,  auction_item}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,daily_values}} -> 
            error_logger:info_msg("auction_items table already_exists");
        Other ->
            error_logger:error_msg("auction_items table creation failed , 
                                    reason = ~p~n",[Other])
    end.

store_items([],_Nr) ->
    [];
%% store_items(_, Nr) when Nr>14 ->
%%     % just while test will successed
%%     ok;
store_items([{<<"item">>,Properties,BodyL}|ItemList],Nr) ->
    case checkType(Properties, [<<"objecttype">>,<<"subtype">>]) of
	["thing","boardgame"] ->
	    % check whether game already stored and this is a fresh update or not:
	    [ObjId, Name, UserName, Id] =  checkType(Properties,[<<"objectid">>,<<"objectname">>,<<"username">>,<<"id">>]),
	    {Body,C}= case BodyL of
		[] ->
		    {[],[]};
		[Body1|C1] ->
		     {Body1,C1}
	    end,

	    AI=#auction_item{item_no=Nr,
			     id=Id,
			     sub_type="boardgame",
			     object_type="thing",
			     object_id=ObjId,
			     object_name=Name,
			     username=UserName},
	    Auction_item=fill_other_item_info(AI,
					     Body,
					     C),
	    mnesia:dirty_write(auction_items,Auction_item);
	    %% riak_handler:store(integer_to_list(Nr),Auction_item); do not store to Riak yet

	["thing",Other] ->
	    % out_of_scope_for_now
	    io:format("not a boardgame, rather ~p~n",[Other]);
	_ -> io:format("will_be_find_later, Properties = ~p~n",[Properties]), 
	    will_be_find_later
    end,
    store_items(ItemList,Nr+1).

checkType(_Properties,[]) ->
    [];
checkType(Properties,[Key|List]) ->
    case lists:keyfind(Key, 1, Properties) of
	{Key, Result} ->
	    [binary_to_list(Result)|checkType(Properties,List)];
	_ ->[not_found|checkType(Properties,List)]
    end.

fill_other_item_info(AI,{<<"body">>,_,[Body]},CList) ->
    Text=binary_to_list(Body),
    Condition=find_info(Text,"Condition:"),
    Language=find_info(Text,"Language:"),
    Lang_dep=find_info(Text,"Language dependency:"),
    Version=find_info(Text,"Version:"),
    
    Starting_bid=case find_info(Text,"Starting bid") of
	    "" -> find_info(Text,"Soft reserve");
	    S_Bid -> S_Bid
    end,
    Bin=case find_info(Text,"BIN:") of
	    "" -> find_info(Text,"Buyout price");
	    Bin1 -> Bin1
    end,
    Presense=find_info(Text,"Presence at Essen:"),
    Auction_ends=case find_info(Text,"Auction ends:") of
		     "" -> find_info(Text,"Auction end");
		     Info ->
				Info
		 end,
    {Actual_bid,Winner,Comments} = check_comments(no_bid,"",CList,{AI#auction_item.item_no,AI#auction_item.object_id},[]),
    Sold=case find_info(Text,"Sold:") of
	     "" -> check_bid(Bin,Actual_bid);
	     _ -> yes
    end,
    AI#auction_item{condition=Condition,    
		    language=Language,
		    lang_dep=Lang_dep,
		    version=Version,
		    starting_bid=Starting_bid,
		    sold=Sold,
		    bin=Bin,
		    actual_bid=Actual_bid,
		    actual_winner=Winner,
		    presense_at_essen=Presense,
		    auction_ends=Auction_ends,
		    comments=Comments,
		    body=Body}.

find_info(String,Pattern) ->
    RegExp=string:concat(string:concat("^.*(",Pattern),").*$"),
    Res=case re:run(String,RegExp,[multiline,{capture,all,list},caseless]) of
	nomatch ->
	    "";
	{match,[Result|_Other]} ->
	    re:replace(Result,Pattern,"", [{return, list}, global]);
	_ ->
	    ""
	end,
    remove_special_str(Res,[":","[b]","[/b]"]).


check_comments(no_bid,"",[],_,_) ->
    {no_bid,"",[]};
check_comments(Actual_Bid,Winner,[],_,Comments) ->
    {Actual_Bid,Winner,Comments};
check_comments(Actual_Bid,Winner,[{<<"comment">> ,_Properties,[]}|C_List],{Item_no,Object_id},Comments) ->
    check_comments(Actual_Bid,Winner,C_List,{Item_no,Object_id},Comments);
check_comments(Actual_Bid,Winner,[{<<"comment">> ,Properties,[Body|_]}|C_List],{Item_no,Object_id},Comments) ->
    Bid=find_number_or_bin(binary_to_list(Body),Actual_Bid),
    UserName=checkType(Properties,[<<"username">>]),
    Winner1=case Bid of
	no_bid -> Winner;
	_ -> UserName
    end,
    check_comments(Bid,Winner1,C_List,{Item_no,Object_id},lists:append(Comments, [#comment{connected_item_no=Item_no,
											  connected_object_id=Object_id,
											  username=UserName,
											  bid=Bid}])).

find_number_or_bin(Text,OrigValue) ->
    case re:run(Text,"\\d+",[{capture, all, list}]) of
	{match,[Bid|_]} ->
	    Bid;
	nomatch ->
	    UText=string:to_upper(Text),
	    case string:str(UText,"BIN") of
		0 ->
		    OrigValue;
		_ ->
		    bin
	    end
   end.

find(GameId) when is_integer(GameId) ->
    case mnesia:dirty_match_object(auction_items, #auction_item{_ = '_', object_id=integer_to_list(GameId)}) of
	[] ->
	    not_found;
	ObjList ->
	   summarize(ObjList)
    end;
find(GameName) ->
    case mnesia:dirty_match_object(auction_items, #auction_item{_ = '_', object_name=GameName}) of
	[] ->
	    not_found;
	ObjList ->
	   summarize(ObjList)
    end.

what_wins(User) when is_atom(User) ->
    what_wins(atom_to_list(User));
what_wins(User) ->
    case mnesia:dirty_match_object(auction_items, #auction_item{_ = '_', actual_winner=[User]}) of
	[] ->
	    not_found;
	ObjList ->
	   summarize_wins(ObjList)
    end.    
    
traded_by(User)  when is_atom(User) ->
    traded_by(atom_to_list(User));
traded_by(User) ->
    case mnesia:dirty_match_object(auction_items, #auction_item{_ = '_', username=User}) of
	[] ->
	    not_found;
	ObjList ->
	   summarize(ObjList)
    end.    

summarize([]) ->
    "";
summarize([Obj|ObjList]) ->
    io:format("------------ITEM FOUND-----------~n",[]),
    print("Item_no: ",Obj#auction_item.item_no),
    print("Id: ",Obj#auction_item.id),
    print("Game id: ",Obj#auction_item.object_id),
    print("Game name:",Obj#auction_item.object_name),
    print("Owned by:",Obj#auction_item.username),
    print("Sold: ",Obj#auction_item.sold),
    print("Condition: ",Obj#auction_item.condition),
    print("Language: ",Obj#auction_item.language),
    print("Lang_dep: ",Obj#auction_item.lang_dep),
    print("Version: ",Obj#auction_item.version),
    print("Starting_bid: ",Obj#auction_item.starting_bid),
    print("BIN: ",Obj#auction_item.bin),
    print("Actual bid: ",Obj#auction_item.actual_bid),
    print("Actual winner: ",Obj#auction_item.actual_winner),
    print("Presense at Essen: ",Obj#auction_item.presense_at_essen),
    print("Auction ends: ",Obj#auction_item.auction_ends),
    io:format("------------ITEM END-----------~n",[]),
    summarize(ObjList).
   
print(Category,Text) when is_integer(Text) ->
    io:format("~s ~p~n",[Category,Text]);
print(Category,Text) when is_atom(Text) ->
    io:format("~s ~p~n",[Category,Text]);
print(Category,Text) ->
    try
	io:format("~s ~ts~n",[Category,Text])
    catch error:badarg ->
	io:format("~s ~p~n",[Category,Text])
    end.

summarize_wins([]) ->
    "";
summarize_wins([Obj|ObjList]) ->
    io:format("------------ITEM FOUND-----------~n",[]),
    print("Item_no: ",Obj#auction_item.item_no),
    print("Game name:",Obj#auction_item.object_name),
    print("Owned by:",Obj#auction_item.username),
    print("Condition: ",Obj#auction_item.condition),
    print("Language: ",Obj#auction_item.language),
    print("Lang_dep: ",Obj#auction_item.lang_dep),
    print("BIN: ",Obj#auction_item.bin),
    print("Actual bid: ",Obj#auction_item.actual_bid),
    print("Actual winner: ",Obj#auction_item.actual_winner),
    print("Presense at Essen: ",Obj#auction_item.presense_at_essen),
    print("Auction ends: ",Obj#auction_item.auction_ends),
    io:format("------------ITEM END-----------~n",[]),
    summarize_wins(ObjList).

remove_special_str(Text,List) when is_binary(Text) ->
    remove_special_str(binary_to_list(Text),List);
remove_special_str(Text,[]) ->
    Text;
remove_special_str(Text,[SubStr|StrList]) ->
    Pos=string:str(Text,SubStr),
    T1 = case Pos of
	   0 -> Text;
	   N -> L=string:left(Text,N-1),
		R=string:substr(Text,N+length(SubStr)),	
		L++R
       end,
    remove_special_str(T1,StrList).
    
check_bid(_Bin,no_bid) -> false;
check_bid("",_Actual_bid) -> false;
check_bid(_Bin,bin) -> true; 
check_bid(Bin,Bin) -> true; 
check_bid(Bin,Bid) ->
    BinN=find_number_or_bin(Bin,Bin),
    BidN=find_number_or_bin(Bid,Bid),
    BinN==BidN. 
    
