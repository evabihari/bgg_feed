-module(auction_list).
-export([read/1, read/0]).
-export([find/1]).
-export([what_wins/1]).
-export([traded_by/1]).
-export([items_without_bid/0,items_without_bid/1]).
-export([dump_to_file/0]).
-export([read_by_page_proc/4]).

% -export([remove_special_str/2]).
-include("../include/record.hrl").

-define(ESSEN_NSA_2016,"https://boardgamegeek.com/xmlapi/geeklist/211885").

-define(STRANGE_CHR_REPLACEMENTS,[{[226,130,172]," EUR"},{[195,175],"i"}, {[195,188],"ü"},{[195,132],"A"},
				  {[195,136],"E"},{[195,32],"a "},{[195,164],"a"},{[195,169],"é"},{[195,182],"ö"},
				  {[195,150],"O"}]).
-define(OUT,"Auctions.csv").
-define(DROPBOX_PUBLIC,"~/Dropbox/Public/").


% {[226,128,156],""},{[226,128,157],""},{"[/size]",""}
% {[226,128,153],""},{[226,128,147],""}

-define(REMOVE_STRANGE_CHARS,[":","[b]","[/b]","\r","[u]","[/u]","[B]","[/B]",";",[239,187,191],[195,162],[194,381],[194,8364]] ).

read() ->
    read(?ESSEN_NSA_2016).
read(Url) ->
    bgg_feed_utils:start_apps(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_auction_table(),
    read(Url,1).

read(Url,3) ->
    %was not able to read the list with comments 5 times
    %reason of the problem can be that the size of the list so huge, BGG is not able to respond in time
    %try to ask information per pages (100 item per page)
    % 1 step -> ask the list without comments and get the size of the list
    {ok, Body}=ask_url(Url),
    Struct=mochiweb_html:parse(Body),
    Path="geeklist/numitems",
    [{<<"numitems">>,[],[BinNum]}]=mochiweb_xpath:execute(Path,Struct),
    ItemNo=list_to_integer(binary_to_list(BinNum)),
    PageNo=1+ItemNo div 100,
    read_by_page(Url,PageNo,PageNo);  
read(Url,N) ->
    Url1=Url ++ "?comments=1",
    io:format(" hackney:request, Url1=~p~n",[Url1]),
    case hackney:request(get,Url1,[],<<"?comments=1">>) of
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
	    store_items(ItemList,1),
            bgg_feed_utils:dump_to_file(auction_items,?OUT),
	    Cmd="cp "++ ?OUT ++ " "++ ?DROPBOX_PUBLIC ++ ".",
	    os:cmd(Cmd);
	{ok, 202,_Head,_Ref} ->
	    io:format("request accepted, wait a bit,",[]),
	    timer:sleep(10000),
	    read(Url,1);
	{error, closed} ->
	    %let'ss try agin ?
	    io:format("request failed, let's try again ~p, N=~p~n",[{error, closed},N]),
	    timer:sleep(10000),
	    read(Url,N+1);	 
	{error, timeout} ->
	    %let'ss try agin ?
	    io:format("request failed, let's try again ~p, N=~p~n",[{error, timeout},N]),
	    timer:sleep(10000),
	    read(Url,N+1);	    
	Result -> io:format("Http request failed, Result=~p~n",[Result])
    end.

read_by_page(_Url,0,_PageNo) ->
    ok;
read_by_page(Url,N,PageNo) ->
    Pid=spawn(?MODULE,read_by_page_proc,[Url,N,PageNo,self()]),
    io:format("read_by_page_proc for N=~p spawned, Pid=~p ~n",[N,Pid]),
    receive
	{ask_url_ok,Pid} ->
	    io:format("ask_url finished, ack received, N=~p~n",[N])
    after
	60000 ->
	    io:format("ask_url not finished in time for N=~p, potential hanging, restart hackney~n",[N]),
	    try
		hackney:stop()
	    catch
		_Exit -> ok
	    end,
	    timer:sleep(10000),
	    hackney:start(),
	    %% as hackney was restarted let's kill the process which detected teh hanging;
	    %% new process will be spawned instead
	    exit(Pid,normal),
	    read_by_page(Url,N,PageNo)
    end,
    timer:sleep(1000),
    read_by_page(Url,N-1,PageNo).
    
read_by_page_proc(Url,N,PageNo,Sender_Pid) ->
    io:format(" read_by_page, Url=~p,PageNo=~p, N=~p~n",[Url,PageNo,N]),
    StartNum=100*(N-1)+1,
    Url1=Url ++ "?comments=1&page="++integer_to_list(N),
    {ok, Body}=ask_url(Url1),
    Sender_Pid ! {ask_url_ok,self()},
    Struct=mochiweb_html:parse(Body),
    Path="geeklist/item",
    ItemList=mochiweb_xpath:execute(Path,Struct),
    store_items(ItemList,StartNum). 

ask_url(Url) ->
    io:format("ask_url, Url=~p~n",[Url]),
    case hackney:request(get,Url,[], <<>>, [{pool,default},{timeout, 150000}, {max_connections, 100}]) of
	{ok,200,_Head,Ref} ->
	    io:format("ask_url, Url=~p request answer code = 200~n",[Url]),
	    {ok,Body} = hackney:body(Ref),
	    {ok,Body};
	{ok, 202,_Head,_Ref} ->
	    io:format("request accepted, wait a bit,Url=~p~n",[Url]),
	    timer:sleep(1000),
	    ask_url(Url);
	{error, closed} ->
	    %let'ss try agin ?
	    io:format("aks_url request failed, let's try again Url=~p, ~p~n",[Url,{error, closed}]),
	    timer:sleep(10000),
	    ask_url(Url);	 
	{error, timeout} ->
	    %let'ss try agin ?
	    io:format("aks_url request failed, let's try again Url=~p, ~p~n",[Url,{error, timeout}]),
	    timer:sleep(10000),
	    ask_url(Url);
	{error, bad_request} ->
	    %let'ss try agin ?
	    io:format("aks_url request failed, let's try again Url=~p, ~p~n",[Url,{error, bad_request}]),
	    timer:sleep(15000),
	    ask_url(Url);
	{error, enetdown} ->
	    %let'ss try agin ?
	    io:format("aks_url request failed, let's try again Url=~p, ~p~n",[Url,{error, enetdown}]),
	    timer:sleep(25000),
	    ask_url(Url);
	Result -> io:format("Http request (ask_url) failed, Result=~p~n",[Result])
    end.
create_auction_table()->
    case mnesia:create_table(auction_items, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, 
                               record_info(fields, auction_item)},
                              {record_name,  auction_item}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,auction_items}} -> 
            error_logger:error_msg("auction_items table creation failed , 
                                    reason = ~p~n",[{already_exists,auction_items}]);
        Other ->
            error_logger:error_msg("auction_items table creation failed , 
                                    reason = ~p~n",[Other])
    end.

store_items([],_Nr) ->
    [];
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
    
    %% Starting_bid1=case find_info(Text,"Starting bid") of
    %% 	    "" -> find_info(Text,"Soft reserve");
    %% 	    S_Bid -> S_Bid
    %% end,
    Starting_bid1=remove_special_str(find_info_opts(Text,["Starting bid","Soft reserve"]),["Starting bid","Soft reserve","Starting Bid",
											  "COLOR=#FF0000","[size=10]","[/size]","[b]","[/b]",
											  "[size=14]"]),
    Starting_bid=replace_strange_chars(Starting_bid1,?STRANGE_CHR_REPLACEMENTS),
    %% Bin2=case find_info(Text,"BIN:") of
    %% 	    "" -> find_info(Text,"Buyout price");
    %% 	    Bin1 -> Bin1
    %% end,
    Bin1=remove_special_str(find_info_opts(Text,["BIN","Buyout price"]),["[b]","[/b]","Bin","[size=10]","[/size]"]),
    Bin=replace_strange_chars(Bin1,?STRANGE_CHR_REPLACEMENTS),
    Presense1=find_info_opts(Text,["Attendance","At Spiel","att. Spiel","Attending","Presence in Essen","Available in Essen","In Essen",
				     "Presence at Essen","At the fair","Presence","Essen:","Att. Spiel"]),
    Presense=remove_strs(Presense1,["at SPIEL'16 ","at SPIEL ","Presence ","at SPIEL'16"," '16","at SPIEL'15","Attendence in Essen ",
					"on the fair ","at Spiel'16 ","at SPIEL16","Att. Spiel "," at Spiel","  in Essen "]),
    Auction_ends=case find_info(Text,"Auction ends:") of
		     "" -> find_info(Text,"Auction end");
		     Info ->
				Info
		 end,
    {Actual_bid,Winner,Comments} = check_comments(no_bid,"",CList,{AI#auction_item.item_no,AI#auction_item.object_id},[]),
    Sold1=case find_info(Text,"Sold") of
	     "" -> check_bid(Bin,Actual_bid);
	     _ -> true
    end,
    Sold2=case string:str(Condition,"[-]") of
	     0 -> false;
	     _ -> true
    end,
    Sold=Sold1 or Sold2,

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

find_info_opts(_S,[]) ->
    "";
find_info_opts(String,[Pattern|PList]) ->
    case find_info(String,Pattern) of
	"" ->
	    find_info_opts(String,PList);
	Result ->
	    Result
    end.

find_info(String,Pattern) ->
    RegExp=string:concat(string:concat("^.*(",Pattern),").*$"),
    Res=case re:run(String,RegExp,[multiline,{capture,all,list},caseless]) of
	nomatch ->
	    "";
	{match,[Result|_Other]} ->
	    R1=re:replace(Result,Pattern,"", [{return, list}, global]),
	    re:replace(R1,[226,128,147],"-", [{return, list}, global]);
	_ ->
	    ""
	end,
    remove_special_str(Res,?REMOVE_STRANGE_CHARS).


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
    print("Item_no: \t",Obj#auction_item.item_no),
    print("Id: \t\t",Obj#auction_item.id),
    print("Game id: \t",Obj#auction_item.object_id),
    print("Game name: \t",Obj#auction_item.object_name),
    print("Owned by: \t",Obj#auction_item.username),
    print("Sold: \t\t",Obj#auction_item.sold),
    print("Condition: \t",Obj#auction_item.condition),
    print("Language: \t",Obj#auction_item.language),
    print("Lang_dep: \t",Obj#auction_item.lang_dep),
    print("Version: \t",Obj#auction_item.version),
    print("Starting_bid: \t",Obj#auction_item.starting_bid),
    print("BIN: \t\t",Obj#auction_item.bin),
    print("Actual bid: \t",Obj#auction_item.actual_bid),
    print("Actual winner: \t",Obj#auction_item.actual_winner),
    print("Pres. & Essen: \t",Obj#auction_item.presense_at_essen),
    print("Auction ends: \t",Obj#auction_item.auction_ends),
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
    print("Item_no: \t",Obj#auction_item.item_no),
    print("Game name:\t",Obj#auction_item.object_name),
    print("Owned by:\t",Obj#auction_item.username),
    print("Condition: \t",Obj#auction_item.condition),
    print("Language: \t",Obj#auction_item.language),
    print("Lang_dep: \t",Obj#auction_item.lang_dep),
    print("BIN: \t\t",Obj#auction_item.bin),
    print("Actual bid: \t",Obj#auction_item.actual_bid),
    print("Actual winner: \t",Obj#auction_item.actual_winner),
    print("Sold: \t\t",Obj#auction_item.sold),
    print("Pres. & Essen: \t",Obj#auction_item.presense_at_essen),
    print("Auction ends: \t",Obj#auction_item.auction_ends),
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
    
remove_strs(String,[]) ->
    String;
remove_strs(String,[SubStr|List]) ->
    remove_strs(remove_str(String,SubStr),List).

remove_str(String,SubStr) ->
    re:replace(String,SubStr,"", [{return, list}, global]).

check_bid(_Bin,no_bid) -> false;
check_bid("",_Actual_bid) -> false;
check_bid(_Bin,bin) -> true; 
check_bid(Bin,Bin) -> true; 
check_bid(Bin,Bid) ->
    BinN=find_number_or_bin(Bin,Bin),
    BidN=find_number_or_bin(Bid,Bid),
    BinN==BidN. 
    
replace_strange_chars(String,[]) ->
    String;
replace_strange_chars(String,[{K,V}|List]) ->
    Res= re:replace(String,K,V,[{return, list}, global]),
    replace_strange_chars(Res,List).

items_without_bid() ->
    items_without_bid(no_file).

items_without_bid(FileName) ->

    case mnesia:dirty_match_object(auction_items, #auction_item{_ = '_', actual_bid=no_bid, sold=false}) of
	[] ->
	    not_found;
	ObjList ->
	    case FileName of
		no_file ->
		    short_list(ObjList);
		FName  ->
		    bgg_feed_utils:dump_to_file(ObjList,FName)
		end
	    end.    

short_list([]) ->
    [];
short_list([Obj|List]) ->
    io:format("------------ITEM FOUND-----------~n",[]),
    print("Item_no: \t",Obj#auction_item.item_no),
    print("Game name:\t",Obj#auction_item.object_name),
%    print("Owned by:\t",Obj#auction_item.username),
    print("Condition: \t",Obj#auction_item.condition),
%    print("Language: \t",Obj#auction_item.language),
%    print("Lang_dep: \t",Obj#auction_item.lang_dep),
    print("BIN: \t\t",Obj#auction_item.bin),
    print("Starting bid: \t",Obj#auction_item.starting_bid),
%    print("Actual winner: \t",Obj#auction_item.actual_winner),
%    print("Sold: \t\t",Obj#auction_item.sold),
    print("Pres. & Essen: \t",Obj#auction_item.presense_at_essen),
    print("Auction ends: \t",Obj#auction_item.auction_ends),
    short_list(List).



dump_to_file() ->
    bgg_feed_utils:dump_to_file(auction_items,?OUT),
    Cmd="cp "++ ?OUT ++ " "++ ?DROPBOX_PUBLIC ++ ".",
    os:cmd(Cmd).
