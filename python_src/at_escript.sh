#!/usr/bin/env /Users/evabihari/.kerl/r18/bin/escript
%% -*- erlang -*-

main([Id]) ->
	Default="/Users/evabihari/external/bgg_feed",
	Deps=Default ++ "/deps",
        shell_default:cd(Default),
        Script_dir=filename:dirname(escript:script_name()),
	{ok,DirList}=file:list_dir(Deps),
	io:format("Script_dir=~s, Id=~p ~n",[Script_dir,Id]),
        true = code:add_pathz("ebin"),
	io:format("ok1 ~n",[]),
	set_code_path(Deps,DirList),


    ok = application:ensure_started(inets),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    hackney:start(),
  %  {atomic,ok}=mnesia:create_schema([node()]),
    mnesia:start(),
    	bgg_feed_utils:start_apps(),
        %% mnesia:create_schema([node()]),
        mnesia:start(),
	io:format("Mnesia started~n",[]),
	bgg_feed_utils:create_games_table(),
	io:format("games table created?~n",[]),
	io:format("games table size=~p~n",[mnesia:table_info(games,size)]),
        bgg_to_airtable:game_to_airtable(Id).


set_code_path(_Base,[]) ->
			ok;
set_code_path(Base,[D|Dirs]) ->
			     io:format("code:add_pathz ~s~n",[Base++"/"++D]),
			     code:add_pathz(Base++"/"++D++"/ebin"),
			     set_code_path(Base,Dirs).
