-module(bgg_feed_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-define(TIMEOUT, 30000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
        {ok, { {simple_one_for_one, 5, 1}, [child_spec()]} }.

%% Helper macro for declaring children of supervisor
child_spec() -> {
  bgg_feed_worker,
  {bgg_feed_parse, start_link, []},
  transient,
  ?TIMEOUT,
  worker,
  [bgg_feed_parse]
}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
