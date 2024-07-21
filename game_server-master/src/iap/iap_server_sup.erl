-module(iap_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                         permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    ets:new(iap_server, [ordered_set, public, named_table, {keypos, 1}]),
    CpuAmount = erlang:system_info(schedulers_online),
    PoolName = iap_verify_worker_pool,
    PoolArgs = [{name, {local, PoolName}},
                {worker_module, iap_server},
                {size, CpuAmount}, {max_overflow, 2 * CpuAmount}],
    PoolSpec = poolboy:child_spec(PoolName, PoolArgs, []),
    {ok, {{one_for_one, 5, 10}, [PoolSpec]}}.
