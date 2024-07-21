%%----------------------------------------------------
%% 进程监控器
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(cc_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

-include("common.hrl").

%% @hidden
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% @hidden
init([NodeId, Host, Port]) ->
    gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {cc_alarm_handler, null}), 
    List = [
        {cc_logger, {cc_logger, start_link, []}, permanent, 10000, worker, [cc_logger]}
        ,{srv_rand, {srv_rand, start_link, []}, permanent, 10000, worker, [srv_rand]}
        ,{srv_cache, {srv_cache, start_link, []}, permanent, 10000, worker, [srv_cache]}
        ,{acceptor_sup, {acceptor_sup, start_link, []}, permanent, 10000, supervisor, [acceptor_sup]}
        ,{listener, {listener, start_link, [Port]}, permanent, 10000, worker, [listener]}
        ,{srv_net, {srv_net, start_link, [NodeId, Host, Port]}, permanent, 10000, worker, [srv_net]}
        ,{robot, {robot, start_link, []}, permanent, 10000, worker, [robot]}
        ,{u, {u, start_link, [15]}, permanent, 10000, worker, [u]}
        ,{srv_notice, {srv_notice, start_link, []}, permanent, 10000, worker, [srv_notice]}
        ,{srv_guild, {srv_guild, start_link, []}, permanent, 10000, worker, [srv_guild]}
    ],
    {ok, {{one_for_one, 5, 1}, List}}.
