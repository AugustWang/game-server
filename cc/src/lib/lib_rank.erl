%%----------------------------------------------------
%% 排行榜
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_rank).
-export([
        stop/0
        ,update_day_rank/0
        ,update_week_rank/0
        ,reset_day_rank/0
        ,reset_week_rank/0
        ,set_myrank/1
        ,update_luck_rank/0
    ]
).

-include("common.hrl").

update_luck_rank() ->
    srv_luck ! update_rank.

update_day_rank() ->
    ?INFO("update_day_rank.....", []),
    srv_rank ! update_day_rank.

update_week_rank() ->
    ?INFO("update_week_rank.....", []),
    srv_rank ! update_week_rank.

reset_day_rank() ->
    ?INFO("reset_day_rank.....", []),
    db:execute("update rank set exp_day = 0, win_day = 0, gold_day = 0;").

reset_week_rank() ->
    ?INFO("reset_week_rank.....", []),
    db:execute("update rank set exp_week = 0, win_week = 0, gold_week = 0;").

stop() ->
    ?INFO("save rank.....", []),
    case catch gen_server:call(srv_rank, save) of
        {'EXIT', Error1} -> ?ERR("ERROR:~w", [Error1]);
        ok -> ok
    end,
    case catch gen_server:call(srv_luck, save) of
        {'EXIT', Error2} -> ?ERR("ERROR:~w", [Error2]);
        ok -> ok
    end.

set_myrank(Rs) ->
    {Win, _, _, _} = Rs#role.game_count,
    gen_server:cast(srv_rank, {set_myrank, Rs#role.id, 
            Rs#role.lev, Rs#role.exp, Win, Rs#role.power}).
