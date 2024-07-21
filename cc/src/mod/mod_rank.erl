%%----------------------------------------------------
%% 排行榜
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_rank).
-export([handle/3]).

-include("common.hrl").

%% 等级排行列表
handle(21001, [RankType,Type], Rs) ->
    case lists:member(RankType,[1,2,3,4]) andalso lists:member(Type, [1,2,3]) of
        true ->
            self() ! {task, ?TASK_OPEN_BUILDING, {add, 1, 1}},
            Msg = {rank_list 
                , RankType 
                , Type 
                , Rs#role.id 
                , Rs#role.pid_sender 
                , Rs#role.lev 
                , Rs#role.name 
            },
            srv_rank ! Msg;
        false -> 
            ?WARNING("Error Rank Arg [21001, {~w, ~w}]", [RankType, Type])
    end,
    {ok};

%% 我的阶段奖励
handle(21002, [], Rs) ->
    Msg = {my_max_rank 
        , Rs#role.id 
        , Rs#role.pid_sender 
    },
    srv_rank ! Msg,
    {ok};

%% 领取奖励
handle(21003, [], Rs) ->
    Msg = {accept_reward 
        , Rs#role.id
        , Rs#role.pid
        , Rs#role.pid_sender
    },
    srv_rank ! Msg,
    {ok};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.
