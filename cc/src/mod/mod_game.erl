%%----------------------------------------------------
%% 61内部协议 - 游戏战斗
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_game).
-export([handle/3]).

-include("common.hrl").

%% 游戏结束
handle(6111, Result, Rs) ->
    [
        GamePid
        ,GameMode 
        ,Score
        ,IsWin
        ,BaseGold
        ,BaseExp
        ,TeamNum
        ,_CardAdd
        ,IsGuildTeam
        ,IsWithOsex
        ,MyDaoju
        ,HoldTime
    ] = Result,
    #role{ 
        id = Rid
        ,pid_conn = PidConn 
        ,guild_b = GuildB
        ,guild_id = GuildId
    } = Rs,
    {TeamExpAdd1, TeamGoldAdd1} = case {GameMode, TeamNum} of
        {1, 2} -> {0.2 * BaseExp, 0.1 * BaseGold};
        {1, 3} -> {0.3 * BaseExp, 0.2 * BaseGold};
        {2, 2} -> {0.2 * BaseExp, 0.1 * BaseGold};
        {2, 3} -> {0.3 * BaseExp, 0.2 * BaseGold};
        _ -> {0, 0}
    end,
    TeamExpAdd = util:ceil(TeamExpAdd1),
    TeamGoldAdd = util:ceil(TeamGoldAdd1),
    {Win, Draw, Lost, Escape} = Rs#role.game_count,
    All = Win+Lost+Draw,
    WinRate = case All > 0 of
        true -> util:ceil((Win/All)*100);
        false -> 0
    end,
    ExpCardAdd = case lib_role:get_exp2_time(Rs#role.exp2_etime) > 0 of
        true -> BaseExp;
        false -> 0
    end,
    %% 活动加成
    ExpMul = lib_festival:get(exp_mul),
    GoldMul = lib_festival:get(gold_mul),
    ExpFestAdd = case ExpMul > 1 of
        true -> (ExpMul - 1) * BaseExp;
        false -> 0
    end,
    GoldFestAdd = case GoldMul > 1 of
        true -> (GoldMul - 1) * BaseGold;
        false -> 0
    end,
    {Win1, Draw1, Lost1, Escape1} = case IsWin of
        1  ->
            %% 排行榜数据更新 胜利次数
            gen_server:cast(srv_rank, {add_win, Rs#role.id, 1}),
            case GameMode of
                1 -> self() ! {task, ?TASK_WIN1};
                2 -> self() ! {task, ?TASK_WIN2};
                101 -> ok;
                _ -> 
                    ?WARNING("Unexpected GameMode: ~w", [GameMode]),
                    ok
            end,
            {Win + 1, Draw, Lost, Escape};
        0  -> {Win, Draw + 1, Lost, Escape};
        -1 -> {Win, Draw, Lost + 1, Escape};
        _  -> 
            ?ERR("unexpectant value -> IsWin:~w", [IsWin]),
            {Win, Draw, Lost, Escape}
    end,
    BenefitExpAdd = lib_role:guild_benefit_addition(exp, BaseExp, GuildB),
    BenefitGoldAdd = lib_role:guild_benefit_addition(gold, BaseGold, GuildB),
    %% ?INFO("BenefitGoldAdd:~w -> ~w", [GuildB, BenefitGoldAdd]),
    %% 计算总经验
    ExpTotal1 = BaseExp + ExpCardAdd + TeamExpAdd + BenefitExpAdd + ExpFestAdd,
    ExpTotal = util:ceil(ExpTotal1),
    Rs1 = lib_role:add_exp(Rs, ExpTotal),
    %% 计算总金币
    GoldTotal1 = BaseGold + TeamGoldAdd + BenefitGoldAdd + GoldFestAdd,
    GoldTotal = util:ceil(GoldTotal1),
    Rs2 = lib_role:add_attr(Rs1, gold, GoldTotal, 1001),
    ContinueWin = case IsWin of
        1 -> 
            self() ! {task, ?TASK_CONTINUE_WIN, {add, 1}},
            Rs2#role.continue_win + 1;
        _ -> 
            self() ! {task, ?TASK_CONTINUE_WIN, {set_to, 0}},
            0
    end,
    Rs3 = Rs2#role{
        game_count = {Win1, Draw1, Lost1, Escape1} 
        ,status = 2 
        ,score = Rs2#role.score + Score 
        ,pid_room = Rs2#role.pid_room1 
        ,win_rate = WinRate
        ,continue_win = ContinueWin
        ,is_win = IsWin
    },
    %% 重新计算角色属性
    MyItems = lib_item:get_myitems(),
    Rs4 = lib_role:calc_attrs(Rs3, MyItems),
    %% 获得战斗奖励
    GuildV = case {GuildId, IsWin, env:get(guild_act), IsGuildTeam, GameMode} of
        {0, _, _   , _, _} -> 0;
        {_, 1, true, 1, 1} -> 17;
        {_, 1, true, 1, 2} -> 12;
        {_, 1, _   , _, _} -> 2;
        {_, _, _   , _, _} -> 0
    end,
    case GuildV > 0 of
        true ->
            gen_server:cast(srv_guild, {add_guild_v, GuildId, Rs#role.id, GuildV});
        false ->
            ok
    end,
    gen_server:cast(self(), {handle_event, 14037, [IsWin, GameMode, GuildV]}),
    %% 发送数据
    ExpFestAdd1 = util:ceil(ExpFestAdd),
    GoldFestAdd1 = util:ceil(GoldFestAdd),
    GamePid ! {game_result, [Rid, Win1, Draw1, Lost1, Escape1,
            BaseGold, TeamGoldAdd, BenefitGoldAdd, 
            BaseExp, TeamExpAdd, ExpCardAdd, BenefitExpAdd, 
            ExpFestAdd1, GoldFestAdd1, HoldTime, Score]},
    %% 触发任务
    case GameMode of
        1 -> self() ! {task, ?TASK_JOIN1};
        2 -> self() ! {task, ?TASK_JOIN2};
        101 -> ok;
        _ -> 
            ?WARNING("Unexpected GameMode: ~w", [GameMode]),
            ok
    end,
    case TeamNum > 1 of
        true -> 
            self() ! {task, ?TASK_WITH_TEAM},
            case IsWithOsex of
                1 -> self() ! {task, ?TASK_WITH_OSEX};
                0 -> ok
            end,
            case IsGuildTeam of
                1 -> self() ! {task, ?TASK_WITH_GUILD};
                0 -> ok
            end;
        false -> ok
    end,
    %% 退出游戏后设置为同步处理Socket Data
    PidConn ! {set_process_mode, sync},
    lib_item:put_mydaojus(MyDaoju),
    {ok, Rs4};

handle(6115, [FromPid, FromPidS], Rs) ->
    case Rs#role.status of
        1 -> lib_conn:pack_send(FromPidS, 13009, [3]);
        2 ->
            case catch gen_server:call(Rs#role.pid_room, 
                    get_room_id, 800) of
                {ok, Type, RoomId} ->
                    lib_conn:pack_send(FromPidS, 13009, [0]),
                    gen_server:cast(FromPid, {handle_event,
                            13003, [Type, RoomId]});
                _ ->
                    lib_conn:pack_send(FromPidS, 13009, [1])
            end;
        3 -> lib_conn:pack_send(FromPidS, 13009, [4]);
        _ -> lib_conn:pack_send(FromPidS, 13009, [1])
    end,
    {ok};

%% 副本翻牌 - 增加物品/金币/点券
handle(6120, [Nth, Id, Num, Tips], Rs) when Num > 0 ->
    %% ?INFO("get_reward:~w", [{Nth, Id, Num}]),
    SpendResult = case Nth >= 3 of
        true -> lib_role:spend(card, 100, Rs, 2008);
        false -> {ok, Rs}
    end,
    Rs1 = case SpendResult of
        {ok, Rs0} ->
            case Id of
                1 -> lib_role:add_attr(Rs0, gold, Num, 1011);
                2 -> lib_role:add_attr(Rs0, card, Num, 3006);
                _ ->
                    MyItems = lib_item:get_myitems(),
                    case lib_item:add_items([{Id, Num}], MyItems) of
                        {ok, MyItems1, My} ->
                            lib_item:put_myitems(MyItems1),
                            lib_item:add_item_notice(My, Rs0#role.pid_sender),
                            case Tips of
                                1 ->
                                    lib_conn:pack_cast(world, 15008, [1, Rs0#role.id, Rs0#role.name, Id]),
                                    Rs0#role{save = [{myitems, MyItems1}]};
                                _ ->
                                    Rs0
                            end;
                        {error, at_full} ->
                            lib_conn:send_code(Rs#role.pid_sender, 17000101),
                            Rs;
                        {error, Error} ->
                            ?WARNING("handle 6020 error: ~w", [Error]),
                            Rs
                    end
            end;
        {error, _} ->
            ?WARNING("No card when get_reward 3", []),
            Rs
    end,
    {ok, Rs1};

%% 拾取道具
%% handle(6115, [Tid], Rs) ->
%%     case lib_item:add_mydaoju(Tid) of
%%         {ok, MyDaoju} -> 
%%             lib_conn:pack_send(Rs#role.pid_sender, 17001, [MyDaoju]),
%%             ok;
%%         {error, at_full} -> ok;
%%         {error, _} -> ok
%%     end,
%%     {ok};

%% FIX 数据修复
handle(6199, [], Rs) ->
    Status = case Rs#role.status of
        0 -> 
            self() ! {reset_stop_timer, 2000},
            1;
        Sta -> 
            %% ?INFO("Online when fix: ~s", [Rs#role.name]),
            Sta
    end,
    %% 增加日常任务
    case Rs#role.lev >= 8 of
        true ->
            E = Rs#role.enable_dtask,
            E1 = lists:foldl(fun(X, AccIn) ->
                        case lists:member(X, AccIn) of
                            true -> AccIn;
                            false -> [X | AccIn]
                        end
                end, E, [154, 155, 156]),
            case E == E1 of
                true -> 
                    {ok};
                false ->
                    %% MyTasks = lib_task:get_mytasks(),
                    %% T1 = lib_task:init_mytask(154, 0),
                    %% T2 = lib_task:init_mytask(155, 0),
                    %% T3 = lib_task:init_mytask(156, 0),
                    %% lib_task:put_mytasks([T1, T2, T3 | MyTasks]),
                    %% lib_conn:pack_send(Rs#role.pid_sender, 11020, [3]),
                    Rs1 = Rs#role{enable_dtask = E1, status = Status},
                    %% ?INFO("~w -> ~w", [E, E1]),
                    io:format(".", []),
                    {ok, Rs1}
            end;
        false -> 
            {ok}
    end;

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%%. === 私有函数 ===

