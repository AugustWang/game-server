%%----------------------------------------------------
%% 游戏战斗(匹配队伍后)
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
%%' header
-module(srv_game1).
-behaviour(gen_server).
%% 导出公开接口
-export([start/1]).
%% 导出回调接口
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% 导出其它模式共享的内部接口
-export([
        init1/2
        ,get_role/2
        ,set_role/2
        ,get_npc/2
        ,rand_pos2/1
        ,check_buff/2
        ,monster_produce/3
        ,rand_npc_type/4
        ,is_guild_team/1
        ,is_with_osex/1
        ,get_event/3
        ,get_event/4
        ,chk_events/3
        ,chk_event/3
        ,set_event/4
        ,del_event/3
        ,set_init_pos/2
        ,is_npc_pos/3
    ]).

-include("game.hrl").

-define(HIT_ROW_INTERVAL_TIME, 70). %% 力道传播间隔时间
-define(HIT_COL_INTERVAL_TIME, 100). %% 力道传播间隔时间
%% -define(HIT_ROW_INTERVAL_TIME, 270). %% 力道传播间隔时间
%% -define(HIT_COL_INTERVAL_TIME, 300). %% 力道传播间隔时间
%%.

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

%%' 新建连接
start(L) ->
    gen_server:start(?MODULE, L, []).
%%. ================================================

%%----------------------------------------------------------------------
%% Callback functions from gen_server
%%----------------------------------------------------------------------

%%' init
init(Arg) ->
    init1(1, Arg).

%% 初始化玩家数据和地图ID
init1(GameMode, [{RoomId1, Pid1}, {RoomId2, Pid2}]) ->
    case is_process_alive(Pid1) andalso is_process_alive(Pid2) of
        true ->
            Roles1 = gen_server:call(Pid1, get_roles),
            Roles2 = gen_server:call(Pid2, get_roles),
            RLength1 = erlang:length(Roles1),
            RLength2 = erlang:length(Roles2),
            RLength = case RLength1 > RLength2 of
                true -> RLength1;
                false -> RLength2
            end,
            MapId = util:rand_element(data_map_appear:get(RLength)),
            %% MapId = 44,
            NewRoles1 = [copy_game_attr(X, RoomId1, Pid1, 1, MapId, GameMode) || X <- Roles1],
            NewRoles2 = [copy_game_attr(X, RoomId2, Pid2, 2, MapId, GameMode) || X <- Roles2],
            AllRoles = NewRoles1 ++ NewRoles2,
            PosList = data_init_pos:get(MapId),
            NewRoles = set_init_pos(AllRoles, PosList),
            init2(GameMode, MapId, NewRoles);
        false ->
            ?WARNING("room_not_alive, GameMode:~w, Args:~w", 
                [GameMode, [{RoomId1, Pid1}, {RoomId2, Pid2}]]),
            {stop, room_not_alive}
    end;
init1(GameMode, [{RoomId1, Pid1}]) when GameMode > 100 ->
    case is_process_alive(Pid1) of
        true ->
            Roles1 = gen_server:call(Pid1, get_roles),
            MapId = util:get_val(map_id, data_carbon:get({1, 1})),
            AllRoles = [copy_game_attr(X, RoomId1, Pid1, 1, MapId, GameMode) || X <- Roles1],
            PosList = data_init_pos:get(MapId),
            NewRoles = set_init_pos(AllRoles, PosList),
            init2(GameMode, MapId, NewRoles);
        false ->
            ?WARNING("room_not_alive, GameMode:~w, Args:~w", 
                [GameMode, [{RoomId1, Pid1}]]),
            {stop, room_not_alive}
    end.

%% 初始化NPC
init2(GameMode = 1, MapId, NewRoles) ->
    [DataMapNpc, _] = data_map_npc:get(MapId),
    NpcProRate = util:rand_element(DataMapNpc),
    init3(GameMode, MapId, NewRoles, NpcProRate);
init2(GameMode = 2, MapId, NewRoles) ->
    NpcProRate = [{7, 1, 100}], %% 固定一个怪
    init3(GameMode, MapId, NewRoles, NpcProRate);
init2(GameMode, MapId, NewRoles) when GameMode > 100 ->
    Npcs = get_carbon_npcs(GameMode),
    NpcProRate = [{X, 1, 100} || {X, _} <- Npcs],
    init3(GameMode, MapId, NewRoles, NpcProRate).

init3(GameMode, MapId, Roles, NpcProRate) ->
    %% 初始化进程字典
    put(max_item_id, 0),
    put(max_npc_id, 0),
    %% 初始化地图信息
    Map = data_map:get(MapId),
    Width = get_map_element(width, Map),
    Height = get_map_element(height, Map),
    BrickNum = get_map_element(brick_num, Map),
    {AvgLev, MinLev} = get_amlev(Roles),
    RoleFallDropHp = data_average:get({0, AvgLev}),
    RoleHitDropHp = data_average:get({999, AvgLev}),
    %% 初始化正在加载资源的玩家
    Loading = [R#irole.id || R <- Roles],
    put(loading, Loading),
    %% 初始化状态
    State = #state{ 
        type = GameMode
        ,map_id = MapId
        ,roles = Roles
        ,npc_pro_rate = NpcProRate
        ,falled_hp = RoleFallDropHp
        ,role_hit_drop_hp = RoleHitDropHp
        ,role_avg_lev = AvgLev
        ,width = Width
        ,height = Height
        ,brick_num = BrickNum
        ,avg_lev = AvgLev
        ,min_lev = MinLev
    },
    %% 发送数据，通知游戏加载开始
    InitData = gen_init_data(GameMode, MapId, Roles, NpcProRate),
    Senders = [R#irole.pid_sender || R <- Roles],
    lib_conn:pack_cast(Senders, 14009, InitData),
    %% 如果一定时间后仍有角色没有加载完成，强制开始
    erlang:send_after(60 * 1000, self(), force_start),
    Ref = erlang:send_after(3 * 1000, self(), delay_start),
    put(delay_start, Ref),
    %% TEST
    %% erlang:send_after(util:rand(10000, 60 * 1000), self(), test),
    {ok, State}.
%%.

handle_call(_Request, _From, State) ->
    ?INFO("Not matched request: ~w, Game type: ~w", [_Request, State#state.type]),
    {noreply, State}.

%%' 重连，加载场景元素
handle_cast({send_element, PidSender}, 
    #state{
        type = GameMode
        ,roles = Roles 
        ,map_id = MapId 
        ,npc_pro_rate = NpcProRate 
    } = State) ->
    Data = gen_init_data(GameMode, MapId, Roles, NpcProRate),
    lib_conn:pack_send(PidSender, 14009, Data),
    %% ?INFO("send_element! ~w", [Role]),
    {noreply, State};
%%. =================================================

%%' 发送我的道具
handle_cast({send_mydaojus, Rid, PidSender}, State) ->
    case get_role(Rid, State) of
        false -> ok;
        I ->
            lib_conn:pack_send(PidSender, 17001, [I#irole.mydaoju]),
            ok
    end,
    {noreply, State};
%%.

%%' 道具使用
handle_cast({use_mydaoju, Rid, PidSender, Pos}, State) ->
    State1 = case get_role(Rid, State) of
        false -> 
            State;
        I when I#irole.hp =< 0 ->
            lib_conn:pack_send(PidSender, 17003, [Pos, 0]),
            State;
        I ->
            Daoju = I#irole.mydaoju,
            case get_mydaoju(Pos, Daoju) of
                0 -> 
                    lib_conn:pack_send(PidSender, 17003, [Pos, 0]),
                    State;
                Tid ->
                    #irole{x = X, y = Y} = I,
                    V = data_map_pos:get({State#state.map_id, X, Y}),
                    case (V == 1 orelse V == 2) andalso 
                        (chk_events(X, Y, [item, skill]) == false orelse Tid == 120018) of
                        true ->
                            Daoju1 = del_mydaoju(Pos, Daoju),
                            use_daoju(Rid, Tid, 0, I, State),
                            I#irole.pid ! {task, ?TASK_USE_DAOJU, {add, Tid, 1}},
                            lib_conn:pack_send(PidSender, 17003, [Pos, 1]),
                            set_role(I#irole{mydaoju = Daoju1}, State);
                        false ->
                            lib_conn:pack_send(PidSender, 17003, [Pos, 0]),
                            State
                    end
            end
    end,
    {noreply, State1};

handle_cast({use_skill, Rid, PidSender, SkillId}, State) ->
    use_skill(Rid, PidSender, SkillId, State),
    {noreply, State};

%% 丢弃道具
%% handle_cast({drop_daoju, Rid, ItemId}, State) ->
%%     Items = get_items(Rid),
%%     case lists:keyfind(ItemId, 2, Items) of
%%         false -> ok;
%%         #iitem{} -> 
%%             NewItems = lists:keydelete(ItemId, 2, Items),
%%             set_items(Rid, NewItems),
%%             ok
%%     end,
%%     {noreply, State};
%%. ============================================================

%%' 角色动作处理
handle_cast({hit, Rid, X0, Y00, X, Y1, Type}, State) ->
    Y = fix_y(Y1),
    Y0 = fix_y(Y00),
    I = get_role(Rid, State),
    Dir = if
        X0 < X -> right;
        X0 > X -> left;
        Y0 > Y1 -> up;
        Y0 < Y1 -> down;
        Y0 == 18 andalso Y == 3 -> down;
        true ->
            ?WARNING("undefined hit dir ---> X0:~w, Y0:~w, X:~w, Y:~w", [X0, Y0, X, Y]),
            undefined
    end,
    LastHT = case get({last_hit, Rid}) of
        undefined -> 0;
        TmpHT -> TmpHT
    end,
    Now = util:unixtime(micro),
    put({last_hit, Rid}, Now),
    Interval = (Now - LastHT) * 1000,
    GameMode = State#state.type,
    NewState = if
        I == false -> 
            ?INFO("Role not found when 'hit'! [~w]", [Rid]),
            State;
        Interval < 350 ->
            %% ?INFO("dmg_speed too fast 1, Id:~w, Interval:~w, Speed:~w", [I#irole.id, Interval, I#irole.hit_speed]),
            %% lib_conn:send_code(I#irole.pid_sender, 103),
            State;
        I#irole.hp =< 0 -> 
            %% ?INFO("Still 'hit' when HP=<0 ! [~w]", [Rid]),
            State;
        Interval < (I#irole.hit_speed - 150) ->
            %% ?INFO("dmg_speed too fast 2, Id:~w, Interval:~w, Speed:~w", [I#irole.id, Interval, I#irole.hit_speed]),
            %% lib_conn:send_code(I#irole.pid_sender, 103),
            State;
        I#irole.stop_move -> 
            %% ?INFO("Still 'hit' when stop_move! [~w]", [Rid]),
            State;
        GameMode > 100, Dir == up ->
            State;
        GameMode > 100, Dir == down ->
            State;
        true -> 
            #irole{team = Team, row_dmg = RowDmg, col_dmg = ColDmg, dmg = Dmg, crit = Crit
                %% , attack_all = AttackAll, power_pill = PowerPill 
            } = I,
            MyBuffs = get_mybuffs(Rid),
            %% 大力丸
            Dmg1 = case State#state.type > 100 of
                true -> I#irole.fb_attack;
                false ->
                    case check_buff(3, MyBuffs) of
                        true -> Dmg + 100;
                        false -> Dmg
                    end
            end,
            %% 雷神之力
            case use_buff(13, MyBuffs, Rid, State) of
                {ok, _, MyBuffs1} -> 
                    put_buff(Rid, MyBuffs1),
                    F = fun(#irole{team = Team1, x = X2, y = Y2}) ->
                            case Team =/= Team1 of
                                true -> add_break(X2, Y2, State, Rid, 0);
                                false -> skip
                            end
                    end,
                    lists:foreach(F, State#state.roles);
                {error, _} -> ok
            end,
            {RowDmg1, ColDmg1} = case Type == 1 of
                true -> {2, 1};
                false -> {RowDmg, ColDmg}
            end,
            {Hx, Hit} = calc_dmg({Dmg1, Crit}),
            case chk_event(X, Y, broken_pos) == false orelse Type == 1 of
                true ->
                    %% 处理力道
                    self() ! {hit_brick, Rid, {Hx, Hit}, X, Y, X, Y, RowDmg1, ColDmg1, Dir, empty, 0};
                false -> ok
            end,
            lib_conn:pack_cast(State#state.senders, 11001, [Rid, X0, Y0, X, Y, Hx]),
            %% adjust_hit_pos(X0, Y0, V0, I, State)
            %% V0 = get_pos_val(State#state.map_id, X0, Y0),
            %% I2 = I#irole{
            %%     x  = X0
            %%     ,y = Y0
            %%     ,v = V0
            %% },
            %% set_role(I2, State)
            State
    end,
    {noreply, NewState};
%%. ============================================================

%%' 角色退出
handle_cast({role_exit, Rid}, State) ->
    NewState = case get_role(Rid, State) of 
        false -> 
            %% ?INFO("role_exit Rid:~w", [Rid]),
            State;
        Role ->
            NewRoles = lists:keydelete(Role#irole.id, 2, State#state.roles),
            self() ! game_over_check,
            Senders = [R#irole.pid_sender || R <- NewRoles],
            lib_conn:pack_cast(State#state.senders, 14012, [Rid]),
            Role#irole.pid_room1 ! {role_exit, Rid},
            State#state{roles = NewRoles, senders = Senders}
    end,
    {noreply, NewState};
%%. =================================================

%%' set_pid_sender
handle_cast({set_pid_sender, Rid, PidSender}, State) ->
    State2 = case get_role(Rid, State) of
        false -> 
            ?WARNING("Role not found when set_pid_sender, Rid:~w", [Rid]),
            State;
        I ->
            gen_server:cast(I#irole.pid_room1, {set_pid_sender, Rid, PidSender}),
            State1 = set_role(I#irole{pid_sender = PidSender}, State),
            Senders = lists:delete(I#irole.pid_sender, State1#state.senders),
            State1#state{senders = Senders}
    end,
    {noreply, State2};

handle_cast({set_lev, Rid, Lev}, State) ->
    NewState2 = case get_role(Rid, State) of
        false -> State;
        Role -> 
            ?INFO("@game set_lev, Rid:~w, Lev:~w", [Rid, Lev]),
            set_role(Role#irole{lev = Lev}, State)
    end,
    {noreply, NewState2};
%%. ============================================================

handle_cast(_Msg, State) ->
    ?INFO("Not matched message: ~w, Game type: ~w", [_Msg, State#state.type]),
    {noreply, State}.

%%' Ready Go
handle_info(delay_start, State) ->
    erase(delay_start),
    case get(loading) of
        [] -> 
            lib_conn:pack_cast(State#state.senders, 14027, []),
            erlang:send_after(3000, self(), game_start);
        _ -> 
            ok
    end,
    {noreply, State};
%%. ============================================================

%%' 强制开始游戏，不再等待其它玩家
handle_info(force_start, State) ->
    case get(loading) of
        [] -> ok;
        Loading ->
            F = fun(Id) -> 
                    lib_conn:send_error(Id, 14000101),
                    gen_server:cast(self(), {role_exit, Id})
            end,
            lists:foreach(F, Loading),
            %% 游戏准备开始
            lib_conn:pack_cast(State#state.senders, 14027, []),
            erlang:send_after(3000, self(), game_start)
    end,
    {noreply, State};
%%.

%%' Game start
handle_info(game_start, State) ->
    self() ! loop,
    GameTime = data_config:get(game_time),
    lib_conn:pack_cast(State#state.senders, 14019, [GameTime]),
    NowTime = util:unixtime(),
    State1 = State#state{
        start_time = NowTime
        ,end_time = NowTime + GameTime
    },
    RoleNum = length(State#state.roles),
    NpcX = if 
        RoleNum < 3 -> data_config:get(npc_num_1v1);
        RoleNum < 5 -> data_config:get(npc_num_2v2);
        RoleNum < 7 -> data_config:get(npc_num_3v3);
        RoleNum < 9 -> data_config:get(npc_num_4v4);
        true -> ?WARNING("RoleNum (~w) ", [RoleNum])
    end,
    NpcNum = util:ceil(State#state.brick_num * NpcX / 96),
    State2 = gen_npcs(NpcNum, State1),
    self() ! start_active_element,
    case State#state.type of
        1 ->
            erlang:send_after(58000, self(), {gen_npc, 38}),
            erlang:send_after(88000, self(), {gen_npc, 38}),
            ok;
        _ -> 
            ok
    end,
    {noreply, State2};
%%. ============================================================

%%' Start active element of map
handle_info(start_active_element, State) ->
    MapId = State#state.map_id,
    Element = get_map_element(element, MapId, []),
    F = fun
        ({2, MoveTime, StopTime, X1, Y1, X2, Y2}) -> 
            set_cloud_epos(MapId, X1, Y1, X2, Y2),
            self() ! {ae_2, 1, MoveTime, StopTime, X1, Y1, X2, Y2};
        ({3, MoveTime, _StopTime, X1, Y1, X2, Y2}) -> 
            set_event(X1, Y1, fall, {MoveTime, X2, Y2});
        (Else) -> 
            ?INFO("Unexpected element: ~w", [Else])
    end,
    lists:foreach(F, Element),
    {noreply, State};

%% 地图动态元素数据结构:
%% * {类型,移动时间,停留时间,起点X,起点Y,终点X,终点Y}
%%
%% 协议数据结构:
%% * [类型, 动作, 始X, 始Y, 终X, 终Y]
%%
%% 协议中的动作值:
%% * 1=停留在目标点，此时目标点有[云]
%% * 2=从起始点开始移动，此时起始点[云]消失
%%
%% 类型:
%% * 1: 单向移动
%% * 2: 双向移动
%% * 3: 瀑布 
%%
%% Step:
%% * 1: 在始点停留, 停留时间＝StopTime
%% * 2: 从始点运动, 持续时间＝MoveTime
%% * 3: 在终点停留, 停留时间＝StopTime
%% * 4: 从终点运动, 持续时间＝MoveTime

%% 云开始移动，所以在云上的玩家停止移动
%% 云停止，所有在云上的玩家恢复移动

%% 双向云
handle_info({ae_2, Step, MoveTime, StopTime, X1, Y1, X2, Y2}, State) ->
    {Time, Step1, Data, State2} = case Step of
        1 -> 
            set_event(X1, Y1, cloud, 1),
            {State1, Ids} = at_cloud(stop, X2, Y2, X1, Y1, State),
            exit_cloud(X1, Y1, Ids),
            {StopTime, 2, [2, 1, X2, Y2, X1, Y1, Ids], State1};
        2 -> 
            set_event(X1, Y1, cloud, 0),
            {State1, Ids} = at_cloud(move, X1, Y1, X2, Y2, State),
            {MoveTime, 3, [2, 2, X1, Y1, X2, Y2, Ids], State1};
        3 -> 
            set_event(X2, Y2, cloud, 1),
            {State1, Ids} = at_cloud(stop, X1, Y1, X2, Y2, State),
            exit_cloud(X2, Y2, Ids),
            {StopTime, 4, [2, 1, X1, Y1, X2, Y2, Ids], State1};
        4 -> 
            set_event(X2, Y2, cloud, 0),
            {State1, Ids} = at_cloud(move, X2, Y2, X1, Y1, State),
            {MoveTime, 1, [2, 2, X2, Y2, X1, Y1, Ids], State1}
    end,
    %% ?INFO("Time:~w, Step1:~w, Data:~w", [Time, Step1, Data]),
    lib_conn:pack_cast(State#state.senders, 16015, Data),
    erlang:send_after(Time, self(), 
        {ae_2, Step1, MoveTime, StopTime, X1, Y1, X2, Y2}),
    {noreply, State2};

%%. ============================================================

handle_info({loading, Rid, Rate}, State) ->
    AllSenders = [R#irole.pid_sender || R <- State#state.roles],
    lib_conn:pack_cast(AllSenders, 14022, [Rid, Rate]),
    {noreply, State};

%%' 客户端资源加载完成
handle_info({loaded, Rid, PidSender}, State) ->
    Loading = get(loading),
    Loading1 = lists:delete(Rid, Loading),
    put(loading, Loading1),
    Senders = [PidSender | State#state.senders],
    AllSenders = [R#irole.pid_sender || R <- State#state.roles],
    case Loading1 of
        [] -> 
            case State#state.start_time > 0 of
                true ->
                    %% 游戏己经开始，重新进入
                    F1 = fun(#irole{id = Id}) ->
                            lib_conn:pack_send(PidSender, 14023, [Id])
                    end,
                    lists:foreach(F1, State#state.roles),
                    %% 重发NPC
                    F2 = fun(#inpc{id = Id, type = Tid, x = X, y = Y}) ->
                            lib_conn:pack_send(PidSender, 12005, [Id, Tid, X, Y])
                    end,
                    lists:foreach(F2, State#state.npcs),
                    lib_conn:pack_send(PidSender, 14027, []),
                    NowTime = util:unixtime(),
                    GameTime = State#state.end_time - NowTime,
                    lib_conn:pack_send(PidSender, 14019, [GameTime]),
                    rebuild_scene(PidSender),
                    ok;
                false ->
                    lib_conn:pack_cast(Senders, 14023, [Rid]),
                    case get(delay_start) of
                        undefined ->
                            lib_conn:pack_cast(Senders, 14027, []),
                            erlang:send_after(3000, self(), game_start);
                        _ -> ok
                    end
            end;
        _ -> 
            %% 还有人没有加载完成，继续等待
            lib_conn:pack_cast(AllSenders, 14023, [Rid])
    end,
    State1 = State#state{senders = Senders},
    {noreply, State1};
%%. ============================================================

%%' set_npc_xy
handle_info({set_npc_xy, NpcId, X, Y}, State) ->
    NewState = case get_npc(NpcId, State) of 
        false -> State;
        Ni -> 
            NewNi = Ni#inpc{x = X, y = Y},
            set_npc(NewNi, State)
    end,
    {noreply, NewState};
%%. ============================================================

%%' NPC死亡
handle_info({npc_die, Rid, NpcId, Score}, State) ->
    NewState = case get_npc(NpcId, State) of 
        false -> 
            ?ERR("Npc not found when npc_die:~w", [NpcId]),
            State;
        Npc ->
            #inpc{type = NpcType ,x = X ,y = Y} = Npc,
            %% ?INFO("Npc stop [Id:~w, X:~w, Y:~w]", [NpcId, X, Y]),
            #state{team1_score = T1Score, team2_score = T2Score} = State,
            Team = case get_role(Rid, State) of
                #irole{pid = Pid, team = Team1} ->
                    Pid ! {task, ?TASK_KILL_MONSTER},
                    Team1;
                _Other -> 
                    ?WARNING("Role not found when npc_die:~w", [_Other]),
                    1
            end,
            HighTeam = case T1Score > T2Score of
                true -> 1;
                false -> 2
            end,
            MyTeamStatus = case Team of
                HighTeam -> high;
                _ -> low
            end,
            %% 怪物死亡，掉落产出物品
            case monster_produce(NpcType, MyTeamStatus, State#state.min_lev) of
                0 -> skip;
                ItemTypeId -> self() ! {fall_item, ItemTypeId, 0, X, Y}
            end,
            TgType = case NpcType of
                38 -> 3;
                _ -> 1
            end,
            self() ! {add_score, Rid, Score, TgType, NpcId}, %% 给最后打死怪的角色加分
            NewNpcs = lists:keydelete(NpcId, 2, State#state.npcs),
            %% 生成NPC
            self() ! gen_npc,
            %% 秒杀全屏怪
            do_second_hit(NpcType, Rid, NewNpcs),
            State#state{npcs = NewNpcs}
    end,
    {noreply, NewState};

handle_info({npc_second_die, Rid, NpcId, Score, HpMax}, State) ->
    lib_conn:pack_cast(State#state.senders, 12009, [NpcId, HpMax, 0, 2]),
    self() ! {add_score, Rid, Score, 3, NpcId}, %% 给最后打死怪的角色加分
    NewNpcs = lists:keydelete(NpcId, 2, State#state.npcs),
    %% 生成NPC
    self() ! gen_npc,
    NewState = State#state{npcs = NewNpcs},
    {noreply, NewState};
%%. =================================================

%%' 玩家得分
handle_info({add_score, Rid, AddScore, TgType, TgRid}, State) ->
    NewState = case get_role(Rid, State) of
        false -> 
            ?WARNING("Role not found when add_score: ~w", [Rid]),
            State;
        Role ->
            #state{team1_score = T1Score, team2_score = T2Score} = State,
            #irole{score = Score, team = Team} = Role,
            {NewT1Score, NewT2Score} = case Team of
                1 -> {T1Score + AddScore, T2Score};
                2 -> {T1Score, T2Score + AddScore};
                _ ->
                    ?WARNING("Team undefined: ~w", [Team]),
                    {T1Score, T2Score}
            end,
            Mul = case check_buff(1, Rid) of
                true -> 2; %% 有分数翻倍BUFF
                false -> 1
            end,
            %% NewScore = Score + util:floor(AddScore * Mul),
            NewScore = Score + AddScore * Mul,
            NewRole = Role#irole{score = NewScore},
            lib_conn:pack_cast(State#state.senders, 11005, 
                [Rid, NewScore, TgType, TgRid]),
            set_role(NewRole, State#state{
                    team1_score = NewT1Score, team2_score = NewT2Score})
    end,
    {noreply, NewState};
%%. =================================================

%%' gen_npc
handle_info(gen_npc, State) ->
    {noreply, gen_npc(0, State)};

handle_info({gen_npc, Type}, State) ->
    {noreply, gen_npc(Type, State)};
%%. =================================================

%%' 道具效果实现

%% 加速BUFF
handle_info({add_move_speed_buff, BuffId, Rid, ToSpeed}, State) ->
    I = get_role(Rid, State),
    Add = case I of
        false -> 0;
        _ -> ToSpeed - I#irole.move_speed
    end,
    CheckBuff = check_buff(5, Rid),
    NewState = if
        Add == 0 ->
            State;
        CheckBuff == true, Add < 0 ->
            %% 无敌中，不减速
            State;
        true ->
            MoveSpeed1 = fix_move_speed(I#irole.move_speed + Add),
            AddSpeed = MoveSpeed1 - I#irole.move_speed,
            lib_conn:pack_cast(State#state.senders, 11008, [Rid, 2, MoveSpeed1]),
            I2 = I#irole{move_speed = MoveSpeed1},
            put({move_speed_buff, BuffId, Rid}, AddSpeed),
            set_role(I2, State)
    end,
    {noreply, NewState};

%% 增加攻击速度
handle_info({add_dmg_speed_buff, BuffId, Rid, ToSpeed}, State) ->
    I = get_role(Rid, State),
    Add = case I of
        false -> 0;
        _ -> ToSpeed - I#irole.dmg_speed
    end,
    NewState = if
        Add == 0 -> State;
        true ->
            DmgSpeed1 = fix_dmg_speed(I#irole.dmg_speed + Add),
            AddSpeed = DmgSpeed1 - I#irole.dmg_speed,
            lib_conn:pack_cast(State#state.senders, 11008, [Rid, 3, DmgSpeed1]),
            HitSpeed = calc_hit_speed(DmgSpeed1),
            I2 = I#irole{dmg_speed = DmgSpeed1, hit_speed = HitSpeed},
            put({dmg_speed_buff, BuffId, Rid}, AddSpeed),
            set_role(I2, State)
    end,
    {noreply, NewState};



%% 1000 - 600 = 400
%% 400 + 1200 = 1600
%% 
%% Ex1:
%% CurSpeed = 1600
%% AddSpeed = 600
%% NewSpeed = 1600
%% AddSpeed1 = 600,

%% Ex2:
%% CurSpeed = 1500
%% AddSpeed = 600
%% NewSpeed = 1600
%% AddSpeed1 = 500,

%% Ex3:
%% CurSpeed = 450
%% AddSpeed = -100
%% NewSpeed = 400
%% AddSpeed - (NewSpeed - CurSpeed) = AddSpeed1 = -50,

%% 删除加速BUFF
handle_info({del_move_speed_buff, BuffId, Rid}, State) ->
    I = get_role(Rid, State),
    DimSpeed = get({move_speed_buff, BuffId, Rid}),
    NewState = if
        I == false -> State;
        DimSpeed == undefined ->
            ?INFO("Undefined speed_buff, BuffId:~w, Rid:~w", [BuffId, Rid]),
            State;
        true ->
            AddSpeed = -DimSpeed,
            CurSpeed = I#irole.move_speed,
            NewSpeed = fix_move_speed(I#irole.move_speed + AddSpeed),
            AddSpeed1 = AddSpeed - (NewSpeed - CurSpeed),
            fix_move_buff(Rid, AddSpeed1),
            lib_conn:pack_cast(State#state.senders, 11008, [Rid, 2, NewSpeed]),
            I2 = I#irole{move_speed = NewSpeed},
            set_role(I2, State)
    end,
    {noreply, NewState};

%% 删除加速BUFF
handle_info({del_dmg_speed_buff, BuffId, Rid}, State) ->
    I = get_role(Rid, State),
    AddSpeed = get({dmg_speed_buff, BuffId, Rid}),
    NewState = if
        I == false -> State;
        AddSpeed == undefined ->
            ?INFO("Undefined speed_buff, BuffId:~w, Rid:~w", [BuffId, Rid]),
            State;
        true ->
            erase({last_hit, Rid}),
            Speed1 = fix_dmg_speed(I#irole.dmg_speed - AddSpeed),
            lib_conn:pack_cast(State#state.senders, 11008, [Rid, 3, Speed1]),
            HitSpeed = calc_hit_speed(Speed1),
            I2 = I#irole{dmg_speed = Speed1, hit_speed = HitSpeed},
            set_role(I2, State)
    end,
    {noreply, NewState};

%% 增加横向攻击
handle_info({add_row_dmg, Rid, Val, Time}, State) ->
    NewState = case get_role(Rid, State) of 
        false -> State;
        Role ->
            case Time > 0 of 
                true ->
                    erlang:send_after(Time * 1000, self(), {add_row_dmg, Rid, -Val, 0});
                false -> skip
            end,
            RowDmg = Role#irole.row_dmg + Val,
            RowDmg1 = case RowDmg =< 3 of 
                true -> RowDmg;
                false -> 3
            end,
            NewRole = Role#irole{row_dmg = RowDmg1},
            set_role(NewRole, State)
    end,
    {noreply, NewState};

%% 增加纵向攻击
handle_info({add_col_dmg, Rid, Val, Time}, State) ->
    NewState = case get_role(Rid, State) of 
        false -> State;
        Role ->
            case Time > 0 of 
                true ->
                    erlang:send_after(Time * 1000, self(), {add_col_dmg, Rid, -Val, 0});
                false -> skip
            end,
            ColDmg = Role#irole.col_dmg + Val,
            ColDmg1 = case ColDmg =< 2 of
                true -> ColDmg;
                false -> 2
            end,
            NewRole = Role#irole{col_dmg = ColDmg1},
            set_role(NewRole, State)
    end,
    {noreply, NewState};

%% 增加攻击
handle_info({add_dmg, Rid, Val, Time}, State) ->
    NewState = case get_role(Rid, State) of 
        false -> State;
        Role ->
            case Time > 0 of 
                true ->
                    erlang:send_after(Time * 1000, self(), {add_dmg, Rid, -Val, 0});
                false -> skip
            end,
            NewRole = Role#irole{dmg = Role#irole.dmg + Val},
            set_role(NewRole, State)
    end,
    {noreply, NewState};

%% 大力丸
%% handle_info({power_pill, Rid, Val, Time}, State) ->
%%     NewState = case get_role(Rid, State) of 
%%         false -> State;
%%         Role ->
%%             case Time > 0 of 
%%                 true ->
%%                     erlang:send_after(Time * 1000, self(), {power_pill, Rid, 0, 0});
%%                 false -> skip
%%             end,
%%             NewRole = Role#irole{power_pill = Val},
%%             set_role(NewRole, State)
%%     end,
%%     {noreply, NewState};

%% 设置雷神之力次数
%% handle_info({attack_all, Rid, Val}, State) ->
%%     NewState = case get_role(Rid, State) of 
%%         false -> State;
%%         Role ->
%%             NewRole = Role#irole{attack_all = Role#irole.attack_all + Val},
%%             set_role(NewRole, State)
%%     end,
%%     {noreply, NewState};

%% 增加血量
handle_info({add_hp, Rid, Val, _Time}, State) ->
    I = get_role(Rid, State),
    NewState = if
        I == false -> State;
        I#irole.hp =< 0 -> State;
        true ->
            #irole{hp_max = HpMax, hp = Hp} = I,
            Hp1 = Hp + Val,
            Hp2 = case Hp1 > HpMax of
                true -> HpMax;
                false -> Hp1
            end,
            I2 = I#irole{hp = Hp2},
            lib_conn:pack_cast(State#state.senders, 11018, [Rid, Hp2, HpMax, 3]),
            set_role(I2, State)
    end,
    {noreply, NewState};

handle_info({loop_add_hp, Rid, Val, Time}, State) ->
    case Time =< 0 of
        true -> {noreply, State};
        false ->
            I = get_role(Rid, State),
            NewState = if
                I == false -> State;
                I#irole.hp =< 0 -> State;
                true ->
                    #irole{hp_max = HpMax, hp = Hp} = I,
                    Hp1 = Hp + Val,
                    Hp2 = case Hp1 > HpMax of
                        true -> HpMax;
                        false -> Hp1
                    end,
                    I2 = I#irole{hp = Hp2},
                    lib_conn:pack_cast(State#state.senders, 11018, [Rid, Hp2, HpMax, 3]),
                    erlang:send_after(1000, self(), {loop_add_hp, Rid, Val, Time - 1}),
                    set_role(I2, State)
            end,
            {noreply, NewState}
    end;

%% 随机传送
handle_info({rand_transport, Rid, _Val, _Time}, State) ->
    NewState = case get_role(Rid, State) of 
        false -> State;
        Role ->
            {X, Y} = rand_pos(State),
            NewRole = Role#irole{x = X, y = Y},
            lib_conn:pack_cast(State#state.senders, 16007, [Rid, X, Y]),
            %% ?INFO("rand_transport:~w, ~w", [X, Y]),
            set_role(NewRole, State)
    end,
    {noreply, NewState};
%%. ============================================================

%%' BUFF

%% -  ---------
%% ID Buff
%% -  ---------
%% 1  分数翻倍
%% 2  隐身
%% 3  力量拳套
%% 4  恶魔
%% 5  无敌
%% 6  蜘蛛网
%% 13 雷神之力
%% 14 护盾
%% 16 可移动的炸弹
%% -  ---------

handle_info({add_buff, BuffId, Rid, Ctl, CtlTime}, State) ->
    MyBuffs = get_mybuffs(Rid),
    Buff = data_buff:get(BuffId),
    Role = get_role(Rid, State),
    if
        BuffId == 0 -> 
            ok;
        Buff == undefined -> 
            ?INFO("Undefined BuffId: ~w", [BuffId]),
            ok;
        Role == false -> ok;
        true ->
            #buff{disabled = Disabled, remove = Remove, ieffect = Ieffect} = Buff,
            case is_disabled_buff(Disabled, MyBuffs) of
                true -> ok;
                false ->
                    {ok, MyBuffs1} = remove_buff(Remove, MyBuffs, Rid, State),
                    NowTime = util:unixtime(),
                    %% #irole{pid_sender = S} = Role,
                    %% 获得上一次同类BUFF剩余的效果
                    Last = case get_mybuff(BuffId, MyBuffs1) of
                        false -> 0;
                        MyBuff0 ->
                            %% 存在同类BUFF
                            case BuffId of
                                13 -> MyBuff0#ibuff.ctl2;
                                _ ->
                                    case MyBuff0#ibuff.del_ref of
                                        undefined -> ok;
                                        Ref -> erlang:cancel_timer(Ref)
                                    end,
                                    RemainTime = MyBuff0#ibuff.ctl1 - NowTime,
                                    case RemainTime > 0 of
                                        true -> RemainTime;
                                        false -> 0
                                    end
                            end
                    end,
                    %% 同类BUFF效果叠加处理
                    MyBuff = case BuffId of
                        13 -> #ibuff{id = BuffId, ctl2 = Last + Ctl};
                        _ ->
                            CtlTime1 = case Ieffect of
                                1 -> CtlTime + Last;
                                _ -> CtlTime
                            end,
                            DelRef = erlang:send_after(CtlTime1 * 1000, self(), {del_buff, BuffId, Rid}),
                            #ibuff{id = BuffId, ctl1 = NowTime + CtlTime1, ctl2 = Ctl, del_ref = DelRef}
                    end,
                    MyBuffs2 = lists:keystore(BuffId, 2, MyBuffs1, MyBuff),
                    put_buff(Rid, MyBuffs2),
                    extra_do_add_buff(Rid, BuffId, Ctl),
                    lib_conn:pack_cast(State#state.senders, 14033, [Rid, BuffId, 1]),
                    ok
            end
    end,
    {noreply, State};

handle_info({clear_mybuffs, Rid}, State) ->
    MyBuffs = get_mybuffs(Rid),
    do_clear_buff(MyBuffs, Rid, State#state.senders),
    put_buff(Rid, []),
    {noreply, State};

handle_info({del_buff, BuffId, Rid}, State) ->
    MyBuffs = get_mybuffs(Rid),
    case do_del_buff(BuffId, MyBuffs, Rid, State) of
        {ok, MyBuffs1} -> 
            put_buff(Rid, MyBuffs1);
        {error, error_rid} -> ok;
        {error, _Reason} -> 
            ?INFO("Error when del_buff! [BuffId:~w, Rid:~w, Reason:~w]", 
                [BuffId, Rid, _Reason]),
            ok
    end,
    {noreply, State};

%% 转移BUFF
handle_info({move_buff, BuffId, FromRid, ToRid}, State) ->
    case get_mybuff(BuffId, FromRid) of
        false -> ok;
        Buff ->
            CtlTime = Buff#ibuff.ctl2 - util:unixtime(),
            case CtlTime > 0 of
                true ->
                    self() ! {del_buff, BuffId, FromRid},
                    self() ! {add_buff, BuffId, ToRid, 0, CtlTime},
                    ok;
                false ->
                    ?WARNING("Unexpected value when move_buff, ctl2:~w, CtlTime:~w", 
                        [Buff#ibuff.ctl2, CtlTime]),
                    ok
            end
    end,
    {noreply, State};
%%.

%%' set_skill
handle_info({set_skill, Rid, SkillId, Time, X, Y}, State) ->
    MySkills = get_myskills(Rid),
    case Time > 0 of
        true ->
            Ref = erlang:send_after(Time * 1000, self(), {set_skill, Rid, SkillId, 0, X, Y}),
            MySkills1 = lists:keystore(SkillId, 1, MySkills, {SkillId, Ref, X, Y}),
            put_myskill(Rid, MySkills1),
            lib_conn:pack_cast(State#state.senders, 16012, [Rid, SkillId, 1, X, Y]),
            case meet_role(Rid, X, Y, State#state.roles) of
                false ->
                    case State#state.type > 100 of
                        true ->
                            case find_npc(X, Y, State#state.npcs) of
                                false -> ok;
                                Npc -> self() ! {npc_meet_skill, Rid, Npc#inpc.id, X, Y}
                            end;
                        false ->
                            ok
                    end;
                I2 -> self() ! {meet_skill, Rid, I2#irole.id, X, Y}
            end,
            ok;
        false ->
            case get_myskill(SkillId, MySkills) of
                false -> ok;
                {_, Ref, _, _} -> erlang:cancel_timer(Ref)
            end,
            MySkills1 = del_myskill(SkillId, MySkills),
            put_myskill(Rid, MySkills1),
            del_event(X, Y, skill),
            lib_conn:pack_cast(State#state.senders, 16012, [Rid, SkillId, 0, X, Y]),
            ok
    end,
    {noreply, State};
%%.

%%' 角色投放物品，一定时间内不作用在自己身上
%% handle_info({throw_item, Tid, Rid, X, Y, Time}, State) ->
%%     case Time > 0 of 
%%         true ->
%%             put({throw_item, Tid, Rid, X, Y}, true),
%%             self() ! {fall_item, Tid, Rid, X, Y},
%%             erlang:send_after(Time * 1000, self(), {throw_item, Tid, Rid, X, Y, 0});
%%         false -> 
%%             %% 清除记录
%%             erase({throw_item, Tid, Rid, X, Y})
%%     end,
%%     {noreply, State};
%%. ============================================================

%%' 修复地板
handle_info({recover_pos, X, Y}, State) ->
    del_event(X, Y, broken_pos),
    del_event(X, Y, break),
    lib_conn:pack_cast(State#state.senders, 16001, [X, Y, 0, 0]),
    {noreply, State};
%%. =================================================

%%' 加分
handle_info({hit_npc_response, Rid, NpcId, HpMax, Hp, Type}, State) ->
    Role = get_role(Rid, State),
    Npc = get_npc(NpcId, State),
    if 
        Role == false -> 
            %% ?INFO("error hit_npc Rid:~w", [Rid]),
            ok;
        Npc == false -> 
            ?ERR("error hit_npc NpcId:~w", [NpcId]),
            ok;
        true ->
            lib_conn:pack_cast(State#state.senders, 12009, [NpcId, HpMax, Hp, Type]),
            DropRoleHp = State#state.role_hit_drop_hp,
            %% ?INFO("hit_npc_response->Id:~w, Hp:~w, Action:~w", [Rid, DropRoleHp, Npc#inpc.action]),
            case Npc#inpc.action of
                guard -> 
                    self() ! {role_sub_hp, 0, Rid, DropRoleHp, 0},
                    lib_conn:pack_send(Role#irole.pid_sender, 11020, [1]);
                _ -> skip
            end
    end,
    {noreply, State};
%%. =================================================

%%' 敲砖块
%%  Last表示上一次所敲击的对象
%%  Last = empty | brick | npc
handle_info({hit_brick, Rid, DmgArg, X, Y, X0, Y0, RowDmg, ColDmg, Dir, Last, Index}, State) ->
    case chk_event(X, Y, broken_pos) of
        false ->
            #state{npcs = Npcs, map_id = MapId, type = GameType} = State,
            case data_map_pos:get({MapId, X, Y}) > 0 of
                true ->
                    %% 会心攻击
                    {Hx, Hit} = DmgArg,
                    case dmg_npc(GameType, X, Y, Npcs, Rid, Hit, Hx, false) of
                        true -> 
                            %% 敲到怪，地板不碎
                            process_power(Rid, DmgArg, X0, Y0, State, RowDmg, ColDmg, Dir, npc, Index);
                        false ->
                            case Last of
                                npc -> ok; %% 上一次敲到了NPC，后面不再碎砖块
                                _ ->
                                    %% 地板上没有怪，地板碎
                                    case State#state.type < 100 of
                                        true ->
                                            AddBreak = Hit div 3,
                                            add_break(X, Y, AddBreak, State, Rid, 0),
                                            ok;
                                        false -> ok
                                    end,
                                    process_power(Rid, DmgArg, X0, Y0, State, 
                                        RowDmg, ColDmg, Dir, brick, Index)
                            end
                    end;
                false -> 
                    %% 不可行走的坐标，忽略
                    ok
            end;
        true -> ok
    end,
    {noreply, State};
%%. =================================================

%%' TEST
handle_info(test, State) ->
    P = self(),
    Data = {
        P
        ,length(State#state.roles)
        ,erlang:process_info(P, memory) 
        ,erlang:process_info(P, message_queue_len) 
    },
    io:format("~n~w", [Data]),
    %% ?INFO("Game State: ~p", [State]),
    %% ?INFO("Game Dict:~p", [get()]),
    %% A = lists:foldl( fun(P, Acc0) -> [{P, erlang:process_info(P, registered_name), erlang:process_info(P, memory), erlang:process_info(P, message_queue_len), erlang:process_info(P, current_function), erlang:process_info(P, initial_call)} | Acc0] end, [], [self()]),
    %% io:format("~n~p", [A]),
    erlang:send_after(util:rand(1000, 180 * 1000), self(), test),
    {noreply, State};
%%.

%%' 物品掉落(测试用)
handle_info({fall_item, Type, Rid}, State) ->
    case get_role(Rid, State) of
        false -> ?INFO("Role not found: ~w", [Rid]);
        Role ->
            Faller = case Type == 120013 of
                true -> Rid;
                false -> 0
            end,
            self() ! {fall_item, Type, Faller, Role#irole.x, Role#irole.y}
    end,
    {noreply, State};
%%. =================================================

%%' 物品掉落
handle_info({fall_item, Type, Faller, X, Y}, State) ->
    case get_event(X, Y, item) of
        undefined ->
            ItemId = get(max_item_id) + 1,
            put(max_item_id, ItemId),
            set_event(X, Y, item, {ItemId, Type, Faller}),
            lib_conn:pack_cast(State#state.senders, 16005, [ItemId, Type, Faller, X, Y]),
            ok;
        _ -> skip
    end,
    {noreply, State};
%%. =================================================

%%' set_stop_move
handle_info({set_stop_move, Rid, Val}, State) ->
    %% ?INFO("set_stop_move stop_move ~w:~w", [Val, Rid]),
    State1 = case get_role(Rid, State) of
        false -> State;
        I ->
            I1 = I#irole{stop_move = Val},
            set_role(I1, State)
    end,
    {noreply, State1};
%%. =================================================

%%' 拾取物品
handle_info({pick_daoju, Id, X, Y, ItemId, Tid}, State) ->
    case get_event(X, Y, item) of
        {ItemId, Tid, Faller} -> 
            UseType = case data_daoju:get(Tid) of
                undefined -> 0;
                #daoju{use_type = Tmp} -> Tmp
            end,
            %% IsMyFalling = get({throw_item, Tid, Id, X, Y}) == true,
            I = get_role(Id, State),
            V = data_map_pos:get({State#state.map_id, X, Y}),
            NewState = if
                I == false ->
                    State;
                I#irole.hp =< 0 ->
                    lib_conn:pack_send(I#irole.pid_sender, 11009, [0, Id, ItemId, X, Y]),
                    State;
                V =/= 1, V =/=2 ->
                    lib_conn:pack_send(I#irole.pid_sender, 11009, [0, Id, ItemId, X, Y]),
                    State;
                %% IsMyFalling ->
                %%     %% 不拾取自己[刚]扔下的物品
                %%     State;
                Tid == 130001 ->
                    %% 增加金币3
                    lib_conn:pack_cast(State#state.senders, 11009, [1, Id, ItemId, X, Y]),
                    del_event(X, Y, item),
                    set_role(I#irole{gold = I#irole.gold + 3}, State);
                Tid == 130002 ->
                    %% 增加金币10
                    lib_conn:pack_cast(State#state.senders, 11009, [1, Id, ItemId, X, Y]),
                    del_event(X, Y, item),
                    set_role(I#irole{gold = I#irole.gold + 10}, State);
                UseType == 1, I#irole.hp > 0 ->
                    %% 系统自动使用
                    use_daoju(Id, Tid, Faller, I, State),
                    lib_conn:pack_cast(State#state.senders, 11009, [1, Id, ItemId, X, Y]),
                    del_event(X, Y, item),
                    State;
                true ->
                    lib_conn:pack_cast(State#state.senders, 11009, [1, Id, ItemId, X, Y]),
                    del_event(X, Y, item),
                    %% I#irole.pid ! {handle_event, 6115, [Tid]},
                    case add_mydaoju(I#irole.mydaoju, Tid) of
                        {ok, MyDaoju} ->
                            lib_conn:pack_send(I#irole.pid_sender, 17001, [MyDaoju]),
                            set_role(I#irole{mydaoju = MyDaoju}, State);
                        {error, _} -> State
                    end
            end,
            {noreply, NewState};
        undefined ->
            case Tid of
                120016 -> 
                    %% 炸弹已被拾取，继续移动
                    self() ! {set_stop_move, Id, false};
                _ -> ok
            end,
            {noreply, State}
    end;
%%. =================================================

%%' meet_skill
handle_info({meet_skill, FromRid, Rid, X, Y}, State) ->
    case get_event(X, Y, skill) of 
        undefined -> 
            self() ! {set_stop_move, Rid, false},
            ?INFO("undefined when meet_skill, Rid:~w, ~w", [Rid, {X, Y}]),
            ok;
        Iskill ->
            #iskill{
                id = SkillId
                ,attack = Attack
                ,sort = Sort
                ,time_ef = TimeEf
                ,ctl1 = Ctl1
                ,ctl2 = Ctl2
                ,ctl3 = Ctl3
            } = Iskill,
            case check_buff(5, Rid) of
                true ->
                    %% At supper status, ignore skill harm.
                    case Sort of
                        2 -> self() ! {set_stop_move, Rid, false};
                        _ -> ok
                    end,
                    %% ?INFO("At supper when meet_skill, Rid:~w, ~w, Sort:~w", [Rid, {X, Y}, Sort]),
                    ok;
                false ->
                    DropHp = util:ceil(Attack * Ctl2 + Ctl3),
                    case Sort of
                        1 -> 
                            self() ! {add_buff, 10, Rid, Ctl1, TimeEf},
                            self() ! {skill_effect_loop, FromRid, Sort, Rid, DropHp, TimeEf},
                            ok;
                        2 -> 
                            self() ! {add_buff, 11, Rid, 0, TimeEf},
                            self() ! {skill_effect_loop, FromRid, Sort, Rid, DropHp, TimeEf},
                            ok;
                        3 -> 
                            self() ! {add_buff, 17, Rid, 0, TimeEf},
                            self() ! {se3_loop, FromRid, Rid, DropHp, TimeEf},
                            ok;
                        _ -> ok
                    end
            end,
            del_event(X, Y, skill),
            lib_conn:pack_cast(State#state.senders, 16012, [Rid, SkillId, 0, X, Y]),
            ok
    end,
    {noreply, State};

handle_info({npc_meet_skill, FromNpcId, NpcId, X, Y}, State) ->
    case get_event(X, Y, skill) of 
        undefined -> 
            ?INFO("undefined when meet_skill, NpcId:~w, ~w", [NpcId, {X, Y}]),
            ok;
        Iskill ->
            #iskill{
                id = SkillId
                ,attack = Attack
                ,sort = Sort
                ,time_ef = TimeEf
                %% ,ctl1 = Ctl1
                ,ctl2 = Ctl2
                ,ctl3 = Ctl3
            } = Iskill,
            DropHp = util:ceil(Attack * Ctl2 + Ctl3),
            case Sort of
                1 -> 
                    self() ! {npc_skill_effect_loop, FromNpcId, Sort, NpcId, DropHp, TimeEf},
                    ok;
                2 -> 
                    self() ! {npc_skill_effect_loop, FromNpcId, Sort, NpcId, DropHp, TimeEf},
                    ok;
                3 -> 
                    self() ! {npc_se3_loop, FromNpcId, NpcId, DropHp, TimeEf},
                    ok;
                _ -> ok
            end,
            del_event(X, Y, skill),
            lib_conn:pack_cast(State#state.senders, 16012, [NpcId, SkillId, 0, X, Y]),
            ok
    end,
    {noreply, State};
%%. =================================================

%%' skill effect 3 loop
%% Nth = 秒数
handle_info({se3_loop, FromRid, Rid, SubHp, Nth}, State) ->
    case Nth =< 0 orelse Nth > 4 of
        true -> 
            erase({se3_loop, Rid}),
            ok;
        false ->
            case get_role(Rid, State) of
                false -> 
                    erase({se3_loop, Rid}),
                    ok;
                I when I#irole.hp =< 0 -> 
                    erase({se3_loop, Rid}),
                    ok;
                _I ->
                    RateList = [0.4, 0.3, 0.2, 0.1],
                    Rate = lists:nth(Nth, RateList),
                    SubHp1 = util:ceil(SubHp * Rate),
                    self() ! {role_sub_hp, FromRid, Rid, SubHp1, 13},
                    %% ?INFO("se3_loop [Rid:~w, SubHp:~w, nth:~w]", [Rid, SubHp1, Nth]),
                    Ref = erlang:send_after(1000, self(), {se3_loop, FromRid, Rid, SubHp, Nth - 1}),
                    put({se3_loop, Rid}, Ref),
                    ok
            end
    end,
    {noreply, State};

handle_info({npc_se3_loop, FromNpcId, NpcId, SubHp, Nth}, State) ->
    case Nth =< 0 orelse Nth > 4 of
        true -> 
            erase({npc_se3_loop, NpcId}),
            ok;
        false ->
            case get_npc(NpcId, State) of
                false -> 
                    erase({npc_se3_loop, NpcId}),
                    ok;
                I when I#inpc.hp =< 0 -> 
                    erase({npc_se3_loop, NpcId}),
                    ok;
                _I ->
                    RateList = [0.4, 0.3, 0.2, 0.1],
                    Rate = lists:nth(Nth, RateList),
                    SubHp1 = util:ceil(SubHp * Rate),
                    self() ! {hit_npc, FromNpcId, NpcId, SubHp1},
                    Ref = erlang:send_after(1000, self(), 
                        {npc_se3_loop, FromNpcId, NpcId, SubHp, Nth - 1}),
                    put({npc_se3_loop, NpcId}, Ref),
                    ok
            end
    end,
    {noreply, State};
%%. =================================================

%%' skill_effect_loop
handle_info({skill_effect_loop, FromRid, Sort, Rid, DropHp, TimeEf}, State) ->
    case TimeEf =< 0 of
        true -> 
            erase({skill_effect_loop, Sort, Rid}),
            ok;
        false ->
            case get_role(Rid, State) of
                false -> 
                    erase({skill_effect_loop, Sort, Rid}),
                    ok;
                I when I#irole.hp =< 0 -> 
                    erase({skill_effect_loop, Sort, Rid}),
                    ok;
                _I ->
                    case get({skill_effect_loop, Sort, Rid}) of
                        undefined -> ok;
                        Ref1 -> erlang:cancel_timer(Ref1)
                    end,
                    self() ! {role_sub_hp, FromRid, Rid, DropHp, Sort + 10},
                    %% ?INFO("skill_effect_loop [Rid:~w, DropHp:~w, TimeEf:~w, Sort:~w]", [Rid, DropHp, TimeEf, Sort]),
                    Ref = erlang:send_after(1000, self(), {skill_effect_loop, FromRid, Sort, Rid, DropHp, TimeEf - 1}),
                    put({skill_effect_loop, Sort, Rid}, Ref),
                    ok
            end
    end,
    {noreply, State};

handle_info({npc_skill_effect_loop, FromNpcId, Sort, NpcId, DropHp, TimeEf}, State) ->
    case TimeEf =< 0 of
        true -> 
            erase({npc_skill_effect_loop, Sort, NpcId}),
            ok;
        false ->
            case get_npc(NpcId, State) of
                false -> 
                    erase({npc_skill_effect_loop, Sort, NpcId}),
                    ok;
                I when I#inpc.hp =< 0 -> 
                    erase({npc_skill_effect_loop, Sort, NpcId}),
                    ok;
                _I ->
                    case get({npc_skill_effect_loop, Sort, NpcId}) of
                        undefined -> ok;
                        Ref1 -> erlang:cancel_timer(Ref1)
                    end,
                    self() ! {hit_npc, FromNpcId, NpcId, DropHp},
                    Ref = erlang:send_after(1000, self(), 
                        {npc_skill_effect_loop, FromNpcId, Sort, NpcId, DropHp, TimeEf - 1}),
                    put({npc_skill_effect_loop, Sort, NpcId}, Ref),
                    ok
            end
    end,
    {noreply, State};
%%. =================================================

%%' 人往上弹
handle_info({role_up, Rid}, State) ->
    case get_role(Rid, State) of
        false -> ok;
        #irole{x = X, y = Y} -> self() ! {role_up, Rid, X, Y}
    end,
    {noreply, State};

handle_info({role_up, Rid, X, Y}, State) ->
    NewState = case get_role(Rid, State) of 
        false -> State;
        I -> 
            case chk_falling(Rid) of
                true -> I#irole.pid ! {task, ?TASK_UP_REALIVE};
                false -> ok
            end,
            del_falling(Rid),
            {X1, Y1} = get_pos(up, State#state.map_id, X, Y),
            I2 = I#irole{
                break_ref = undefined
                ,stop_move = true
            },
            cancel_timer(I#irole.break_ref),
            set_falling(Rid, up, X1, Y1, 400),
            lib_conn:pack_cast(State#state.senders, 16010, [Rid, X, Y, X1, Y1]),
            set_role(I2, State)
    end,
    {noreply, NewState};
%%. =================================================

%%' 人往下掉
handle_info({fall, Rid, From}, State) ->
    case get_role(Rid, State) of
        false -> ok;
        #irole{x = X, y = Y} -> self() ! {fall, Rid, From, X, Y}
    end,
    {noreply, State};

handle_info({fall, Rid, From, X0, Y0}, State) ->
    I = get_role(Rid, State),
    IsFalling = chk_falling(Rid),
    NewState = if
        I == false -> 
            ?INFO("Role not found when fall, ~w", [[Rid, From, X0, Y0]]),
            State;
        I#irole.hp =< 0 -> 
            ?INFO("Hp=<0 when fall, ~w", [[Rid, From, X0, Y0]]),
            State;
        IsFalling ->
            ?INFO("repeat falling, ~w", [[Rid, From, X0, Y0]]),
            State;
        true -> 
            {X, Y} = get_pos(down, State#state.map_id, X0, Y0),
            cancel_timer(I#irole.break_ref),
            I2 = I#irole{
                break_ref = undefined
                ,stop_move = true
            },
            %% ?INFO("fall:~w ~w", [Rid, [X0, Y0, X, Y]]),
            %% 广播摔落消息
            Type = case From of
                fall -> 2;
                _ -> 0
            end,
            lib_conn:pack_cast(State#state.senders, 16003, [Rid, Type, X0, Y0, X, Y]),
            set_falling(Rid, From, X, Y, 900),
            set_role(I2, State)
    end,
    {noreply, NewState};

%% From = fall | broken | {broken, Rid} | up
%%        fall          = 瀑布
%%        broken        = 砖块碎,或在梯子上站太久
%%        {broken, Rid} = 砖块被别人敲碎
%%        up            = 向上弹
handle_info({reached_pos, Rid, From, X, Y}, State) ->
    I = get_role(Rid, State),
    IsBroken = chk_event(X, Y, broken_pos),
    NewState = if
        I == false -> 
            State;
        IsBroken ->
            del_falling(Rid),
            case From of
                up ->
                    {X1, Y1} = get_pos(up, State#state.map_id, X, Y),
                    self() ! {role_up, Rid, X1, Y1},
                    ok;
                _ ->
                    {X1, Y1} = get_pos(down, State#state.map_id, X, Y),
                    self() ! {fall, Rid, From, X1, Y1},
                    ok
            end,
            %% ?INFO("continue fall:~w, ~w", [Rid, [{X, Y}, {X1, Y1}]]),
            State;
        I#irole.hp =< 0 ->
            case get({re_alive_ref, Rid}) of
                undefined -> 
                    %% ?INFO("*** 1 *** hp=<0 when reached_pos, Mode:~w, ~w", [State#state.type, [Rid, From, X, Y]]),
                    ok;
                {Ref1, X1, Y1} ->
                    %% ?INFO("*** 2 *** hp=<0 when reached_pos, Mode:~w, ~w", [State#state.type, [Rid, From, X, Y]]),
                    erlang:cancel_timer(Ref1),
                    Time = data_config:get(re_alive_time),
                    ReAliveRef = erlang:send_after(Time, self(), {re_alive, Rid, X, Y}),
                    del_event(X1, Y1, dead),
                    set_event(X, Y, dead, true),
                    put({re_alive_ref, Rid}, ReAliveRef),
                    ok
            end,
            del_falling(Rid),
            V = get_pos_val(State#state.map_id, X, Y),
            I2 = I#irole{
                x0 = I#irole.x
                ,y0 = I#irole.y
                ,x = X
                ,y = Y
                ,v = V
                ,break_ref = undefined
                ,stop_move = false
            },
            lib_conn:pack_send(I#irole.pid_sender, 16004, []),
            set_role(I2, State);
        true ->
            case From of
                broken ->
                    self() ! {role_sub_hp, 0, Rid, State#state.falled_hp, 1};
                {broken, FromRid} ->
                    self() ! {role_sub_hp, FromRid, Rid, State#state.falled_hp, 1};
                _ -> ok
            end,
            del_falling(Rid),
            Time = data_config:get(break_interval_time),
            V = get_pos_val(State#state.map_id, X, Y),
            I2 = I#irole{
                x0 = I#irole.x
                ,y0 = I#irole.y
                ,x = X
                ,y = Y
                ,v = V
                ,break_ref = gen_break_ref(Rid, X, Y, V, Time)
                ,stop_move = false
            },
            %% ?INFO("reached_pos stop_move false:~w ~w", [Rid, [X, Y]]),
            I3 = move_trigger(I2, X, Y, State#state.roles),
            lib_conn:pack_send(I#irole.pid_sender, 16004, []),
            set_role(I3, State)
    end,
    {noreply, NewState};
%%. =================================================

%%' 地板破碎(破碎程度加1)
handle_info({break, Rid, X, Y, V}, State) ->
    NewState = case get_role(Rid, State) of
        false -> State;
        I ->
            #irole{hp = Hp, x = X1, y = Y1} = I,
            %% 处理破碎动作
            add_break(X, Y, State, Rid, 0),
            %% 如果玩家没有死，并且还在原来格子， 继续触发break
            case Hp > 0 andalso X1 == X andalso Y1 == Y of
                true -> 
                    NewI = I#irole{break_ref = gen_break_ref(Rid, X, Y, V)},
                    set_role(NewI, State);
                false -> State
            end
    end,
    {noreply, NewState};
%%. =================================================

%%' 让角色所处的坐标点破碎
handle_info({broken, Rid}, State) ->
    case get_role(Rid, State) of
        false -> 
            ok;
        I ->
            #irole{x = X, y = Y} = I,
            set_break(X, Y, 99, State, 0, 0),
            ok
    end,
    {noreply, State};
%%. =================================================

%%' 角色移动
%%  int8 当前动作(0移动,4敲击)
%%  int8 当前方向(37左,38上,39右,40下)
%%  int8 移动类型:
%%          1  = 点对点
%%          2  = 停留
%%          3  = 朝一个方向行走
%%          98 = 每个格子发送一次更新位置坐标
%%          99 = 直接飞到当前坐标
handle_info({move, Id, Act, Dir, MoveType, Z, XX, YY, X1, Y1}, 
    State) when MoveType == 98 ->
    V1 = get_pos_val(State#state.map_id, X1, Y1),
    I = get_role(Id, State),
    IsBroken = chk_event(X1, Y1, broken_pos),
    NewState = if
        I == false ->
            State;
        V1 == 0 ->
            lib_conn:pack_cast(State#state.senders, 16009, 
                [Id, I#irole.x, I#irole.y]),
            %% ?INFO("Move to blank pos!  [Rid:~w, MapId:~w ~w <- ~w -> ~w]", 
            %%     [Id, State#state.map_id, {I#irole.x0, I#irole.y0}, {I#irole.x, I#irole.y}, {X1, Y1}]),
            State;
        I#irole.hp =< 0 ->
            %% ?INFO("Still move when HP=<0 ! [Rid:~w]", [Id]),
            State;
        IsBroken ->
            case chk_falling(Id) of
                true -> 
                    %% 摔落过程中，或者是不用处理的数据，忽略
                    State;
                false -> 
                    #irole{break_ref = BreakRef} = I,
                    cancel_timer(BreakRef),
                    {X2, Y2} = case {I#irole.x, I#irole.y} of
                        {X1, Y1} -> 
                            ?INFO("Return ~w! [Rid:~w, MapId:~w ~w <- ~w -> ~w]", 
                                [{X1, Y1}, Id, State#state.map_id, 
                                    {I#irole.x0, I#irole.y0}, 
                                    {I#irole.x, I#irole.y}, {X1, Y1}]),
                            {I#irole.x0, I#irole.y0};
                        Tmp -> Tmp
                    end,
                    V2 = data_map_pos:get({State#state.map_id, X2, Y2}),
                    IsBroken1 = chk_event(X2, Y2, broken_pos),
                    case IsBroken1 == false andalso V2 > 0 of
                        true -> 
                            lib_conn:pack_cast(State#state.senders, 16009, [Id, X2, Y2]);
                        false -> 
                            ?INFO("fall! [Rid:~w, MapId:~w ~w <- ~w -> ~w]", 
                                [Id, State#state.map_id, {I#irole.x0, I#irole.y0}, {I#irole.x, I#irole.y}, {X1, Y1}]),
                            self() ! {fall, Id, broken, X1, Y1}
                    end,
                    I2 = I#irole{
                        break_ref = undefined
                        ,stop_move = false
                    },
                    set_role(I2, State)
            end;
        I#irole.stop_move ->
            case chk_falling(Id) of
                true -> 
                    %% ?INFO("Still move when stop_move and falling! [Rid:~w]", [Id]),
                    ok;
                false ->
                    lib_conn:pack_cast(State#state.senders, 16009, 
                        [Id, I#irole.x, I#irole.y]),
                    ?INFO("Still move when stop_move! [Rid:~w, MapId:~w ~w <- ~w -> ~w, Hp:~w]", 
                        [Id, State#state.map_id, {I#irole.x0, I#irole.y0}, {I#irole.x, I#irole.y}, {X1, Y1}, I#irole.hp]),
                    ok
            end,
            State;
        true ->
            lib_conn:pack_cast(State#state.senders, 11003, [Id, Act, Dir, MoveType, Z, XX, YY]),
            #irole{x0 = X0, y0 = Y0, x = X, y = Y, v = V, break_ref = BR} = I,
            BreakRef = if 
                State#state.type > 100 -> undefined;
                BR == undefined -> gen_break_ref(Id, X1, Y1, V1);
                true ->
                    if
                        (V == 3 orelse V == 4) andalso (V1 == 3 orelse V1 == 4) ->
                            %% 仍然在楼梯中
                            %% ?INFO("仍然在楼梯中"),
                            BR;
                        (V == 3 orelse V == 4) andalso (V1 == 1 orelse V1 == 2) ->
                            %% 走出楼梯
                            %% ?INFO("走出楼梯"),
                            cancel_timer(BR),
                            erlang:send_after(3000, self(), {break, Id, X1, Y1, V1});
                        (V == 1 orelse V == 2) andalso (V1 == 3 orelse V1 == 4) ->
                            %% 走进楼梯
                            %% ?INFO("走进楼梯"),
                            cancel_timer(BR),
                            FallTime = data_config:get(fall_time),
                            erlang:send_after(FallTime, self(), {fall, Id, broken});
                        X == X1 andalso Y == Y1 ->
                            %% 仍在原来格子
                            %% ?INFO("仍在原来格子"),
                            BR;
                        true ->
                            %% ?INFO("正常走"),
                            cancel_timer(BR),
                            erlang:send_after(3000, self(), {break, Id, X1, Y1, V1})
                    end
            end,
            I2 = move_trigger(I, X1, Y1, State#state.roles),
            {NewX0, NewY0} = case X =/= X0 orelse Y =/= Y0 of
                true -> {X, Y};
                false -> {X0, Y0}
            end,
            I3 = I2#irole{
                x0 = NewX0
                ,y0 = NewY0
                ,x = X1
                ,y = Y1
                ,v = V1
                ,break_ref = BreakRef
            },
            set_role(I3, State)
    end,
    {noreply, NewState};

%% 角色移动(仅转发数据)
handle_info({move, Id, Act = 9, Dir, MoveType, Z, XX, YY, _X, _Y}, State) ->
    lib_conn:pack_cast(State#state.senders, 11003, 
        [Id, Act, Dir, MoveType, Z, XX, YY]),
    {noreply, State};

handle_info({move, Id, Act, Dir, MoveType, Z, XX, YY, X, Y}, 
    State) ->
    I = get_role(Id, State),
    IsBroken = chk_event(X, Y, broken_pos),
    V = get_pos_val(State#state.map_id, X, Y),
    if
        V == 0 -> ok;
        I == false -> ok;
        I#irole.hp =< 0 -> ok;
        I#irole.stop_move -> 
            %% ?INFO("2 Still move when stop_move! 
            %%     [Id:~w, Act:~w, Dir:~w, MoveType:~w, X:~w, Y:~w]", 
            %%     [Id, Act, Dir, MoveType, X, Y]),
            ok;
        IsBroken -> ok;
        true ->
            lib_conn:pack_cast(State#state.senders, 11003, 
                [Id, Act, Dir, MoveType, Z, XX, YY])
    end,
    {noreply, State};

handle_info({force_move, Id, X, Y, V}, State) ->
    I = get_role(Id, State),
    State1 = if
        I == false -> 
            ?WARNING("I==false when force_move, Id:~w", [Id]),
            State;
        I#irole.hp =< 0 -> 
            ?WARNING("hp=<0 when force_move, Id:~w", [Id]),
            State;
        true ->
            #irole{break_ref = BreakRef} = I,
            cancel_timer(BreakRef),
            case chk_event(X, Y, broken_pos) of
                true -> 
                    self() ! {fall, Id, broken, X, Y},
                    State;
                false -> 
                    I2 = move_trigger(I, X, Y, State#state.roles),
                    I3 = I2#irole{
                        x0 = I2#irole.x
                        ,y0 = I2#irole.y
                        ,x = X
                        ,y = Y
                        ,v = V
                        ,break_ref = gen_break_ref(Id, X, Y, V)
                        ,stop_move = false
                    },
                    lib_conn:pack_cast(State#state.senders, 16009, [Id, X, Y]),
                    set_role(I3, State)
            end
    end,
    {noreply, State1};
%%. =================================================

%%' NPC AI
handle_info({npc_action, Id, Action}, State) ->
    Ni = get_npc(Id, State),
    NewNi = if
        Ni == false -> Ni;
        true ->
            {ActionVal, {X, Y}} = if
                Action == hidden -> {0, {Ni#inpc.x, Ni#inpc.y}}; %% hidden
                Ni#inpc.action == hidden -> {1, rand_pos2(State)}; %% pop up
                true -> {2, {Ni#inpc.x, Ni#inpc.y}} %% switch
            end,
            %% ?INFO("Id:~w, ActionVal:~w, X:~w, Y:~w", [Id, ActionVal, X, Y]),
            case Action of
                hidden -> 
                    lib_conn:pack_cast(State#state.senders, 12011, [Id, ActionVal, 0, X, Y]);
                show -> 
                    lib_conn:pack_cast(State#state.senders, 12011, [Id, ActionVal, 0, X, Y]);
                guard -> 
                    lib_conn:pack_cast(State#state.senders, 12011, [Id, ActionVal, 1, X, Y]);
                _Other ->
                    ?WARNING("Action(~w) undefined", [_Other])
            end,
            Ni#inpc{action = Action, x = X, y = Y}
    end,
    NewState = set_npc(NewNi, State),
    {noreply, NewState};
%%. =================================================

%%' 广播消息
handle_info({pack_cast, Cmd, Data}, State) ->
    lib_conn:pack_cast(State#state.senders, Cmd, Data),
    {noreply, State};
%%. =================================================

%%' 玩家扣血
%% Type: 掉落类型
%%         0=其它
%%         1=摔落
%%         11=技能(类型1)
%%         12=技能(类型2)
%%         13=技能(类型3)
handle_info({role_sub_hp, FromRid, Rid, DropHp, Type}, State) ->
    Role = get_role(Rid, State),
    NewState = if
        Role == false -> State;
        Role#irole.hp =< 0 -> 
            %% ?INFO("role_sub_hp when hp =< 0, Rid:~w, SubHp:~w, Type:~w", [Rid, DropHp, Type]),
            State;
        true ->
            MyBuffs = get_mybuffs(Rid),
            case check_buff(5, MyBuffs) of
                true ->
                    %% 无敌中
                    State;
                false ->
                    DropHp1 = case Type == 1 of 
                        true -> DropHp;
                        false ->
                            case use_buff(14, DropHp, MyBuffs, Rid, State) of
                                {ok, SubHp1, MyBuffs1} -> 
                                    put_buff(Rid, MyBuffs1),
                                    lib_conn:pack_cast(State#state.senders, 14053, [Rid, SubHp1]),
                                    %% ?INFO("使用了护盾：扣血(~w) - 吸收(~w) = 实际扣血(~w)", 
                                    %% [DropHp, SubHp1, DropHp - SubHp1]),
                                    DropHp - SubHp1;
                                {error, _} -> DropHp
                            end
                    end,
                    case DropHp1 =< 0 of
                        true -> State;
                        false ->
                            #irole{break_ref = BreakRef, hp_max = HpMax, x = X, y = Y, team = Team} = Role,
                            Hp = Role#irole.hp - DropHp1,
                            FromRole = get_role(FromRid, State),
                            if
                                FromRole == false -> ok;
                                Team == FromRole#irole.team ->
                                    %% 误伤
                                    lib_conn:pack_send(FromRole#irole.pid_sender, 14050, [Rid]),
                                    ok;
                                Team =/= FromRole#irole.team andalso Hp =< 0 ->
                                    case State#state.type of
                                        1 -> 
                                            %% 完成击杀角色任务
                                            FromRole#irole.pid ! {task, ?TASK_KILL_ROLE1};
                                        2 -> 
                                            %% 记录对战中的杀人数
                                            self() ! {kill, FromRid, 1, 2, Rid},
                                            %% 完成击杀角色任务
                                            FromRole#irole.pid ! {task, ?TASK_KILL_ROLE2};
                                        _ -> ok
                                    end,
                                    FromRole#irole.pid ! {task, ?TASK_KILL_ROLE},
                                    %% 被人陷害，给陷害者加分
                                    Score = data_config:get(fall_score),
                                    self() ! {add_score, FromRid, Score, 2, Rid};
                                    ok;
                                true -> ok
                            end,
                            {BreakRef1, Hp1, HoldTime} = case Hp > 0 of
                                true -> 
                                    lib_conn:pack_cast(State#state.senders, 11018, [Rid, Hp, HpMax, Type]),
                                    {BreakRef, Hp, 999};
                                false -> 
                                    lib_conn:pack_cast(State#state.senders, 11019, [Rid, X, Y, Type]),
                                    self() ! game_over_check,
                                    case State#state.type of
                                        1 ->
                                            %% 积分模式，复活处理
                                            Time = case Type of
                                                1 -> 800 + data_config:get(re_alive_time);
                                                _ -> data_config:get(re_alive_time)
                                            end,
                                            ReAliveRef = erlang:send_after(Time, self(), {re_alive, Rid, X, Y}),
                                            put({re_alive_ref, Rid}, {ReAliveRef, X, Y}),
                                            set_event(X, Y, dead, true),
                                            self() ! {clear_mybuffs, Rid},
                                            ok;
                                        _ ->
                                            ok
                                    end,
                                    cancel_timer(BreakRef),
                                    {undefined, 0, util:unixtime() - State#state.start_time}
                            end,
                            %% Role#irole.pid ! {add_attr, hp, -DropHp1},
                            NewRole = Role#irole{hp = Hp1, break_ref = BreakRef1, hold_time = HoldTime},
                            set_role(NewRole, State)
                    end
            end
    end,
    {noreply, NewState};
%%. =================================================

%%' 复活
handle_info({re_alive, Rid, X, Y}, State) ->
    NewState = case get_role(Rid, State) of 
        false -> State;
        I ->
            erase({re_alive_ref, Rid}),
            #irole{hp_max = HpMax} = I,
            del_event(X, Y, dead),
            I2 = I#irole{hp = HpMax, x = X, y = Y, stop_move = false},
            %% 复活后无敌3秒
            self() ! {add_buff, 5, Rid, 0, 3},
            lib_conn:pack_cast(State#state.senders, 14031, [Rid, X, Y]),
            lib_conn:pack_cast(State#state.senders, 11018, [Rid, HpMax, HpMax, 2]),
            I3 = move_trigger(I2, X, Y, State#state.roles),
            set_role(I3, State)
    end,
    {noreply, NewState};
%%. =================================================

%% 检查是否游戏结束
handle_info(game_over_check, State) ->
    case length([R || R <- State#state.roles, R#irole.team == 1]) of
        0 -> erlang:send_after(1000, self(), game_over);
        _ -> 
            case length([R || R <- State#state.roles, R#irole.team == 2]) of
                0 -> erlang:send_after(1000, self(), game_over);
                _ -> ok
            end
    end,
    {noreply, State};
%%.

%%' 游戏结束 game over
handle_info(game_over, State) ->
    case get(game_over_sign) of
        undefined ->
            put(game_over_sign, true),
            %% F0 = fun(I) ->
            %%         I#irole.pid_room1 ! {ready_reset, I#irole.id}
            %% end,
            %% lists:foreach(F0, State#state.roles),
            [gen_fsm:send_all_state_event(NpcInfo#inpc.pid, stop) 
                || NpcInfo <- State#state.npcs],
            F1 = fun(I, Acc) -> I#irole.score + Acc end,
            Team1 = [I || I <- State#state.roles, I#irole.team == 1],
            Team2 = [I || I <- State#state.roles, I#irole.team == 2],
            IsGuildTeam1 = is_guild_team(Team1),
            IsGuildTeam2 = is_guild_team(Team2),
            IsWithOsex1 = is_with_osex(Team1),
            IsWithOsex2 = is_with_osex(Team2),
            TeamNum1 = length(Team1),
            TeamNum2 = length(Team2),
            TeamScore1 = lists:foldl(F1, 0, Team1),
            TeamScore2 = lists:foldl(F1, 0, Team2),
            Differ = erlang:abs(TeamScore1 - TeamScore2),
            GameMode = State#state.type,
            WinTeam = if
                TeamNum1 == 0 -> 2;
                TeamNum2 == 0 -> 1;
                TeamScore1 > TeamScore2 -> 1;
                TeamScore1 < TeamScore2 -> 2;
                TeamScore1 == TeamScore2 -> 0
            end,
            F2 = fun(I) ->
                    BaseExp = case WinTeam == I#irole.team of
                        true -> I#irole.score * 1 + 1 * I#irole.lev * 5;
                        false -> I#irole.score * 1
                    end,
                    {TeamNum, IsGuildTeam, IsWithOsex} = case I#irole.team of
                        1 -> {TeamNum1, IsGuildTeam1, IsWithOsex1};
                        2 -> {TeamNum2, IsGuildTeam2, IsWithOsex2}
                    end,
                    CardAdd = case I#irole.exp_card > 1 of
                        true -> BaseExp * (I#irole.exp_card - 1);
                        false -> 0
                    end,
                    IsWin = case WinTeam == I#irole.team of
                        true -> 1;
                        false -> case WinTeam == 0 of
                                true -> 0;
                                false -> -1
                            end
                    end,
                    I#irole.pid_room1 ! {ready_reset, I#irole.id, IsWin},
                    lib_conn:pack_send(I#irole.pid_sender, 14008, [IsWin+2, Differ]),
                    Gold = I#irole.gold + util:ceil(I#irole.score * 0.2),
                    I#irole.pid ! {handle_event, 6111, [
                            self()
                            ,GameMode 
                            ,I#irole.score 
                            ,IsWin
                            ,Gold
                            ,BaseExp
                            ,TeamNum
                            ,CardAdd
                            ,IsGuildTeam
                            ,IsWithOsex 
                            ,I#irole.mydaoju
                            ,I#irole.hold_time
                        ]}
            end,
            lists:foreach(F2, State#state.roles),
            put(result_len, length(State#state.roles)),
            erlang:send_after(10000, self(), force_stop),
            ok;
        true -> ok
    end,
    {noreply, State};

handle_info({game_result, Result}, State) ->
    GameResults = case get(game_results) of
        undefined -> [];
        R -> R
    end,
    GameResults1 = [Result | GameResults],
    ResultLen = case get(result_len) of
        undefined -> length(State#state.roles);
        Len -> Len
    end,
    case ResultLen == length(GameResults1) of
        true ->
            lib_conn:pack_cast(State#state.senders, 14021, [GameResults1]),
            {stop, normal, State};
        false ->
            put(game_results, GameResults1),
            {noreply, State}
    end;

handle_info(force_stop, State) ->
    {stop, normal, State};

%%. =================================================

%%' 定时循环
handle_info(loop, State) ->
    RemainTime = State#state.end_time - util:unixtime(),
    case RemainTime > 0 of 
        true -> 
            lib_conn:pack_cast(State#state.senders, 14019, [RemainTime]),
            erlang:send_after(10000, self(), loop);
        false -> self() ! game_over
    end,
    {noreply, State};
%%. =================================================

handle_info(_Info, State) ->
    ?INFO("Not matched info: ~w, Game type: ~w", [_Info, State#state.type]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

%%' normal get/set

%% get_items(Rid) ->
%%     case get({items, Rid}) of
%%         undefined -> [];
%%         Items -> Items 
%%     end.
%% 
%% set_items(Rid, Items) ->
%%     put({items, Rid}, Items).

get_role(Rid, State) ->
    lists:keyfind(Rid, 2, State#state.roles).

set_role(Role, State) ->
    L = lists:keyreplace(Role#irole.id, 2, State#state.roles, Role),
    State#state{roles = L}.

get_npc(Id, State) ->
    lists:keyfind(Id, 2, State#state.npcs).

set_npc(Ni, State) ->
    L = lists:keyreplace(Ni#inpc.id, 2, State#state.npcs, Ni),
    State#state{npcs = L}.

get_break(X, Y1) ->
    Y = case Y1 of
        0 -> 18; %% 和set_break保存时对应
        _ -> Y1
    end,
    case get_event(X, Y, break) of 
        undefined -> 0;
        Val -> Val
    end.

%% 砖块破碎程度加1
add_break(X, Y, State, Rid, Type) ->
    add_break(X, Y, 1, State, Rid, Type).
add_break(X, Y1, AddDepth, State, Rid, Type) ->
    Y = case Y1 of
        0 -> 18; %% 最后一行保存为18行
        _ -> Y1
    end,
    Break = get_break(X, Y),
    MaxDepth = data_map_depth:get({State#state.map_id, X, Y}),
    case Break >= MaxDepth of
        true -> 
            %% 己经破碎
            skip;
        false -> set_break(X, Y, Break + AddDepth, State, Rid, Type)
    end.

%% 设置砖块破碎程度
%% Rid  : 让砖块破碎的角色ID，可为0
%% Type : 类型(0=正常,1=会心)
set_break(X, Y1, Depth, State, Rid, Type) ->
    Y = fix_y(Y1),
    case Depth > 0 andalso chk_event(X, Y, dead) of
        true ->
            %% 有尸体在，不破碎
            skip;
        false ->
            MapId = State#state.map_id,
            MaxDepth = data_map_depth:get({MapId, X, Y}),
            case MaxDepth > 0 of
                true ->
                    Depth1 = case Depth > MaxDepth of
                        true -> MaxDepth;
                        false -> Depth
                    end,
                    set_event(X, Y, break, Depth1),
                    lib_conn:pack_cast(State#state.senders, 16001, [X, Y, Depth1, Type]),
                    case Depth1 >= MaxDepth of 
                        true -> 
                            set_event(X, Y, broken_pos, true),
                            del_event(X, Y, item),
                            #state{falled_hp = DropHp, roles = Roles, senders = Senders, 
                                map_id = MapId, npcs = Npcs} = State,
                            hidden_npc(X, Y, Npcs),
                            %% 检测该格子上是否有人或怪
                            Team = case get_role(Rid, State) of
                                false -> 0;
                                I -> I#irole.team 
                            end,
                            fall(role, Rid, Team, DropHp, X, Y, Roles, Senders, MapId),
                            RecoverTime = data_config:get(break_recover_time),
                            erlang:send_after(RecoverTime, self(), {recover_pos, X, Y});
                        false -> skip
                    end;
                false -> skip
            end
    end.

%% 不包括有[怪｜完全破碎｜人]的点
rand_pos(State) ->
    rand_pos(State, 0).
rand_pos(State, Index) when Index < 100 ->
    MapId = State#state.map_id,
    {X, Y} = lib_map:rand_pos(MapId),
    case chk_event(X, Y, broken_pos) of
        false ->
            case is_npc_pos(State#state.npcs, X, Y) of
                true -> rand_pos(State, Index + 1);
                false ->
                    case is_role_pos(State#state.roles, X, Y) of
                        true -> rand_pos(State, Index + 1);
                        false -> {X, Y}
                    end
            end;
        true -> rand_pos(State, Index + 1)
    end;
rand_pos(State, Index) -> 
    ?WARNING("rand_pos runing times too mary! [Index:~w]", [Index]),
    MapId = State#state.map_id,
    lib_map:rand_pos(MapId).

%% 不包括有[怪/完全破碎/道具/技能]的点
rand_pos2(State) ->
    rand_pos2(State, 0).
rand_pos2(State, Index) when Index < 100 ->
    MapId = State#state.map_id,
    {X, Y} = lib_map:rand_pos(MapId),
    case chk_events(X, Y, [broken_pos, item, fall, skill]) of
        true -> rand_pos2(State, Index + 1);
        false -> 
            case is_npc_pos(State#state.npcs, X, Y) of
                true -> rand_pos2(State, Index + 1);
                false -> {X, Y}
            end
    end;
rand_pos2(State, Index) -> 
    ?WARNING("rand_pos2 runing times too mary! [Mode:~w, MapId:~w, Index:~w]", [State#state.type, State#state.map_id, Index]),
    MapId = State#state.map_id,
    lib_map:rand_pos(MapId).

is_npc_pos([Npc | T], X, Y) ->
    case Npc#inpc.x == X andalso Npc#inpc.y == Y of
        true -> true;
        false -> is_npc_pos(T, X, Y)
    end;
is_npc_pos([], _X, _Y) -> false.

is_role_pos([Role | T], X, Y) ->
    case Role#irole.x == X andalso Role#irole.y == Y of
        true -> true;
        false -> is_role_pos(T, X, Y)
    end;
is_role_pos([], _X, _Y) -> false.

%%. =================================================

%%' NPC生成
gen_npcs(0, State) -> State;
gen_npcs(Num, State) ->
    State1 = gen_npc(0, State),
    gen_npcs(Num - 1, State1).

gen_npc(TypeIn, #state{npc_pro_rate = ProRate, npcs = Npcs, 
        map_id = MapId, avg_lev = AvgLev} = State) ->
    Type = case TypeIn > 0 of
        true -> TypeIn;
        false ->
            Rand = util:rand(1, 100),
            rand_npc_type(Rand, ProRate, ProRate, Npcs)
    end,
    case Type > 0 of
        true ->
            Id = get(max_npc_id) + 1,
            put(max_npc_id, Id),
            {X, Y} = rand_pos2(State),
            Pid = srv_npc:start([1, Id, Type, MapId, X, Y, self(), AvgLev]),
            Npc = #inpc{id = Id, type = Type, x = X, y = Y, pid = Pid},
            State#state{npcs = [Npc | Npcs]};
        false -> 
            State
    end.

rand_npc_type(Rand, [{Tid, Min, Max} | T], ProRate, Npcs) ->
    case Rand >= Min andalso Rand =< Max of
        true -> Tid;
        false -> rand_npc_type(Rand, T, ProRate, Npcs)
    end;
rand_npc_type(_, [], _, _Npcs) -> 0.
%%. ===================================================================

%%' 角色动作处理

%% 解析力道
process_power(_Rid, _Hit, _X, _Y, _State, 0, 0, _Dir, _Last, _Index) ->
    ok;

%% →
process_power(Rid, Hit, X, Y, _State, RowDmg, _ColDmg, right, Last, Index) ->
    if
        Index < RowDmg ->
            Index1 = Index + 1,
            erlang:send_after(?HIT_ROW_INTERVAL_TIME, self(), {hit_brick, Rid, Hit, X + Index1, Y,
                    X, Y, RowDmg, 0, right, Last, Index1});
        true ->
            ok
    end;

%% ←
process_power(Rid, Hit, X, Y, _State, RowDmg, _ColDmg, left, Last, Index) ->
    if
        Index < RowDmg ->
            Index1 = Index + 1,
            erlang:send_after(?HIT_ROW_INTERVAL_TIME, self(), {hit_brick, Rid, Hit, X - Index1, Y,
                    X, Y, RowDmg, 0, left, Last, Index1});
        true ->
            ok
    end;

%% ↑向上依次处理
process_power(Rid, Hit, X, Y, _State, _RowDmg, ColDmg, up, Last, Index) ->
    case Index < ColDmg of
        true ->
            Index1 = Index + 1,
            Y1 = Y - Index1 * 3,
            Y2 = case Y1 =< 0 of
                true -> Y1 + 18;
                false -> Y1
            end,
            erlang:send_after(?HIT_COL_INTERVAL_TIME, self(), {hit_brick, Rid, Hit, X, Y2,
                    X, Y, 0, ColDmg, up, Last, Index1});
        false -> ok
    end;

%% ↓
process_power(Rid, Hit, X, Y, _State, _RowDmg, ColDmg, down, Last, Index) ->
    if
        Index < ColDmg ->
            Index1 = Index + 1,
            Y1 = Y + Index1 * 3,
            Y2 = case Y1 > 18 of
                true -> Y1 - 18;
                false -> Y1
            end,
            erlang:send_after(?HIT_COL_INTERVAL_TIME, self(), {hit_brick, Rid, Hit, X, Y2,
                    X, Y, 0, ColDmg, down, Last, Index1});
        true ->
            ok
    end;

%% undefined
process_power(Rid, Hit, X, Y, _State, _RowDmg, _ColDmg, undefined, _Last, _Index) ->
    ?WARNING("undefined hit dir! [Rid:~w, Hit:~w, X:~w, Y:~w]", [Rid, Hit, X, Y]).
%%.

%%' 检查地板上是否有怪可攻击
%% @spec () -> true | false
dmg_npc(1, X, Y, [I | T], Rid, Hit, Type, Reply) ->
    Reply1 = case I#inpc.action =/= hidden andalso I#inpc.x =:= X andalso I#inpc.y =:= Y of 
        true -> 
            gen_fsm:send_all_state_event(I#inpc.pid, {hit, Rid, Hit, Type}),
            true;
        false -> Reply
    end,
    dmg_npc(1, X, Y, T, Rid, Hit, Type, Reply1);

dmg_npc(2, X, Y, [I | T], Rid, Hit, Type, Reply) ->
    Reply1 = case I#inpc.x =:= X andalso I#inpc.y =:= Y of 
        true -> 
            gen_fsm:send_all_state_event(I#inpc.pid, {hit, Rid, Hit, Type}),
            true;
        false -> Reply
    end,
    dmg_npc(2, X, Y, T, Rid, Hit, Type, Reply1);

dmg_npc(Mode, X, Y, [I | T], Rid, Hit, Type, Reply) when Mode > 100 ->
    Reply1 = case I#inpc.x =:= X andalso I#inpc.y =:= Y of 
        true -> 
            NpcId = case I#inpc.type == 0 of
                true -> I#inpc.id div 1000;
                false -> I#inpc.id
            end,
            self() ! {hit_npc, Rid, NpcId, Hit},
            true;
        false -> Reply
    end,
    dmg_npc(Mode, X, Y, T, Rid, Hit, Type, Reply1);

dmg_npc(_GameType, _X, _Y, [], _Rid, _Hit, _Type, Reply) ->
    Reply.
%%. ================================================

%%' 砖块破碎，检测是否有角色在该砖块上，如果有则让该角色往下掉
%% Rid = 攻击者ID，可为0
%% DropHp = 被摔落的角色扣血值
fall(role, Rid, Team, DropHp, X, Y, [I | T], Senders, MapId) ->
    #irole{id = TgRid, x = X1, y = Y1, hp = Hp} = I,
    case X1 =:= X andalso Y1 =:= Y andalso Hp > 0 of 
        true -> self() ! {fall, TgRid, {broken, Rid}, X, Y};
        false -> ok
    end,
    fall(role, Rid, Team, DropHp, X, Y, T, Senders, MapId);
fall(role, _, _, _, _X, _Y, [], _Senders, _MapId) ->
    ok.

hidden_npc(X, Y, [#inpc{pid = Pid, x = X, y = Y} | T]) ->
    gen_fsm:send_all_state_event(Pid, hidden),
    hidden_npc(X, Y, T);
hidden_npc(X, Y, [_ | T]) ->
    hidden_npc(X, Y, T);
hidden_npc(_X, _Y, []) ->
    ok.
%%.

%%' === 获得特定坐标点 ===
get_pos(Type, MapId, X, Y) ->
    get_pos(Type, MapId, X, Y, 0, X, Y, 0, 0).

%% index记录递归次数，防止死循环
%% Index1为寻找Y轴的次数
%% Index2为寻找X轴的次数

get_pos(Type, MapId, X0, Y0, Offset, X, Y, Index1, Index2) when Index1 < 6 ->
    Add = case data_map_pos:get({MapId, X, Y}) of
        3 -> 1;
        4 -> 1;
        _ -> 3
    end,
    Y1 = case Type of
        up -> Y - Add;
        down -> Y + Add
    end,
    NewY = if
        Y1 > 18 -> Y1 - 18;
        Y1 =< 0 -> Y1 + 18;
        true -> Y1
    end,
    %% ?INFO("get_pos ~w ~w: X:~w, Y(~w -> ~w)", [Index1, Type, X, Y, NewY]),
    case data_map_pos:get({MapId, X, NewY}) =< 0 
        orelse chk_events(X, NewY, [broken_pos, fall])
        orelse (data_map_pos:get({MapId, X+1, NewY}) =< 0 
            andalso data_map_pos:get({MapId, X-1, NewY}) =< 0)
        of 
        true -> get_pos(Type, MapId, X0, Y0, Offset, X, NewY, Index1 + 1, Index2);
        false -> {X, NewY}
    end;

get_pos(Type, MapId, X0, Y0, Offset, X, Y, Index1, Index2) when Index2 < 15 -> 
    Offset1 = case Offset =< 0 of
        true -> - Offset + 1;
        false -> - Offset
    end,
    case Offset1 > 15 of
        true ->
            ?WARNING("Offset > 15 when get_pos! 
                [Type:~w, MapId:~w, X0:~w, Y0:~w, Offset:~w, X:~w, Y:~w, Index1:~w, Index2:~w]",
                [Type, MapId, X0, Y0, Offset, X, Y, Index1, Index2]),
            lib_map:rand_pos(MapId);
        false ->
            X1 = X0 + Offset1,
            case X1 > 15 orelse X1 < 0 of
                true ->
                    %% ?INFO("#1 get_pos Index2:~w ~w: X(~w -> ~w), Y:~w", [Index2, Type, X, X1, Y]),
                    get_pos(Type, MapId, X0, Y0, Offset1, X1, Y, Index1, Index2);
                false -> 
                    %% ?INFO("#2 get_pos Index2:~w ~w: X(~w -> ~w), Y:~w", [Index2, Type, X, X1, Y]),
                    get_pos(Type, MapId, X0, Y0, Offset1, X1, Y, 0, Index2 + 1)
            end
    end;

%% 未找到，随机给一个点
get_pos(Type, MapId, X0, Y0, Offset, X, Y, Index1, Index2) -> 
    ?WARNING("runing times too mary when get_pos! 
        [Type:~w, MapId:~w, X0:~w, Y0:~w, Offset:~w, X:~w, Y:~w, Index1:~w, Index2:~w]",
        [Type, MapId, X0, Y0, Offset, X, Y, Index1, Index2]),
    lib_map:rand_pos(MapId).
%%. =================================================

%%' 游戏内道具使用
%%  * 没血量时不可调用
%%  * 非正常砖块不可调用，如云，楼梯
%% Faller: 0=系统道具，大于0时为投放道具的角色ID
use_daoju(Rid, Tid, Faller, I, State) ->
    #daoju{ctl1 = Ev1, ctl2 = Ev2, ctl_time = Time, buff_id = BuffId}
    = data_daoju:get(Tid),
    case Ev1 of 
        4  -> self() ! {add_row_dmg, Rid, Ev2, Time};
        5  -> self() ! {add_col_dmg, Rid, Ev2, Time};
        8  -> 
            Team = I#irole.team,
            F = fun(#irole{id = TargetRid, team = Team1}) ->
                    case Team == Team1 of
                        true -> self() ! {add_hp, TargetRid, Ev2, Time};
                        false -> skip
                    end
            end,
            lists:foreach(F, State#state.roles);
        9  -> 
            Team = I#irole.team,
            F = fun(#irole{id = TargetRid, team = Team1}) ->
                    case Team =/= Team1 of
                        true ->
                            self() ! {add_buff, BuffId, TargetRid, Ev2, Time};
                        false -> skip
                    end
            end,
            lists:foreach(F, State#state.roles);
        11 -> erlang:send_after(1000, self(), {rand_transport, Rid, Ev2, Time});
        12 -> 
            %% 产生一个物品
            #irole{x = X, y = Y} = I,
            self() ! {fall_item, Ev2, Rid, X, Y};
        13 -> 
            %% 地雷B
            #irole{x = X, y = Y} = I,
            %% ?INFO("B，x:~w, y:~w", [X, Y]),
            set_break(X, Y, 99, State, Faller, 0);
        16 ->
            %% 弹簧
            self() ! {role_up, Rid},
            ok;
        17  -> 
            Team = I#irole.team,
            F = fun(#irole{id = TargetRid, team = Team1}) ->
                    case Team == Team1 of
                        true -> 
                            MyBuffs = get_mybuffs(TargetRid),
                            {ok, MyBuffs1} = remove_buff(Ev2, MyBuffs, TargetRid, State),
                            put_buff(TargetRid, MyBuffs1),
                            ok;
                        false -> skip
                    end
            end,
            lists:foreach(F, State#state.roles);
        _ when BuffId > 0 -> 
            self() ! {add_buff, BuffId, Rid, Ev2, Time};
        _ ->
            ?WARNING("undefined ctl1: ~w", [Ev1]),
            ok
    end,
    lib_conn:pack_cast(State#state.senders, 17004, [Rid, Tid]).
%%. ================================================

%%' 游戏内技能使用
use_skill(Rid, PidSender, SkillId, State) ->
    #skill{
        sort = Sort
        ,time = Time1
        ,time_cd = TimeCd1
        ,time_ef = TimeEf1
        ,ctl1 = Ctl1
        ,ctl2 = Ctl2
        ,ctl3 = Ctl3
    } = data_skill:get(SkillId),
    Time = util:ceil(Time1 / 1000),
    TimeCd = util:ceil(TimeCd1 / 1000),
    TimeEf = util:ceil(TimeEf1 / 1000),
    I = get_role(Rid, State),
    Now = util:unixtime(),
    RestCd = get_rest_cd(Rid, SkillId, Now),
    V = data_map_pos:get({State#state.map_id, I#irole.x, I#irole.y}),
    if
        I == false ->
            ok;
        I#irole.hp =< 0 ->
            lib_conn:pack_send(PidSender, 17009, [SkillId, 0, RestCd]),
            ok;
        V =/= 1, V =/= 2 ->
            lib_conn:pack_send(PidSender, 17009, [SkillId, 0, RestCd]),
            ?INFO("UseSkill X:~w, Y:~w, V:~w", [I#irole.x, I#irole.y, V]),
            ok;
        RestCd > 0 ->
            %% cd ing ...
            ?INFO("cd ing ... Rid:~w, SkillId:~w", [Rid, SkillId]),
            lib_conn:pack_send(PidSender, 17009, [SkillId, 0, RestCd]),
            ok;
        true ->
            #irole{x = X, y = Y, attack = Attack, pid = Pid, team = Team} = I, 
            case Sort of 
                1 ->
                    %% 毒雾
                    case chk_events(X, Y, [skill, item]) of
                        false ->
                            Iskill = #iskill{
                                id = SkillId
                                ,rid  = Rid
                                ,attack = Attack
                                ,sort = Sort
                                ,time_ef = TimeEf
                                ,use_time = Now
                                ,ctl1 = Ctl1
                                ,ctl2 = Ctl2
                                ,ctl3 = Ctl3
                            },
                            set_event(X, Y, skill, Iskill),
                            self() ! {set_skill, Rid, SkillId, Time, X, Y},
                            Pid ! {task, ?TASK_USE_SKILL, {add, Sort, 1}},
                            lib_conn:pack_send(PidSender, 17009, [SkillId, 1, TimeCd]),
                            put({skill_cd_etime, Rid, SkillId}, Now + TimeCd),
                            ok;
                        true -> 
                            lib_conn:pack_send(PidSender, 17009, [SkillId, 0, RestCd]),
                            ok
                    end;
                2 ->
                    %% 冰冻
                    case chk_events(X, Y, [skill, item]) of
                        false ->
                            Iskill = #iskill{
                                id = SkillId
                                ,rid  = Rid
                                ,attack = Attack
                                ,sort = Sort
                                ,time_ef = TimeEf
                                ,use_time = Now
                                ,ctl1 = Ctl1
                                ,ctl2 = Ctl2
                                ,ctl3 = Ctl3
                            },
                            set_event(X, Y, skill, Iskill),
                            self() ! {set_skill, Rid, SkillId, Time, X, Y},
                            Pid ! {task, ?TASK_USE_SKILL, {add, Sort, 1}},
                            lib_conn:pack_send(PidSender, 17009, [SkillId, 1, TimeCd]),
                            put({skill_cd_etime, Rid, SkillId}, Now + TimeCd),
                            ok;
                        true -> 
                            lib_conn:pack_send(PidSender, 17009, [SkillId, 0, RestCd]),
                            ok
                    end;
                3 ->
                    %% 火焰
                    case chk_events(X, Y, [skill, item]) of
                        false ->
                            Iskill = #iskill{
                                id = SkillId
                                ,rid  = Rid
                                ,attack = Attack
                                ,sort = Sort
                                ,time_ef = TimeEf
                                ,use_time = Now
                                ,ctl1 = Ctl1
                                ,ctl2 = Ctl2
                                ,ctl3 = Ctl3
                            },
                            set_event(X, Y, skill, Iskill),
                            self() ! {set_skill, Rid, SkillId, Time, X, Y},
                            Pid ! {task, ?TASK_USE_SKILL, {add, Sort, 1}},
                            lib_conn:pack_send(PidSender, 17009, [SkillId, 1, TimeCd]),
                            put({skill_cd_etime, Rid, SkillId}, Now + TimeCd),
                            ok;
                        true -> 
                            lib_conn:pack_send(PidSender, 17009, [SkillId, 0, RestCd]),
                            ok
                    end;
                4  -> 
                    %% 雷神之力
                    self() ! {add_buff, 13, Rid, Ctl1, Time},
                    Pid ! {task, ?TASK_USE_SKILL, {add, Sort, 1}},
                    lib_conn:pack_send(PidSender, 17009, [SkillId, 1, TimeCd]),
                    put({skill_cd_etime, Rid, SkillId}, Now + TimeCd),
                    ok;
                5  -> 
                    At = util:ceil(Attack * Ctl2 + Ctl3),
                    self() ! {add_buff, 14, Rid, At, Time},
                    F = fun(#irole{id = TargetRid, team = Team1}) ->
                            case Team == Team1 of
                                true -> 
                                    MyBuffs = get_mybuffs(TargetRid),
                                    {ok, MyBuffs1} = remove_buff(Ctl1, MyBuffs, TargetRid, State),
                                    put_buff(TargetRid, MyBuffs1),
                                    ok;
                                false -> ok
                            end
                    end,
                    lists:foreach(F, State#state.roles),
                    Pid ! {task, ?TASK_USE_SKILL, {add, Sort, 1}},
                    lib_conn:pack_send(PidSender, 17009, [SkillId, 1, TimeCd]),
                    put({skill_cd_etime, Rid, SkillId}, Now + TimeCd),
                    ok;
                6 ->
                    %% 自然治疗
                    AddHp = util:ceil(Attack * Ctl2 + Ctl3),
                    F = fun(#irole{id = TargetRid, team = Team1}) ->
                            case Team == Team1 of
                                true -> 
                                    %% self() ! {loop_add_hp, TargetRid, AddHp, 1};
                                    self() ! {add_hp, TargetRid, AddHp, 0};
                                false -> skip
                            end
                    end,
                    lists:foreach(F, State#state.roles),
                    Pid ! {task, ?TASK_USE_SKILL, {add, Sort, 1}},
                    lib_conn:pack_send(PidSender, 17009, [SkillId, 1, TimeCd]),
                    put({skill_cd_etime, Rid, SkillId}, Now + TimeCd),
                    ok;
                _  -> 
                    ok
            end
    end.

get_rest_cd(Rid, SkillId, Now) ->
    Rest = case get({skill_cd_etime, Rid, SkillId}) of
        undefined -> 0;
        T -> T - Now
    end,
    case Rest > 0 of
        true -> Rest;
        false -> 0
    end.
%%. ================================================

%%' 获得道具栏空位置
%% get_item_pos([]) -> 1;
%% get_item_pos([IItem]) -> 
%%     case IItem#iitem.pos of
%%         1 -> 2;
%%         2 -> 1
%%     end;
%% get_item_pos(_) -> 0.
%%. ================================================

%%' 获得人物在地图中的初始位置
set_init_pos(Roles, PosList) ->
    set_init_pos(Roles, PosList, []).
set_init_pos([I|RT], PosList, Reply) ->
    {X, Y} = util:rand_element(PosList),
    PosList2 = lists:delete({X, Y}, PosList),
    I2 = I#irole{x = X, y = Y},
    set_init_pos(RT, PosList2, [I2 | Reply]);
set_init_pos([], _, Reply) ->
    Reply;
set_init_pos(_, [], Reply) ->
    Reply.
%%. ================================================

%%' 获得当前战斗角色等级平均值和最小值
get_amlev(Roles) ->
    LevList = [Lev || #irole{lev = Lev} <- Roles],
    L = length(LevList),
    {util:ceil(lists:sum(LevList) / L), lists:min(LevList)}.
%%. ================================================

%%' 计算角色攻击力
%% () -> {Huixin, NewDmg}
calc_dmg({Dmg, Crit}) ->
    case util:rate1000(Crit) of
        true -> {1, Dmg * 2};
        false -> {0, Dmg}
    end.
%%. ================================================

%%' 怪物掉落产出物品
monster_produce(NpcType, MyTeamStatus, MinLev) ->
    Rand = util:rand(1, 1000),
    ProRate = data_prorate:get(NpcType),
    pro_process(MyTeamStatus, Rand, MinLev, ProRate).

pro_process(high, Rand, MinLev, [{Tid, ML, {Min, Max}, _} | T]) ->
    case Rand >= Min andalso Rand =< Max of
        true -> 
            case MinLev < ML of 
                true -> 0;
                false -> Tid
            end;
        false -> pro_process(high, Rand, MinLev, T)
    end;
pro_process(low, Rand, MinLev, [{Tid, ML, _, {Min, Max}} | T]) ->
    case Rand >= Min andalso Rand =< Max of
        true -> 
            case MinLev < ML of 
                true -> 0;
                false -> Tid
            end;
        false -> pro_process(low, Rand, MinLev, T)
    end;
pro_process(_, _, _, []) -> 0.
%%. ================================================

%%' myskills operation
get_myskills(Rid) ->
    case get({myskill, Rid}) of
        undefined -> [];
        B -> B
    end.

get_myskill(SkillId, MySkills) ->
    lists:keyfind(SkillId, 1, MySkills).

put_myskill(Rid, MySkills) ->
    put({myskill, Rid}, MySkills).

del_myskill(SkillId, MySkills) ->
    lists:keydelete(SkillId, 1, MySkills).
%%.

%%' Buff
get_mybuffs(Rid) ->
    case get({buff, Rid}) of
        undefined -> [];
        B -> B
    end.

%% () -> false | #ibuff
get_mybuff(BuffId, Rid) when is_integer(Rid)->
    MyBuffs = get_mybuffs(Rid),
    lists:keyfind(BuffId, 2, MyBuffs);

get_mybuff(BuffId, MyBuffs) ->
    lists:keyfind(BuffId, 2, MyBuffs).

put_buff(Rid, MyBuffs) ->
    put({buff, Rid}, MyBuffs).

set_buff(Buff, MyBuffs) ->
    lists:keyreplace(Buff#ibuff.id, 2, MyBuffs, Buff).

do_clear_buff([MyBuff | MyBuffs], Rid, Senders) ->
    case MyBuff#ibuff.del_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    extra_do_del_buff(Rid, MyBuff#ibuff.id, MyBuff),
    lib_conn:pack_cast(Senders, 14033, [Rid, MyBuff#ibuff.id, 0]),
    do_clear_buff(MyBuffs, Rid, Senders);
do_clear_buff([], _Rid, _Senders) -> ok.

do_del_buff(BuffId, MyBuffs, Rid, State) ->
    MyBuff = get_mybuff(BuffId, MyBuffs),
    Role = get_role(Rid, State),
    if
        MyBuff == false -> 
            {error, error_id};
        Role == false -> 
            {error, error_rid};
        true ->
            case MyBuff#ibuff.del_ref of
                undefined -> ok;
                Ref -> erlang:cancel_timer(Ref)
            end,
            extra_do_del_buff(Rid, BuffId, MyBuff),
            lib_conn:pack_cast(State#state.senders, 14033, [Rid, BuffId, 0]),
            {ok, lists:keydelete(BuffId, 2, MyBuffs)}
    end.

%% 增加BUFF附加处理
extra_do_add_buff(Rid, 10, Ctl) ->
    self() ! {add_move_speed_buff, 10, Rid, Ctl};
extra_do_add_buff(Rid, 15, Ctl) ->
    self() ! {add_move_speed_buff, 15, Rid, Ctl};
extra_do_add_buff(Rid, 18, Ctl) ->
    self() ! {add_dmg_speed_buff, 18, Rid, Ctl};
extra_do_add_buff(_Rid, _BuffId, _Ctl) ->
    ok.

%% 删除BUFF时的附加处理
%% 取消技能效果
extra_do_del_buff(Rid, 10, _) ->
    self() ! {del_move_speed_buff, 10, Rid},
    case get({skill_effect_loop, 1, Rid}) of
        undefined -> ok;
        Ref1 -> 
            erase({skill_effect_loop, 1, Rid}),
            erlang:cancel_timer(Ref1)
    end;
%% 取消技能效果
extra_do_del_buff(Rid, 11, _) ->
    self() ! {set_stop_move, Rid, false},
    case get({skill_effect_loop, 2, Rid}) of
        undefined -> ok;
        Ref1 -> 
            erase({skill_effect_loop, 2, Rid}),
            erlang:cancel_timer(Ref1)
    end;
%% 高级加速
extra_do_del_buff(Rid, 15, _) ->
    self() ! {del_move_speed_buff, 15, Rid};
%% 引爆定时炸弹
extra_do_del_buff(Rid, 16, MyBuff) ->
    RestTime = MyBuff#ibuff.ctl2 - util:unixtime(), 
    case RestTime =< 0 of
        true ->
            self() ! {broken, Rid},
            ok;
        false -> 
            ok
    end;
%% 火焰灼热
extra_do_del_buff(Rid, 17, _) ->
    case get({se3_loop, Rid}) of
        undefined -> ok;
        Ref1 -> 
            %% ?INFO("clear skill 17, Rid:~w", [17]),
            erase({se3_loop, Rid}),
            erlang:cancel_timer(Ref1)
    end;
%% 删除攻速
extra_do_del_buff(Rid, 18, _MyBuff) ->
    self() ! {del_dmg_speed_buff, 18, Rid};
extra_do_del_buff(_Rid, _BuffId, _) ->
    ok.

use_buff(BuffId, MyBuffs, Rid, State) ->
    use_buff(BuffId, 1, MyBuffs, Rid, State).

use_buff(BuffId, Num, MyBuffs, Rid, State) ->
    case get_mybuff(BuffId, MyBuffs) of
        false -> {error, no_buff};
        MyBuff -> 
            Ctl2 = MyBuff#ibuff.ctl2 - Num, 
            case Ctl2 > 0 of
                true ->
                    MyBuff1 = MyBuff#ibuff{ctl2 = Ctl2},
                    MyBuffs1 = set_buff(MyBuff1, MyBuffs),
                    {ok, Num, MyBuffs1};
                false -> 
                    case remove_buff([BuffId], MyBuffs, Rid, State) of
                        {ok, MyBuffs1} -> {ok, MyBuff#ibuff.ctl2, MyBuffs1};
                        {error, Reason} -> {error, Reason}
                    end
            end
    end.

check_buff(BuffId, Rid) when is_integer(Rid) ->
    MyBuffs = get_mybuffs(Rid),
    check_buff(BuffId, MyBuffs);
check_buff(BuffId, MyBuffs) ->
    lists:keymember(BuffId, 2, MyBuffs).

is_disabled_buff([], _MyBuffs) -> false;
is_disabled_buff([Id | T], MyBuffs) ->
    case lists:keymember(Id, 2, MyBuffs) of
        true -> true;
        false -> is_disabled_buff(T, MyBuffs)
    end.

remove_buff([], MyBuffs, _Rid, _State) -> {ok, MyBuffs};
remove_buff([Id | T], MyBuffs, Rid, State) ->
    MyBuffs1 = case lists:keymember(Id, 2, MyBuffs) of
        true -> 
            case do_del_buff(Id, MyBuffs, Rid, State) of
                {ok, MyBuffs2} -> MyBuffs2;
                {error, error_rid} -> MyBuffs;
                {error, _Reason} -> 
                    ?INFO("Error when remove_buff! [BuffId:~w, Rid:~w, Reason:~w]", [Id, Rid, _Reason]),
                    MyBuffs
            end;
        false -> MyBuffs
    end,
    remove_buff(T, MyBuffs1, Rid, State).
%%.

%%' copy_game_attr
copy_game_attr({Id, Name, Pid, PidSender}, RoomId, PidRoom1, Team, MapId, GameMode) ->
    case catch gen_server:call(Pid, {copy_game_attr, GameMode, self(), MapId}) of
        {'EXIT', Reason} ->
            ?WARNING("Exit when call role(~w):~p", [Id, Reason]),
            #irole{
                id = Id 
                ,name = Name 
                ,pid = Pid 
                ,pid_sender = PidSender
                ,pid_room1 = PidRoom1
                ,room_id = RoomId
                ,team = Team
            };
        [Lev, Sex, HpMax, Dmg, DmgSpeed, MoveSpeed, 
            Crit, WeaponId, MyDaoju, Attack, Pos102, Pos104, 
        WinRate, Skilled, GuildId, FbAttack] -> 
            #irole{
                id = Id 
                ,name = Name 
                ,pid = Pid 
                ,pid_sender = PidSender
                ,pid_room1 = PidRoom1
                ,room_id = RoomId
                ,team = Team
                ,sex = Sex
                ,hp = HpMax
                ,hp_max = HpMax
                ,dmg = Dmg
                ,dmg_speed = DmgSpeed
                ,hit_speed = calc_hit_speed(DmgSpeed)
                ,move_speed = MoveSpeed
                ,lev = Lev
                ,crit = Crit
                ,weapon_id = WeaponId
                ,mydaoju = MyDaoju
                ,attack = Attack
                ,pos102 = Pos102
                ,pos104 = Pos104
                ,win_rate = WinRate
                ,skilled = Skilled
                ,guild_id = GuildId
                ,fb_attack = FbAttack
            }
    end.
%%.

%%' cancel_timer(Ref) -> void
cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).
%%.

%%' gen_break_ref() -> Ref
gen_break_ref(Id, X, Y, V) ->
    gen_break_ref(Id, X, Y, V, 0).
gen_break_ref(Id, X, Y, V, Time) ->
    case is_ladder(V) of
        true -> 
            %% 在楼梯中
            FallTime = data_config:get(fall_time),
            erlang:send_after(FallTime, self(), {fall, Id, broken});
        false ->
            BreakTime = case Time > 0 of 
                true -> Time;
                false -> data_config:get(break_interval_time)
            end,
            erlang:send_after(BreakTime, self(), {break, Id, X, Y, V})
    end.

is_ladder(3) -> true;
is_ladder(4) -> true;
is_ladder(_) -> false.
%%.

%%' gen_init_data() -> [...]
gen_init_data(GameMode, MapId, Roles, NpcProRate) ->
    F = fun(X) -> [[Y] || Y <- X] end,
    RolesInfo = [[
            Id
            ,Team
            ,X
            ,Y
            ,Name
            ,Sex
            ,Hp
            ,HpMax
            ,Dmg
            ,DmgSpeed
            ,MoveSpeed
            ,WeaponId
            ,Lev
            ,Pos102
            ,Pos104
            ,WinRate
            ,F(Skilled)
        ] || #irole{
            id          = Id
            ,team       = Team
            ,x          = X
            ,y          = Y
            ,name       = Name
            ,sex        = Sex
            ,hp         = Hp
            ,hp_max     = HpMax
            ,dmg        = Dmg
            ,dmg_speed  = DmgSpeed
            ,move_speed = MoveSpeed
            ,weapon_id  = WeaponId
            ,lev        = Lev
            ,pos102     = Pos102
            ,pos104     = Pos104
            ,win_rate   = WinRate
            ,skilled    = Skilled
        } <- Roles],
    NpcIds = [[X] || {X, _, _} <- NpcProRate],
    NpcIds1 = case GameMode of
        1 ->  [[38] | NpcIds];
        _ -> NpcIds
    end,
    [MapId, GameMode, NpcIds1, RolesInfo].
%%.

%%' 移动时触发的事件
move_trigger(I, X, Y, Roles) ->
    Rid = I#irole.id,
    %% 检测是否踩到技能
    SkillSort = case get_event(X, Y, skill) of 
        undefined -> 0;
        Iskill when Iskill#iskill.rid == Rid -> 
            %% 踩到自己施放的技能
            UsedTime = util:unixtime() - Iskill#iskill.use_time,
            case UsedTime > 1 of
                true -> 
                    %% ?INFO("踩到自己施放的技能,Rid:~w, UsedTime:~w", [Rid, UsedTime]),
                    self() ! {meet_skill, 0, Rid, X, Y},
                    Iskill#iskill.sort;
                false -> 0
            end;
        Iskill -> 
            %% 踩到别人施放的技能
            self() ! {meet_skill, Iskill#iskill.rid, Rid, X, Y},
            Iskill#iskill.sort
    end,
    %% 检测是否有炸弹转移
    case check_buff(16, Rid) of
        true ->
            case meet_role(Rid, X, Y, Roles) of
                false -> ok;
                I2 -> self() ! {move_buff, 16, Rid, I2#irole.id}
            end;
        false ->
            ok
    end,
    %% 检测是否有瀑布
    IsFall = case chk_event(X, Y, fall) of
        true -> 
            %% ?INFO("On fall ~w", [Rid]),
            self() ! {fall, Rid, fall, X, Y},
            true;
        false -> false
    end,
    %% 检测是否有机关
    case get_event(X, Y, switch1) of
        undefined -> ok;
        NpcSkillId -> 
            Attack = util:get_val(fb_attack, data_npc_skill:get(NpcSkillId), 10),
            self() ! {role_sub_hp, 0, Rid, Attack, 0}
    end,
    %% 检测是否有道具可拾取
    ItemType = case get_event(X, Y, item) of
        {ItemRid, Type, _} -> 
            self() ! {pick_daoju, Rid, X, Y, ItemRid, Type},
            Type;
        undefined -> 0
    end,
    case SkillSort == 2 orelse ItemType == 120016 orelse IsFall of
        true -> 
            %% ?INFO("stop_move [Rid:~w, SkillId:~w, ItemTypeId:~w, IsFall:~w]", [Rid, SkillSort, ItemType, IsFall]),
            I#irole{stop_move = true};
        false -> I
    end.
%%.

%%' 查找相遇的角色
%% () -> false | #irole
meet_role(Rid, X, Y, [I | T]) ->
    #irole{id = Rid1, x = X1, y = Y1} = I,
    case Rid =/= Rid1 andalso X == X1 andalso Y == Y1 of
        true -> I;
        false -> meet_role(Rid, X, Y, T)
    end;
meet_role(_Rid, _X, _Y, []) -> false.
%%.

find_npc(X, Y, [I | T]) ->
    #inpc{x = X1, y = Y1} = I,
    case X == X1 andalso Y == Y1 of
        true -> I;
        false -> find_npc(X, Y, T)
    end;
find_npc(_X, _Y, []) -> false.

%%' 较正Y坐标，如果在梯子上，则较正为梯子下方
%% adjust_y(MapId, X, Y) ->
%%     case data_map_pos:get({MapId, X, Y}) of
%%         3 -> Y + 1;
%%         4 -> Y + 2;
%%         _ -> Y
%%     end.
%%.

%%' get_map_element(Key, Map) -> Val
get_map_element(Key, MapId) when is_integer(MapId) ->
    Map = data_map:get(MapId),
    get_map_element(Key, Map, 0);
get_map_element(Key, Map) ->
    get_map_element(Key, Map, 0).

get_map_element(Key, MapId, DefaultValue) when is_integer(MapId) ->
    Map = data_map:get(MapId),
    get_map_element(Key, Map, DefaultValue);
get_map_element(Key, Map, DefaultValue) ->
    case lists:keyfind(Key, 1, Map) of
        false -> DefaultValue;
        {_, Tmp1} -> Tmp1
    end.
%%.

%%' get_pos_val
get_pos_val(MapId, X, Y) ->
    case data_map_pos:get({MapId, X, Y}) of
        0 -> get_event(X, Y, cloud, 0);
        Val -> Val
    end.
%%.

%%' 查找一个点上的所有角色ID
at_cloud(Type, X1, Y1, X2, Y2, State) ->
    {NR, Reply} = at_cloud(Type, X1, Y1, X2, Y2, State#state.roles, [], []),
    {State#state{roles = NR}, Reply}.

at_cloud(move, X1, Y1, X2, Y2, [I = #irole{id = Rid, x = X1, y = Y1} | T], NR, Reply) ->
    NR1 = [I#irole{
            x0 = X1
            ,y0 = Y1
            ,x = X2
            ,y = Y2
            ,stop_move = true 
        } | NR],
    %% ?INFO("at_cloud stop_move true:~w, ~w, ~w", [Rid, X2, Y2]),
    Reply1 = [[Rid] | Reply],
    at_cloud(move, X1, Y1, X2, Y2, T, NR1, Reply1);

at_cloud(stop, X1, Y1, X2, Y2, [I = #irole{id = Rid, x = X2, y = Y2} | T], NR, Reply) ->
    %% ?INFO("at_cloud stop_move false:~w, ~w, ~w", [Rid, X2, Y2]),
    NR1 = [I#irole{stop_move = false} | NR],
    Reply1 = [[Rid] | Reply],
    at_cloud(stop, X1, Y1, X2, Y2, T, NR1, Reply1);

at_cloud(Type, X1, Y1, X2, Y2, [I | T], NR, Reply) ->
    at_cloud(Type, X1, Y1, X2, Y2, T, [I | NR], Reply);

at_cloud(_Type, _X1, _Y1, _X2, _Y2, [], NR, Reply) -> {NR, Reply}.
%%.

%%' falling ref
set_falling(Rid, From, X, Y, Time) ->
    Ref = erlang:send_after(Time, self(), {reached_pos, Rid, From, X, Y}),
    put({falling_ref, Rid}, Ref).

chk_falling(Rid) ->
    case get({falling_ref, Rid}) of
        undefined -> false;
        _ -> true
    end.

del_falling(Rid) ->
    case get({falling_ref, Rid}) of
        undefined -> ok;
        FallingRef -> 
            erlang:cancel_timer(FallingRef),
            erase({falling_ref, Rid})
    end.
%%.

%%' adjust_hit_pos
%% adjust_hit_pos(_, _, 1, _I, State) -> State;
%% adjust_hit_pos(X, Y, V, I, State) when V == 3; V == 4 ->
%%     #irole{id = Rid, break_ref = BreakRef} = I,
%%     {V1, Y1} = case V of
%%         3 -> {1, Y + 1};
%%         4 -> {2, Y - 1}
%%     end,
%%     ?INFO("adjust_hit_pos, V(~w -> ~w) Y(~w -> ~w)", [V, V1, Y, Y1]),
%%     cancel_timer(BreakRef),
%%     BreakRef1 = gen_break_ref(Rid, X, Y1, V1),
%%     I2 = I#irole{
%%         y0 = Y
%%         ,y = Y1
%%         ,v = V1
%%         ,break_ref = BreakRef1
%%     },
%%     I3 = move_trigger(I2, X, Y1, State#state.roles),
%%     set_role(I3, State);
%% adjust_hit_pos(_, _, _, _I, State) -> State.
%%.

%%' is_guild_team
is_guild_team([#irole{guild_id = Gid}, #irole{guild_id = Gid}]) when Gid > 0 -> 1;
is_guild_team([#irole{guild_id = Gid}, #irole{guild_id = Gid}, #irole{guild_id = Gid}]) when Gid > 0 -> 1;
is_guild_team(_) -> 0.
%%.

%%' is_with_osex
is_with_osex([_]) -> 0;
is_with_osex([#irole{sex = Sex}, #irole{sex = Sex}]) -> 0;
is_with_osex([#irole{sex = Sex}, #irole{sex = Sex}, #irole{sex = Sex}]) -> 0;
is_with_osex(_) -> 1.
%%.

%%' fix_move_speed(Speed) -> Speed1.
fix_move_speed(Speed) ->
    SpeedMax = data_config:get(move_speed_max),
    SpeedMin = data_config:get(move_speed_min),
    if
        Speed > SpeedMax -> SpeedMax;
        Speed < SpeedMin -> SpeedMin;
        true -> Speed
    end.

fix_dmg_speed(Speed) ->
    SpeedMax = data_config:get(dmg_speed_max),
    SpeedMin = data_config:get(dmg_speed_min),
    if
        Speed > SpeedMax -> SpeedMax;
        Speed < SpeedMin -> SpeedMin;
        true -> Speed
    end.
%%.

%%' fix_y
fix_y(Y) ->
    case Y of
        0 -> 18; %% 最后一行保存为18行
        _ -> Y
    end.
%%.

%%' rebuild_scene
rebuild_scene(PidSender) ->
    rebuild_scene1(PidSender, get()).

rebuild_scene1(PidSender, [{{event, X, Y}, Events} | T]) ->
    rebuild_scene2(PidSender, X, Y, Events),
    rebuild_scene1(PidSender, T);
rebuild_scene1(PidSender, [_ | T]) ->
    rebuild_scene1(PidSender, T);
rebuild_scene1(_PidSender, []) -> ok.

rebuild_scene2(PidSender, X, Y, [{break, Depth} | T]) ->
    lib_conn:pack_send(PidSender, 16001, [X, Y, Depth, 0]),
    rebuild_scene2(PidSender, X, Y, T);
rebuild_scene2(PidSender, X, Y, [{item, {ItemId, Type, Faller}} | T]) ->
    lib_conn:pack_send(PidSender, 16005, [ItemId, Type, Faller, X, Y]),
    rebuild_scene2(PidSender, X, Y, T);
rebuild_scene2(PidSender, X, Y, [_ | T]) ->
    rebuild_scene2(PidSender, X, Y, T);
rebuild_scene2(_PidSender, _X, _Y, []) -> ok.
%%.

%%' 我的战斗道具
get_mydaoju(Pos, Daoju) ->
    case lists:keyfind(Pos, 1, Daoju) of
        false -> 0;
        {_, Tid} -> Tid
    end.

add_mydaoju(Daoju, Tid) ->
    case get_mydaoju_pos(Daoju) of
        0 -> {error, at_full};
        Pos -> 
            Daoju1 = [{Pos, Tid} | Daoju],
            {ok, Daoju1}
    end.

del_mydaoju(Pos, Daoju) ->
    lists:keydelete(Pos, 1, Daoju).

get_mydaoju_pos(Daoju) ->
    get_mydaoju_pos(1, Daoju).
get_mydaoju_pos(Index, Daoju) when Index =< ?MYDAOJU_MAX ->
    case lists:keymember(Index, 1, Daoju) of
        true -> get_mydaoju_pos(Index + 1, Daoju);
        false -> Index
    end;
get_mydaoju_pos(Index, _Daoju) when Index > ?MYDAOJU_MAX -> 0.
%%.

%%' cloud
set_cloud_epos(MapId, X1, Y1, X2, Y2) ->
    {X11, V11} = case data_map_pos:get({MapId, X1 - 1, Y1}) of
        0 ->
            case data_map_pos:get({MapId, X1 + 1, Y1}) of
                0 -> 
                    ?WARNING("No cloud_epos: ~w", [{X1, Y1}]),
                    {0, 0};
                V1 -> {X1 + 1, V1}
            end;
        V1 -> {X1 - 1, V1}
    end,
    {X22, V22} = case data_map_pos:get({MapId, X2 - 1, Y2}) of
        0 ->
            case data_map_pos:get({MapId, X2 + 1, Y2}) of
                0 -> 
                    ?WARNING("No cloud_epos: ~w", [{X2, Y2}]),
                    {0, 0};
                V2 -> {X2 + 1, V2}
            end;
        V2 -> {X2 - 1, V2}
    end,
    set_event(X1, Y1, cloud_epos, {X11, Y1, V11}),
    set_event(X2, Y2, cloud_epos, {X22, Y2, V22}),
    ok.

exit_cloud(_X, _Y, []) -> ok;
exit_cloud(X, Y, Ids) ->
    {X1, Y1, V1} = get_event(X, Y, cloud_epos),
    F = fun([Id]) ->
            self() ! {force_move, Id, X1, Y1, V1}
    end,
    lists:foreach(F, Ids).
%%.

%%' Events:
%%
%%  * {fall      , {MoveTime, X2, Y2}   }
%%  * {skill     , #iskill              }
%%  * {item      , {ItemId, Tid, Faller}}
%%  * {break     , Depth                }
%%  * {broken_pos, true                 }
%%  * {dead      , true                 }
%%  * {cloud_epos, {X, Y}               } %% 云退出点
%%  * {switch1   , NpcSkillId           } %% 机关1

get_events(X, Y) -> 
    case get({event, X, Y}) of
        undefined -> [];
        Tmp -> Tmp
    end.

get_event(X, Y, Key) ->
    get_event(X, Y, Key, undefined).

get_event(X, Y, Key, DefaultValue) ->
    Events = get_events(X, Y),
    case lists:keyfind(Key, 1, Events) of
        false -> DefaultValue;
        {_, Val} -> Val
    end.

chk_events(X, Y, [Key | T]) ->
    case chk_event(X, Y, Key) of
        true -> true;
        false -> chk_events(X, Y, T)
    end;
chk_events(_X, _Y, []) -> false.

chk_event(X, Y, Key) ->
    Events = get_events(X, Y),
    case lists:keyfind(Key, 1, Events) of
        false -> false;
        _ -> true
    end.

set_event(X, Y, Key, Val) ->
    Events = get_events(X, Y),
    Events1 = lists:keystore(Key, 1, Events, {Key, Val}),
    put({event, X, Y}, Events1).

del_event(X, Y, Key) ->
    Events = get_events(X, Y),
    Events1 = lists:keydelete(Key, 1, Events),
    put({event, X, Y}, Events1).
%%.

do_second_hit(38, Rid, [#inpc{pid = Pid} | Npcs]) ->
    gen_fsm:send_all_state_event(Pid, {second_hit, Rid}),
    do_second_hit(38, Rid, Npcs);
do_second_hit(_, _, _) -> ok.

calc_hit_speed(DmgSpeed) ->
    ServerDmgSpeed = data_config:get(init_dmg_speed),
    ClientDmgSpeed = data_config:get(init_client_dmg_speed),
    SpeedHit1 = ClientDmgSpeed / (DmgSpeed / ServerDmgSpeed),
    util:ceil(SpeedHit1 * 7).

get_carbon_npcs(Mode) ->
    Cid = Mode - 100,
    get_carbon_npcs(Cid, 1, []).

get_carbon_npcs(Cid, Pass, Reply) ->
    case data_carbon:get({Cid, Pass}) of
        undefined -> Reply;
        Data ->
            M = util:get_val(monster_id, Data),
            Reply1 = M ++ Reply,
            get_carbon_npcs(Cid, Pass + 1, Reply1)
    end.

fix_move_buff(_Rid, 0) ->
    ok;
fix_move_buff(Rid, AddSpeed1) ->
    TagBuffId = case AddSpeed1 > 0 of
        true -> 15;
        false -> 10
    end,
    %% 仍需增加 AddSpeed1, 改为[减少将要扣的值]
    case get({move_speed_buff, TagBuffId, Rid}) of
        undefined -> ok;
        Will ->
            Will1 = Will - AddSpeed1,
            ?INFO("Will1(~w) = Will(~w) - AddSpeed1(~w), BuffId:~w", [Will1, Will, AddSpeed1, TagBuffId]),
            put({move_speed_buff, TagBuffId, Rid}, Will1)
    end.

%% vim: set foldmethod=marker foldmarker=%%',%%.:
