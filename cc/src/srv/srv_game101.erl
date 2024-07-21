%%----------------------------------------------------
%% 竞技房间服务(匹配队伍后)
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_game101).
-behaviour(gen_server).
-export([start/1]).
-import(srv_game1, [
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("game.hrl").

-define(HIT_ROW_INTERVAL_TIME, 70). %% 力道传播间隔时间
-define(HIT_COL_INTERVAL_TIME, 100). %% 力道传播间隔时间
%% -define(HIT_ROW_INTERVAL_TIME, 270). %% 力道传播间隔时间
%% -define(HIT_COL_INTERVAL_TIME, 300). %% 力道传播间隔时间

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
init([GameMode, RoomId, PidRoom]) ->
    init1(GameMode, [{RoomId, PidRoom}]).
%%.

handle_call(_Request, _From, State) ->
    srv_game1:handle_call(_Request, _From, State).

handle_cast(Msg, State) ->
    srv_game1:handle_cast(Msg, State).

%%' 游戏正式开始
handle_info(game_start, State) ->
    Pass = case get(game101_pass) of
        undefined -> 
            put(game101_pass, 1),
            1;
        TmpN -> 
            del_force_pass(),
            erase(passing),
            put(game101_pass, TmpN + 1),
            TmpN + 1
    end,
    State1 = case Pass of
        1 ->
            self() ! loop,
            erlang:send_after(100, self(), {gen_npc, 106}),
            erlang:send_after(1000, self(), {gen_npc, 102}),
            erlang:send_after(2000, self(), {gen_npc, 101}),
            GameTime = 180,
            lib_conn:pack_cast(State#state.senders, 14019, [GameTime]),
            NowTime = util:unixtime(),
            State#state{
                start_time = NowTime
                ,end_time = NowTime + GameTime
            };
        2 ->
            erlang:send_after(4000, self(), {gen_npc, 106}),
            erlang:send_after(5000, self(), {gen_npc, 102}),
            erlang:send_after(6000, self(), {gen_npc, 101}),
            MapId = util:get_val(map_id, data_carbon:get({1, Pass})),
            PosList = data_init_pos:get(MapId),
            NewRoles0 = set_init_pos(State#state.roles, PosList),
            NewRoles = reset_role_hp(NewRoles0),
            GameTime = 180,
            NewPos = [[R#irole.id, R#irole.x, R#irole.y] || R <- NewRoles],
            lib_conn:pack_cast(State#state.senders, 14063, [MapId, Pass, NewPos]),
            lib_conn:pack_cast(State#state.senders, 14019, [GameTime]),
            NowTime = util:unixtime(),
            clear_scene(),
            State#state{
                start_time = NowTime
                ,end_time = NowTime + GameTime
                ,roles = NewRoles
                ,npcs = []
                ,map_id = MapId
            };
        3 ->
            erlang:send_after(4000, self(), {gen_npc, 103}),
            erlang:send_after(8000, self(), {gen_npc, 106}),
            erlang:send_after(9000, self(), {gen_npc, 102}),
            erlang:send_after(10000, self(), {gen_npc, 101}),
            erlang:send_after(15000, self(), {gen_npc_104, 10}),
            MapId = util:get_val(map_id, data_carbon:get({1, Pass})),
            PosList = data_init_pos:get(MapId),
            NewRoles0 = set_init_pos(State#state.roles, PosList),
            NewRoles = reset_role_hp(NewRoles0),
            GameTime = 180,
            NewPos = [[R#irole.id, R#irole.x, R#irole.y] || R <- NewRoles],
            lib_conn:pack_cast(State#state.senders, 14063, [MapId, Pass, NewPos]),
            lib_conn:pack_cast(State#state.senders, 14019, [GameTime]),
            NowTime = util:unixtime(),
            clear_scene(),
            State#state{
                start_time = NowTime
                ,end_time = NowTime + GameTime
                ,roles = NewRoles
                ,npcs = []
                ,map_id = MapId
            }
    end,
    self() ! start_active_element,
    {noreply, State1};
%%. ============================================================

%%' NPC死亡
handle_info({npc_die, _Rid, _NpcId, _Score}, State) ->
    {noreply, State};
%%. =================================================

%%' 屏蔽add_score加分消息
handle_info({add_score, _Rid, _AddScore, _TgType, _TgRid}, State) ->
    {noreply, State};
%%. =================================================

%%' gen_npc
handle_info({gen_npc, Type}, State) ->
    {noreply, gen_npc(Type, State)};

handle_info({gen_one_npc, Type, X, Y}, State) ->
    Data = data_npc_carbon:get({Type, State#state.type - 100}),
    Hp = util:get_val(hp, Data),
    Sort = util:get_val(type, Data),
    State1 = gen_npc(X, Y, Type, Sort, Hp, State, 1),
    {noreply, State1};

handle_info({gen_npc_104, Num}, State) ->
    State1 = case get(game_over_sign) of
        undefined ->
            Type = 104,
            Data = data_npc_carbon:get({Type, State#state.type - 100}),
            Sort = util:get_val(type, Data),
            Hp = 1,
            {X, Y} = rand_pos2(State),
            case Num of
                0 -> ok;
                1 -> 
                    erlang:send_after(20000, self(), {gen_npc_104, 10}),
                    ok;
                _ ->
                    erlang:send_after(300, self(), {gen_npc_104, Num - 1}),
                    ok
            end,
            gen_npc(X, Y, Type, Sort, Hp, State, 1);
        _ -> State
    end,
    {noreply, State1};

handle_info({del_npc, NpcId}, State) ->
    State1 = case lists:keyfind(NpcId, 2, State#state.npcs) of
        false ->
            ?INFO("No npc:~w", [NpcId]),
            State;
        Npc ->
            #inpc{hp_max = HpMax, ref = Ref} = Npc,
            erlang:cancel_timer(Ref),
            Npcs = lists:keydelete(NpcId, 2, State#state.npcs),
            %% ?INFO("del_npc:~w", [Npc#inpc.type]),
            lib_conn:pack_cast(State#state.senders, 12009, 
                [NpcId, HpMax, 0, 0]),
            State#state{npcs = Npcs}
    end,
    {noreply, State1};

handle_info({test_hit_npc, Rid}, State) ->
    F = fun(I) ->
            self() ! {hit_npc, Rid, I#inpc.id, 10000000}
    end,
    [F(X) || X <- State#state.npcs],
    {noreply, State};

handle_info({hit_npc, _Rid, NpcId, Attack}, State) ->
    %% ?INFO("hit_npc, _Rid:~w, NpcId:~w, Attack:~w", [_Rid, NpcId, Attack]),
    State1 = case lists:keyfind(NpcId, 2, State#state.npcs) of
        false ->
            ?INFO("No npc:~w", [NpcId]),
            State;
        Npc ->
            #inpc{hp_max = HpMax, hp = Hp, ref = Ref,
                type = Type, x = X, y = Y} = Npc,
            case lists:member(Type, [101, 102, 103]) of
                true ->
                    case Type == 103 andalso is_supper_boss() of
                        true -> 
                            %% ?INFO("Boss supper ..."),
                            State;
                        false ->
                            %% ?INFO("T:~w, Hp:~w, A:~w", [Type, Hp, Attack]),
                            Hp1 = Hp - Attack,
                            Hp2 = case Hp1 =< 0 of
                                true -> 0;
                                false -> Hp1
                            end,
                            %% ?INFO("Hp: ~w -> ~w", [Hp, Hp2]),
                            Npcs = case Hp2 of
                                0 -> 
                                    self() ! pass_check,
                                    erlang:cancel_timer(Ref),
                                    lists:keydelete(NpcId, 2, State#state.npcs);
                                _ -> 
                                    Npc1 = Npc#inpc{hp = Hp1},
                                    lists:keyreplace(NpcId, 2, State#state.npcs, Npc1)
                            end,
                            lib_conn:pack_cast(State#state.senders, 12009, 
                                [NpcId, HpMax, Hp2, 0]),
                            case Type of
                                103 ->
                                    %% BOSS
                                    SupperNth = case get(boss_supper_nth) of
                                        undefined -> 1;
                                        NthTmp -> NthTmp
                                    end,
                                    WaterHp = HpMax - HpMax * (SupperNth / 3),
                                    case Hp2 < WaterHp of
                                        true ->
                                            case is_supper_boss() of
                                                true -> ok;
                                                false ->
                                                    put(boss_supper_nth, SupperNth + 1),
                                                    set_boss_supper_flag(true),
                                                    erlang:send_after(200, self(), {gen_npc, 106}),
                                                    erlang:send_after(500, self(), {gen_npc, 102}),
                                                    erlang:send_after(1000, self(), {gen_npc, 101}),
                                                    lib_conn:pack_cast(State#state.senders,
                                                        14060, [NpcId, 8, X, Y]),
                                                    ok
                                            end;
                                        false ->
                                            ok
                                    end,
                                    ok;
                                _ -> ok
                            end,
                            State#state{npcs = Npcs}
                    end;
                false -> State
            end
    end,
    {noreply, State1};

handle_info({attack_pos, X, Y, SkillId}, State) ->
    case find_near_role(X, Y, 0, State#state.roles) of
        false ->
            ok;
        Role ->
            Attack = util:get_val(fb_attack, data_npc_skill:get({SkillId, State#state.type - 100}), 10),
            self() ! {role_sub_hp, 0, Role#irole.id, Attack, 0}
    end,
    {noreply, State};

handle_info({npc_move, NpcId}, State) ->
    State1 = case lists:keyfind(NpcId, 2, State#state.npcs) of
        false ->
            ?INFO("No npc:~w", [NpcId]),
            State;
        Npc ->
            %% ?INFO("NpcId:~w, Index:~w", [NpcId, Index]),
            #inpc{x = X, y = Y} = Npc,
            X1 = if
                X =< 0 -> X + 1;
                X >= 15 -> X - 1;
                true ->
                    case util:rand(1, 2) of
                        1 -> X - 1;
                        2 -> X + 1
                    end
            end,
            case data_map_pos:get({State#state.map_id, X1, Y}) > 0 of
                true ->
                    lib_conn:pack_cast(State#state.senders,
                        14060, [NpcId, 2, X1, Y]),
                    %% ?INFO("npc_move ~w -> ~w", [X, X1]),
                    Npc1 = Npc#inpc{x = X1},
                    Npcs = lists:keyreplace(NpcId, 2, 
                        State#state.npcs, Npc1),
                    State#state{npcs = Npcs};
                false -> 
                    State
            end
    end,
    {noreply, State1};


handle_info({npc_loop, NpcId, Index}, State) ->
    State1 = case lists:keyfind(NpcId, 2, State#state.npcs) of
        false ->
            ?INFO("No npc:~w", [NpcId]),
            State;
        I ->
            #inpc{type = Type, x = X, y = Y, sort = Sort} = I,
            case Index of
                1 -> lib_conn:pack_cast(State#state.senders, 
                        12005, [NpcId, Type, X, Y]);
                _ -> ok
            end,
            %% ?INFO("NpcId:~w, Index:~w, Type:~w", [NpcId, Index, Type]),
            SkillId = case Sort of
                1 ->
                    case find_near_role(X, Y, 1, State#state.roles) of
                        false ->
                            self() ! {npc_move, NpcId},
                            2;
                        Role ->
                            lib_conn:pack_cast(State#state.senders,
                                14060, [NpcId, 1, Role#irole.x, Role#irole.y]),
                            Time = get_attack_time(X, Y, Role#irole.x, Role#irole.y),
                            erlang:send_after(Time, self(), 
                                {attack_pos, Role#irole.x, Role#irole.y, 1}),
                            1
                    end;
                2 ->
                    case rand_element(State#state.roles) of
                        undefined -> ok;
                        Role ->
                            lib_conn:pack_cast(State#state.senders,
                                14060, [NpcId, 3, Role#irole.x, Role#irole.y]),
                            Time = get_attack_time(X, Y, Role#irole.x, Role#irole.y),
                            erlang:send_after(Time, self(), 
                                {attack_pos, Role#irole.x, Role#irole.y, 3})
                    end,
                    3;
                3 ->
                    case is_supper_boss() of
                        false ->
                            case find_near_role(X, Y, 1, State#state.roles) of
                                false ->
                                    case rand_element(State#state.roles) of
                                        undefined -> ok;
                                        Role ->
                                            lib_conn:pack_cast(State#state.senders,
                                                14060, [NpcId, 5, Role#irole.x, Role#irole.y]),
                                            Time = get_attack_time(X, Y, Role#irole.x, Role#irole.y),
                                            erlang:send_after(Time, self(), 
                                                {attack_pos, Role#irole.x, Role#irole.y, 5})
                                    end,
                                    5;
                                Role ->
                                    lib_conn:pack_cast(State#state.senders,
                                        14060, [NpcId, 4, Role#irole.x, Role#irole.y]),
                                    Time = get_attack_time(X, Y, Role#irole.x, Role#irole.y),
                                    erlang:send_after(Time, self(), 
                                        {attack_pos, Role#irole.x, Role#irole.y, 4}),
                                    4
                            end;
                        true ->
                            3
                    end;
                4 ->
                    %% 毒圈
                    Times104 = util:get_val(time, data_npc_skill:get(
                            {6, State#state.type - 100}), 3),
                    case Index > Times104 of
                        true -> self() ! {del_npc, NpcId};
                        false -> self() ! {attack_pos, X, Y, 6}
                    end,
                    6;
                6 ->
                    %% 机关
                    case chk_event(X, Y, switch1) of
                        true ->
                            %% 切换到待机
                            lib_conn:pack_cast(State#state.senders,
                                14060, [NpcId, 10, X, Y]),
                            del_event(X, Y, switch1),
                            10;
                        false ->
                            lib_conn:pack_cast(State#state.senders,
                                14060, [NpcId, 9, X, Y]),
                            set_event(X, Y, switch1, 9),
                            self() ! {attack_pos, X, Y, 9},
                            9
                    end;
                _ -> 
                    3
            end,
            Cd = util:get_val(cd, data_npc_skill:get({SkillId, State#state.type - 100}), 3000),
            Ref = erlang:send_after(Cd,
                self(), {npc_loop, NpcId, Index + 1}),
            I2 = I#inpc{ref = Ref},
            Npcs = lists:keyreplace(NpcId, 2, 
                State#state.npcs, I2),
            State#state{npcs = Npcs}
    end,
    {noreply, State1};
%%. =================================================

handle_info({pass_loaded, Rid, _PidSender}, State) ->
    case get(pass_roles) of
        undefined -> ok;
        PassRoles ->
            PassRoles1 = lists:delete(Rid, PassRoles),
            case PassRoles1 of
                [] ->
                    erase(pass_roles),
                    self() ! game_start,
                    ok;
                _ -> 
                    put(pass_roles, PassRoles1)
            end
    end,
    {noreply, State};

%% 强制过关
handle_info(force_pass, State) ->
    case get(pass_roles) of
        undefined -> ok;
        _PassRoles ->
            erase(pass_roles),
            self() ! game_start,
            ?INFO("force_pass, PassRoles:~w", [_PassRoles]),
            ok
    end,
    {noreply, State};

handle_info(pass_check, State) ->
    case get(passing) of
        undefined ->
            case get(game101_pass) of
                1 ->
                    case chk_cleanup(State) of
                        true ->
                            put(passing, true),
                            AllRoleIds = [R#irole.id || R <- State#state.roles],
                            put(pass_roles, AllRoleIds),
                            lib_conn:pack_cast(State#state.senders, 14061, []),
                            cleanup_npc(State),
                            set_force_pass(),
                            ok;
                        false ->
                            ok
                    end;
                2 ->
                    case chk_cleanup(State) of
                        true ->
                            put(passing, true),
                            AllRoleIds = [R#irole.id || R <- State#state.roles],
                            put(pass_roles, AllRoleIds),
                            lib_conn:pack_cast(State#state.senders, 14061, []),
                            cleanup_npc(State),
                            set_force_pass(),
                            ok;
                        false -> 
                            ok
                    end;
                3 ->
                    case chk_cleanup_boss(State) of
                        true ->
                            put(passing, true),
                            cleanup_npc(State),
                            self() ! game_over,
                            ok;
                        false -> 
                            case chk_cleanup(State) of
                                true -> 
                                    case is_supper_boss() of
                                        true -> 
                                            %% 取消BOSS无敌 
                                            case get_boss(State) of
                                                false -> ok;
                                                Boss ->
                                                    lib_conn:pack_cast(State#state.senders,
                                                        14060, [Boss#inpc.id, 11, Boss#inpc.x, Boss#inpc.y]),
                                                    set_boss_supper_flag(false),
                                                    ok
                                            end;
                                        false ->
                                            ok
                                    end;
                                false -> ok
                            end
                    end;
                _ -> ok
            end;
        _ -> ok
    end,
    {noreply, State};

%% 检查是否游戏结束
handle_info(game_over_check, State) ->
    case length([R || R <- State#state.roles, R#irole.hp > 0]) of
        0 -> erlang:send_after(2000, self(), game_over);
        _T1 -> ok
    end,
    {noreply, State};

handle_info({get_reward, Rid, Pos1}, State) ->
    I = get_role(Rid, State),
    Nth =  get_reward_nth(Rid),
    OpenedPos = get_opened_pos(),
    Pos = get_reward_pos(Pos1, OpenedPos),
    IsRePos = lists:member(Pos, OpenedPos),
    %% ?INFO("get_reward0, Rid:~w, Pos:~w", [Rid, Pos]),
    if 
        I == false ->
            ok;
        Nth > 3 ->
            ok;
        IsRePos ->
            lib_conn:send_code(I#irole.pid_sender, 14000103);
        true ->
            %% ?INFO("get_reward1, Rid:~w, Rid:~w, Pos:~w", [Rid, I#irole.id, Pos]),
            {Id, Num, Tips} = calc_reward(State#state.type),
            I#irole.pid ! {handle_event, 6120, [Nth, Id, Num, Tips]},
            lib_conn:pack_cast(State#state.senders, 14066, [Pos, Rid, Id, Num]),
            set_reward_nth(Rid, Nth + 1),
            set_opened_pos([Pos | OpenedPos]),
            ok
    end,
    {noreply, State};

handle_info(over, State) ->
    lib_conn:pack_cast(State#state.senders, 14068, []),
    {stop, normal, State};

handle_info(force_get_reward, State) ->
    force_get_reward(State#state.roles),
    erlang:send_after(6000, self(), over),
    %% 全部人都已翻牌
    F = fun(I) ->
            I#irole.pid ! {set_attr, [{status, 2}, {pid_room, I#irole.pid_room1}]}
    end,
    lists:foreach(F, State#state.roles),
    {noreply, State};

%%' 游戏结束 game over
handle_info(game_over, State) ->
    case get(game_over_sign) of
        undefined ->
            put(game_over_sign, true),
            AliveRoleNum = length([R || R <- State#state.roles, R#irole.hp > 0]),
            %% RoleNum = 1,
            Pass = get(game101_pass),
            IsCleanup = chk_cleanup(Pass, State),
            IsWin = if
                AliveRoleNum =< 0 -> -1;
                IsCleanup -> 1;
                true -> -1
            end,
            cleanup_npc(State),
            clear_scene(),
            case IsWin of
                -1 ->
                    erlang:send_after(15000, self(), force_stop),
                    F = fun(I) ->
                            I#irole.pid_room1 ! {ready_reset, I#irole.id, IsWin},
                            I#irole.pid ! {set_attr, [{status, 2}, {pid_room, I#irole.pid_room1}]}
                    end,
                    lists:foreach(F, State#state.roles),
                    lib_conn:pack_cast(State#state.senders, 14008, [IsWin + 2, 0]);
                1 ->
                    lib_conn:pack_cast(State#state.senders, 14008, [IsWin + 2, 0]),
                    erlang:send_after(20000, self(), force_get_reward),
                    erlang:send_after(30000, self(), force_stop),
                    F = fun(I) ->
                            I#irole.pid ! {task, ?TASK_CARBON_PASS, {add, State#state.type - 100, 1}},
                            I#irole.pid_room1 ! {ready_reset, I#irole.id, IsWin}
                    end,
                    lists:foreach(F, State#state.roles),
                    ok
            end,
            ok;
        true -> ok
    end,
    {noreply, State};
%%. =================================================

%% 地板破碎(破碎程度加1)
handle_info({break, _Rid, _X, _Y, _V}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    srv_game1:handle_info(_Info, State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

gen_npc(Type, State) ->
    gen_npc(Type, State, 0).

gen_npc(Type, State, Time) ->
    Data = data_npc_carbon:get({Type, State#state.type - 100}),
    Hp = util:get_val(hp, Data),
    Sort = util:get_val(type, Data),
    Pass = get(game101_pass),
    M = util:get_val(monster_id, data_carbon:get({1, Pass})),
    Borns = util:get_val(Type, M),
    gen_npc(Borns, Type, Sort, Hp, State, Time).

gen_npc([{X, Y} | Borns], Type, Sort, Hp, State, Time) ->
    State1 = case is_npc_pos(State#state.npcs, X, Y) of
        true -> State;
        false ->
            Time1 = case Time > 0 of
                true -> Time;
                false -> util:rand(1, 3000)
            end,
            gen_npc(X, Y, Type, Sort, Hp, State, Time1)
    end,
    gen_npc(Borns, Type, Sort, Hp, State1, Time);
gen_npc([], _Type, _Sort, _Hp, State, _Time) -> 
    State.

gen_npc(X, Y, Type, Sort, Hp, State, Time) ->
    Id = get(max_npc_id) + 1,
    put(max_npc_id, Id),
    Ref = erlang:send_after(Time,
        self(), {npc_loop, Id, 1}),
    Npc = #inpc{id = Id, type = Type, x = X, y = Y,
        hp = Hp, hp_max = Hp, ref = Ref, sort = Sort},
    NewNpcs = case Type of
        103 ->
            Npc1001 = #inpc{id = Id * 1000 + 1, type = 0, x = X-1, y = Y},
            Npc1002 = #inpc{id = Id * 1000 + 2, type = 0, x = X+1, y = Y},
            [Npc, Npc1001, Npc1002 | State#state.npcs];
        _ -> [Npc | State#state.npcs]
    end,
    State#state{npcs = NewNpcs}.
%%.

find_near_role(X, Y, Range, [I | T]) ->
    #irole{x = X1, y = Y1, hp = Hp} = I,
    case Hp > 0 andalso Y == Y1 andalso (X + Range) >= X1 
        andalso (X - Range) =< X1 of
        true -> I;
        false -> find_near_role(X, Y, Range, T)
    end;
find_near_role(_Rid, _X, _Y, []) -> false.

get_attack_time(Ax1, Ay1, Bx1, By1) ->
    Ax = Ax1 * 50,
    Ay = Ay1 * 36,
    Bx = Bx1 * 50,
    By = By1 * 36,
    Len = math:sqrt(((Ax - Bx) * (Ax - Bx)) + ((Ay - By) * (Ay - By))),
    %% ?INFO("{~w, ~w} -> {~w, ~w} LEN:~w", [Ax, Ay, Bx, By, Len]),
    util:ceil(Len / 0.3).

chk_cleanup(Pass, State) ->
    case Pass of
        3 -> chk_cleanup2(State);
        _ -> chk_cleanup(State)
    end.

chk_cleanup(State) ->
    not (lists:keymember(101, #inpc.type, State#state.npcs)
        orelse lists:keymember(102, #inpc.type, State#state.npcs)).

chk_cleanup_boss(State) ->
    not lists:keymember(103, #inpc.type, State#state.npcs).

chk_cleanup2(State) ->
    not (lists:keymember(101, #inpc.type, State#state.npcs)
        orelse lists:keymember(102, #inpc.type, State#state.npcs)
        orelse lists:keymember(103, #inpc.type, State#state.npcs)
    ).

cleanup_npc(State) ->
    F = fun 
        (#inpc{ref = undefined}) ->
            ok;
        (#inpc{ref = Ref}) ->
            erlang:cancel_timer(Ref)
    end,
    [F(X) || X <- State#state.npcs].

clear_scene() ->
    clear_scene1(get()).

clear_scene1([{{event, X, Y}, _} | T]) ->
    erase({event, X, Y}),
    clear_scene1(T);
clear_scene1([_ | T]) ->
    clear_scene1(T);
clear_scene1([]) -> ok.

calc_reward(Mode) ->
    Reward = case get(rewarded_index) of
        undefined -> [];
        Re -> Re
    end,
    Data = data_reward:get(Mode),
    calc_reward(Data, Reward).

calc_reward(Data, Reward) ->
    Rand = util:rand(1, 100000),
    {Index, Id, Num, Tips} = reward_process(1, Rand, Data),
    case lists:member(Index, Reward) of
        true ->
            calc_reward(Data, Reward);
        false ->
            %% ?INFO("calc_reward, Id:~w, Num:~w, rewarded_index:~w", [Id, Num, [Index | Reward]]),
            put(rewarded_index, [Index | Reward]),
            {Id, Num, Tips}
    end.

reward_process(Index, Rand, [{Id, Num, {Min, Max}, {tips, Tips}, _} | T]) ->
    case Rand >= Min andalso Rand =< Max of
        true -> {Index, Id, Num, Tips};
        false -> reward_process(Index + 1, Rand, T)
    end.

force_get_reward([Role | RoleT]) ->
    Rid = Role#irole.id,
    case get_reward_nth(Rid) of
        1 ->
            %% ?INFO("force_get_reward1, Rid:~w", [Rid]),
            self() ! {get_reward, Rid, 0},
            self() ! {get_reward, Rid, 0},
            ok;
        2 ->
            %% ?INFO("force_get_reward2, Rid:~w", [Rid]),
            self() ! {get_reward, Rid, 0},
            ok;
        _ ->
            ok
    end,
    force_get_reward(RoleT);
force_get_reward([]) ->
    ok.

get_reward_pos(0, OpenedPos) ->
    Rand = util:rand(0, 23),
    case lists:member(Rand, OpenedPos) of
        true -> get_reward_pos(0, OpenedPos);
        false -> Rand
    end;
get_reward_pos(Nth, _) -> Nth.

get_reward_nth(Rid) ->
    case get({Rid, reward_nth}) of
        undefined -> 1;
        N -> N
    end.

set_reward_nth(Rid, N) ->
    put({Rid, reward_nth}, N).

get_opened_pos() ->
    case get(opened_pos) of
        undefined -> [];
        TmpPos -> TmpPos
    end.

set_opened_pos(D) ->
    put(opened_pos, D).

rand_element(Roles) ->
    Roles1 = [R || R <- Roles, R#irole.hp > 0],
    util:rand_element(Roles1).

is_supper_boss() ->
    case get(supper_boss) of
        undefined -> true;
        Tmp -> Tmp
    end.

set_boss_supper_flag(Bool) ->
    put(supper_boss, Bool).

get_boss(State) ->
    lists:keyfind(103, #inpc.type, State#state.npcs).

reset_role_hp(Roles) ->
    F = fun(R) -> 
            %% case R#irole.hp > 0 of
            %%     true -> R#irole{hp = R#irole.hp_max};
            %%     false -> R
            %% end
            R#irole{hp = R#irole.hp_max}
    end,
    [F(R) || R <- Roles].

set_force_pass() ->
    case get(force_pass_ref) of
        undefined -> ok;
        Ref1 -> erlang:cancel_timer(Ref1)
    end,
    Ref = erlang:send_after(15000, self(), force_pass),
    put(force_pass_ref, Ref).

del_force_pass() ->
    case get(force_pass_ref) of
        undefined -> ok;
        Ref1 -> erlang:cancel_timer(Ref1)
    end,
    erase(force_pass_ref).

%% vim: set foldmethod=marker foldmarker=%%',%%.:
