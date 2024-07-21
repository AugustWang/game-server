%%----------------------------------------------------
%% 竞技房间服务(匹配队伍后)
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_game2).
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
init(Arg) ->
    init1(2, Arg).
%%.

handle_call(_Request, _From, State) ->
    srv_game1:handle_call(_Request, _From, State).

handle_cast(Msg, State) ->
    srv_game1:handle_cast(Msg, State).

%%' 游戏正式开始
handle_info(game_start, State) ->
    self() ! loop,
    GameTime = data_config:get(mode2_game_time),
    lib_conn:pack_cast(State#state.senders, 14019, [GameTime]),
    NowTime = util:unixtime(),
    State1 = State#state{
        start_time = NowTime
        ,end_time = NowTime + GameTime
    },
    RoleNum = length(State#state.roles),
    NpcX = if 
        RoleNum < 3 -> data_config:get(mode2_npc_num_1v1);
        RoleNum < 5 -> data_config:get(mode2_npc_num_2v2);
        RoleNum < 7 -> data_config:get(mode2_npc_num_3v3);
        RoleNum < 9 -> data_config:get(mode2_npc_num_4v4);
        true -> ?WARNING("RoleNum (~w) ", [RoleNum])
    end,
    self() ! {gen_npc, NpcX},
    self() ! start_active_element,
    {noreply, State1};
%%. ============================================================

%%' NPC死亡
handle_info({npc_die, Rid, NpcId, _Score}, State) ->
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
            %% self() ! {add_score, Rid, Score, 1, NpcId}, %% 给最后打死怪的角色加分
            NewNpcs = lists:keydelete(NpcId, 2, State#state.npcs),
            State#state{npcs = NewNpcs}
    end,
    {noreply, NewState};
%%. =================================================

%%' 屏蔽add_score加分消息
handle_info({add_score, _Rid, _AddScore, _TgType, _TgRid}, State) ->
    {noreply, State};
%%. =================================================

%%' 玩家得分(竞技模式中，杀一个人获得一分)
handle_info({kill, Rid, AddScore, TgType, TgRid}, State) ->
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
            NewScore = Score + AddScore,
            NewRole = Role#irole{score = NewScore},
            lib_conn:pack_cast(State#state.senders, 11005, 
                [Rid, NewScore, TgType, TgRid]),
            set_role(NewRole, State#state{
                    team1_score = NewT1Score, team2_score = NewT2Score})
    end,
    {noreply, NewState};
%%. =================================================

%%' gen_npc
handle_info({gen_npc, Num}, State) ->
    Time = data_config:get(mode2_npc_interval_time),
    erlang:send_after(Time * 1000, self(), {gen_npc, Num}),
    Num1 = Num - length(State#state.npcs),
    {noreply, gen_npc(Num1, State)};
%%. =================================================

%% 检查是否游戏结束
handle_info(game_over_check, State) ->
    case length([R || R <- State#state.roles, R#irole.hp > 0, R#irole.team == 1]) of
        0 -> erlang:send_after(2000, self(), game_over);
        _T1 -> 
            case length([R || R <- State#state.roles, R#irole.hp > 0, R#irole.team == 2]) of
                0 -> erlang:send_after(2000, self(), game_over);
                _T2 -> 
                    %% ?INFO("Game, Team1:~w, Team2:~w", [_T1, _T2]),
                    ok
            end
    end,
    {noreply, State};

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
            AliveNum1 = length([I || I <- State#state.roles, I#irole.team == 1, I#irole.hp > 0]),
            AliveNum2 = length([I || I <- State#state.roles, I#irole.team == 2, I#irole.hp > 0]),
            Team1 = [I || I <- State#state.roles, I#irole.team == 1],
            Team2 = [I || I <- State#state.roles, I#irole.team == 2],
            IsGuildTeam1 = is_guild_team(Team1),
            IsGuildTeam2 = is_guild_team(Team2),
            IsWithOsex1 = is_with_osex(Team1),
            IsWithOsex2 = is_with_osex(Team2),
            TeamNum1 = length(Team1),
            TeamNum2 = length(Team2),
            GameMode = State#state.type,
            WinTeam = if
                AliveNum1 == AliveNum2 -> 0;
                AliveNum1 > AliveNum2 -> 1;
                AliveNum1 < AliveNum2 -> 2
            end,
            F2 = fun(I) ->
                    BaseExp = case WinTeam == I#irole.team of
                        true -> I#irole.lev * 3 + I#irole.score * 30 + I#irole.lev * 5;
                        false -> I#irole.lev * 3 + I#irole.score * 30
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
                        false -> 
                            case WinTeam == 0 of
                                true -> 0;
                                false -> -1
                            end
                    end,
                    I#irole.pid_room1 ! {ready_reset, I#irole.id, IsWin},
                    lib_conn:pack_send(I#irole.pid_sender, 14008, [IsWin+2, 0]),
                    Gold = I#irole.gold + I#irole.score * 30,
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
%%. =================================================

handle_info(_Info, State) ->
    srv_game1:handle_info(_Info, State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

%%' normal get/set

%%' NPC生成
gen_npc(0, State) -> State;
gen_npc(Num, State) ->
    State1 = gen_npc(State),
    gen_npc(Num - 1, State1).

gen_npc(#state{map_id = MapId, avg_lev = AvgLev, npc_pro_rate = ProRate,
    npcs = Npcs} = State) ->
    Rand = util:rand(1, 100),
    Type = rand_npc_type(Rand, ProRate, ProRate, Npcs),
    Id = get(max_npc_id) + 1,
    put(max_npc_id, Id),
    {X, Y} = rand_pos2(State),
    Npc = #inpc{id = Id, type = Type, x = X, y = Y, pid = srv_npc:start([2, Id, Type, MapId, X, Y, self(), AvgLev])},
    NewNpcs = [Npc | Npcs],
    State#state{npcs = NewNpcs}.

%% vim: set foldmethod=marker foldmarker=%%',%%.:
