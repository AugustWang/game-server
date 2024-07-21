%%----------------------------------------------------
%% 副本1
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_game11).
-behaviour(gen_server).
-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-import(srv_game1, [
        get_role/2
        ,set_role/2
        ,get_npc/2
        ,rand_pos2/1
        ,check_buff/2
        ,del_carcass/1
        ,monster_produce/2
        ,set_init_pos/2
        ,get_avglev/1
        ,rand_npc_type/2
        ,copy_game_attr/5
        ,gen_npc/2
    ]).

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
    {ok, Pid} = gen_server:start(?MODULE, L, []),
    Pid.
%%. ================================================

%%----------------------------------------------------------------------
%% Callback functions from gen_server
%%----------------------------------------------------------------------

%%' init
init([{RoomId1, Pid1, CarbonId, Pass}]) ->
    put(max_item_id, 0),
    put(max_npc_id, 0),
    put(mortuary, []),
    put(items_pos, []),
    Roles1 = gen_server:call(Pid1, get_roles),
    MapId = util:rand_element(data_map_appear:get({CarbonId, Pass})),
    Map = data_map:get(MapId),
    {_, Width} = lists:keyfind(width, 1, Map),
    {_, Height} = lists:keyfind(height, 1, Map),
    {_, BrickNum} = lists:keyfind(brick_num, 1, Map),
    AllRoles = [copy_game_attr(X, RoomId1, Pid1, 1, MapId) || X <- Roles1],
    PosList = data_init_pos:get(MapId),
    NewRoles = set_init_pos(AllRoles, PosList),
    AvgLev = get_avglev(NewRoles),
    RoleFallDropHp = data_average:get({0, AvgLev}),
    RoleHitDropHp = data_average:get({999, AvgLev}),
    Senders = [R#irole.pid_sender || R <- NewRoles],
    Loading = [R#irole.id || R <- NewRoles],
    [DataMapNpc, _NpcNum] = data_map_npc:get(MapId),
    NpcProRate = util:rand_element(DataMapNpc),
    put(loading, Loading), %% 记录正在加载资源的玩家
    Type = 11,
    State = #state{ 
        type = Type
        ,map_id = MapId
        ,roles = NewRoles
        ,senders = Senders
        ,falled_hp = RoleFallDropHp
        ,role_hit_drop_hp = RoleHitDropHp
        ,role_avg_lev = AvgLev
        ,width = Width
        ,height = Height
        ,brick_num = BrickNum
        ,avg_lev = AvgLev
        ,npc_pro_rate = NpcProRate
        ,carbon_id = CarbonId 
        ,pass = Pass
    },
    %% 通知游戏（加载）开始
    %% ?INFO("进入游戏：~w:~w", [self(), NewRoles]),
    %% lib_conn:pack_cast(Senders, 14007, [MapId]),
    Role = [[Ri#irole.id, Ri#irole.team, Ri#irole.x, Ri#irole.y, Ri#irole.name, Ri#irole.sex, Ri#irole.hp,  Ri#irole.hp_max, Ri#irole.dmg, Ri#irole.dmg_speed, Ri#irole.move_speed, Ri#irole.weapon_id] || Ri <- State#state.roles],
    NpcIds = [[X] || {X, _, _} <- NpcProRate],
    lib_conn:pack_cast(Senders, 14009, [MapId, Type, NpcIds, Role]),
    %% 如果一定时间后仍有角色没有加载完成，强制开始
    erlang:send_after(60 * 1000, self(), force_start),
    %% erlang:send_after(1000, self(), test),
    {ok, State}.
%%.

handle_call(_Request, _From, State) ->
    srv_game1:handle_call(_Request, _From, State).

handle_cast(Msg, State) ->
    srv_game1:handle_cast(Msg, State).

%%' 游戏正式开始
handle_info(game_start, State) ->
    [_DataMapNpc, NpcNum] = data_map_npc:get(State#state.map_id),
    {noreply, gen_npc(NpcNum, State)};
%%. ============================================================

%%' NPC死亡
handle_info({npc_die, Rid, NpcId, _Score}, State) ->
    NewState = case get_npc(NpcId, State) of 
        false -> 
            ?ERR("Npc not found when npc_die:~w", [NpcId]),
            State;
        _Npc ->
            self() ! {add_score, Rid, 1, 1, NpcId}, %% 给最后打死怪的角色加分
            NewNpcs = lists:keydelete(NpcId, 2, State#state.npcs),
            case length(NewNpcs) =< 0 of
                true -> erlang:send_after(2000, self(), game_over);
                false -> ok
            end,
            State#state{npcs = NewNpcs}
    end,
    {noreply, NewState};
%%. =================================================

%%' 游戏结束
handle_info(game_over, State) ->
    F0 = fun(I) ->
            I#irole.pid_room1 ! {ready_reset, I#irole.id}
    end,
    lists:foreach(F0, State#state.roles),
    [gen_fsm:send_all_state_event(NpcInfo#inpc.pid, stop) 
        || NpcInfo <- State#state.npcs],
    F1 = fun(I, Acc) -> I#irole.score + Acc end,
    Team1 = [I || I <- State#state.roles, I#irole.team == 1],
    Team2 = [I || I <- State#state.roles, I#irole.team == 2],
    TeamNum1 = length(Team1),
    TeamNum2 = length(Team2),
    TeamScore1 = lists:foldl(F1, 0, Team1),
    TeamScore2 = lists:foldl(F1, 0, Team2),
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
            TeamNum = case I#irole.team of
                1 -> TeamNum1;
                2 -> TeamNum2
            end,
            TeamAdd = case TeamNum of
                4 -> 0.4 * BaseExp;
                3 -> 0.3 * BaseExp;
                2 -> 0.2 * BaseExp;
                1 -> 0;
                _ -> ok
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
            lib_conn:pack_send(I#irole.pid_sender, 14008, [IsWin+2]),
            Total = util:ceil(BaseExp + TeamAdd + CardAdd),
            {I#irole.id, IsWin, I#irole.gold, BaseExp, util:ceil(TeamAdd), CardAdd, Total}
    end,
    Result = [F2(I) || I <- State#state.roles],
    RewardType = State#state.carbon_id * 10 + State#state.pass,
    F3 = fun(I) ->
            I#irole.pid ! {game_over, RewardType, I#irole.score, Result}
    end,
    lists:foreach(F3, State#state.roles),
    {stop, normal, State};
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

%% vim: set foldmethod=marker foldmarker=%%',%%.:
