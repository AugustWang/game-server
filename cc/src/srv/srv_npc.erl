%%----------------------------------------------------
%% NPC进程
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_npc).
-behaviour(gen_fsm).  
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, code_change/4, terminate/3]).   
-export([
        start/1
        ,move/2
        ,waiting/2
        ,action_start/2
        ,hidden/2
        ,show/2
        ,guard/2
    ]
).
-include("common.hrl").
-include("npc.hrl").

-record(state, {
        id
        ,tid %% NPC类型ID
        ,game_type %% 游戏模式 1＝积分模式，2＝竞技模式
        ,pid_room
        ,hp
        ,hp_max
        ,dmg_type
        ,score %% 分值
        ,x
        ,y
        ,avg_lev
    }
).
%% --- 对外接口 ---

%% 新建连接
start(L) ->
    {ok, Pid} = gen_fsm:start(?MODULE, L, []),
    Pid.

%% --- 服务器内部实现 ---

init([Type, Id, Tid, _MapId, X, Y, PidRoom, AvgLev]) ->  
    %% Type: 游戏模式
    erlang:link(PidRoom),
    Npc = data_npc:get(Tid),
    State = #state{ 
        id = Id
        ,tid = Tid
        ,game_type = Type
        ,pid_room = PidRoom
        ,hp = Npc#npc.hp
        ,hp_max = Npc#npc.hp
        ,dmg_type = Npc#npc.dmg_type
        ,score = Npc#npc.score
        ,x = X
        ,y = Y
        ,avg_lev = AvgLev
    },
    Time = case Type of
        1 -> util:rand(0, 6000);
        2 -> util:rand(0, 100)
    end,
    {ok, action_start, State, Time}.

handle_event(stop, _StateName, State) ->  
    {stop, normal, State};

handle_event({second_hit, Rid}, _StateName, State) ->
    #state{id = Id, pid_room = PidRoom, score = Score, hp_max = HpMax} = State,
    PidRoom ! {npc_second_die, Rid, Id, Score, HpMax},
    {stop, normal, State};

handle_event({hit, Rid, Dmg, Type}, StateName, State) ->
    #state{id = Id, pid_room = PidRoom, hp = Hp0, hp_max = HpMax, score = Score} = State,
    Hp1 = Hp0 - Dmg,
    Hp = case Hp1 < 0 of
        true -> 0;
        false -> Hp1
    end,
    NewState = State#state{hp = Hp},
    PidRoom ! {hit_npc_response, Rid, Id, HpMax, Hp, Type},
    case Hp =< 0 of 
        true -> 
            PidRoom ! {npc_die, Rid, Id, Score},
            {stop, normal, NewState};
        false -> 
            case State#state.game_type of
                1 -> {next_state, StateName, NewState, 5000};
                _ -> {next_state, waiting, NewState}
            end
    end;

handle_event(hidden, _StateName, State) ->
    {next_state, hidden, State, 0};

handle_event(_Msg, _StateName, _State) ->  
    ?WARNING("Not matched:~w", [_Msg]),
    {next_state, ok, ok}.  
  
handle_sync_event(_A, _B, _C, _D) ->  
    {reply, ok, ok, ok}.  

handle_info(_A, _B, _C) ->  
    {next_state, ok, ok}.  

code_change(_A, _B, _C, _D) ->  
    {ok, ok, ok}.  
  
terminate(_Reason, _StateName, _State) ->  
    ok. 

move(_StateName, State) ->
    ?INFO("move -> ~w", [_StateName]),
    {next_state, move, State}.

waiting(timeout, State) ->
    ?INFO("waiting -> ~w", [timeout]),
    {next_state, waiting, State};

waiting(_StateName, State) ->
    ?INFO("waiting -> ~w", [_StateName]),
    {next_state, waiting, State, 3600 * 1000}.

%% 动作开始
action_start(timeout, State) ->
    #state{id = Id, tid = Tid, game_type = GameType, x = X, y = Y,
        pid_room = PidRoom, dmg_type = DmgType} = State,
    case GameType of
        1 ->
            Act = case DmgType of
                2 -> util:rand(0, 1);
                _ -> 0
            end,
            NextStateName = case Act of
                0 -> show;
                1 -> guard
            end,
            PidRoom ! {pack_cast, 12005, [Id, Tid, X, Y]},
            {next_state, NextStateName, State, 1000};
        _ -> 
            PidRoom ! {pack_cast, 12005, [Id, Tid, X, Y]},
            {next_state, waiting, State}
    end.

%% 隐藏
hidden(timeout, State) ->
    #state{id = Id, tid = Tid, pid_room = PidRoom, hp_max = HpMax} = State,
    PidRoom ! {npc_action, Id, hidden},
    Time = case Tid of 
        38 -> 500;
        _ -> util:rand(3000, 5000)
    end,
    {next_state, show, State#state{hp = HpMax}, Time}.

%% 生长
show(timeout, State) ->
    #state{id = Id, tid = Tid, pid_room = PidRoom, dmg_type = DmgType, 
    avg_lev = AvgLev} = State,
    PidRoom ! {npc_action, Id, show},
    {StateName, Time} = case {Tid, DmgType} of
        {38, _} -> 
            KeepTime = case AvgLev of
                1 -> 6000;
                2 -> 5000;
                3 -> 4000;
                4 -> 3000;
                5 -> 2000;
                _ -> 1500
            end,
            {hidden, KeepTime};
        {_ , 2} -> {guard, 4000};
        {_ , _} -> {hidden, util:rand(8000, 10000)}
    end,
    {next_state, StateName, State, Time}.

%% 警戒
guard(timeout, State) ->
    #state{id = Id, pid_room = PidRoom, dmg_type = DmgType} = State,
    PidRoom ! {npc_action, Id, guard},
    Time = 2000,
    StateName = case DmgType of
        2 -> 
            case util:rand(0, 1) of
                0 -> hidden;
                1 -> show
            end;
        _ -> hidden
    end,
    {next_state, StateName, State, Time}.
