%%----------------------------------------------------
%% 竞技房间服务(匹配队伍前)
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_room1).
-behaviour(gen_server).
-export([
        start/4
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("ihall.hrl").

-define(RE_ENTER_WAITING_TIME, 30 * 1000).
-define(HOST_EXIT_TIME, 60 * 1000).
-define(ROOM_HAND_TIME,  5 * 60 * 1000).

-record(state, {
        id
        ,type       %% 游戏类型(模式)
        ,host       %% 房主ID
        ,map_id = 1
        ,max_pos
        ,status = 0 %% 0=等待中, 1=匹配中, 2=游戏中
        ,roles = []
        ,senders = []
        ,name = <<>>
        ,password = <<>>
        ,closed_pos = [2]
        ,carbon_id
        ,pass = 0 %% 副本当前关卡
        ,pass_max = 0
        ,is_win
    }
).

%% info of role
-record(iroom, {
        id
        ,pos = 0
        ,status = 0    %% 0=未准备,1=准备就绪
        ,pid 
        ,pid_sender
        ,name          %% 玩家名字
        ,sex
        ,lev
        ,win_rate = 0
        ,exp2_etime = 0
        ,game_count
        ,equ_info
        ,power
    }
).

%% --- 对外接口 ---

%% 新建连接
start(Type, RoomId, RoomName, Ihall) ->
    gen_server:start(?MODULE, [Type, RoomId, RoomName, Ihall], []).

%% --- 服务器内部实现 ---

init([Type, RoomId, RoomName, Ihall]) ->
    start_room_hand_timer(),
    I1 = ihall_to_iroom(Ihall),
    I = I1#iroom{status = 1},
    MaxPos = 3,
    State = #state{ 
        id = RoomId
        ,name = RoomName
        ,type = Type
        ,host = I#iroom.id
        ,max_pos = MaxPos
        ,roles = [I]
        ,senders = [I#iroom.pid_sender]
        ,is_win = Ihall#ihall.is_win
    },
    %% ?INFO("init room1 RoomId:~w, Rid:~w, Name:~s", [RoomId, Rid, RoomName]),
    %% 初始化邀请列表 
    put(invite_roles, []),
    I#iroom.pid ! {set_attr, [{status, 2}, {pid_room1, self()}]},
    %% ?INFO("create ok ~w", [I#iroom.id]),
    lib_conn:pack_send(I#iroom.pid_sender, 13005, [RoomId, Type, RoomName]),
    {ok, State}.

%% 队伍己匹配成功，供srv_room2获取队伍信息
handle_call(get_roles, _From, State) ->
    Roles = [{Id, Name, Pid, PidSender} || #iroom{id = Id, name = Name,
            pid = Pid, pid_sender = PidSender } <- State#state.roles],
    %% 重置邀请列表
    put(invite_roles, []),
    {reply, Roles, State#state{status = 2}};

%% 检查是否可以重新进入房间
handle_call({enter_check, Rid}, _From, State) ->
    reset_room_hand_timer(),
    Reply = case get_role(Rid, State) of
        false -> 0;
        _Role -> 1
    end,
    {reply, Reply, State};

handle_call(get_room_id, _From, State) ->
    {reply, {ok, State#state.type, State#state.id}, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% 发送房间队伍成员信息
handle_cast({send_roles, PidSender}, State) ->
    reset_room_hand_timer(),
    Data = [[
            Id
            ,Sex
            ,Pos
            ,Name
            ,Status
            ,Lev
            ,get_equ_tid(102, EquInfo)
            ,get_equ_tid(97, EquInfo)
            ,get_equ_tid(103, EquInfo)
            ,get_equ_tid(104, EquInfo)
            ,get_equ_max_lev(EquInfo)
            ,WinRate 
            ,lib_role:get_exp2_time(Exp2ETime)
            ,Power
        ] || #iroom{
            id = Id
            ,sex = Sex
            ,pos = Pos
            ,name = Name
            ,status = Status
            ,lev = Lev
            ,win_rate = WinRate 
            ,exp2_etime = Exp2ETime
            ,equ_info = EquInfo
            ,power = Power
        } <- State#state.roles],
    Closed = [[C] || C <- State#state.closed_pos],
    %% ?INFO("14001:~w", [Data]),
    lib_conn:pack_send(PidSender, 14001, [State#state.host, Data, Closed]),
    {noreply, State};

%% 邀请
handle_cast({invite, Rid, Name}, #state{id = Id, type = Type} = State) ->
    reset_room_hand_timer(),
    MaxNum = State#state.max_pos - length(State#state.closed_pos),
    RoleNum = length(State#state.roles),
    lib_conn:pack_send(Rid, 14029, [Name, Type, Id, RoleNum, MaxNum]),
    InviteRoles = get(invite_roles),
    case lists:member(Rid, InviteRoles) of
        true -> skip;
        false -> put(invite_roles, [Rid | InviteRoles])
    end,
    {noreply, State};

%% 邀请
handle_cast({send_13003, PidSender}, State) ->
    reset_room_hand_timer(),
    #state{id = RoomId, type = Type, name = RoomName, roles = Roles, closed_pos = ClosedPos,
    map_id = MapId, max_pos = MaxPos} = State,
    NumMax = MaxPos - length(ClosedPos),
    Num = length(Roles),
    lib_conn:pack_send(PidSender, 13003, [0, RoomId, Type, RoomName, 
            MapId, NumMax, Num]),
    {noreply, State};

%% 角色加入队伍
handle_cast({add_role, Password, Ihall}, State) ->
    #ihall{
        id = Rid 
        ,name = Name
        ,pid = Pid
        ,pid_sender = PidSender
        ,sex = Sex
        ,lev = Lev
        ,win_rate = WinRate
        ,exp2_etime = Exp2ETime
        ,equ_info = EquInfo
        ,power = Power
    } = Ihall,
    I = ihall_to_iroom(Ihall),
    reset_room_hand_timer(),
    #state{id = RoomId, type = Type, name = RoomName, roles = Roles, closed_pos = ClosedPos,
    map_id = MapId, password = Password1, max_pos = MaxPos} = State,
    Pos = get_pos(Roles, ClosedPos),
    NumMax = MaxPos - length(ClosedPos),
    Num = length(Roles),
    InviteRoles = get(invite_roles),
    BeInvited = lists:member(Rid, InviteRoles),
    if
        Pos >= MaxPos ->
            lib_conn:pack_send(PidSender, 13003, [1, RoomId, Type, RoomName, MapId, NumMax, Num]),
            {noreply, State};
        Password1 =/= <<>> andalso Password =:= <<>> andalso BeInvited == false ->
            lib_conn:pack_send(PidSender, 13003, [3, RoomId, Type, RoomName, MapId, NumMax, Num]),
            {noreply, State};
        Password1 =/= Password andalso BeInvited == false ->
            %% 密码错误
            lib_conn:pack_send(PidSender, 13004, [1, RoomId, Type]),
            {noreply, State};
        true ->
            Role = I#iroom{pos = Pos},
            NewState = case get_role(Rid, State) of
                false -> add_role(Role, State);
                _Role -> set_role(Role, State)
            end,
            Senders = [R#iroom.pid_sender || R <- NewState#state.roles],
            NewState2 = NewState#state{senders = Senders},
            srv_hall ! {set_room_num, Type, RoomId, NumMax, Num + 1},
            Pid ! {set_attr, [{status, 2}, {pid_room1, self()}]},
            case Password =:= <<>> of 
                true ->
                    lib_conn:pack_send(PidSender, 13003, [0, RoomId, Type, RoomName, 
                            MapId, NumMax, Num + 1]);
                false ->
                    lib_conn:pack_send(PidSender, 13004, [0, RoomId, Type])
            end,
            Exp2Time = lib_role:get_exp2_time(Exp2ETime),
            Data14003 = [Rid, Sex, Pos, Name, Lev
                ,get_equ_tid(102, EquInfo)
                ,get_equ_tid(97, EquInfo)
                ,get_equ_tid(103, EquInfo)
                ,get_equ_tid(104, EquInfo)
                ,get_equ_max_lev(EquInfo)
                , WinRate, Exp2Time, Power],
            lib_conn:pack_cast(Senders, 14003, Data14003),
            %% ?INFO("14003:~w", [Data14003]),
            {noreply, NewState2}
    end;

%% 房客准备/取消游戏
handle_cast({ready, Rid, Status}, State) ->
    reset_room_hand_timer(),
    case State#state.host =:= Rid of 
        true ->
            ?WARNING("Recv host ready:~w(~w)", [Rid, Status]),
            {noreply, State};
        false ->
            NewState = case get_role(Rid, State) of 
                false -> 
                    ?WARNING("error ready Rid:~w, RoomId:~w", [Rid, self()]),
                    State;
                Role ->
                    NewRole = Role#iroom{status = Status},
                    lib_conn:pack_cast(State#state.senders, 14005, [Rid, Status]),
                    %% ?INFO("ready rid:~w(~w)", [Rid, Status]),
                    NewState1 = set_role(NewRole, State),
                    case Status of
                        0 ->
                            %% 取消准备，房主退出计时器要清掉
                            end_host_exit_timer();
                        _ ->
                            case lists:keyfind(0, #iroom.status, NewState1#state.roles) of
                                false -> 
                                    %% 等待房主开始，计时开始...
                                    end_host_exit_timer(),
                                    start_host_exit_timer(State#state.host);
                                _ -> ok
                            end,
                            ok
                    end,
                    NewState1
            end,
            {noreply, NewState}
    end;

%% 房主开始/取消游戏
handle_cast({start, Rid, Status, ToRoomId}, State) ->
    reset_room_hand_timer(),
    #state{type = Type, pass = Pass, host = Host, carbon_id = CarbonId, id = Id, is_win = IsWin} = State,
    if
        Host =/= Rid ->
            ?WARNING("非房主发送了开始游戏的信号:~w(~w)", [Rid, Status]),
            {noreply, State};
        %% Type > 10 andalso Pass == 0 ->
        %%     ?WARNING("没有选择副本难度:~w", [Type]),
        %%     {noreply, State};
        true ->
            NewState = case Status =:= 1 of
                true ->
                    %% 房主点开始的动作，取消计时
                    end_host_exit_timer(),
                    F = fun(I) ->
                            I#iroom.status =:= 1 orelse I#iroom.id =:= State#state.host
                    end,
                    case lists:all(F, State#state.roles) of 
                        true ->
                            lib_conn:pack_cast(State#state.senders, 14025, [Rid, Status]),
                            %% ?INFO("start rid:~w(~w)", [Rid, Status]),
                            AvgLev = get_avg_lev(State#state.roles),
                            AvgGames = get_avg_games(State#state.roles),
                            MustWin = if
                                ToRoomId > 0 -> 0;
                                AvgGames =< 3 -> 1;
                                AvgLev < 6, IsWin < 1 -> 1;
                                true -> 0
                            end,
                            srv_hall ! {game_start, Type, Id, self(), length(State#state.roles), CarbonId, Pass, AvgLev, MustWin, ToRoomId},
                            end_room_hand_timer(),
                            State#state{status = 1};
                        false ->
                            %% 还有人没准备好，不能开始，重新计时
                            lib_conn:send_code(Rid, 13003104),
                            reset_room_hand_timer(),
                            %% ?INFO("ready roles:~w", [State#state.roles]),
                            State
                    end;
                false ->
                    case Type > 100 of
                        true ->
                            ok;
                        false -> 
                            srv_hall ! {match_del, State#state.type, State#state.id},
                            ok
                    end,
                    %% 取消，重新开始房主计时
                    case lists:keyfind(0, #iroom.status, State#state.roles) of
                        false -> 
                            %% 等待房主开始，计时开始...
                            start_host_exit_timer(State#state.host);
                        _ -> ok
                    end,
                    %% ?INFO("start rid:~w(~w)", [Rid, Status]),
                    lib_conn:pack_cast(State#state.senders, 14025, [Rid, Status]),
                    State#state{status = 0}
            end,
            {noreply, NewState}
    end;

%% 切换房间模式
handle_cast({set_type, Rid, ToType}, State) ->
    #state{id = Id, type = FromType, host = Host} = State,
    NewState = case Host =:= Rid of 
        true ->
            srv_hall ! {set_type, Id, FromType, ToType},
            lib_conn:pack_cast(State#state.senders, 14035, [ToType]),
            State#state{type = ToType};
        false ->
            ?WARNING("The message of 'set_type' is sent by NOT host! [Rid:~w, Host:~w, Type:~w]", 
                [Rid, State#state.host, ToType]),
            State
    end,
    {noreply, NewState};

%% 退出房间
handle_cast({role_exit, Rid}, State) ->
    self() ! {role_exit, Rid},
    {noreply, State};

%% 踢人
handle_cast({kick, Rid, Kid}, State) ->
    reset_room_hand_timer(),
    case State#state.host =:= Rid of 
        true ->
            NewState = case get_role(Kid, State) of
                false -> 
                    ?WARNING("kid(~w) not found, RoomId:~w", [Rid, State#state.id]),
                    lib_conn:pack_cast(State#state.senders, 14015, [Kid]),
                    State;
                Role ->
                    ?INFO("kick(~w - ~w)", [Rid, Kid]),
                    NewRoles = del_role(Role, State),
                    Role#iroom.pid ! {set_attr, [{status, 1}]},
                    NumMax = State#state.max_pos - length(State#state.closed_pos),
                    Num = length(NewRoles),
                    srv_hall ! {set_room_num, State#state.type, State#state.id, NumMax, Num},
                    lib_conn:pack_cast(State#state.senders, 14015, [Kid]),
                    Senders = [R#iroom.pid_sender || R <- NewRoles],
                    State#state{roles = NewRoles, senders = Senders}
            end,
            {noreply, NewState};
        false -> 
            ?ERR("kick(~w - ~w) not host(~w)", [Rid, Kid, State#state.host]),
            {noreply, State}
    end;

%% 设置发送进程ID，角色断线重连后重新设置
handle_cast({set_pid_sender, Rid, PidSender}, State) ->
    NewState2 = case get_role(Rid, State) of
        false -> 
            %% ?ERR("Role not found! set_pid_sender Rid:~w, RoomId:~w", [Rid, State#state.id]),
            State;
        Role ->
            NewState = set_role(Role#iroom{pid_sender = PidSender}, State),
            Senders = [R#iroom.pid_sender || R <- NewState#state.roles],
            NewState#state{senders = Senders}
    end,
    {noreply, NewState2};

handle_cast({set_lev, Rid, Lev}, State) ->
    NewState2 = case get_role(Rid, State) of
        false -> State;
        Role -> 
            %% ?INFO("@room set_lev, Rid:~w, Lev:~w", [Rid, Lev]),
            set_role(Role#iroom{lev = Lev}, State)
    end,
    {noreply, NewState2};

%% 开房/关闭房间位置
handle_cast({set_pos, Rid, Pos, Status}, State) ->
    reset_room_hand_timer(),
    case State#state.host =:= Rid of
        true ->
            %% ?INFO("set_pos:Rid~w, Pos:~w, Status:~w", [Rid, Pos, Status]),
            NewClosedPos = case Status == 0 of
                true -> 
                    case lists:member(Pos, State#state.closed_pos) of
                        true -> State#state.closed_pos;
                        false -> 
                            [Pos | State#state.closed_pos]
                    end;
                false -> lists:delete(Pos, State#state.closed_pos)
            end,
            NewState = State#state{closed_pos = NewClosedPos},
            NumMax = State#state.max_pos - length(NewClosedPos),
            Num = length(State#state.roles),
            srv_hall ! {set_room_num, State#state.type, State#state.id, NumMax, Num},
            lib_conn:pack_cast(State#state.senders, 14018, [Pos, Status]),
            {noreply, NewState};
        false ->
            ?WARNING("*** warning *** set_pos - not host:~w", [Rid]),
            {noreply, State}
    end;

%% 修改房间信息
handle_cast({mod_info, Rid, MapId, RoomName, Password}, State) ->
    reset_room_hand_timer(),
    case State#state.host =:= Rid of
        true ->
            NewState = State#state{
                map_id = MapId 
                ,name = RoomName
                ,password = Password
            },
            Lock = case Password of
                <<>> -> 0;
                _ -> 1
            end,
            srv_hall ! {set_room_lock, State#state.type, State#state.id, Lock},
            srv_hall ! {set_room_info, State#state.type, State#state.id, RoomName, Password},
            lib_conn:pack_cast(State#state.senders, 14017, [0, MapId, RoomName, Password]),
            {noreply, NewState};
        false ->
            ?WARNING("mod_info - not host:~w", [Rid]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    ?INFO("Not matched message: ~w", [_Msg]),
    {noreply, State}.

%% 重置准备状态，游戏结束后srv_room2调用
handle_info({ready_reset, Rid, IsWin}, State) ->
    #state{host = Host} = State,
    reset_room_hand_timer(),
    NewState = case get_role(Rid, State) of 
        false -> 
            ?ERR("error ready_reset Rid:~w, RoomId:~w, State:~p", [Rid, self(), State]),
            State;
        Role ->
            %% 约定时间内没有回到队伍则自动退出
            
            NewRole = Role#iroom{status = 0, game_count = add_game_count(IsWin, Role#iroom.game_count)},
            %% ?INFO("ready_reset rid:~w", [Rid]),
            set_role(NewRole, State)
    end,
    case Rid == Host of
        true -> 
            srv_hall ! {set_room_status, State#state.type, State#state.id, 0},
            {noreply, NewState#state{status = 0, is_win = IsWin}};
        false -> {noreply, NewState}
    end;

%% 退出房间
handle_info({force_role_exit, Rid}, State) ->
    case State#state.status of
        0 ->
            self() ! {role_exit, Rid},
            ok;
        _ ->
            %% ?WARNING("Unexpected force_role_exit! Rid:~w, Status:~w", 
            %% [Rid, State#state.status]),
            ok
    end,
    {noreply, State};

handle_info({role_exit, Rid}, State) ->
    NewState = case get_role(Rid, State) of
        false -> 
            %% 为客户端容错，不在房间时仍发送14004给自己
            %% lib_conn:pack_send(Rid, 14004, [Rid]),
            State;
        Role ->
            case State#state.status of
                0 -> reset_room_hand_timer();
                _ -> ok
            end,
            %% ?INFO("role_exit Rid:~w(~w), RoomId:~w", [Rid, State#state.host, self()]),
            Status = case State#state.status of
                1 ->
                    %% 正在匹配时有人退出，中止匹配
                    lib_conn:pack_cast(State#state.senders, 14025, [State#state.host, 0]),
                    srv_hall ! {match_del, State#state.type, State#state.id},
                    0;
                S -> S
            end,
            NewRoles = del_role(Role, State),
            Senders = [R#iroom.pid_sender || R <- NewRoles],
            NumMax = State#state.max_pos - length(State#state.closed_pos),
            Num = length(NewRoles),
            %% 更新大厅列表中的房间人数
            srv_hall ! {set_room_num, State#state.type, State#state.id, NumMax, Num},
            %% ?INFO("role_exit ~w", [Rid]),
            Role#iroom.pid ! {set_attr, [{status, 1}]},
            lib_conn:pack_cast(State#state.senders, 14004, [Rid]),
            case Num > 0 of
                true -> 
                    case Rid =:= State#state.host of
                        true -> 
                            %% 退出计时器
                            end_host_exit_timer(),
                            %% 老房主退出，寻找新房主
                            [I | _] = NewRoles,
                            I1 = I#iroom{status = 1}, %% 把房主设为己准备状态
                            State1 = set_role(I1, State),
                            lib_conn:pack_cast(State#state.senders, 14013, [I#iroom.id]),
                            %% 更换了新房主，如果人数大于1的，全都准备了，开始计时
                            case Num > 1  andalso State#state.status == 0 of
                                true ->
                                    case lists:keyfind(0, #iroom.status, State1#state.roles) of
                                        false ->
                                            start_host_exit_timer(I#iroom.id);
                                        _ -> ok
                                    end;
                                false -> ok
                            end,
                            State1#state{host = I#iroom.id, roles = NewRoles, senders = Senders, status = Status};
                        false -> 
                            State#state{roles = NewRoles, senders = Senders, status = Status}
                    end;
                false -> 
                    %% 没有其它玩家，结束房间进程
                    self() ! stop,
                    State#state{roles = NewRoles, senders = Senders, status = Status}
            end
    end,
    {noreply, NewState};

handle_info(force_stop, State) ->
    F = fun(#iroom{id = Rid, pid = Pid, pid_sender = PidSender}) ->
            lib_conn:pack_send(PidSender, 14004, [Rid]),
            ?INFO("force_stop ~w", [Rid]),
            Pid ! {set_attr, [{status, 1}]}            
    end,
    lists:foreach(F, State#state.roles),
    srv_hall ! {set_room_num, State#state.type, State#state.id, 0, 0},
    {stop, normal, State};


handle_info(all_role_exit, State) ->
    case State#state.status of
        0 ->
            F = fun(#iroom{id = Rid, pid = Pid, pid_sender = PidSender}) ->
                    lib_conn:pack_send(PidSender, 14004, [Rid]),
                    %% ?INFO("all_role_exit ~w", [Rid]),
                    Pid ! {set_attr, [{status, 1}]}            
            end,
            lists:foreach(F, State#state.roles),
            srv_hall ! {set_room_num, State#state.type, State#state.id, 0, 0},
            {stop, normal, State};
        _ ->
            ?WARNING("Unexpected all_role_exit! [Status:~w]", [State#state.status]),
            {noreply, State}
    end;


handle_info(stop, State) ->
    end_room_hand_timer(),
    {stop, normal, State};

handle_info(test, State) ->
    ?INFO("Room State: ~p", [State]),
    ?INFO("Room Dict:~p", [get()]),
    {noreply, State};

handle_info({pack_cast, Cmd, Data}, State) ->
    lib_conn:pack_cast(State#state.senders, Cmd, Data),
    {noreply, State};

handle_info(_Info, State) ->
    ?INFO("Not matched info: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---

add_role(Role, State) ->
    State#state{roles = [Role | State#state.roles]}.

del_role(Role, State) ->
    lists:keydelete(Role#iroom.id, 2, State#state.roles).

get_role(Rid, State) ->
    lists:keyfind(Rid, 2, State#state.roles).

set_role(Role, State) ->
    L = lists:keyreplace(Role#iroom.id, 2, State#state.roles, Role),
    State#state{roles = L}.

get_pos(Roles, Closed) ->
    get_pos(Roles, Closed, 0).
get_pos(Roles, Closed, Pos) ->
    case lists:keymember(Pos, #iroom.pos, Roles) orelse lists:member(Pos, Closed) of
        true -> get_pos(Roles, Closed, Pos + 1);
        false -> Pos
    end.
start_host_exit_timer(Host) ->
    TimerRef = erlang:send_after(?HOST_EXIT_TIME, self(), {force_role_exit, Host}),
    put(host_exit_ref, TimerRef),
    ok.

end_host_exit_timer() ->
    %% ?INFO("end timer",[]), 
    case get(host_exit_ref) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref),
            erase(host_exit_ref)
    end,
   ok.

start_room_hand_timer() ->
    %% ?INFO("start room hand timer", []),
    TimerRef = erlang:send_after(?ROOM_HAND_TIME, self(), all_role_exit),
    put(room_exit_ref, TimerRef).

end_room_hand_timer() ->
    %% ?INFO("end room hand timer", []),
    case get(room_exit_ref) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref),
            erase(room_exit_ref)
    end.
reset_room_hand_timer() ->
    end_room_hand_timer(),
    start_room_hand_timer().

%% ihall 转换成 iroom
ihall_to_iroom(Ihall) ->
    #ihall{
        id = Rid 
        ,name = Name
        ,pid = Pid
        ,pid_sender = PidSender
        ,sex = Sex
        ,lev = Lev
        ,win_rate = WinRate
        ,exp2_etime = Exp2ETime
        ,game_count = GameCount
        ,equ_info   = EquInfo
        ,power      = Power
    } = Ihall,
    #iroom{
        id = Rid 
        ,name = Name
        ,pid = Pid
        ,pid_sender = PidSender
        ,status = 0
        ,sex = Sex
        ,lev = Lev
        ,win_rate = WinRate
        ,exp2_etime = Exp2ETime
        ,game_count = GameCount
        ,equ_info   = EquInfo
        ,power      = Power
    }.

%%' 获得当前战斗角色等级平均值
get_avg_lev(Roles) ->
    LevList = [Lev || #iroom{lev = Lev} <- Roles],
    L = length(LevList),
    util:ceil(lists:sum(LevList) / L).

get_avg_games(Roles) ->
    L = [W + D + L || #iroom{
            game_count = {W, D, L, _}} <- Roles],
    Avg = lists:sum(L) / length(L),
    util:ceil(Avg).

%%. ================================================

add_game_count(IsWin, {Win, Draw, Lost, Escape}) ->
    case IsWin of
        1  -> {Win + 1, Draw, Lost, Escape};
        0  -> {Win, Draw + 1, Lost, Escape};
        -1 -> {Win, Draw, Lost + 1, Escape};
        _  -> {Win, Draw, Lost, Escape}
    end.

get_equ_tid(Pos, EquInfo) ->
    case lists:keyfind(Pos, 1, EquInfo) of
        false -> 0;
        {_, Tid, _} -> Tid
    end.

get_equ_max_lev(EquInfo) ->
    case [Lev || {_, _, Lev} <- EquInfo] of
        [] -> 0;
        L -> lists:max(L)
    end.
