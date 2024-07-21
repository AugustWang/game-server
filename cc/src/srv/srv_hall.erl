%%----------------------------------------------------
%% 游戏大厅服务
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_hall).
-behaviour(gen_server).
-export([
        start_link/0
        ,join_room/4
        ,create_room/2
        ,room_list/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("ihall.hrl").

-define(MATCH_ROBOT_TIME, 1 * 1000).
%% -define(MATCH_ROBOT_TIME, 1 * 1000).

-record(state, {
        %% matches  = [] %% 挑战类型房间 [#match{}, ...]
    }
).

-record(room, { 
        id
        ,name = <<>>
        ,password = <<>>
        ,map_id = 0
        ,type = 0             %% 类型
        ,num_max = 0
        ,num = 0
        ,pid
        ,map_name = <<>>
        ,creator_id = 0
        ,creator_name = <<>>
        ,lock = 0        %% 是否己加锁(1=是,0=否)
        ,status = 0      %% 房间状态(0=等待中,1=游戏中)
    }).

-record(match, {
        room_id
        ,pid_room
        ,role_num
        ,robot_ref
        ,avg_lev = 0 %% 平均等级
        ,status      %% undefined | waiting | {find, RoomId}
        ,next_ref    %% 下一次查找定时器
        ,must_win = 0 %% 游戏次数
    }
).

%% --- 对外接口 ---

%% 新建连接
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join_room(RoomType, RoomId, Password, Rs) ->
    Ri = role_to_ihall(Rs),
    gen_server:cast(srv_hall, {join_room, RoomType, RoomId, Password, Ri}).

create_room(Type, Rs) ->
    Ri = role_to_ihall(Rs),
    gen_server:cast(srv_hall, {create_room, Type, Ri}).

room_list(Type, Id) ->
    gen_server:cast(srv_hall, {room_list, Type, Id}).

%% --- 服务器内部实现 ---

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    State = #state{},
    %% 如果添加了模式，这里一定要添加相应类型的数据初始化
    lists:foreach(fun(Mode) -> 
                put({rooms, Mode}, []),
                put({matches, Mode}, [])
        end, ?GAME_MODES),
    put(max_id, 1), %% 当前房间最大ID
    put(ids, []), %% 回收回来的房间ID, 供重复利用 [id, ...]
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({room_list, Type, PidSender}, State) ->
    Data = [{
            R#room.id
            ,R#room.type
            ,R#room.name
            ,R#room.map_id
            ,R#room.num_max
            ,R#room.num
            ,R#room.status
            ,R#room.lock
        } || R <- get_rand_rooms(Type)],
    lib_conn:pack_send(PidSender, 13001, [Data]),
    {noreply, State};

%% 创建房间 {create_room, Type, #ihall{}}
handle_cast({create_room, Type, Ri}, State) ->
    Id = get_room_id(),
    RoomName = get_room_name(),
    try
        case srv_room1:start(Type, Id, RoomName, Ri) of
            {ok, Pid} ->
                Room = #room{ 
                    id = Id 
                    ,type = Type
                    ,num_max = 3
                    ,num = 1
                    ,pid = Pid
                    ,name = RoomName
                    ,map_name = <<"-">>
                    ,creator_id = Ri#ihall.id
                    ,creator_name = Ri#ihall.name
                },
                add_room(Type, Room);
            _ -> ok
        end
        catch T:X ->
            ?WARNING("~w:~w", [T, X])
    end,
    {noreply, State};

%% 快速加入
handle_cast({join_room, Type, 0, Password, Ri}, State) ->
    Room = get_rand_room(Type, 0),
    case Room == no_room of
        true -> gen_server:cast(srv_hall, {create_room, Type, Ri});
        false -> gen_server:cast(srv_hall, {join_room, Type, Room#room.id, Password, Ri})
    end,
    {noreply, State};

%% 加入房间
handle_cast({join_room, _, RoomId, _, Ri}, State) when RoomId > 1000000 ->
    lib_conn:send_code(Ri#ihall.pid_sender, 13003102),
    {noreply, State};

handle_cast({join_room, Type, RoomId, Password, Ri}, State) when Type > 0 ->
    Room = get_room(Type, RoomId),
    Alive = Room =/= false andalso is_process_alive(Room#room.pid),
    if
        Room == false ->
            lib_conn:send_code(Ri#ihall.pid_sender, 13003100);
        Alive == false ->
            del_room(Type, RoomId),
            ?WARNING("not alive room:~w", [Room]),
            lib_conn:send_code(Ri#ihall.pid_sender, 13003103);
        Room#room.status =/= 0 ->
            lib_conn:send_code(Ri#ihall.pid_sender, 13003102);
        Room#room.num >= Room#room.num_max ->
            lib_conn:send_code(Ri#ihall.pid_sender, 13003101);
        true ->
            gen_server:cast(Room#room.pid, {add_role, Password, Ri})
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    ?WARNING("Not matched message: ~w", [_Msg]),
    {noreply, State}.

%% 设置房间人数上限及当前人数
handle_info({set_room_num, Type, RoomId, NumMax, Num}, State) ->
    case get_room(Type, RoomId) of
        false -> 
            ?WARNING("Room not found when set_room_num:Type:~w, Id:~w, Num:~w", [Type, RoomId, Num]),
            skip;
        Room->
            case Num > 0 of 
                true ->
                    NewRoom = Room#room{num_max = NumMax, num = Num},
                    set_room(Type, NewRoom);
                false -> 
                    %% 房间中没有人，删除房间
                    del_room(Type, RoomId),
                    case Type < 100 of
                        true ->
                            Matches = get_matches(Type),
                            case get_match(RoomId, Matches) of
                                false -> ok;
                                Match ->
                                    Matches1 = del_match(Match, Matches),
                                    put_matches(Type, Matches1),
                                    ok
                            end;
                        false ->
                            ok
                    end
            end
    end,
    {noreply, State};

%% 设置房间锁定状态
handle_info({set_room_lock, Type, RoomId, Status}, State) ->
    case get_room(Type, RoomId) of
        false -> 
            ?WARNING("Room not found when set_room_lock:~w", [RoomId]),
            ok;
        Room->
            NewRoom = Room#room{lock = Status},
            set_room(Type, NewRoom)
    end,
    {noreply, State};

%% 修改房间类型
handle_info({set_type, RoomId, FromType, ToType}, State) ->
    case get_room(FromType, RoomId) of
        false -> 
            ?WARNING("Room not found when set_type! [RoomId:~w, FromType:~w, ToType:~w]", [RoomId, FromType, ToType]),
            State;
        Room->
            del_room2(FromType, RoomId),
            add_room(ToType, Room)
    end,
    {noreply, State};

%% 设置房间信息,名和密码
handle_info({set_room_info, Type, RoomId, RoomName, Password}, State) ->
    case get_room(Type, RoomId) of
        false ->
            ?WARNING("Room not found when set_room_info:~w", [RoomId]),
            ok;
        Room -> 
            NewRoom = Room#room{
                name = RoomName,
            password = Password
        },
            set_room(Type, NewRoom)
    end,
    {noreply, State};

%% 设置房间游戏状态
handle_info({set_room_status, Type, RoomId, Status}, State) ->
    case get_room(Type, RoomId) of
        false -> 
            ?WARNING("Room not found when set_room_status! RoomId:~w, Type:~w, status:~w", [RoomId, Type, Status]),
            ok;
        Room->
            NewRoom = Room#room{status = Status},
            set_room(Type, NewRoom)
    end,
    {noreply, State};

%% 接收由srv_room1发来的游戏开始消息
handle_info({game_start, Type, RoomId, PidRoom, RoleNum, _CarbonId, _Pass, AvgLev, MustWin, ToRoomId}, State) ->
    case get_room(Type, RoomId) of
        false -> 
            ?WARNING("Room not found when game_start:~w", [RoomId]),
            ok;
        Room->
            %% ?INFO("AvgLev:~w, MustWin:~w, ToRoomId:~w", [AvgLev, MustWin, ToRoomId]),
            Status = if
                ToRoomId > 0 -> {find, ToRoomId};
                MustWin == 1 -> waiting;
                true -> undefined
            end,
            Match = #match{
                room_id = RoomId 
                ,pid_room = PidRoom 
                ,role_num = RoleNum 
                ,avg_lev = AvgLev
                ,status = Status
                ,must_win = MustWin
            },
            case Type of 
                1 -> self() ! {match, Type, Match};
                2 -> self() ! {match, Type, Match};
                _ when Type > 100 -> 
                    erlang:spawn(srv_game101, start, [[Type, RoomId, PidRoom]]);
                _ -> skip
            end,
            NewRoom = Room#room{status = 1},
            set_room(Type, NewRoom)
    end,
    {noreply, State};

handle_info({match_del, Type, RoomId}, State) ->
    case get_room(Type, RoomId) of
        false -> 
            ?WARNING("Room not found when match_del:~w", [RoomId]),
            ok;
        Room->
            NewRoom = Room#room{status = 0},
            set_room(Type, NewRoom)
    end,
    Matches = get_matches(Type),
    case get_match(RoomId, Matches) of
        false -> ok;
        Match ->
            Matches1 = del_match(Match, Matches),
            put_matches(Type, Matches1),
            ok
    end,
    {noreply, State};

handle_info({match_team, Type, RoomId, Nth}, State) ->
    %% ?INFO("match_team, Type:~w, RoomId:~w, Nth:~w", [Type, RoomId, Nth]),
    Matches = get_matches(Type),
    MyMatch = lists:keyfind(RoomId, #match.room_id, Matches),
    MaxNth = get_max_nth(Type, MyMatch),
    if
        MyMatch == false -> 
            ?WARNING("Match not found:~w", [{match_team, Type, RoomId, Nth}]),
            ok;
        Nth > MaxNth ->
            #match{
                role_num = RoleNum
                ,avg_lev = AvgLev
            } = MyMatch,
            catch robot ! {create_team, hall, Type, RoleNum, AvgLev, 0, RoomId},
            MyMatch1 = MyMatch#match{next_ref = undefined},
            set_match(Type, MyMatch1, Matches),
            %% ?INFO("start robot"),
            ok;
        true ->
            #match{
                role_num = RoleNum
                ,pid_room = PidRoom
                ,avg_lev = AvgLev
            } = MyMatch,
            case is_process_alive(PidRoom) of
                true ->
                    FIxedMatches = [M || M <- Matches, M#match.role_num == RoleNum],
                    DiffMax = case Nth of
                        1 -> 2;
                        2 -> 4;
                        3 -> 6;
                        4 -> 8;
                        _ -> 10
                    end,
                    case search_team(RoomId, AvgLev, AvgLev, DiffMax, FIxedMatches, FIxedMatches) of
                        false -> do_next_match(Type, Nth, MyMatch, Matches);
                        ToMatch ->
                            case is_process_alive(ToMatch#match.pid_room) of
                                true ->
                                    Matches1 = del_match(MyMatch, Matches),
                                    Matches2 = del_match(ToMatch, Matches1),
                                    put_matches(Type, Matches2),
                                    match_ok(Type, MyMatch, ToMatch);
                                false ->
                                    ?WARNING("Found not alive tomatch!", []),
                                    Matches1 = del_match(ToMatch, Matches),
                                    do_next_match(Type, Nth, MyMatch, Matches1)
                            end
                    end;
                false -> 
                    ?WARNING("Found not alive mymatch!", []),
                    Matches1 = del_match(MyMatch, Matches),
                    put_matches(Type, Matches1)
            end
    end,
    {noreply, State};

handle_info({match, Type, Match = #match{ pid_room = PidRoom, 
            status = {find, RoomId1}}}, State) ->
    Matches = get_matches(Type),
    ToMatch = lists:keyfind(RoomId1, #match.room_id, Matches),
    if
        ToMatch == false -> 
            %% ?WARNING("Match not found:{find, ~w}", [RoomId1]),
            PidRoom ! force_stop,
            %% add_match(Type, Match, Matches),
            %% self() ! {match_team, Type, RoomId, 1},
            ok;
        true ->
            case is_pid(ToMatch#match.pid_room) andalso 
                is_process_alive(ToMatch#match.pid_room) of
                true -> match_ok(Type, Match, ToMatch);
                false -> ?WARNING("Found not alive tomatch!", [])
            end,
            Matches1 = del_match(ToMatch, Matches),
            put_matches(Type, Matches1)
    end,
    {noreply, State};

handle_info({match, Type, Match = #match{room_id = RoomId, 
            status = Status, role_num = RoleNum, avg_lev = AvgLev,
            must_win = MustWin}}, State) ->
    Matches = get_matches(Type),
    add_match(Type, Match, Matches),
    case Status of
        waiting ->
            catch robot ! {create_team, hall, Type, RoleNum, AvgLev, MustWin, RoomId},
            %% erlang:send_after(1000, robot, {create_team, hall, Type, RoleNum, AvgLev, MustWin, RoomId}),
            ok;
        _ -> self() ! {match_team, Type, RoomId, 1}
    end,
    {noreply, State};

handle_info(test, State) ->
    ?INFO("Hall State: ~p", [State]),
    ?INFO("dict: ~p", [get()]),
    {noreply, State};

handle_info(_Info, State) ->
    ?WARNING("Not matched info: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?WARNING("srv_hall terminate! [Reason:~w]", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---
get_rand_room(Type, Status) ->
    Rooms = get({rooms, Type}),
    F = fun(L) -> L#room.status == Status andalso L#room.lock == 0 andalso L#room.num < L#room.num_max end,
    Rooms2 = lists:filter(F, Rooms),
    case length(Rooms2)>0 of
        false -> no_room;
        true ->
            Len = length(Rooms2),
            Nth = util:rand(1, Len),
            lists:nth(Nth, Rooms2)
    end.

get_rand_rooms(Type) ->
    Rooms = get({rooms, Type}),
    Len = length(Rooms),
    NumMax = 6,
    case Len > NumMax of
        true -> 
            {Rooms1, Len1} = get_rand_rooms(Rooms, Len, [], 0, 0, NumMax),
            case Len1 >= NumMax of
                true -> Rooms1;
                false ->
                    %% 如果等待状态的房间不足6个，显示数个游戏中的房间
                    {Rooms2, _Len2} = 
                    get_rand_rooms(Rooms, Len, [], 0, 1, NumMax - Len1),
                    Rooms1 ++ Rooms2
            end;
        false -> 
            Rooms
            %% LenShow = NumMax - Len,
            %% case LenShow > 0 of
            %%     true ->
            %%         Show = get_show_room(Type, LenShow),
            %%         Rooms ++ Show;
            %%     false -> Rooms
            %% end
    end.

get_rand_rooms([], _Len, Reply, ReLen, _Status, _NumMax) -> 
    {Reply, ReLen};
get_rand_rooms(_Rooms, _Len, Reply, ReLen, _Status, NumMax) when ReLen >= NumMax -> 
    {Reply, ReLen};
get_rand_rooms(Rooms, Len, Reply, ReLen, Status, NumMax) ->
    Nth = util:rand(1, Len),
    Room = lists:nth(Nth, Rooms),
    NewRooms = lists:keydelete(Room#room.id, 2, Rooms),
    NewLen = Len - 1,
    {NewReply, NewReLen} = case Room#room.status =:= Status of 
        true -> {[Room | Reply], ReLen + 1};
        false -> {Reply, ReLen}
    end,
    get_rand_rooms(NewRooms, NewLen, NewReply, NewReLen, Status, NumMax).

%% get_room(Type) ->
%%     get({rooms, Type}).

get_room(Type, 0) ->
    Rooms = get({rooms, Type}),
    lists:keyfind(0, #room.status, Rooms);

get_room(Type, RoomId) ->
    Rooms = get({rooms, Type}),
    lists:keyfind(RoomId, 2, Rooms).

set_room(Type, Room) ->
    Rooms = get({rooms, Type}),
    NewRooms = lists:keyreplace(Room#room.id, 2, Rooms, Room),
    put({rooms, Type}, NewRooms).

add_room(Type, Room) ->
    Rooms = get({rooms, Type}),
    NewRooms = lists:keystore(Room#room.id, 2, Rooms, Room),
    put({rooms, Type}, NewRooms).

del_room(Type, RoomId) ->
    Rooms = get({rooms, Type}),
    NewRooms = lists:keydelete(RoomId, 2, Rooms),
    recycle_room_id(RoomId), %% 回收房间ID
    put({rooms, Type}, NewRooms).

%% 删除房间，不回收房间ID
del_room2(Type, RoomId) ->
    Rooms = get({rooms, Type}),
    NewRooms = lists:keydelete(RoomId, 2, Rooms),
    put({rooms, Type}, NewRooms).

get_matches(Type) ->
    get({matches, Type}).

put_matches(Type, Matches) ->
    put({matches, Type}, Matches).

get_match(RoomId, Matches) ->
    lists:keyfind(RoomId, 2, Matches).

del_match(#match{room_id = RoomId, robot_ref = RobotRef, next_ref = NextRef}, Matches) ->
    cancel_timer(RobotRef),
    cancel_timer(NextRef),
    lists:keydelete(RoomId, #match.room_id, Matches).

%% del_match(Type, RoomId, Matches) ->
%%     NewMatches = lists:keydelete(RoomId, 2, Matches),
%%     put({matches, Type}, NewMatches).

add_match(Type, Match, Matches) ->
    NewMatches = lists:keystore(Match#match.room_id, 2, Matches, Match),
    put({matches, Type}, NewMatches).

set_match(Type, Match, Matches) ->
    NewMatches = lists:keyreplace(Match#match.room_id, 2, Matches, Match),
    put({matches, Type}, NewMatches).

%% 获取一个房间名称
get_room_name() ->
    case util:rand(1, 6) of
        1 -> <<"#1">>;
        2 -> <<"#2">>;
        3 -> <<"#3">>;
        4 -> <<"#4">>;
        5 -> <<"#5">>;
        6 -> <<"#6">>
    end.

%% 获取一个可用房间ID号
%% () -> RoomId
get_room_id() ->
    case get(ids) of
        [] -> 
            MaxId = get(max_id),
            put(max_id, MaxId + 1),
            MaxId + 1;
        [H | T] -> 
            put(ids, T),
            H
    end.

%% 回收房间ID号
recycle_room_id(Id) ->
    Ids = get(ids),
    put(ids, [Id | Ids]).

%% get_show_room(Type, Num) ->
%%     get_show_room(Type, Num, []).
%% 
%% get_show_room(Type, Num, Reply) when Num > 0 ->
%%     Reply1 = [get_show_room({Type, Num}) | Reply],
%%     get_show_room(Type, Num - 1, Reply1);
%% get_show_room(_Type, 0, Reply) -> 
%%     Reply.
%% 
%% get_show_room({Type, Id}) ->
%%     NumMax = case util:rand(1, 10) of
%%         1 -> 1;
%%         2 -> 2;
%%         3 -> 3;
%%         _ -> 1
%%     end,
%%     Num = util:rand(1, NumMax),
%%     Lock = case util:rand(1, 10) of
%%         1 -> 1;
%%         _ -> 0
%%     end,
%%     #room{ 
%%         id = 100 * 10000 + Id 
%%         ,type = Type
%%         ,num_max = NumMax
%%         ,num = Num
%%         ,name = get_room_name()
%%         ,map_name = <<"-">>
%%         ,status = 1
%%         ,lock = Lock
%%     }.

%% 角色记录转换成大厅记录
role_to_ihall(Rs) ->
    #role{
        id = Rid
        ,name = Name 
        ,pid = Pid 
        ,pid_sender = PidSender
        ,sex = Sex
        ,lev = Lev
        ,win_rate = WinRate
        ,exp2_etime = Exp2Time
        ,game_count = GameCount
        ,is_win     = IsWin
        ,equ_info   = EquInfo
        ,power      = Power
    } = Rs,
    #ihall{ 
        id = Rid 
        ,name = Name 
        ,pid = Pid 
        ,pid_sender = PidSender
        ,sex = Sex
        ,lev = Lev
        ,win_rate = WinRate
        ,exp2_etime = Exp2Time
        ,game_count = GameCount
        ,is_win     = IsWin
        ,equ_info   = EquInfo
        ,power      = Power
    }.

match_ok(Type, 
    #match{room_id = RoomId1, pid_room = PidRoom1}, 
    #match{room_id = RoomId2, pid_room = PidRoom2}) ->
    match_ok(Type, {RoomId1, PidRoom1}, {RoomId2, PidRoom2});

match_ok(1, {RoomId, PidRoom}, {RoomId2, PidRoom2}) ->
    erlang:spawn(srv_game1, start, [[{RoomId, PidRoom}, {RoomId2, PidRoom2}]]);
match_ok(2, {RoomId, PidRoom}, {RoomId2, PidRoom2}) ->
    erlang:spawn(srv_game2, start, [[{RoomId, PidRoom}, {RoomId2, PidRoom2}]]);
match_ok(T, {RoomId, PidRoom}, {RoomId2, PidRoom2}) ->
    ?WARNING("Error Type:~w", [{T, {RoomId, PidRoom}, {RoomId2, PidRoom2}}]).

%%' cancel_timer(Ref) -> void
cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).
%%.

do_next_match(Type, Nth, Match, Matches) ->
    Ref = erlang:send_after(2000, self(), 
        {match_team, Type, Match#match.room_id, Nth + 1}),
    Match1 = Match#match{next_ref = Ref},
    set_match(Type, Match1, Matches).

search_team(_RoomId, LevMin, LevMax, DiffMax, _Matches, _Matches) 
when LevMax - LevMin > DiffMax -> 
    false;
search_team(RoomId, LevMin, LevMax, DiffMax, [Match | MatchesT], Matches) ->
    #match{
        room_id = RoomId1
        ,avg_lev = Lev1
        ,status = Status
    } = Match,
    if
        RoomId == RoomId1; Status == waiting -> 
            search_team(RoomId, LevMin, LevMax, DiffMax, MatchesT, Matches);
        Lev1 >= LevMin, Lev1 =< LevMax ->
            Match;
        true ->
            search_team(RoomId, LevMin, LevMax, DiffMax, MatchesT, Matches)
    end;
search_team(RoomId, LevMin, LevMax, DiffMax, [], Matches) -> 
    %% ?INFO("LevMin:~w, LevMax:~w, DiffMax:~w", [LevMin, LevMax, DiffMax]),
    search_team(RoomId, LevMin-1, LevMax+1, DiffMax, Matches, Matches).

get_max_nth(Type, #match{role_num = RoleNum}) ->
            case {Type, RoleNum} of
                {1, 1} -> 5;
                {1, 2} -> 6;
                {1, 3} -> 7;
                {2, 1} -> 7;
                {2, 2} -> 8;
                {2, 3} -> 9;
                _ -> 5
            end;
get_max_nth(_Type, _) -> 5.

%% vim: set foldmethod=marker foldmarker=%%',%%.:
