%%----------------------------------------------------
%% 测试用客户端
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(robot).
-compile(export_all).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("rs.hrl").

%% -define(HOST_REMOTE, "58.61.153.166", 8000).
%% -define(HOST_REMOTE, "216.12.208.198", 8000).
-define(TCP_OPTS, [binary, {packet, 0}, {nodelay, true}, {delay_send, false}, {exit_on_close, false}]).
-define(TCP_HANDSHAKING,    <<"ABCDEFGHIJKLMN876543210">>).

-record(state, {
        at_hall = [] %% 在大厅
        ,at_game = [] %% 在游戏中
        ,ip
        ,port
        ,battle_min = 0
    }
).

%% 新建连接
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --- 服务器内部实现 ---

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    process_flag(trap_exit, true),
    {_, Port} = application:get_env(cc, tcp_port),
    State = #state{
        ip = "127.0.0.1"
        ,port = Port
    },
    erlang:send_after(1000, self(), login_robots),
    {ok, State}.

handle_call(count_robots, _From, State) ->
    Count = length(State#state.at_hall) +
    length(State#state.at_game),
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    ?ERR("Not matched message: ~w", [_Msg]),
    {noreply, State}.

handle_info(reg, State) ->
    #state{} = State,
    {noreply, State};

handle_info(login_robots, State) ->
    Nth = case get(login_robots_nth) of
        undefined -> 
            put(login_robots_nth, 0),
            0;
        N -> 
            put(login_robots_nth, N + 1),
            N + 1
    end,
    Num = 10, %% 执行一次登陆的个数
    Start = Nth * Num + 1,
    End = Start + Num - 1,
    Ids = lists:seq(Start, End),
    %% ?INFO("Ids:~w", [Ids]),
    #state{ip = Ip, port = Port, at_hall = AtHall, 
        at_game = AtGame} = State,
    Online = AtHall ++ AtGame,
    spawn(fun() -> login_robots(Ids, Ip, Port, Online) end),
    {noreply, State};

handle_info({login_robots, Start, Num}, State) ->
    #state{ip = Ip, port = Port, at_hall = AtHall, 
        at_game = AtGame} = State,
    Online = AtHall ++ AtGame,
    Ids = lists:seq(Start, Start + Num - 1),
    spawn(fun() -> login_robots(Ids, Ip, Port, Online) end),
    {noreply, State};

handle_info(prep_stop, State) ->
    case get(battle_loop_ref) of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    {noreply, State};

handle_info({battle, Min}, State) when is_integer(Min) ->
    case get(battle_loop_ref) of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    self() ! battle,
    {noreply, State#state{battle_min = Min}};

handle_info(battle, State) ->
    AtHallNum = length(State#state.at_hall),
    AtGameNum = length(State#state.at_game),
    %% ?INFO("AtHallNum:~w, AtGameNum:~w, battle_min:~w", [AtHallNum, AtGameNum, State#state.battle_min]),
    case AtGameNum < State#state.battle_min andalso AtHallNum > 1 of
        true ->
            Type = case util:rand(1, 2) of
                1 -> 2;
                _ -> 1
            end,
            Num = case util:rand(1, 4) of
                1 -> 1;
                2 -> 2;
                3 -> 3;
                _ -> 1
            end,
            self() ! {create_team, robot, Type, Num, 0, 0, 0},
            Ref = erlang:send_after(50, self(), battle),
            put(battle_loop_ref, Ref),
            ok;
        false -> 
            Ref = erlang:send_after(10000, self(), battle),
            put(battle_loop_ref, Ref),
            ok
    end,
    {noreply, State};

handle_info({login_ok, AccId, Rid, Pid, Lev}, State) ->
    %% ?INFO("login_ok aid:~w", [AccId]),
    #state{at_hall = AtHall, at_game = AtGame} = State,
    IsAtHall = lists:keyfind(AccId, 1, AtHall),
    IsAtGame = lists:keyfind(AccId, 1, AtGame),
    State1 = if
        IsAtHall == false, IsAtGame == false ->
            %% AtHall1 = lists:keystore(AccId, 1, AtHall, {AccId, Rid, Pid, Lev}),
            AtHall1 = AtHall ++ [{AccId, Rid, Pid, Lev}],
            State#state{at_hall = AtHall1};
        true ->
            State
    end,
    {noreply, State1};

handle_info({msg, AccId, Msg}, State) ->
    #state{at_hall = AtHall, at_game = AtGame} = State,
    case lists:keyfind(AccId, 1, AtHall) of
        false ->
            case lists:keyfind(AccId, 1, AtGame) of
                false ->
                    ?INFO("Not found!", []),
                    ok;
                {_, _, Pid, _} -> 
                    ?INFO("at_game!", []),
                    Pid ! Msg
            end;
        {_, _, Pid, _} -> 
            ?INFO("at_hall", []),
            Pid ! Msg
    end,
    {noreply, State};

handle_info({msgs, Msg}, State) ->
    #state{at_hall = AtHall, at_game = AtGame} = State,
    L = AtHall ++ AtGame,
    F = fun({_, _, Pid, _}) ->
            Pid ! Msg
    end,
    lists:foreach(F, L),
    {noreply, State};

%% 创建队伍
%% Type = 1 | 2
%% Num = 1 | 2
handle_info({create_team, Trigger, Type, Num, AvgLev, MustLost, ToRoomId}, State) ->
    %% ?INFO("{create_team, Trigger:~w, Type:~w, Num:~w, AvgLev:~w}", [Trigger, Type, Num, AvgLev]),
    %% ?INFO("{create_team, Trigger:~w, Type:~w, Num:~w, AvgLev:~w}", [Trigger, Type, Num, AvgLev]),
    login_trigger(State),
    State1 = case get_robot(AvgLev, State) of
        {ok, Pid, State0} ->
            Pid ! {create_room, Trigger, Type, Num, AvgLev, MustLost, ToRoomId},
            State0;
        {error, 0} ->
            ?INFO("No online robot!", []),
            State;
        _Other ->
            ?INFO("No online robot! [~w]", [_Other]),
            State 
    end,
    {noreply, State1};

%% 加入房间
handle_info({join_room, Trigger, Type, RoomId, AvgLev}, State) ->
    State1 = case get_robot(AvgLev, State) of
        {ok, Pid, State0} ->
            Pid ! {join_room, Trigger, Type, RoomId, AvgLev},
            State0;
        _Other ->
            ?INFO("No online robot when join_room! [~w]", [_Other]),
            State 
    end,
    {noreply, State1};

handle_info({set_robot, Type, AccId, From}, State) ->
    %% ?INFO("set_robot, Type:~w, AccId:~w, From:~w", [Type, AccId, From]),
    State1 = case set_robot(Type, AccId, State) of
        {ok, State0} -> State0;
        _Other ->
            ?INFO("Robot not found when set_robot, Type:~w, AccId:~w, From:~w", [Type, AccId, From]),
            State 
    end,
    {noreply, State1};

handle_info({logout, AccId}, State) when is_integer(AccId) ->
    %% ?INFO("logout: ~w", [AccId]),
    #state{at_hall = AtHall , at_game = AtGame} = State,
    State1 = State#state{
        at_hall = lists:keydelete(AccId, 1, AtHall)
        ,at_game = lists:keydelete(AccId, 1, AtGame)
    },
    {noreply, State1};

handle_info(info, State) ->
    P = self(),
    Memory = erlang:process_info(P, memory),
    MessageQueueLen = erlang:process_info(P, message_queue_len),
    ?INFO("
        count at_hall: ~p
        count at_game: ~p
        battle_min   : ~p
        memory       : ~p
        msg_queue_len: ~p
        ", 
        [
            length(State#state.at_hall)
            ,length(State#state.at_game)
            ,State#state.battle_min
            ,Memory
            ,MessageQueueLen
        ]),
    {noreply, State};

handle_info(info2, State) ->
    ?INFO("
        at_hall: ~p
        at_game: ~p
        count at_hall: ~p
        count at_game: ~p
        battle_min: ~p
        ", 
        [
            State#state.at_hall
            ,State#state.at_game
            ,length(State#state.at_hall)
            ,length(State#state.at_game)
            ,State#state.battle_min
        ]),
    {noreply, State};

%% handle_info({test2, AvgLev}, Rs) ->
%%     {ok, Pid, State} = get_robot(AvgLev, Rs),
%%     ?INFO("
%%         count at_hall: ~p
%%         count at_game: ~p
%%         battle_min: ~p
%%         pid: ~p
%%         ", 
%%         [
%%             length(State#state.at_hall)
%%             ,length(State#state.at_game)
%%             ,State#state.battle_min
%%             ,Pid
%%         ]),
%%     {noreply, Rs};

handle_info({'EXIT', _Pid, _Why}, Rs) ->
    %% ?INFO("EXIT, Pid:~w, Why:~w", [Pid, Why]),
    {noreply, Rs};

handle_info({cmd, Cmd, Data}, State) ->
    #state{at_hall = AtHall} = State,
    case AtHall of
        [{_, _, Pid, _} | _] -> 
            Pid ! {cmd, Cmd, Data};
        _ -> 
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    ?ERR("Not matched info: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---

%%' login
login(Rs0) ->
    #rs{acc_id = Id, ip = Host, port = Port} = Rs0,
    case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
        {ok, Socket} ->
            %% io:format("connect ok!~n", []),
            PidSender = spawn_link(fun() -> sender(Socket, 0) end),
            %% init state
            Rs = Rs0#rs{pid_sender = PidSender, socket = Socket},
            Pid = spawn_link(?MODULE, recv_process, [Rs]),
            ok = gen_tcp:controlling_process(Socket, Pid),
            %% util:sleep(200), %% 暂停一会，等接收器初始化完毕
            gen_tcp:send(Socket, ?TCP_HANDSHAKING), %% 发送握手消息
            %% login
            IdS = case env:get(platform) of
                id -> "robot" ++ integer_to_list(Id);
                _ -> integer_to_list(Id)
            end,
            TimeS = integer_to_list(util:unixtime()),
            ServerIdS = "1",
            {_, ServerKey} = application:get_env(cc, server_key),
            M = lists:concat([TimeS, IdS, ServerIdS, ServerKey]),
            Signature = util:md5(M),
            LoginKey = list_to_binary("account_id="++IdS++"&serverid=1&time="
                ++TimeS++"&signature="++Signature),
            PidSender ! {cmd, 10003, [LoginKey]},
            {ok, Pid};
        {error, Reason} ->
            io:format("connect error: ~w~n", [Reason]),
            {error, Reason}
    end.
%%.

%%' 接收消息
recv_process(#rs{socket = Socket} = Rs) ->
    receive
        {tcp, Socket, BinIn} ->
            BinIn2 = case Rs#rs.tmp_package of
                <<>> -> BinIn;
                Tmp -> list_to_binary([Tmp, BinIn])
            end,
            case BinIn2 of
                <<Len:16, Cmd:16, Bin:Len/binary, Bin1/binary>> ->
                    %% io:format("Len:~w, Cmd:~w, Bin:~w~n", [Len, Cmd, Bin]),
                    case robot_unpack:p(Cmd, Bin) of
                        {ok, Data} ->
                            RestLen = byte_size(Bin1),
                            case RestLen > 0 of
                                true -> 
                                    case RestLen > 3000 of
                                        true ->
                                            ?INFO("Rest Bin: ~w", [Bin1]),
                                            self() ! force_stop,
                                            ok;
                                        false ->
                                            self() ! {tcp, Socket, Bin1}
                                    end;
                                false -> skip
                            end,
                            Rs1 = Rs#rs{tmp_package = <<>>},
                            Rs2 = robot_handle:handle(Cmd, Data, Rs1),
                            case get(cmd) of
                                undefined -> ok;
                                Cmd ->
                                    erase(cmd),
                                    io:format("~n[Recv] ~w:~w~n", [Cmd, Data]),
                                    ok;
                                _ -> 
                                    io:format("~n[Recv] ~w:~w~n", [Cmd, Data]),
                                    ok
                            end,
                            case is_record(Rs2, rs) of
                                true -> recv_process(Rs2);
                                false -> recv_process(Rs1)
                            end;
                        {error, _Reason} ->
                            ?WARN("Unpack Error! [Cmd:~w, Reason:~w, Bin:~w]", 
                                [Cmd, _Reason, Bin]),
                            robot ! {logout, Rs#rs.acc_id},
                            Rs#rs.pid_sender ! stop,
                            ok
                    end;
                TmpPackage ->
                    Rs1 = Rs#rs{tmp_package = TmpPackage},
                    recv_process(Rs1)
            end;
        {tcp_closed, Socket} ->
            robot ! {logout, Rs#rs.acc_id},
            Rs#rs.pid_sender ! stop,
            ok;
        {tcp_error, Socket, Reason} ->
            robot ! {logout, Rs#rs.acc_id},
            Rs#rs.pid_sender ! stop,
            ?INFO("tcp_error:~w", [Reason]);
        {info, Info} ->
            ?INFO("Info:~w", [Info]);
        test ->
            ?INFO("*** TEST ***", []),
            ok;
        {walk, Data} ->
            Rs1 = robot_lib:walk(Data, Rs),
            recv_process(Rs1);
        {create_room, Trigger, Type, Num, AvgLev, MustLost, ToRoomId} ->
            %% 创建房间
            case Rs#rs.is_battle of
                1 ->
                    ?INFO("In battle, create_room failure, AccId:~w", [Rs#rs.acc_id]),
                    %% robot ! {create_team, Trigger, Type, Num, AvgLev},
                    recv_process(Rs);
                0 ->
                    %% ?INFO("{create_team, Id:~w, Trigger:~w, AvgLev:~w}", [Rs#rs.id, Trigger, AvgLev]),
                    Rs#rs.pid_sender ! {cmd, 13005, [Type]},
                    Rs1 = Rs#rs{
                        action = {create_room, Type, Num}
                        ,type = Type
                        ,trigger = Trigger
                        ,avg_lev = AvgLev
                        ,to_room_id = ToRoomId
                        ,must_lost = MustLost
                    },
                    recv_process(Rs1)
            end;
        {join_room, Trigger, Type, RoomId, AvgLev} ->
            case Rs#rs.is_battle of
                1 ->
                    ?INFO("In battle, join_room failure, AccId:~w", [Rs#rs.acc_id]),
                    %% robot ! {join_room, Trigger, Type, RoomId, AvgLev},
                    recv_process(Rs);
                0 ->
                    %% ?INFO("{join_room, Id:~w, Trigger:~w, AvgLev:~w}", [Rs#rs.id, Trigger, AvgLev]),
                    %% ?INFO("join_room, Type:~w, RoomId:~w", [Type, RoomId]),
                    Rs#rs.pid_sender ! {cmd, 13003, [Type, RoomId]},
                    Rs1 = Rs#rs{
                        action = {join_room, RoomId, Type}
                        ,type = Type
                        ,trigger = Trigger
                        ,avg_lev = AvgLev
                    },
                    recv_process(Rs1)
            end;
        {use_daoju, Id} ->
            robot_lib:use_daoju(Id, Rs),
            recv_process(Rs);
        stop ->
            %% ?INFO("stop [Rid:~w, battle:~w]", [Rs#rs.acc_id, Rs#rs.is_battle]),
            case Rs#rs.is_battle of
                1 ->
                    StopRef = erlang:send_after(60 * 1000, self(), stop),
                    put(stop_ref, StopRef),
                    ?INFO("robot in_battle when ready stop! [Rid:~w]", [Rs#rs.acc_id]),
                    recv_process(Rs);
                0 ->
                    Rs#rs.pid_sender ! stop,
                    robot ! {logout, Rs#rs.acc_id},
                    gen_tcp:close(Socket),
                    ok
            end;
        force_stop ->
            Rs#rs.pid_sender ! stop,
            robot ! {logout, Rs#rs.acc_id},
            gen_tcp:close(Socket),
            ?INFO("force_stop! [Rid:~w]", [Rs#rs.acc_id]);
        {cmd, Cmd, Data} ->
            put(cmd, Cmd),
            io:format("~n[Send] ~w:~w~n", [Cmd, Data]),
            Rs#rs.pid_sender ! {cmd, Cmd, Data},
            recv_process(Rs);
        Other ->
            Rs#rs.pid_sender ! stop,
            robot ! {logout, Rs#rs.acc_id},
            gen_tcp:close(Socket),
            ?INFO("undefined msg: ~w", [Other])
    end.
%%.

%%' Socket数据发包器
sender(Socket, Index) ->
    receive
        {cmd, Cmd, Data} ->
            %% case lists:member(Cmd, [11003]) of
            %%     true -> ok;
            %%     false -> ?INFO("Send! [Cmd:~w, Data:~w]", [Cmd, Data])
            %% end,
            {ok, Bin} = robot_pack:p(Cmd, Data, Index),
            case gen_tcp:send(Socket, Bin) of
                ok -> 
                    Index1 = case Index >= 255 of
                        false -> Index + 1;
                        true -> 0
                    end,
                    sender(Socket, Index1);
                {error,closed} -> 
                    ok;
                Error -> io:format("Error when send data: ~w~n", [Error])
            end;
        stop -> ok;
        Other ->
            io:format("Error packet:~w~n", [Other]),
            sender(Socket, Index)
    end.
%%. =============================================================

get_robot(0, State) ->
    #state{at_hall = AtHall , at_game = AtGame} = State,
    case util:rand_element(AtHall) of
        undefined -> {error, 0};
        {AccId, Id, Pid, Lev} ->
            case is_process_alive(Pid) of
                true ->
                    State1 = State#state{
                        at_game = [{AccId, Id, Pid, Lev} | AtGame]
                        ,at_hall = lists:keydelete(AccId, 1, AtHall)
                    },
                    {ok, Pid, State1};
                false ->
                    State1 = State#state{
                        at_hall = lists:keydelete(AccId, 1, AtHall)
                    },
                    ?INFO("** 2 ** ~w", [AccId]),
                    get_robot(0, State1)
            end
    end;

get_robot(AvgLev, State) ->
    #state{at_hall = AtHall , at_game = AtGame} = State,
    case get_robot1(AvgLev, AvgLev, AtHall, AtHall) of
        undefined -> {error, 0};
        {AccId, Id, Pid, Lev} ->
            case is_process_alive(Pid) of
                true ->
                    State1 = State#state{
                        at_game = [{AccId, Id, Pid, Lev} | AtGame]
                        ,at_hall = lists:keydelete(AccId, 1, AtHall)
                    },
                    {ok, Pid, State1};
                false ->
                    State1 = State#state{
                        at_hall = lists:keydelete(AccId, 1, AtHall)
                    },
                    ?INFO("** 1 ** ~w", [AccId]),
                    get_robot(AvgLev, State1)
            end
    end.

%% get_robot1(Lev, AtHall) ->
%%     case lists:keyfind(Lev, 4, AtHall) of
%%         false -> get_robot1(Lev-1, Lev+1, AtHall, AtHall);
%%         Data -> Data
%%     end.

get_robot1(LevMin, LevMax, [{AccId, Id, Pid, Lev} | T], AtHall) ->
    %% ?INFO("LevMin:~w, LevMax:~w", [LevMin, LevMax]),
    case Lev >= LevMin andalso Lev =< LevMax of
        true -> {AccId, Id, Pid, Lev};
        false -> get_robot1(LevMin, LevMax, T, AtHall)
    end;
get_robot1(LevMin, LevMax, [], AtHall) when LevMax - LevMin =< 40 ->
    %% ?INFO("NOTICE! LevMin:~w, LevMax:~w", [LevMin, LevMax]),
    get_robot1(LevMin-1, LevMax+1, AtHall, AtHall);
get_robot1(_LevMin, _LevMax, [], []) ->
    undefined;
get_robot1(_LevMin, _LevMax, [], AtHall) ->
    util:rand_element(AtHall).

set_robot(at_hall, AccId, State) ->
    #state{at_hall = AtHall , at_game = AtGame} = State,
    case lists:keyfind(AccId, 1, AtGame) of
        false -> {error, undefined};
        Id ->
            State1 = State#state{
                at_hall = AtHall ++ [Id]
                ,at_game = lists:keydelete(AccId, 1, AtGame)
            },
            {ok, State1}
    end;

set_robot(at_game, AccId, State) ->
    #state{at_hall = AtHall , at_game = AtGame} = State,
    case lists:keyfind(AccId, 1, AtHall) of
        false -> {error, undefined};
        Id ->
            State1 = State#state{
                at_game = [Id | AtGame]
                ,at_hall = lists:keydelete(AccId, 1, AtHall)
            },
            {ok, State1}
    end.

login_robots([AccId | T1], Ip, Port, Online) ->
    case lists:keymember(AccId, 1, Online) of
        false ->
            Rs = #rs{type = 0, trigger = robot, 
                ip = Ip, port = Port, acc_id = AccId, avg_lev = 0},
            login(Rs);
        true -> ok
    end,
    util:sleep(20),
    login_robots(T1, Ip, Port, Online);
login_robots(_, _, _, _) -> ok.

login_trigger(State) ->
    case length(State#state.at_hall) < 6 of
        true -> self() ! login_robots;
        false -> ok
    end.

%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
