%%----------------------------------------------------
%% 角色主进程
%%
%% @author rolong@vip.qq.com
%%----------------------------------------------------
-module(srv_role).
-behaviour(gen_server).
-export([
        create/1
        ,create/3
        ,fix_conn/6
        ,routing/3
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("offline.hrl").

-define(T_SLOW_CALL, 300).

%% --- 对外接口 ---------------------------------

%% 创建一个角色进程
%% -> {ok,Pid} | ignore | {error,Error}
create(Socket) ->
    gen_server:start(?MODULE, [Socket], []).

create(offline, KeyType, Key) ->
    gen_server:start(?MODULE, [offline, KeyType, Key], []).

%% 修复连接进程(处理掉线重连或重复登录的问题)
fix_conn(RolePid, ConnPid, PidSerder, Socket, Ip1, Port) ->
    %% 先通知接管者暂停接收数据
    srv_conn:switch_role_pid(ConnPid, RolePid), 
    Ip = util:ip_to_binary(Ip1),
    gen_server:call(RolePid, {fix_conn, ConnPid, PidSerder, Socket, Ip, Port}).

%% --- 服务器内部实现 ---------------------------------

init([Socket]) ->
    %% io:format("~w", [self()]),
    process_flag(trap_exit, true),
    self() ! gc, 
    case inet:peername(Socket) of
        {ok, {Ip1, Port}} ->
            Ip = util:ip_to_binary(Ip1),
            Rs = #role{pid = self(), socket = Socket, ip = Ip, port = Port},
            {ok, Rs};
        {error, Reason} ->
            R = inet:format_error(Reason),
            {stop, R}
    end;

init([offline, KeyType, Key]) ->
    %% ?INFO("create offline role! [~w:~w] ~w", [KeyType, Key, self()]),
    %% io:format("&"),
    process_flag(trap_exit, true),
    self() ! gc, 
    Pid = self(),
    Rs = #role{pid = self()},
    case lib_authen:init_role_data(KeyType, Key, Rs) of
        {ok, Rs1} ->
            %% save to offline table
            ets:insert(offline, #offline{
                    id              = Rs1#role.id
                    ,account_id     = Rs1#role.account_id
                    ,pid            = Pid
                    ,name           = Rs1#role.name
                }
            ),
            reset_stop_timer(?OFFLINE_CACHE_TIME),
            {ok, Rs1};
        false -> {stop, normal}
    end.

%% 每隔300秒执行一次hibernate
handle_info(gc, Rs) ->
    erlang:send_after(300000, self(), gc),
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], Rs]);

%% 转发Socket数据
handle_info({socket_data, Bin}, Rs) ->
    Rs#role.pid_sender ! {data, Bin},
    {noreply, Rs};

%% 任务处理
handle_info({task, CType}, Rs) ->
    Arg = case CType of
        ?TASK_UP_LEV -> {to, Rs#role.lev};
        _ -> {add, 1}
    end,
    task_process(Rs#role.pid_sender, CType, Arg),
    {noreply, Rs};

%% 任务处理
handle_info({task, CType, Arg}, Rs) ->
    task_process(Rs#role.pid_sender, CType, Arg),
    {noreply, Rs};

%% 设置工会信息
%% handle_info({set_guild, Gid, Name}, Rs) ->
%%     GuildV = case Gid of
%%         0 -> 0;
%%         _ -> Rs#role.guild_v
%%     end,
%%     {noreply, Rs#role{guild_id = Gid, guild_name = Name, guild_v = GuildV}};

handle_info({add_item, Tid, Num}, Rs) ->
    MyItems = lib_item:get_myitems(),
    case lib_item:add_item(Tid, Num, MyItems) of
        {ok, MyItems1, My} ->
            lib_item:add_item_notice(My, Rs#role.pid_sender),
            lib_item:put_myitems(MyItems1),
            {noreply, Rs};
        {error, Reason} -> 
            ?INFO("error when add_item: ~w, Rid:~w", [Reason, Rs#role.id]),
            {noreply, Rs}
    end;

handle_info({send_attr, From}, Rs) ->
    case From of
        {11022, PidSender} ->
            MyItems = lib_item:get_myitems(),
            lib_sender:send(11022, PidSender, Rs, MyItems);
        {11006, PidSender} ->
            lib_sender:send(11006, PidSender, Rs);
        self -> 
            lib_sender:send(11006, Rs);
        _ when is_list(From) -> 
            Attr = lib_sender:to_role_info(Rs),
            lib_conn:pack_cast(From, 11006, [Attr]);
        _ -> 
            ?WARNING("undefined send_attr: ~w", [From])
    end,
    {noreply, Rs};

handle_info({set_game_senders, Senders}, Rs) ->
    {noreply, Rs#role{game_senders = Senders}};

%% 接收sender的进程ID
handle_info({set_pid_sender, PidSender}, Rs) ->
    %% 在系统繁忙时发包器到这里时有可能已经挂掉，所以需要判断下
    case is_pid(PidSender) andalso is_process_alive(PidSender) of
        false -> 
            ?LOG({logout, Rs#role.id, 4}),
            ?WARNING("*** WARNING *** when set_pid_sender", []),
            self() ! stop,
            {noreply, Rs};
        true ->
            {noreply, Rs#role{pid_sender = PidSender}}
    end;

%% 修改多个属性
%% Attr = [{Key, Val}, ...]
handle_info({add_attr, Attr}, Rs) ->
    NewRs = lib_role:add_attr(Rs, Attr),
    {noreply, NewRs};

%% 修改单个属性
handle_info({add_attr, Key, Val}, Rs) ->
    NewRs = lib_role:add_attr(Rs, Key, Val),
    {noreply, NewRs};

handle_info({add_attr, Key, Val, LogType}, Rs) ->
    NewRs = lib_role:add_attr(Rs, Key, Val, LogType),
    {noreply, NewRs};

%% 设置多个属性
%% Attr = [{Key, Val}, ...]
handle_info({set_attr, Attr}, Rs) ->
    %% ?INFO("set_attr(~w):~w", [Rs#role.id, Attr]),
    NewRs = lib_role:set_attr(Rs, Attr),
    state_save(NewRs),
    {noreply, NewRs};

%% 设置单个属性
handle_info({set_attr, Key, Val}, Rs) ->
    %% ?INFO("set_attr(~w):~w", [Rs#role.id, {Key, Val}]),
    NewRs = lib_role:set_attr(Rs, Key, Val),
    state_save(NewRs),
    {noreply, NewRs};

handle_info({task_tail, TaskId}, Rs) ->
    TaskTail = case lists:member(TaskId, Rs#role.task_tail) of
        true -> Rs#role.task_tail;
        false -> [TaskId | Rs#role.task_tail]
    end,
    {noreply, Rs#role{task_tail = TaskTail}};

handle_info({enable_dtask, TaskId}, Rs) ->
    EnableDTask = case lists:member(TaskId, Rs#role.enable_dtask) of
        true -> Rs#role.enable_dtask;
        false -> [TaskId | Rs#role.enable_dtask]
    end,
    {noreply, Rs#role{enable_dtask = EnableDTask}};

%% 初始化与客户端的连接进程
handle_info({event, start_client}, Rs) ->
    {ok, PidConn} = srv_conn:start_link(self(), Rs#role.socket),
    case gen_tcp:controlling_process(Rs#role.socket, PidConn) of
        ok -> 
            PidConn ! loop,
            ok;
        Reason -> 
            ?ERR("error when controlling_process:~w, self():~w, PidConn:~w", [Reason, self(), PidConn]),
            self() ! disconnect
    end,
    {noreply, Rs#role{pid_conn = PidConn}};

%% Client disconnect, ready logout.
handle_info(disconnect, #role{status = 0} = Rs) ->
    ?INFO("stop when status = 0 !"),
    {stop, normal, Rs};

handle_info(disconnect, #role{status = Status} = Rs) ->
    case Status of
        2 -> 
            %% In room, exit ther room immediately.
            gen_server:cast(Rs#role.pid_room, {role_exit, Rs#role.id});
        3 -> 
            case Rs#role.room_type > 100 of
                true ->
                    %% 副本中不重连
                    gen_server:cast(Rs#role.pid_room, {role_exit, Rs#role.id});
                false -> ok
            end;
        _ ->
            ok
    end,
    disconnect(Rs),
    %% ?INFO("~s(~w) disconnect!", [Rs#role.name, Rs#role.id]),
    %% ?INFO("~w disconnect!", [Rs#role.id]),
    %% ?LOG({logout, Rs#role.id, 1}),
    reset_stop_timer(?OFFLINE_CACHE_TIME),
    lib_rank:set_myrank(Rs),
    {noreply, Rs};

handle_info(stop, Rs) ->
    case Rs#role.status of
        0 -> {stop, normal, Rs};
        3 -> 
            %% In game, exit ther game immediately.
            gen_server:cast(Rs#role.pid_room, {role_exit, Rs#role.id}),
            case logout(Rs) of
                {true, _} -> {stop, normal, Rs};
                {false, _} -> 
                    Time = util:rand(5 * 60000, 10 * 60000),
                    reset_stop_timer(Time),
                    {noreply, Rs}
            end;
        _S -> 
            %% ?INFO("S~w ~w", [_S, Rs#role.id]),
            case logout(Rs) of
                {true, _} -> {stop, normal, Rs};
                {false, _} -> 
                    Time = util:rand(5 * 60000, 10 * 60000),
                    reset_stop_timer(Time),
                    {noreply, Rs}
            end
    end;

%% handle_info({refresh_data, Type}, Rs) ->
%%     lib_conn:pack_send(Rs#role.pid_sender, 11020, [Type]),
%%     {noreply, Rs};

handle_info({'EXIT', Pid, _Why}, Rs) ->
    if
        Pid == Rs#role.pid_conn ->
            %% Client disconnect
            %% ?INFO("stop pid_conn: ~w, slef():~w", [Pid, self()]),
            self() ! disconnect;
        Pid == Rs#role.pid_sender ->
            ?INFO("stop pid_sender: ~w, slef():~w", [Pid, self()]),
            self() ! disconnect;
        true ->
            ?INFO("stop pid: ~w, slef():~w", [Pid, self()]),
            ok
    end,
    {noreply, Rs};

%% 处理handle事件，仅限于内部调用
handle_info({handle_event, Cmd, Data}, Rs) ->
    case routing(Cmd, Data, Rs) of
        {noreply, Rs1} when is_record(Rs1, role) -> 
            {noreply, Rs1};
        {noreply, Rs1} -> 
            ?WARNING("Not record role: ~w", [Rs1]),
            {noreply, Rs};
        {Reply, _Rs1} -> 
            ?WARNING("Ignore reply: ~w", [Reply]),
            {noreply, Rs}
    end;

handle_info(shutdown, Rs) ->
    lib_rank:set_myrank(Rs),
    is_port(Rs#role.socket) andalso lib_conn:send_error(Rs#role.socket, 100),
    %% 如果在游戏中则通知退出
    is_pid(Rs#role.pid_room) andalso gen_server:cast(Rs#role.pid_room, {role_exit, Rs#role.id}),
    PidConn = Rs#role.pid_conn,
    case is_pid(PidConn) andalso is_process_alive(PidConn) of
        true -> Rs#role.pid_conn ! shutdown;
        false -> ok
    end,
    erlang:send_after(800, self(), {shutdown, ?MODULE}),
    {noreply, Rs};

handle_info({reset_stop_timer, Time}, Rs) ->
    reset_stop_timer(Time),
    {noreply, Rs};

handle_info({shutdown, _From}, Rs) ->
    PidConn = Rs#role.pid_conn,
    case is_pid(PidConn) andalso is_process_alive(PidConn) of
        true -> erlang:exit(PidConn, kill);
        false -> ok
    end,
    case Rs#role.status > 0 of
        true -> logout(Rs);
        false -> ok
    end,
    %% io:format("-"),
    {stop, normal, Rs};

handle_info(test, Rs) ->
    A = lists:foldl( fun(P, Acc0) -> [{P, erlang:process_info(P, registered_name), erlang:process_info(P, memory), erlang:process_info(P, message_queue_len), erlang:process_info(P, current_function), erlang:process_info(P, initial_call)} | Acc0] end, [], [self()]),
    io:format("sender:~n~p", [A]),
    erlang:send_after(5000, self(), test),
    {noreply, Rs};

handle_info(_Info, Rs) ->
    ?WARNING("Not matched info: ~w", [_Info]),
    {noreply, Rs}.

%% 进入游戏
handle_call({copy_game_attr, Type, PidRoom, MapId}, _From, Rs) ->
    MyItems = lib_item:get_myitems(),
    WeaponId = lib_item:get_weapon_id(MyItems),
    #role{pid_conn = PidConn} = Rs,
    Rs1 = lib_role:add_attr(Rs, hp, Rs#role.hp_max),
    Attr = [
         Rs1#role.lev
        ,Rs1#role.sex
        ,Rs1#role.hp_max
        ,Rs1#role.dmg
        ,Rs1#role.dmg_speed
        ,Rs1#role.move_speed
        ,Rs1#role.crit
        ,WeaponId
        ,lib_item:get_mydaojus()
        ,Rs1#role.attack
        ,Rs1#role.pos102
        ,Rs1#role.pos104
        ,Rs1#role.win_rate
        ,lib_skill:get_skilled(Rs)
        ,Rs1#role.guild_id
        ,Rs1#role.fb_attack
    ],
    Rs2 = Rs1#role{
        status = 3 
        ,room_type = Type 
        ,pid_room = PidRoom 
        ,map_id = MapId 
        ,game_senders = [] 
    },
    state_save(Rs2),
    %% 进入游戏后设置为异步处理Socket Data
    PidConn ! {set_process_mode, async},
    {reply, Attr, Rs2};

%% 修复连接进程
handle_call({fix_conn, ConnPid, PidSerder, Socket, Ip, Port}, _From, Rs) ->
    #role{status = Status, pid_room = PidRoom} = Rs,
    %% 如果有退出定时器则清除它
    erase_stop_timer(),
    case is_pid(Rs#role.pid_conn) andalso is_process_alive(Rs#role.pid_conn) of
        false -> ok;
        true ->
            %% 通知原角色已在别处登陆
            lib_conn:send_error(Rs#role.socket, 11000101),
            %% 如果当前角色正在被别的客户端控制，则将其断开
            erlang:unlink(Rs#role.pid_conn),
            erlang:exit(Rs#role.pid_conn, kill),
            ok
    end,
    %% 开始修复连接进程
    erlang:link(ConnPid),
    case Status of
        2 ->
            %% 如果在房间中，通知退出
            gen_server:cast(PidRoom, {role_exit, Rs#role.id});
        3 -> 
            gen_server:cast(PidRoom, {set_pid_sender, Rs#role.id, PidSerder});
        _ ->
            ok
    end,
    Status1 = case Rs#role.status of
        0 -> 1;
        S -> S
    end,
    NewRs = Rs#role{ 
        pid_conn = ConnPid
        ,pid_sender = PidSerder
        ,socket = Socket
        ,ip = Ip
        ,port = Port 
        ,status = Status1
    },
    state_save(NewRs),
    srv_conn:resume_after(ConnPid, 0), %% 稍后通知接管者继续接收数据
    {reply, NewRs, NewRs};

%% 处理Socket事件
handle_call({handle_event, Cmd, Data}, _From, Rs) ->
    case routing_check(Cmd, Rs) of
        true ->
            case routing(Cmd, Data, Rs) of
                {Reply, Rs1} when is_record(Rs1, role) ->
                    {reply, Reply, Rs1};
                {Reply, Rs1} ->
                    ?WARNING("Not record role: ~w", [Rs1]),
                    {reply, Reply, Rs}
            end;
        false ->
            ?INFO("routing_check error[Cmd: ~w Data:~w]", [Cmd, Data]),
            erlang:exit(Rs#role.pid_conn, kill),
            {reply, noreply, Rs}
    end;

%% 取得当前角色的完整属性
handle_call(get_state, _From, Rs) ->
    {reply, Rs, Rs};

%% 踢人下线
handle_call({force_logout, _Reason}, _From, Rs) ->
    ?INFO("force_logout:~s(~w)", [Rs#role.name, Rs#role.id]),
    case is_pid(Rs#role.pid_room) andalso is_process_alive(Rs#role.pid_room) of
        false -> ok;
        true -> gen_server:cast(Rs#role.pid_room, {role_exit, Rs#role.id})
    end,
    ?LOG({logout, Rs#role.id, 3}),
    is_port(Rs#role.socket) andalso gen_tcp:close(Rs#role.socket),
    is_pid(Rs#role.pid_conn) andalso erlang:exit(Rs#role.pid_conn, kill),
    self() ! stop,
    case Rs#role.guild_id of
        0 -> ok;
        Gid -> 
            gen_server:cast(srv_guild, {role_logout, 
                    Gid, Rs#role.id, Rs#role.lev})
    end,
    {reply, ok, Rs};

handle_call(_Request, _From, Rs) ->
    {noreply, Rs}.

%% 处理handle事件
handle_cast({handle_event, Cmd, Data}, Rs) ->
    T = erlang:now(),
    case routing_check(Cmd, Rs) of
        true ->
            case routing(Cmd, Data, Rs) of
                {noreply, Rs1} when is_record(Rs1, role) -> 
                    test(T, Cmd),
                    {noreply, Rs1};
                {Reply, Rs1} when is_record(Rs1, role) -> 
                    {ok, Bin} = pack:p(Cmd, Reply),
                    Rs#role.pid_sender ! {data, Bin},
                    test(T, Cmd),
                    {noreply, Rs1};
                {_Reply, Rs1} ->
                    ?WARNING("Not record role: ~w", [Rs1]),
                    {noreply, Rs}
            end;
        false ->
            ?INFO("routing_check failure! [Cmd: ~w Data:~w]", [Cmd, Data]),
            erlang:exit(Rs#role.pid_conn, kill),
            
            {noreply, Rs}
    end;

handle_cast(_Msg, Rs) ->
    ?WARNING("Not matched message: ~w", [_Msg]),
    {noreply, Rs}.

%% 进程结束时的处理
terminate(_Reason, Rs) ->
    ets:delete(online, Rs#role.id),
    ets:delete(offline, Rs#role.account_id),
    %% ?INFO("terminate: srv_role(~w)", [self()]),
    %% eprof:stop_profiling(),
    %% eprof:analyze(),
    ok.

code_change(_OldVsn, Rs, _Extra) ->
    {ok, Rs}.

%% --- 私有函数 ------------------------------

%% 同步更新ETS中的角色数据
state_save(Rs) ->
    case Rs#role.id > 0 of
        false -> ignore; %% 没有登录角色的不写入ETS表中
        true ->
            RoleOnline = case ets:lookup(online, Rs#role.id) of 
                [RO] -> RO;
                _ -> #online{} 
            end,
            ets:insert(online, RoleOnline#online{
                    id              = Rs#role.id
                    ,account_id     = Rs#role.account_id
                    ,pid            = Rs#role.pid
                    ,pid_sender     = Rs#role.pid_sender
                    ,pid_room       = Rs#role.pid_room
                    ,name           = Rs#role.name
                    ,room_id        = Rs#role.room_id
                    ,status         = Rs#role.status
                    ,lev            = Rs#role.lev
                    ,sex            = Rs#role.sex
                    ,growth         = Rs#role.growth
                    ,guild_id       = Rs#role.guild_id
                }
            )
    end.

get_sql([{myfriends, L} | T], Sql, Val) ->
    Val1 = util:term_to_string(L),
    case byte_size(Val1) > 0 of
        true ->
            Val2 = [Val1 | Val],
            Sql1 = ["`myfriends` = ~s" | Sql],
            get_sql(T, Sql1, Val2);
        false ->
            get_sql(T, Sql, Val)
    end;
get_sql([{mytasks, L} | T], Sql, Val) ->
    Val1 = util:term_to_string(lib_task:zip(L)),
    case byte_size(Val1) > 0 of
        true ->
            Val2 = [Val1 | Val],
            Sql1 = ["`mytasks` = ~s" | Sql],
            get_sql(T, Sql1, Val2);
        false ->
            get_sql(T, Sql, Val)
    end;
get_sql([{myitems, L} | T], Sql, Val) ->
    Val1 = util:term_to_string(lib_item:zip(L)),
    case byte_size(Val1) > 0 of
        true ->
            Val2 = [Val1 | Val],
            Sql1 = ["`myitems` = ~s" | Sql],
            get_sql(T, Sql1, Val2);
        false ->
            get_sql(T, Sql, Val)
    end;
get_sql([{kvs, L} | T], Sql, Val) ->
    Val1 = util:term_to_string(L),
    case byte_size(Val1) > 0 of
        true ->
            Val2 = [Val1 | Val],
            Sql1 = ["`kvs` = ~s" | Sql],
            get_sql(T, Sql1, Val2);
        false ->
            get_sql(T, Sql, Val)
    end;
get_sql([H | T], Sql, Val) ->
    ?WARNING("undefined save type:~w", [H]),
    get_sql(T, Sql, Val);
get_sql([], Sql, Val) -> [Sql, Val].

%% -> {Status, Rs}
save_to_db(Rs) ->
    T = erlang:now(),
    [Sql, Val] = get_sql(Rs#role.save, [], []),
    case length(Sql) > 0 of
        true ->
            Sql1 = string:join(Sql, ","),
            Sql2 = lists:concat(["UPDATE `role` SET ", Sql1, " WHERE `id` = ~s"]),
            Val1 = Val ++ [Rs#role.id],
            %% ?INFO("save_to_db:\n Sql:~s \nVal:~p", [Sql2, Val1]),
            try 
                Sta = case db:execute(Sql2, Val1) of
                    {error, _} -> false;
                    _ -> true
                end,
                DT = timer:now_diff(erlang:now(), T) / 1000,
                %% ?INFO("save_to_db Rid:~w, T:~w", [Rs#role.id, DT]),
                case DT > 100 of
                    true -> ?INFO("save_to_db Rid:~w, T:~w", [Rs#role.id, DT]);
                    false -> ok
                end,
                {Sta, Rs#role{save = []}}
            catch 
                _T:_X -> 
                    %% ?INFO("ERROR!~w:~w", [_T, _X])
                    ?WARNING("Timeout! Sql:~s, Val:~w", [Sql2, Val1]),
                    {false, Rs}
            end;
        false -> {false, Rs}
    end.

task_process(PidSender, CType, Arg) ->
    MyTasks = lib_task:get_mytasks(),
    case lib_task:process(CType, Arg, MyTasks) of
        {ok} -> skip;
        {ok, NewMyTasks} ->
            lib_task:put_mytasks(NewMyTasks),
            lib_conn:pack_send(PidSender, 11020, [3])
    end.

%%' disconnect
disconnect(Rs) ->
    #role{
        id = Rid
        ,account_id = AccountId
        ,pid = Pid
        ,name = Name
    } = Rs,
    %% save to offline table
    ets:insert(offline, #offline{
            id              = Rid
            ,account_id     = AccountId
            ,pid            = Pid
            ,name           = Name
        }
    ),
    %% delete online data
    ets:delete(online, Rs#role.id),
    %% do guild
    case Rs#role.guild_id of
        0 -> ok;
        Gid -> 
            gen_server:cast(srv_guild, {role_logout, 
                    Gid, Rs#role.id, Rs#role.lev})
    end,
    ok.
%%.

%%' logout
logout(Rs) ->
    OnlineTime = lib_role:get_online_time(Rs),
    OnlineReward = case Rs#role.online_reward of
        {0, 0, 0} -> {0, 0, 0};
        {Nth, StartTime, SumTime} -> 
            StartTime1 = case StartTime > 0 of
                true -> StartTime;
                false -> Rs#role.login_time
            end,
            SumTime1 = SumTime + (util:unixtime() - StartTime1),
            {Nth, 0, SumTime1}
    end,
    Rs1 = Rs#role{online_time = OnlineTime, login_time = 0, online_reward = OnlineReward},
    Save = [
        {kvs, lib_role:get_kvs(Rs1)}
        ,{myitems, lib_item:get_myitems()}
        ,{mytasks, lib_task:get_mytasks()}
        ,{myfriends, lib_sns:get_myfriends()}
    ],
    save_to_db(Rs1#role{save = Save}).
%%.

test(T, Cmd) ->
    DT = timer:now_diff(erlang:now(), T) / 1000,
    case DT > ?T_SLOW_CALL of
        false -> ok;
        true ->
            ?INFO("slow handle [Cmd:~w, T:~w]", [Cmd, DT]),
            ok
    end.

set_stop_timer(Time) ->
    erase_stop_timer(),
    Ref = erlang:send_after(Time, self(), stop),
    put('$stop_timer', Ref).

erase_stop_timer() ->
    case get('$stop_timer') of
        undefined -> ok;
        TimerRef ->
            erlang:cancel_timer(TimerRef),
            erase('$stop_timer')
    end.

reset_stop_timer(Time) ->
    set_stop_timer(Time).

%% 路由前检查 
routing_check(Cmd, Rs) ->
    case lists:member(Cmd, [10003, 10004, 10005]) of
        true -> true;
        false -> Rs#role.id > 0
    end.

%% 路由(为方便查找，本函数放在文件最底部) 
routing(Cmd, Data, Rs) ->
    [H1, H2 | _] = integer_to_list(Cmd),
    try
        Return = case [H1, H2] of
            "10" -> mod_authen:handle(Cmd, Data, Rs);
            "11" -> mod_role:handle(Cmd, Data, Rs);
            "12" -> mod_npc:handle(Cmd, Data, Rs);
            "13" -> mod_hall:handle(Cmd, Data, Rs);
            "14" -> mod_room:handle(Cmd, Data, Rs);
            "16" -> mod_map:handle(Cmd, Data, Rs);
            "17" -> mod_item:handle(Cmd, Data, Rs);
            "18" -> mod_sns:handle(Cmd, Data, Rs);
            "19" -> mod_task:handle(Cmd, Data, Rs);
            "20" -> mod_shop:handle(Cmd, Data, Rs);
            "21" -> mod_rank:handle(Cmd, Data, Rs);
            "22" -> mod_guild:handle(Cmd, Data, Rs);
            "23" -> mod_hero:handle(Cmd, Data, Rs);
            "60" -> mod_event:handle(Cmd, Data, Rs);
            "61" -> mod_game:handle(Cmd, Data, Rs);
            _ -> {error, routing_failure}
        end,
        case Return of
            {ok} -> {noreply, Rs};
            {ok, Reply} when is_list(Reply) -> {Reply, Rs};
            {ok, NewRs} ->
                state_save(NewRs),
                NewRs1 = case NewRs#role.save of
                    [] -> NewRs;
                    _ -> 
                        {_, RR} = save_to_db(NewRs),
                        RR
                end,
                {noreply, NewRs1};
            {ok, Reply, NewRs} ->
                state_save(NewRs),
                NewRs1 = case NewRs#role.save of
                    [] -> NewRs;
                    _ -> 
                        {_, RR} = save_to_db(NewRs),
                        RR
                end,
                {Reply, NewRs1};
            {error, Reason} ->
                erlang:exit(Rs#role.pid_conn, kill),
                ?WARNING("Cmd:~w, Data:~w, Reason:~w", [Cmd, Data, Reason]),
                {noreply, Rs};
            Else ->
                erlang:exit(Rs#role.pid_conn, kill),
                ?WARNING("Cmd:~w, unexpected data:~w", [Cmd, Else]),
                {noreply, Rs}
        end
    catch
        T:X ->
            Arg = [Rs#role.id, Cmd, Data, T, X, erlang:get_stacktrace()],
            ?ERR("Handle Error! [Rid:~w, CMD=~w, DATA=~p, ~w:~w] ~n~p", Arg),
            {error, routing_failure}
    end.
