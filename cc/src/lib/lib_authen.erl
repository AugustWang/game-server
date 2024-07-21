%%----------------------------------------------------
%% 登陆认证
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_authen).
-export([
        login/2
        ,login/3
        ,init_role_data/3
        ,get_role_pid/2
        ,get_online_pid/2
        ,get_offline_pid/2
        ,get_pid_from_ets/2
    ]
).

-include("common.hrl").
-include("offline.hrl").

get_role_pid(role_id, 0) -> false;
get_role_pid(KeyType, Key) ->
    case get_pid_from_ets(KeyType, Key) of 
        false -> 
            case KeyType of 
                name -> 
                    RoleId = lib_role:get_rid(Key),
                    get_role_pid(role_id, RoleId);
                _ ->
                    case srv_role:create(offline, KeyType, Key) of
                        {ok, Pid} -> Pid;
                        _Else ->
                            ?WARNING("error:~w", [_Else]),
                            false
                    end
            end;
        {_, Pid} -> Pid
    end.

login(AccountId, Rs) ->
    login(AccountId, Rs, 0).

login(AccountId, Rs, First) ->
    case get_pid_from_ets(account_id, AccountId) of
        false ->
            case init_role_data(account_id, AccountId, Rs) of 
                {ok, Rs1} -> 
                    Rs2 = Rs1#role{status = 1},
                    #role{
                        id = Rid 
                        ,name = Name 
                        ,ip = Ip 
                        ,growth = Growth 
                        ,upgrade_show = UpgradeShow
                    } = Rs2,
                    do_first(First, Rid, Name),
                    lib_sender:send(11006, Rs2),
                    ?LOG({login, Rid, Ip}),
                    Growth1 = init_growth(Growth),
                    gen_server:cast(srv_rank, {set_myrank, Rid, self()}),
                    %% ?INFO("~s(~w) login from db", [Name, Rid]),
                    %% ?INFO("~w login from db", [Rid]),
                    {ok, [1, Growth1, Rid, UpgradeShow], Rs2};
                false ->
                    {ok, [1, 0, 0, 0], Rs#role{account_id = AccountId}}
            end;
        {From, Pid} ->
            #role{
                pid_conn = PidConn
                ,pid_sender = PidSerder
                ,socket = Socket
                ,ip = Ip
                ,port = Port
            } = Rs,
            case catch srv_role:fix_conn(Pid, PidConn, PidSerder, Socket, Ip, Port) of
                {'EXIT', Error} ->
                    ?WARN("Error when fix_conn: ~w", [Error]),
                    {ok, [0, 0, 0, 0]};
                Rs1 ->
                    case From of
                        offline -> ets:delete(offline, AccountId);
                        _ -> ok
                    end,
                    lib_sender:send(11006, Rs1),
                    ?LOG({login, Rs1#role.id, Ip}),
                    Growth = init_growth(Rs1#role.growth),
                    Rs#role.pid ! stop,
                    %% ?INFO("~w login from ~w", [Rs1#role.id, From]),
                    %% ?INFO("~s(~w) login from ~w", [Rs1#role.name, Rs1#role.id, From]),
                    {ok, [1, Growth, Rs1#role.id, Rs1#role.upgrade_show]}
            end
    end.

init_role_data(account_id, AccountId, Rs) ->
    Sql = "select id, account_id, name, myitems, mytasks, kvs, myfriends from role where account_id = ~s;",
    Data = db:get_row(Sql, [AccountId]),
    init_role_data2(Data, Rs);
init_role_data(role_id, RoleId, Rs) ->
    Sql = "select id, account_id, name, myitems, mytasks, kvs, myfriends from role where id = ~s;",
    Data = db:get_row(Sql, [RoleId]),
    init_role_data2(Data, Rs);
init_role_data(name, Name, Rs) ->
    Sql = "select id, account_id, name, myitems, mytasks, kvs, myfriends from role where name = ~s;",
    Data = db:get_row(Sql, [Name]),
    init_role_data2(Data, Rs).

init_role_data2([], _Rs) -> false;
init_role_data2([Rid, AccountId1, Name, MyItemsBin, MyTasksBin, KvsBin, FriendsBin], Rs) ->
    AccountId = binary_to_list(AccountId1),
    Rs1 = init_default_val(Rs, Rid, Name, AccountId),
    Rs2 = init_kvs(Rs1, KvsBin),
    MyItems = lib_item:init_myitems(MyItemsBin, Rs2#role.growth),
    Rs3 = lib_role:calc_attrs(Rs2, MyItems),
    EnableDTask = init_enable_dtask(Rs3#role.enable_dtask, Rs3#role.lev),
    {TaskTail, DTaskTime} = lib_task:init_mytasks(
        MyTasksBin, Rs3#role.lev, Rs3#role.task_tail, 
        EnableDTask, Rs3#role.dtask_time),
    WinRate = init_win_rate(Rs3#role.game_count),
    Rs4 = Rs3#role{
        task_tail = TaskTail 
        ,dtask_time = DTaskTime 
        ,login_time = util:unixtime() 
        ,win_rate = WinRate
        ,enable_dtask = EnableDTask
    },
    init_friends(FriendsBin),
    Rs0 = lib_skill:init_skill(Rs4),
    {ok, Rs0}.

do_first(1, Rid, Name) ->
    %% 第一次登陆，初始排行
    gen_server:cast(srv_rank, {reg, Rid, Name});
do_first(_, _Rid, _Name) -> ok.

%% init_enable_dtask(EnableDTask, Lev) when Lev >= 8 ->
%%     lists:merge([154, 155, 156], EnableDTask);
init_enable_dtask(EnableDTask, _Lev) ->
    EnableDTask.

init_growth(0) -> 1;
init_growth(Growth) -> Growth.

%% do_guild(#role{guild_id = 0}) -> ok;
%% do_guild(Rs) ->
%%     gen_server:cast(srv_guild, {
%%             role_login 
%%             ,Rs#role.guild_id
%%             ,Rs#role.id
%%             ,Rs#role.lev
%%             ,Rs#role.sex
%%             ,Rs#role.pid_sender 
%%         }).

init_win_rate({Win, Lost, Draw, _}) ->
    All = Win+Lost+Draw,
    case All > 0 of
        true -> util:ceil((Win/All)*100);
        false -> 0
    end.

init_default_val(Rs, Rid, Name, AccountId) ->
    Rs#role{
        id = Rid 
        ,account_id = AccountId
        ,name = Name 
        ,hp = data_config:get(init_hp_max)
        ,hp_max = data_config:get(init_hp_max)
        ,dmg = data_config:get(init_dmg)
        ,move_speed = data_config:get(init_move_speed)
        ,dmg_speed = data_config:get(init_dmg_speed)
    }.

init_kvs(Rs, <<>>) -> Rs;
init_kvs(Rs, KvsBin) ->
    Kvs = util:bitstring_to_term(KvsBin),
    lib_role:init_kvs(Kvs, Rs).

init_friends(undefined) -> ok;
init_friends(FriendsBin) ->
    Friends = util:bitstring_to_term(FriendsBin),
    lib_sns:init_friends(Friends).

%% -> false | {TableName, Pid}
get_pid_from_ets(Type, Id) ->
    case get_online_pid(Type, Id) of
        false -> 
            case get_offline_pid(Type, Id) of
                false -> false;
                Pid -> {offline, Pid}
            end;
        Pid -> {online, Pid}
    end.

%% 离线表中查找
get_offline_pid(account_id, AccountId) ->
    case ets:lookup(offline, AccountId) of
        [] -> 
            false;
        [R] ->
            Pid = R#offline.pid,
            case is_process_alive(Pid) of
                true -> 
                    Pid ! {reset_stop_timer, ?OFFLINE_CACHE_TIME},
                    Pid;
                false ->
                    ?WARNING("Not alive pid in offline table, Rid:~w, Pid: ~w", 
                        [R#offline.id, Pid]),
                    ets:delete(offline, AccountId),
                    false
            end
    end;
get_offline_pid(role_id, Rid) ->
    %% 离线表中查找
    MatchSpec = [{ 
            #offline{pid='$1', id='$2', account_id='$3', _='_'} 
            ,[{'==','$2',Rid}] 
            ,[['$1', '$3']] 
        }],
    case ets:select(offline, MatchSpec) of
        [] -> 
            false;
        [[Pid, AccountId]] -> 
            case is_process_alive(Pid) of
                true -> 
                    Pid;
                false ->
                    ?WARNING("Not alive pid in offline table, Rid:~w, Pid: ~w", 
                        [Rid, Pid]),
                    ets:delete(offline, AccountId),
                    false
            end;
        _Else -> 
            ?WARNING("unexpected data:~w", [_Else]),
            false
    end;
get_offline_pid(name, Name) ->
    %% 离线表中查找
    MatchSpec = [{ 
            #offline{pid='$1', name='$2', account_id='$3', id='$4'} 
            ,[{'==','$2',Name}] 
            ,[['$1', '$3', '$4']] 
        }],
    case ets:select(offline, MatchSpec) of
        [] -> false;
        [[Pid, AccountId, Rid]] -> 
            case is_process_alive(Pid) of
                true -> Pid;
                false ->
                    ?WARNING("Not alive pid in offline table, Rid:~w, Pid: ~w", 
                        [Rid, Pid]),
                    ets:delete(offline, AccountId),
                    false
            end;
        _Else -> 
            ?WARNING("unexpected data:~w", [_Else]),
            false
    end.

%% 在线表中查找角色Pid
get_online_pid(account_id, AccountId) ->
    MatchSpec = [{ 
            #online{pid='$1', account_id='$2', id='$3', _ = '_'} 
            ,[{'==','$2',AccountId}] 
            ,[['$1', '$3']] 
        }],
    case ets:select(online, MatchSpec) of
        [] -> 
            false;
        [[Pid, Rid]] -> 
            case is_process_alive(Pid) of
                true -> 
                    Pid;
                false ->
                    ?WARNING("Not alive pid in online table, Rid:~w, Pid: ~w", [Rid, Pid]),
                    ets:delete(online, Rid),
                    false
            end;
        _Else -> 
            ?WARNING("unexpected data: ~w", [_Else]),
            false
    end;
get_online_pid(role_id, Rid) ->
    case ets:lookup(online, Rid) of
        [] -> 
            false;
        [R] ->
            #online{pid = Pid} = R,
            case is_process_alive(Pid) of
                true -> 
                    Pid;
                false ->
                    ?WARNING("Not alive pid in online table, Rid:~w, Pid: ~w", [Rid, Pid]),
                    ets:delete(online, Rid),
                    false
            end
    end;
get_online_pid(name, Name) ->
    MatchSpec = [{ 
            #online{pid='$1', name='$2', id='$3', _ = '_'} 
            ,[{'==','$2',Name}] 
            ,[['$1', '$3']] 
        }],
    case ets:select(online, MatchSpec) of
        [] -> 
            false;
        [[Pid, Rid]] -> 
            case is_process_alive(Pid) of
                true -> 
                    Pid;
                false ->
                    ?WARNING("Not alive pid in online table, Rid:~w, Pid: ~w", [Rid, Pid]),
                    ets:delete(online, Rid),
                    false
            end;
        _Else -> 
            ?WARNING("unexpected data: ~w", [_Else]),
            false
    end.
