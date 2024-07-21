%% -------------------------------------------------
%% 引擎启动器
%% 
%% @author rolong@vip.qq.com
%% @version {@version}
%% @end
%% -------------------------------------------------
-module(cc).
-behaviour(application).
-export([start/0, stop/0, restart/0, start/2, prep_stop/1, stop/1, init_mysql/1, get_poll_name/0]).

-include("common.hrl").
-include("offline.hrl").

-define(APPS, [sasl, cc, os_mon]).
%% -define(APPS, [sasl, cc]).

get_poll_name() -> 
    case env:get(poll_name) of
        undefined -> ?DB;
        V -> V
    end.

%% @spec start() -> void()
%% @doc 启动cge, 由shell调用，关闭使用{@link stop/0}方法
start() ->
    try
        ok = start_applications(?APPS) 
    after
        timer:sleep(100)
    end.

%% @spec stop() -> void()
%% @doc 关闭cc
stop() ->
    case srv_cache:save_cache() of
        ok ->
            stop_applications(?APPS),
            erlang:halt();
        _ -> ok
    end.

restart() ->
    case srv_cache:save_cache() of
        ok ->
            stop_applications(?APPS),
            start_applications(?APPS),
            ok;
        _ -> ok
    end.

%% @hidden
start(_Type, _Args) ->
    %% [HallNodeNameStr, NodeIdStr, HostStr, PortStr] = init:get_plain_arguments(),
    %% HallNodeName = list_to_atom(HallNodeNameStr),
    %% NodeId = list_to_integer(NodeIdStr),
    %% Host = list_to_binary(HostStr),
    %% Port = list_to_integer(PortStr),
    %% HallNodeName = 'cc@localhost',
    %% Args = init:get_plain_arguments(),
    cc_env:init(),
    init_mysql(get_poll_name()),
    init_ets(),
    inets:start(),
    NodeId = 0,
    Host = "localhost",
    {ok, Port} = application:get_env(cc, tcp_port),
    case cc_sup:start_link([NodeId, Host, Port]) of
        {ok, SupPid} -> 
            LogFileName = "log/error_runtime.log",
            CustomLogFileName = "log/error_custom.log",
            error_logger:logfile({open, LogFileName}),
            cc_logger:logfile({open, CustomLogFileName}),
            supervisor:start_child(cc_sup, {flash_843, {flash_843, start_link, []}, permanent, 10000, worker, [flash_843]}),
            supervisor:start_child(cc_sup, {srv_hall, {srv_hall, start_link, []}, permanent, 10000, worker, [srv_hall]}),
            supervisor:start_child(cc_sup, {srv_rank, {srv_rank, start_link, []}, temporary, 10000, worker, [srv_rank]}),
            supervisor:start_child(cc_sup, {srv_luck, {srv_luck, start_link, []}, temporary, 10000, worker, [srv_luck]}),
            supervisor:start_child(cc_sup, {crontab, {crontab, start_link, []}, temporary, 10000, worker, [crontab]}),
            %% case NodeId > 0 of
            %%     true -> net_adm:ping(HallNodeName);
            %%     false -> skip
            %% end,
            %% ?INFO("plain_arguments:~p", [Args]),
            %% TEST
            %% eprof:start(),
            %% eprof:start_profiling([srv_hall]), %% TEST
            %% eprof:log("log/eprof.txt"),
            %% timer:apply_after(3000, test, start, [1, 200]),
            catch db:execute("UPDATE `log_login` SET `event`= 4, `logout_time`= ~s WHERE event = 0", [util:unixtime()]),
            srv_cache:init_cache(),
            case application:get_env(cc, version_type) of
                {ok, dev} -> u:m();
                _ -> ok
            end,
            {ok, SupPid};
        {error,shutdown} ->
            {error,shutdown}
    end.

%% @hidden
prep_stop(_State) ->
    robot ! prep_stop,
    lib_conn:send_msg(<<"服务器己关闭">>),
    listener:stop(), %% 停止接受新的连接
    lib_role:stop_all(),
    srv_guild:save(),
    lib_rank:stop(),
    util:sleep(1000),
    ok.

%% @hidden
stop(_State) ->
    ok.

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    F = fun (App, Acc) ->
            case Do(App) of
                ok -> [App | Acc];
                {error, {SkipError, _}} -> Acc;
                {error, Reason} ->
                    lists:foreach(Undo, Acc),
                    throw({error, {ErrorTag, App, Reason}})
            end
    end,
    Iterate(F, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(
        fun lists:foldl/3,
        fun application:start/1,
        fun application:stop/1,
        already_started,
        cannot_start_application,
        Apps
    ).

stop_applications(Apps) ->
    manage_applications(
        fun lists:foldr/3,
        fun application:stop/1,
        fun application:start/1,
        not_started,
        cannot_stop_application,
        Apps
    ).

%% mysql数据库连接初始化
init_mysql(Poll) ->
    {ok, DbHost} = application:get_env(cc, db_host),
    {ok, DbPort} = application:get_env(cc, db_port),
    {ok, DbUser} = application:get_env(cc, db_user),
    {ok, DbPass} = application:get_env(cc, db_pass),
    {ok, DbName} = application:get_env(cc, db_name),
    {ok, DbEncode} = application:get_env(cc, db_encode),
    {ok, DbConnNum} = application:get_env(cc, db_connector_num),
    %% mysql:start_link(Poll, DbHost, DbPort, DbUser, DbPass, DbName), %% 与mysql数据库建立连接
    %% LogFun = fun(_, _, _, _) -> ok end,
    LogFun = fun
        %% (Mod, Line, error, P) ->
        %%     {Msg, Params1} = P(),
        %%     io:format("~n[~w:~w]~n" ++ Msg, [Mod, Line] ++ Params1),
        %%     %% cc_logger:notify(warning, "Level:~w, Params1:~w", [Level, Params1], Mod, Line)
        %%     ok;
        (_Mod, _Line, _, _P) -> ok
    end,
    mysql:start_link(Poll, DbHost, DbPort, DbUser, DbPass, DbName, LogFun, DbEncode), %% 与mysql数据库建立连接
    util:for(1, DbConnNum,
        fun(_I) ->
                mysql:connect(Poll, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, true)
        end
    ),
    cc_env:set(poll_name, Poll),
    ok.

init_ets() ->
    ets:new(online, [{keypos, #online.id}, named_table, public, set]),
    ets:new(offline, [{keypos, #offline.account_id}, named_table, public, set]),
    ets:new(npc, [{keypos, 2}, named_table, public, set]),
    ok.
