%%----------------------------------------------------
%% TCP Acceptor
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(acceptor).

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    ?INFO("start ~w...", [?MODULE]),
    {ok, PolicyFile} = application:get_env(flash_policy_file),
    Handshaking = <<"ABCDEFGHIJKLMN876543210">>,
    self() ! {event, start},

    {ok, OnlineMax} = application:get_env(cc, online_max),
    srv_cache ! {acceptor, self()},
    {ok, {LSock, PolicyFile, Handshaking, OnlineMax}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({event, start}, State = {LSock, PolicyFile, Handshaking, OnlineMax}) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            Pid = spawn(fun() -> new_conn(Socket, PolicyFile, Handshaking, OnlineMax) end),
            io:format(".", []),
            case gen_tcp:controlling_process(Socket, Pid) of
                ok -> Pid ! start;
                _Reason ->
                    ?ERR("Error when controlling_process:~w", [_Reason]),
                    Pid ! exit
            end,
            self() ! {event, start},
            {noreply, State};
        {error, Reason} ->
            ?INFO("error when starting acceptor: ~p", [Reason]),
            {stop, Reason, State}
    end;

handle_info({online_max, Num}, {LSock, PolicyFile, Handshaking, _}) ->
    ?INFO("Set online_max: ~w", [Num]),
    {noreply, {LSock, PolicyFile, Handshaking, Num}};

handle_info(_Info, State) ->
    ?INFO("Unexpected info: ~w", [_Info]),
    {noreply, State}.

terminate(closed, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ------------------------------

%% 开始新连接
new_conn(Socket, PolicyFile, Handshaking, OnlineMax) ->
    receive
        start ->
            new_conn0(Socket, PolicyFile, Handshaking, OnlineMax);
        _Else ->
            ?WARN("Unexpected data: ~w", [_Else]),
            gen_tcp:close(Socket)
    end.

%% 开始新连接
new_conn0(Socket, PolicyFile, Handshaking, OnlineMax) ->
    case gen_tcp:recv(Socket, 23, 6000) of
        %% Flash策略文件请求
        {ok, ?FL_POLICY_REQ} ->
            gen_tcp:send(Socket, PolicyFile),
            gen_tcp:close(Socket);
        %% 客户端握手消息
        {ok, Handshaking} ->
            new_conn1(Socket, OnlineMax);
        {ok, <<116, 103, 119, _/binary>>} ->
            %% 腾讯平台
            gen_tcp:recv(Socket, 58 - 23, 6000),
            new_conn0(Socket, PolicyFile, Handshaking, OnlineMax);
        {ok, Data} ->
            ?INFO("Unexpected Handshaking:~w", [Data]),
            gen_tcp:close(Socket);
        _Else ->
            ?INFO("Unexpected message when new_conn: ~w", [_Else]),
            gen_tcp:close(Socket)
    end.

%% 检查在线人数是否达到了上限
new_conn1(Socket, OnlineMax) ->
    overload:request(),
    case ets:info(online, size) >= OnlineMax of
        true ->
            io:format("~n*** Reached top online: ~w ***~n", [OnlineMax]),
            lib_conn:pack_send(Socket, 15001, [101]),
            util:sleep(500), %% 等待消息发送完毕后再关闭Socket
            gen_tcp:close(Socket);
        false -> new_conn2(Socket)
    end.

%% 创建角色进程
new_conn2(Socket) ->
    case srv_role:create(Socket) of
        {ok, PidRole} ->
            case gen_tcp:controlling_process(Socket, PidRole) of
                ok -> PidRole ! {event, start_client};
                Reason ->
                    ?ERR("error when controlling_process:~w, self():~w, PidRole:~w", 
                        [Reason, self(), PidRole]),
                    PidRole ! {event, stop},
                    gen_tcp:close(Socket)
            end;
        Any ->
            ?DEBUG("error when create role process:~w", [Any]),
            gen_tcp:close(Socket)
    end.
