%%----------------------------------------------------
%% flash策略文件请求处理
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(flash_843).
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

%% 开启连接监听服务
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 关闭连接监听服务
stop() ->
    ?INFO("关闭~w...", [?MODULE]),
    ok.

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    Port = 843,
    {ok, TcpOptions} = application:get_env(tcp_options_flash_843),
    {ok, PolicyFile} = application:get_env(flash_policy_file),
    case gen_tcp:listen(Port, TcpOptions) of
        {ok, LSock} ->
            spawn_link(fun() -> loop(LSock, PolicyFile) end),
            {ok, state};
        {error, Reason}->
            ?ERR("无法监听到 ~w:~w~n", [Port, Reason]),
            {stop, listen_failure, state}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

loop(LSock, PolicyFile) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} -> spawn(fun()-> send_file(Sock, PolicyFile) end);
        {error, Reason} -> Reason
    end,
    loop(LSock, PolicyFile).

send_file(Sock, PolicyFile) ->
    case gen_tcp:recv(Sock, 23) of
        {ok, ?FL_POLICY_REQ} -> 
            %% ?INFO("843端口收到请求：~w", [?FL_POLICY_REQ]),
            gen_tcp:send(Sock, PolicyFile);
        _ -> error
    end,
    gen_tcp:close(Sock).
