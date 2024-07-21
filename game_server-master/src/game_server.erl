%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2024
%% Savin Max <mafei.198@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.


-module(game_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(GS_RUNNING, 1).
-define(GS_STOPPING, 2).

-record(state, {status}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    start([development]).

start([Mode]) ->
    error_logger:info_msg("Game Server Starting~n"),
    error_logger:info_msg("env: ~p~n", [application:get_all_env()]),
    application:set_env(game_server, server_environment, Mode),
    ok = application:start(game_server).

stop() ->
    case application:get_env(game_server, server_environment) of
        {ok, test} -> force_stop();
        _ -> gen_server:cast(?SERVER, stop)
    end.

force_stop() ->
    application:stop(sync),
    application:stop(chat_server),
    application:stop(timertask),
    application:stop(leaderboard),
    application:stop(player_server),
    application:stop(game_server),
    application:stop(game_numerical),
    application:stop(record_mapper),
    application:stop(mnesia),
    application:stop(db),
    application:stop(emysql),
    application:stop(gproc),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(inets),
    application:stop(asn1),
    application:stop(crypto),
    ibrowse:stop(),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{status = ?GS_RUNNING}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    case State#state.status of
        ?GS_RUNNING ->
            error_logger:info_msg("==========PREPARING SHUTDOWN APPLICATION===========~n"),
            error_logger:info_msg("================CLOSE TCP LISTENER=================~n"),
            supervisor:terminate_child(game_server_sup, ranch_sup),
            error_logger:info_msg("================CLOSE TCP CONNECTIONS==============~n"),
            supervisor:terminate_child(game_server_sup, {ranch_listener_sup, ranch_tcp_listener}),
            error_logger:info_msg("================SHUTDOWN IAP SERVER================~n"),
            supervisor:terminate_child(game_server_sup, iap_server_sup),
            error_logger:info_msg("================SHUTDOWN TIMERTASK================~n"),
            application:stop(timertask),
            error_logger:info_msg("================SHUTDOWNING PLAYERS================~n"),
            player_factory:shutdown_players(),
            {noreply, State#state{status = ?GS_STOPPING}};
        ?GS_STOPPING ->
            send_msg_to_stop_deamon(is_stopping)
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({finished_shutdown_players, _From}, State) ->
    error_logger:info_msg("==========FINISHED TO SHUTDOWN ALL THE PLAYERS===========~n"),
    init:stop(),
    error_logger:info_msg("======INVOKE init:stop() TO STOP WHOLE APPLICATION=======~n"),
    {noreply, State}.

terminate(_Reason, _State) ->
    send_msg_to_stop_deamon(stopped),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% -spec ensure_started(module()) -> ok.
% ensure_started(App) ->
%     case application:start(App) of
%         ok -> ok;
%         {error, {already_started, App}} -> ok
%     end.

send_msg_to_stop_deamon(Msg) ->
    {ok, Hostname} = inet:gethostname(),
    RemoteNode = list_to_atom("stop_console@" ++ Hostname),
    {stop_deamon, RemoteNode} ! Msg.
