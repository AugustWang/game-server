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


-module(game_connection).

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4, send_data/2, send_data/3, send_multi_data/2, stop/1, sync_stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(protocol, {ref, socket, transport, playerID, last_active_time, timer}).

-define(ACTIVITY_CHECK_DURATION, 30000). %% millisecond
-define(EXPIRE_DURATION, 90). %% seconds

-include("include/gproc_macros.hrl").
-include("../app/include/secure.hrl").
-include ("include/common_const.hrl").
-include ("include/db_schema.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport], Opts).

send_data(Pid, Data) ->
    gen_server:cast(Pid, {send_data, Data}).

send_data(Pid, RequestId, Data) ->
    gen_server:cast(Pid, {send_data, RequestId, Data}).

send_multi_data(Pid, MultiData) ->
    gen_server:cast(Pid, {send_multi_data, MultiData}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

sync_stop(Pid) ->
    gen_server:call(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Ref, Socket, Transport]) ->
    Timer = erlang:send_after(?ACTIVITY_CHECK_DURATION, self(), active_check),
    {ok, #protocol{ref = Ref, 
                   socket = Socket, 
                   transport = Transport, 
                   timer = Timer,
                   last_active_time = time_utils:now()}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    connection_will_stop(State),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    connection_will_stop(State),
    {stop, normal, State};
handle_cast({send_data, RequestId, Data}, State=#protocol{transport = Transport, 
                                                          socket = Socket, 
                                                          playerID = PlayerID}) ->
    error_logger:info_msg("PlayerID: ~p, RequestId: ~p, SendData: ~p~n", 
                          [PlayerID, RequestId, Data]),
    send_single_socket_data(Transport, Socket, RequestId, Data),
    {noreply, State};
handle_cast({send_data, Data}, 
            State=#protocol{transport = Transport, socket = Socket, playerID = PlayerID}) ->
    error_logger:info_msg("PlayerID: ~p, SendData: ~p~n", [PlayerID, Data]),
    send_single_socket_data(Transport, Socket, 0, Data),
    {noreply, State};
handle_cast({send_multi_data, MultiData}, 
            State=#protocol{transport = Transport, socket = Socket, playerID = PlayerID}) ->
    error_logger:info_msg("PlayerID: ~p, MultiData: ~p~n", [PlayerID, MultiData]),
    % error_logger:info_msg("-------------------Start SendMultiData-------------------"),
    % error_logger:info_msg("PlayerID: ~p, Multi: ~p~n", [PlayerID, length(MultiData)]),
    % lists:foreach(fun(Data) ->
    %     error_logger:info_msg("PlayerID: ~p, SendMultiData: ~p~n", [PlayerID, Data])
    % end, MultiData),
    % error_logger:info_msg("-------------------Stop  SendMultiData-------------------~n"),
    PackedData = [pack_response_data(RequestId, Data) || {RequestId, Data} <- MultiData],
    send_socket_data(Transport, Socket, list_to_binary(PackedData)),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(active_check, State=#protocol{timer = Timer, last_active_time = ActiveAt}) ->
    erlang:cancel_timer(Timer),
    case time_utils:now() - ActiveAt >= ?EXPIRE_DURATION of
        true -> 
            connection_will_stop(State),
            error_logger:info_msg("DISCONNECT: tcp_timeout, PlayerID: ~p~n", 
                                  [State#protocol.playerID]),
            {stop, normal, State};
        false ->
            NewTimer = erlang:send_after(?ACTIVITY_CHECK_DURATION, self(), active_check),
            {noreply, State#protocol{timer = NewTimer}}
    end;
handle_info(timeout, State=#protocol{transport = Transport, socket = Socket}) ->
    ok = ranch:accept_ack(State#protocol.ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 2}]),
    {noreply, State};
handle_info({tcp, Socket, CipherData}, 
            State=#protocol{transport = Transport, playerID = PlayerID}) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    RawData = secure:decrypt(?AES_KEY, ?AES_IVEC, CipherData),
    {RequestId, RequestContent} = utils_protocol:decode_integer(RawData),
    {RequestType, _RequestBody} = utils_protocol:decode_short(RequestContent),
    case routes:route(RequestType) of
        {error, Msg} ->
            error_logger:info_msg("PlayerID: ~p, Route Error! Invalid RequestType: ~p~n", 
                                  [PlayerID, RequestType]),
            send_single_socket_data(Transport, Socket, RequestId, {0, Msg}),
            State#protocol{last_active_time = time_utils:now()};
        RoutePath ->
            {Params, _LeftData} = api_decoder:decode(RequestContent),
            error_logger:info_msg("PlayerID: ~p, Request Path: ~p Parmas: ~p~n", 
                                  [PlayerID, RoutePath, Params]),
            handle_request({RoutePath, Params, RequestId}, State)
    end;
handle_info({tcp_closed, _Socket}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_closed, PlayerID: ~p~n", [State#protocol.playerID]),
    connection_will_stop(State),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _Msg}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_error, playerID: ~p~n", [State#protocol.playerID]),
    connection_will_stop(State),
    {stop, normal, State}.

handle_request({Path = {sessions_controller, login}, Params, RequestId}, State) ->
    Udid = proplists:get_value(udid, Params),
    PlayerID = player_data:get_player_id(Udid),
    register_connection(PlayerID),
    player_factory:start_player(PlayerID),
    player:request(PlayerID, Path, proplists_utils:values(Params), RequestId),
    {noreply, State#protocol{playerID = PlayerID, last_active_time = time_utils:now()}};
handle_request({Path, Params, RequestId}, State=#protocol{playerID = PlayerID}) ->
    case PlayerID of
        undefined ->
            connection_will_stop(State),
            {stop, {playerID, undefined}, State};
        _ ->
            player:request(PlayerID, Path, proplists_utils:values(Params), RequestId),
            {noreply, State#protocol{last_active_time = time_utils:now()}}
    end.

encode_response(Response) ->
    case Response of
        {fail, ErrorAtom} ->
            ErrorString = atom_to_binary(ErrorAtom, utf8),
            case game_numerical:find(config_error_msgs, ErrorString) of
                undefined ->
                    api_encoder:encode(fail, {0, ErrorString});
                Conf ->
                    api_encoder:encode(fail, {Conf#config_error_msgs.no, <<"">>})
            end;
        {fail, ErrorAtom, Msg} ->
            ErrorString = atom_to_binary(ErrorAtom, utf8),
            case game_numerical:find(config_error_msgs, ErrorString) of
                undefined ->
                    api_encoder:encode(fail, {0, ErrorString});
                Conf ->
                    api_encoder:encode(fail, {Conf#config_error_msgs.no, Msg})
            end;
        {Protocol, Msg} when is_tuple(Msg) ->
            api_encoder:encode(Protocol, Msg);
        {Protocol, Msg} when is_list(Msg) orelse is_binary(Msg) ->
            api_encoder:encode(Protocol, {Msg});
        {_Protocol, Msg} ->
            error_logger:info_msg("Response Msg type error: ~p", [Msg]),
            erlang:error("Encode response error, Invalid Msg type!")
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State=#protocol{playerID=PlayerID}) ->
    case PlayerID =:= undefined of
        false -> ?UNREG({connection, PlayerID});
        true -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register_connection(PlayerID) ->
    case ?GET_PID({connection, PlayerID}) of
        undefined ->
            ?REG_PID({connection, PlayerID});
        OldConPid when OldConPid =:= self() -> 
            ok;
        OldConPid ->
            game_connection:sync_stop(OldConPid),
            ?REG_PID({connection, PlayerID})
    end.

send_single_socket_data(Transport, Socket, RequestId, Data) ->
    PureData = pack_response_data(RequestId, Data),
    send_socket_data(Transport, Socket, PureData).

pack_response_data(RequestId, Data) ->
    EncodedData = try encode_response(Data) of
                      EncodedResponse -> EncodedResponse
                  catch
                      Exception:Msg ->
                          exception:notify(Exception, Data, Msg),
                          api_encoder:encode(fail, {0, <<"Server Internal Error!">>})
                  end,
    list_to_binary([utils_protocol:encode_integer(RequestId), EncodedData]).

send_socket_data(Transport, Socket, PureData) ->
    CipherData = secure:encrypt(?AES_KEY, ?AES_IVEC, PureData),
    Transport:send(Socket, CipherData).

connection_will_stop(_State=#protocol{playerID = PlayerID,
                                      transport = Transport, socket = Socket}) ->
    if
        PlayerID =/= undefined ->
            player:on_tcp_closed(PlayerID);
        true -> ok
    end,
    if
        Transport =/= undefined andalso Socket =/= undefined ->
            Transport:close(Socket);
        true -> ok
    end.
