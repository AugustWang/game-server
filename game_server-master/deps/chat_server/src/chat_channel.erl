%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014-2024
%%% Savin Max <mafei.198@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%
%%% @doc
%%%        Chat Channel.
%%% @end
%%% Created :  三  3 12 17:29:50 2014 by Savin Max

-module(chat_channel).

-behaviour(gen_server).

%% API
-export([start_link/2,
         history/2,
         broadcast/3,
         join/2,
         leave/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("chat_server_macros.hrl").

-record(state, {channel, maxCacheAmount}).

-define(TAB, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id, MaxCacheAmount) ->
    gen_server:start_link(?MODULE, [Id, MaxCacheAmount], []).

join(PlayerID, Channel) ->
    player:subscribe(PlayerID, Channel).

leave(PlayerID, Channel) ->
    player:unsubscribe(PlayerID, Channel).

% cached history messages.
history(Channel, Amount) ->
    gen_server:call(channel_pid(Channel), {history, Amount}).

broadcast(Channel, MsgType, Msg) ->
    gen_server:cast(channel_pid(Channel), {broadcast, MsgType, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Channel, MaxCacheAmount]) ->
    ?REG_PID({chat_channel, Channel}),
    {ok, #state{channel=Channel, maxCacheAmount = MaxCacheAmount}}.

handle_call({history, Amount}, _From, State) ->
    {reply, history_msg(Amount), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({broadcast, MsgType, Msg}, State=#state{channel=Channel, 
                                                    maxCacheAmount=MaxAmount}) ->
    ?BROADCAST(Channel, MsgType, Msg),
    add_msg(Msg, MaxAmount),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel=Channel}) ->
    ?UNREG({chat_channel, Channel}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_msg(Msg, MaxAmount) ->
    case get(msg_list) of
        undefined -> put(msg_list, [Msg]);
        MsgList -> 
            SubMsgList = lists:sublist(MsgList, MaxAmount),
            put(msg_list, [Msg|SubMsgList])
    end.

history_msg(Amount) ->
    case get(msg_list) of
        undefined -> [];
        MsgList -> lists:sublist(MsgList, Amount)
    end.

channel_pid(Channel) ->
    case ?GET_PID({chat_channel, Channel}) of
        undefined -> 
            {ok, Pid} = chat_server:create_channel(Channel),
            Pid;
        ChannelPid -> ChannelPid
    end.
