%%%-------------------------------------------------------------------
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

%%% @doc
%%%        Load Game Config data from YAML files to ets table for later using.
%%% @end
%%% Created :  一 10 07 23:50:49 2013 by Savin-Max
%%%-------------------------------------------------------------------

-module(game_numerical).

-behaviour(gen_server).

%% API
-export([start_link/0, 
         find/2, 
         all/1, 
         first/1, 
         load_data/1,
         wrap/1,
         next_key/2,
         find_ets/2,
         last/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {table}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

all(TableName) ->
    config_data:all(TableName).

first(TableName) ->
    config_data:first(TableName).

last(TableName) ->
    config_data:last(TableName).

find(TableName, Key) ->
    config_data:find(TableName, Key).

load_data(ConfigModels) ->
    gen_server:call(?SERVER, {load_data, ConfigModels}).

wrap(Fun) ->
    gen_server:call(?SERVER, {wrap, Fun}).

next_key(TableName, Key) -> 
    config_data:next_key(TableName, Key).

find_ets(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [Object] ->
            Object;
        [] ->
            undefined
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({wrap, Fun}, _From, State) ->
    {reply, Fun(), State};
handle_call({load_data, ConfigModels}, _From, State) ->
    load_config_data(ConfigModels),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Info:~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_config_data(ConfigModels) ->
    lists:foreach(fun(ModelName) -> load_config_model(ModelName) end, ConfigModels).

load_config_model(ModelName) ->
    {ok, Records} = db:all(ModelName),
    case ets:info(ModelName) of
        undefined -> ok;
        _ -> ets:delete(ModelName)
    end,
    ets:new(ModelName,
            [ordered_set, protected, named_table, {keypos, 2}, {read_concurrency, true}]),
    ets:insert(ModelName, Records).
