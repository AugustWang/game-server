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
%%%        Manage timertask!
%%% @end
%%% Created :  一  3 24 18:20:18 2014 by Me

-module(timertask).

-behaviour(gen_server).

%% API
-export([start_link/0, 
         sync_add/3, sync_cancel/1, sync_update/3,
         add/3, cancel/1, update/3, lookup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(KEY, "timertask:ordered_set").

-record(state, {}).

-define(TABLE, game_server_timertask).
-define(ORDER_TABLE, game_server_ordered_timertask).


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Key, RunAt, MFA) ->
    gen_server:cast(?SERVER, {add, Key, RunAt, MFA}).

cancel(Key) ->
    gen_server:cast(?SERVER, {cancel, Key}).

update(Key, RunAt, MFA) ->
    gen_server:cast(?SERVER, {update, Key, RunAt, MFA}).

sync_add(Key, RunAt, MFA) ->
    gen_server:call(?SERVER, {add, Key, RunAt, MFA}).

sync_cancel(Key) ->
    gen_server:call(?SERVER, {cancel, Key}).

sync_update(Key, RunAt, MFA) ->
    gen_server:call(?SERVER, {update, Key, RunAt, MFA}).

lookup(Key) ->
    gen_server:call(?SERVER, {lookup, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    timer:send_after(1000, {start_timertask}),
    {ok, Redis} = eredis:start_link(),
    put(redis, Redis),
    {ok, #state{}}.

handle_call({add, Key, RunAt, MFA}, _From, State) ->
    add_timer(Key, RunAt, MFA),
    {reply, ok, State};
handle_call({cancel, Key}, _From, State) ->
    cancel_timer(Key),
    {reply, ok, State};
handle_call({update, Key, RunAt, MFA}, _From, State) ->
    update_timer(Key, RunAt, MFA),
    {reply, ok, State};
handle_call({lookup, Key}, _From, State) ->
    {reply, get_timer(Key), State}.

handle_cast({add, Key, RunAt, MFA}, State) ->
    add_timer(Key, RunAt, MFA),
    {noreply, State};
handle_cast({cancel, Key}, State) ->
    cancel_timer(Key),
    {noreply, State};
handle_cast({update, Key, RunAt, MFA}, State) ->
    update_timer(Key, RunAt, MFA),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_timertask}, State) ->
    try_restart_timer(),
    {noreply, State};
handle_info({timertask, Key, MFA}, State) ->
    try dispatch_to_worker(MFA) of
        _ -> 
            erase(current_timer),
            cancel_timer(Key),
            try_restart_timer()
    catch
        Type:Msg ->
            erase(current_timer),
            cancel_timer(Key),
            throw({Type, Msg})
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_timer(Key, RunAt, MFA) ->
    transaction(fun() ->
        redis_cmd(["zadd", ?KEY, RunAt, Key]),
        redis_cmd(["set", mfa_key(Key), encode(MFA)])
    end),
    try_restart_timer().

cancel_timer(Key) ->
    transaction(fun() ->
        redis_cmd(["zrem", ?KEY, Key]),
        redis_cmd(["del", mfa_key(Key)])
    end),
    try_restart_timer().

update_timer(Key, RunAt, MFA) ->
    transaction(fun() ->
        redis_cmd(["zadd", ?KEY, RunAt, Key]),
        redis_cmd(["set", mfa_key(Key), encode(MFA)])
    end),
    try_restart_timer().

dispatch_to_worker(MFA) ->
    poolboy:transaction(timertask_worker_pool, fun(Worker) ->
        gen_server:call(Worker, {timertask, MFA})
    end).

first_timer() ->
    case redis_cmd(["zrange", ?KEY, 0, 0, "withscores"]) of
        {ok, []} -> undefined;
        {ok, [Key, RunAt]} -> 
            {ok, MFA} = redis_cmd(["get", mfa_key(Key)]),
            {Key, binary_to_integer(RunAt), decode(MFA)}
    end.

get_timer(Key) ->
    case redis_cmd(["zrank", ?KEY, Key]) of
        {ok, undefined} -> undefined;
        {ok, RunAt} -> 
            {ok, MFA} = redis_cmd(["get", mfa_key(Key)]),
            {Key, binary_to_integer(RunAt), decode(MFA)}
    end.

start_timer({Key, RunAt, MFA}) ->
    AfterTime = lists:max([(RunAt - current_time()) * 1000, 0]),
    Timer = erlang:send_after(AfterTime, ?SERVER, {timertask, Key, MFA}),
    put(current_timer, {Key, RunAt, Timer}).

try_restart_timer() ->
    CurrentTimer = get(current_timer),
    FirstTimer = first_timer(),
    if
        CurrentTimer =:= undefined andalso FirstTimer =/= undefined ->
            start_timer(FirstTimer);
        CurrentTimer =/= undefined andalso FirstTimer =:= undefined ->
            {_, _, Timer} = CurrentTimer,
            erlang:cancel_timer(Timer);
        CurrentTimer =/= undefined andalso FirstTimer =/= undefined ->
            case element(2, FirstTimer) < element(2, CurrentTimer) of
                false -> ok;
                true -> 
                    erlang:cancel_timer(element(3, CurrentTimer)),
                    start_timer(FirstTimer)
            end;
        true ->
            ok
    end.

current_time() ->
    Datetime = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Datetime) - 62167219200.

encode(Data) ->
    Value = term_to_binary(Data),
    base64:encode(Value).

decode(Value) ->
    Data = base64:decode(Value),
    binary_to_term(Data).

transaction(Fun) ->
    Redis = get(redis),
    eredis:q(Redis, ["MULTI"]),
    Fun(),
    eredis:q(Redis, ["EXEC"]).

redis_cmd(List) ->
    Redis = get(redis),
    eredis:q(Redis, List).

mfa_key(Key) when is_atom(Key) ->
   "timertask:mfa_set:" ++ atom_to_list(Key);
mfa_key(Key) when is_list(Key) ->
   "timertask:mfa_set:" ++ Key;
mfa_key(Key) when is_binary(Key) ->
   "timertask:mfa_set:" ++ binary_to_list(Key).
