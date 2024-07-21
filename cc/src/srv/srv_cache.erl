%%----------------------------------------------------
%% 缓存
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_cache).
-behaviour(gen_server).
-export([
        start_link/0
        ,get/1
        ,set/2
        ,get_daily_count/1
        ,set_daily_count/2
        ,add_daily_count/1
        ,clear_daily_count/0
        ,init_cache/0
        ,save_cache/0
        ,send_event/2
        ,save_role_events/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("shop.hrl").

-define(ETS_DIR, "dets").
-define(ETS_FILE_NAME, "dets/role_events").

-record(state, {}).

define_key({online_reward, Rid}) -> {1, Rid};
define_key({rank_reward, Rid}) -> {2, Rid};
define_key(Key) -> 
    ?ERR("Key undefined:~w", [Key]),
    Key.

%% --- 对外接口 ---------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_event(Rid, Event) ->
    gen_server:cast(?MODULE, {add_event, Rid, Event}).

init_cache() ->
    gen_server:cast(?MODULE, init_cache).

save_cache() ->
    gen_server:call(?MODULE, save_cache).

save_role_events() ->
    gen_server:cast(?MODULE, save_role_events).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

set(Key, Val) ->
    gen_server:cast(?MODULE, {set, Key, Val}).

%% 计数器(每天零点重置为0)
get_daily_count(Key1) ->
    Key = define_key(Key1),
    gen_server:call(?MODULE, {get_daily_count, Key}, 1000).

%% 设置计数器(每天零点重置为0)
set_daily_count(Key1, Val) ->
    Key = define_key(Key1),
    gen_server:call(?MODULE, {set_daily_count, Key, Val}).

%% 累加计数器(每天零点重置为0)
add_daily_count(Key1) ->
    Key = define_key(Key1),
    gen_server:cast(?MODULE, {add_daily_count, Key}).

%% 清除计数器(每天零点重置为0)
clear_daily_count() ->
    gen_server:cast(?MODULE, clear_daily_count).

%% --- 服务器内部实现 ---------------------------------

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    init_role_events(),
    State = #state{},
    {ok, State}.

handle_call({set_daily_count, K, V}, _From, State) ->
    erlang:put({daily_count, K}, V),
    {reply, ok, State};

handle_call({get_daily_count, Key}, _From, State) ->
    Reply = case erlang:get({daily_count, Key}) of
        undefined -> 0;
        Count -> Count
    end,
    {reply, Reply, State};

handle_call({get, K}, _From, State) ->
    {reply, erlang:get(K), State};

handle_call({is_rebate_shop, ShopId}, _From, State) ->
    {reply, lists:member([ShopId], get_rebate_shop()), State};

handle_call(save_cache, _From, State) ->
    try
        ?INFO("Save ETS Data:~p", [ets:info(role_events, size)]),
        ets:tab2file(role_events, ?ETS_FILE_NAME),
        save_daily_count(),
        {reply, ok, State}
    catch
        T:X ->
            ?ERR("[~p:~p]", [T, X]),
            {reply, error, State}
    end;

handle_call(_Request, _From, State) ->
    ?INFO("undefined request:~p", [_Request]),
    {noreply, State}.

handle_cast({add_daily_count, K}, State) ->
    case erlang:get({daily_count, K}) of
        undefined -> erlang:put({daily_count, K}, 1);
        Count -> erlang:put({daily_count, K}, 1 + Count)
    end,
    {noreply, State};

handle_cast(clear_daily_count, State) ->
    F = fun 
        ({{daily_count, Key}, _}) -> erlang:erase({daily_count, Key});
        (_) -> ok
    end,
    lists:foreach(F, erlang:get()),
    ?INFO("clear_daily_count ...", []),
    {noreply, State};

handle_cast({set, K, V}, State) ->
    erlang:put(K, V),
    {noreply, State};

handle_cast(init_cache, State) ->
    init_daily_count(),
    {noreply, State};

%% 登陆后需处理的事件
handle_cast({login, Rid, Pid}, State) ->
    case erlang:get({login, Rid}) of
        true -> ok;
        undefined ->
            erlang:put({login, Rid}, true),
            Pid ! {handle_event, 6003, []},
            ok
    end,
    case get_events(Rid) of
        [] -> ok;
        Events -> 
            Pid ! {handle_event, 6002, [do_events, Events]},
            set_events(Rid, [])
    end,
    {noreply, State};

handle_cast({add_event, Rid, Event}, State) when is_integer(Rid) ->
    %% ?INFO("~w", [{add_event1, Rid, Event}]),
    add_event(Rid, Event),
    {noreply, State};

handle_cast({add_event, Rids, Event}, State) ->
    %% ?INFO("~w", [{add_event2, Rids, Event}]),
    do_add_event(Rids, Event),
    {noreply, State};

handle_cast(save_role_events, State) ->
    try
        T = erlang:now(),
        ets:tab2file(role_events, ?ETS_FILE_NAME),
        test(T),
        {noreply, State}
    catch
        TT:X ->
            ?ERR("[~p:~p]", [TT, X]),
            {noreply, State}
    end;

handle_cast({send_rebate_shop, PidSender}, State) ->
    RestTime = get_rebate_shop_rest_time(),
    lib_conn:pack_send(PidSender, 20005, [RestTime, get_rebate_shop()]),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?INFO("undefined msg:~p", [_Msg]),
    {noreply, State}.

handle_info(clear_login, State) ->
    F = fun
        ({{login, Id}, _}) ->
            erlang:erase({login, Id});
        (_) -> ok
    end,
    lists:foreach(F, erlang:get()),
    {noreply, State};

handle_info({acceptor, Pid}, State) ->
    Acceptors = case erlang:get(acceptor) of
        undefined -> [];
        Tmp -> Tmp
    end,
    erlang:put(acceptor, [Pid | Acceptors]),
    {noreply, State};

handle_info({online_max, Num}, State) ->
    Acceptors = case erlang:get(acceptor) of
        undefined -> [];
        Tmp -> Tmp
    end,
    F = fun (Pid) -> 
            ?INFO("acceptor:~w", [Pid]),
            Pid ! {online_max, Num} 
    end,
    lists:foreach(F, Acceptors),
    {noreply, State};

handle_info(info, State) ->
    ?INFO("role_events [size:~w, memory:~w]", 
        [ets:info(role_events, size), 
            ets:info(role_events, memory)]),
    {noreply, State};

handle_info(show_poll, State) ->
    try
        mysql_dispatcher ! info
        catch _ : _ -> ok
    end,
    {noreply, State};

handle_info({del_poll, PoolId}, State) ->
    try
        mysql_dispatcher ! {del_pool, PoolId}
        catch _ : _ -> ok
    end,
    {noreply, State};

handle_info(add_poll, State) ->
    try
        Now = util:unixtime(),
        LastTime = case erlang:get(last_add_poll_time) of
            undefined -> 0;
            LT -> LT
        end,
        Interval = Now - LastTime,
        case Interval > 20 of
            true ->
                erlang:put(last_add_poll_time, Now),
                Index = case erlang:get(poll_index) of
                    undefined -> 
                        erlang:put(poll_index, 1),
                        1;
                    N -> 
                        erlang:put(poll_index, N + 1),
                        N + 1
                end,
                PollName1  = "poll" ++ integer_to_list(Index),
                PollName = list_to_atom(PollName1),
                cc:init_mysql(PollName),
                case Index > 1 of
                    true ->
                        DelPollName1  = "poll" ++ integer_to_list(Index - 1),
                        DelPollName = list_to_atom(DelPollName1),
                        erlang:send_after(5000, self(), {del_poll, DelPollName});
                    false -> ok
                end;
            false -> 
                ?INFO("add_poll failed, Interval:~w", [Interval]),
                ok
        end
        catch _ : _ -> ok
    end,
    {noreply, State};

handle_info({refresh_rebate_shop, TimeH}, State) ->
    KeepTime  = 3600 * TimeH,
    %% KeepTime = 260,
    NextTime = util:unixtime() + KeepTime,
    erlang:put(rebate_shop_stop_time, NextTime),
    erlang:send_after(KeepTime * 1000, self(), cancel_rebate_shop),
    refresh_rebate_shop(),
    {noreply, State};

handle_info(cancel_rebate_shop, State) ->
    ?INFO("cancel_rebate_shop"),
    erlang:put(rebate_shop_stop_time, 0),
    erlang:put(rebate_shop, []),
    {noreply, State};

handle_info(_Info, State) ->
    ?INFO("undefined info:~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% ?INFO("Save ETS Data:~n~p~n", [ets:tab2list(?MODULE)]),
    %% ets:tab2file(?MODULE, ?ETS_FILE_NAME),
    ok.

code_change(OldVsn, State, Extra) ->
    ?INFO("OldVsn:~w, Extra:~w", [OldVsn, Extra]),
    {ok, State}.

save_daily_count() ->
    DailyCount = [{K, V} || {{daily_count, K}, V} <- erlang:get()],
    ?INFO("Save DailyCount:~p", [length(DailyCount)]),
    {Date, _} = erlang:localtime(),
    Val = util:term_to_string({Date, DailyCount}),
    case db:execute("update `data` set `val` = ~s where `key` = 'daily_count';", [Val]) of
        0 ->
            case db:get_one("select `key` from `data` where `key` = 'daily_count'") of
                null -> db:execute("insert into `data`(`key`, `val`) value('daily_count', ~s)", [Val]);
                _ -> ok
            end;
        _ -> ok
    end.

init_daily_count() ->
    case db:get_one("select `val` from `data` where `key` = 'daily_count'") of
        null -> ok;
        Val ->
            {Date, _} = erlang:localtime(),
            case util:bitstring_to_term(Val) of
                {Date, DailyCount} ->
                    ?INFO("Init DailyCount:~p", [length(DailyCount)]),
                    F = fun ({K, V}) ->
                            erlang:put({daily_count, K}, V)
                    end,
                    lists:foreach(F, DailyCount);
                {_, _} -> ok;
                undefined -> ok;
                _Other -> 
                    ?INFO("undefined data when init_cache! [~p]", [_Other]),
                    ok
            end
    end.

init_role_events() ->
    case filelib:is_dir(?ETS_DIR) of
        false -> file:make_dir(?ETS_DIR);
        true -> ok
    end,
    case filelib:is_file(?ETS_FILE_NAME) of
        true ->
            ets:file2tab(?ETS_FILE_NAME),
            ?INFO("Init role_events! [size:~w, memory:~w]", [ets:info(role_events, size), ets:info(role_events, memory)]),
            ok;
        false ->
            ets:new(role_events, [{keypos, 1}, named_table, protected, set]),
            ?INFO("New role_events!", []),
            ok
    end.

%% 处理新发布的事件
do_add_event([Rid | Rids], Event) ->
    add_event(Rid, Event),
    do_add_event(Rids, Event);
do_add_event([], _Event) -> ok.

add_event(Rid, {del_fans, FansId}) ->
    case del_event(Rid, {add_fans, FansId}) of
        true -> ok;
        false ->
            case lib_authen:get_online_pid(role_id, Rid) of
                false -> set_event(Rid, {del_fans, FansId});
                Pid -> Pid ! {handle_event, 6001, {del_fans, FansId}}
            end
    end;
add_event(Rid, Event) ->
    case lib_authen:get_online_pid(role_id, Rid) of
        false -> set_event(Rid, Event);
        Pid -> Pid ! {handle_event, 6001, Event}
    end.

get_events(Rid) ->
    case ets:lookup(role_events, Rid) of
        [{_, Es}] -> Es;
        [] -> [];
        Else ->
            ?WARNING("undefined role_events: ~w", [Else]),
            []
    end.

set_event(Rid, Event) ->
    Events = get_events(Rid),
    case lists:member(Event, Events) of
        true -> ok;
        false ->
            Events1 = [Event | Events],
            ets:insert(role_events, {Rid, Events1})
    end.

set_events(Rid, Events) ->
    ets:insert(role_events, {Rid, Events}).

del_event(Rid, Event) ->
    Events = get_events(Rid),
    case lists:member(Event, Events) of
        true ->
            Events1 = lists:delete(Event, Events),
            ets:insert(role_events, {Rid, Events1}),
            true;
        false -> false
    end.

refresh_rebate_shop() ->
    RebateShop = get_all_rebate_shop(),
    Rt = util:rand_element(6, RebateShop),
    ?INFO("refresh_rebate_shop:~p", [Rt]),
    erlang:put(rebate_shop, Rt).

get_rebate_shop() ->
    case erlang:get(rebate_shop) of
        undefined -> [];
        D -> D
    end.

get_all_rebate_shop() ->
    Ids = data_shop:get(ids),
    get_all_rebate_shop(Ids, []).

get_all_rebate_shop([], Reply) -> Reply;
get_all_rebate_shop([Id | Ids], Reply) ->
    case data_shop:get(Id) of
        undefined -> get_all_rebate_shop(Ids, Reply);
        #shop{rebate = Rebate} ->
            Reply1 = case Rebate > 0 andalso Rebate < 10 of
                true -> [[Id] | Reply];
                false -> Reply
            end,
            get_all_rebate_shop(Ids, Reply1)
    end.

get_rebate_shop_rest_time() ->
    case erlang:get(rebate_shop_stop_time) of
        undefined -> 3600 * 72;
        T -> 
            Time = T - util:unixtime(),
            case Time > 0 of
                true -> Time;
                false -> 3600 * 72
            end
    end.

%% zip(L) -> zip(L, []).
%% zip([I | T], Reply) ->
%%     #role_events{
%%         id      = X1
%%         ,lev    = X2
%%         ,events = X3
%%         ,utime  = X4
%%     } = I,
%%     zip(T, [{X1, X2, X3, X4} | Reply]);
%% zip([], Reply) -> Reply.
%% 
%% unzip(L) -> unzip(L, []).
%% unzip([{X1, X2, X3, X4} | T], Reply) ->
%%     I = #role_events{
%%         id      = X1
%%         ,lev    = X2
%%         ,events = X3
%%         ,utime  = X4
%%     },
%%     unzip(T, [I | Reply]);
%% unzip([X | T], Reply) ->
%%     ?INFO(" *** undefined Data:~w", [X]),
%%     unzip(T, Reply);
%% unzip([], Reply) -> Reply.

test(T) ->
    Size = ets:info(role_events, size),
    DT = timer:now_diff(erlang:now(), T) / 1000,
    case DT > 0 of
        false -> ok;
        true ->
            ?INFO("save role_events time:~w, Size:~w", [DT, Size]),
            ok
    end.
