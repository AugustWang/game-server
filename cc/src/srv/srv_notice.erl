%%----------------------------------------------------
%% 统计服务
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_notice).
-behaviour(gen_server).
-export([
        start_link/0 
        ,send_notice/1 
        ,update_notice/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

-record(state, {
        notices = []
    }).

%% --- 对外接口 ---------------------------------

start_link() ->
    ?INFO("start ~w...", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_notice(PidSender) ->
    gen_server:cast(srv_notice, {send_notice, PidSender}).

update_notice() ->
    srv_notice ! update_notice.

%% --- 服务器内部实现 ---------------------------------

init([]) ->
    State = #state{},
    %% update_notice
    erlang:send_after(10 * 1000, self(), update_notice),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({send_notice, PidSender}, State) ->
    case get_notice(State) of
        [] -> ok;
        Notice -> lib_conn:pack_send(PidSender, 15005, [Notice])
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_notice, State) ->
    case get_notice() of
        [] -> {noreply, State};
        Notices ->
            State1 = State#state{notices = Notices},
            Notice = get_notice(State),
            lib_conn:pack_cast(world, 15005, [Notice]),
            {noreply, State1}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    ?INFO("OldVsn:~w, Extra:~w", [OldVsn, Extra]),
    {ok, State}.

get_notice() ->
    try
        Now = util:unixtime(),
        Data = db:get_all("SELECT `id`, `title`, `start_time`, `expired_time` FROM `sys_notices` WHERE `expired_time` > ~s;", [Now]),
        %% ?INFO("update_notice, count:~w", [length(Data)]),
        [{Id, Msg, StartTime, EndTime} || [Id, Msg, StartTime, EndTime] <- Data]
        catch T:X ->
            ?ERR("~w:~w", [T, X]),
            []
    end.

get_notice(State) ->
    Now = util:unixtime(),
    [{Id, Msg} || {Id, Msg, S, E} <- State#state.notices, Now > S, Now < E].
