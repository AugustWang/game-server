%%----------------------------------------------------
%% cc 日志处理器
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(cc_logger).
-export([start_link/0, logfile/1, notify/5, log/1]).
-export([
    init/1, 
    handle_event/2, handle_call/2, handle_info/2, 
    terminate/2, code_change/3
]).

-record(state, {
        debug = off %% off | on
    }).

%% Log Type Define

%% 1001	战斗奖励
%% 1002	在线奖励
%% 1003	连续登陆奖励
%% 1004	邀请好友奖励
%% 1005	轮盘抽奖
%% 1006	轮盘抽奖返利
%% 1007	使用物品
%% 1008	名人堂奖励
%% 1009	任务奖励
%% 1010	出售物品
%% 1011	翻牌获得
%% 1012	后台赠送
%% 1013	玩家充值

%% 2001	购买物品
%% 2002	创建公会
%% 2003	强化装备消耗
%% 2004	合成宝石消耗
%% 2005	装备转移消耗
%% 2006	鉴定物品消耗
%% 2007	物品续费消耗
%% 2008	翻牌消耗
%% 2009	购买道具

start_link() ->
    case gen_event:start_link({local, ?MODULE}) of
        {ok, Pid} ->
            ok = gen_event:add_handler(Pid, ?MODULE, []),
            {ok, Pid};
        Err -> Err
    end.

logfile({open, File}) ->
    case lists:member(cc_logger_h, gen_event:which_handlers(cc_logger)) of
        true -> {error, allready_have_logfile};
        _ -> gen_event:add_handler(cc_logger, cc_logger_h, File)
    end;
logfile(close) ->
    case gen_event:delete_handler(cc_logger, cc_logger_h, normal) of
        {error,Reason} -> {error,Reason};
        _ -> ok
    end;
logfile(filename) ->
    case gen_event:call(cc_logger, cc_logger_h, filename) of
        {error,_} -> {error, no_log_file};
        Val -> Val
    end.

notify(Type, F, A, Module, Line) ->
    case catch erlang:iolist_to_binary(io_lib:format(F, A)) of
        {'EXIT', Reason} -> 
            error_logger:format(" ** ~p~n", [Reason]);
        Msg when Type =:= error -> %% 需要特殊格式的类型
            Event = try erlang:error(undef) catch
                error:undef ->
                    Format = " ** Tips == ~ts~n ** Stacktrace == ~p~n",
                    Arg = [Msg, tl(erlang:get_stacktrace())],
                    {Type, group_leader(), self(), Format, Arg, Module, Line}
            end,
            gen_event:notify(?MODULE, Event);
        Msg -> 
            Event = {Type, group_leader(), self(), "~ts~n", [Msg], Module, Line},
            gen_event:notify(?MODULE, Event)
    end.

log(Data) ->
    gen_event:notify(?MODULE, {log, Data}).

%% -------------- gen_event callback ------------------

init(_) -> {ok, #state{}}.

handle_event({error, _GL, Pid, Format, Arg, Module, Line}, State) -> 
    io_format("~n~s ####~n ** Node == ~p~n" ++ Format
        ,[add_header(error, Pid, Module, Line), node(Pid)] ++ Arg
    ),
    {ok, State};

handle_event({debug, _GL, Pid, Format, Arg, Module, Line}, State) -> 
    case State#state.debug of
        on ->
            io_format("~s " ++ Format
                ,[add_header(debug, Pid, Module, Line) | Arg]
            );
        _ -> skip
    end,
    {ok, State};

handle_event({Type, _GL, Pid, Format, Arg, Module, Line}, State) -> 
    io_format("~s " ++ Format
        ,[add_header(Type, Pid, Module, Line) | Arg]
    ),
    {ok, State};

handle_event({debug, Debug}, State) -> 
    {ok, State#state{debug = Debug}};

handle_event({log, _}, State) -> 
    {ok, State};

handle_event(Event, State) -> 
    error_logger:warning_msg(
        " ** process == ~p received an unexpected event == ~w~n"
        " ** state == ~p~n"
        ,[?MODULE, Event, State]),
    {ok, State}.

handle_call(_Request, State) -> {ok, error, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason , _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ------------ peivate functions ---------------------

io_format(F, A) ->
    try io:format(F, A) catch
        T:W -> 
            error_logger:format(
                " ** ~p:~p~n ** ~p~n"
                ,[T, W, erlang:get_stacktrace()]
            )
    end.
        
add_header(Type, Pid, Module, Line) ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:localtime(),
    io_lib:format(
        "## ~p ~p-~p-~p ~s:~s:~s[~p:~p] ~p"
		,[Type, Y, Mo, D, t(H), t(Mi), t(S), Module, Line, Pid]
    ).

t(X) when is_integer(X) ->
    t1(integer_to_list(X));
t(_) -> "".

t1([X]) -> [$0,X];
t1(X)   -> X.
