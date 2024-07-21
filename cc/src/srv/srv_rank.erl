%%----------------------------------------------------
%% 排行榜服务
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_rank).
-behaviour(gen_server).
-export([
        start_link/0
    ]
).
-export([init_rank/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("lev.hrl").
-record(state, {
    }
).
 
-define(UPDATE_RANK_TIME, 24 * 60 * 60 * 1000).
%% 排行榜数据
-record(myrank_data,
    {
        id              = 0     %% 角色ID
        ,lev            = 0     %% 等级
        ,name           = <<>>  %% 角色名字
        ,exp_day        = 0
        ,exp_week       = 0
        ,exp_all        = 0
        ,win_day        = 0
        ,win_week       = 0
        ,win_all        = 0
        ,gold_day       = 0
        ,gold_week      = 0
        ,gold_all       = 0
        ,power_all      = 0
        ,trand_exp_day  = 2 %%趋势
        ,trand_exp_week = 2
        ,trand_exp_all  = 2
        ,trand_win_day  = 2
        ,trand_win_week = 2
        ,trand_win_all  = 2
        ,trand_gold_day = 2
        ,trand_gold_week= 2
        ,trand_gold_all = 2
        ,trand_power_all= 2
        ,had_accepted_reward = 0 %% 奖励 0未领，1已领
    }
).

%% --- 对外接口 ---

%% 新建连接
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --- 服务器内部实现 ---

init([]) ->
    ?INFO("start ~w...~n", [?MODULE]),
    State = #state{},
    init_rank(), %% 数据库初始化,只使用一次
    init_toprank_snapshot(), %% 排名列表 100个 X 9项
    init_myrank_snapshot(),%% 个人排名 5000个 X 9项
    put(rank_data, []), %% 用于保存及更新角色的排名数据：胜利 经验 等级 金币
    {ok, State}.

handle_call(save, _From, State) ->
    save_rank_data(),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% 获取排名列表
handle_info({rank_list, RankType, Type, Rid, PidSender, Lev, Name}, State) ->
    MyRankData  = get_myrank_info(Rid, RankType, Type, Lev, Name),
    Data        = get_rank(top, RankType, Type),
    %% 头部为个人的，尾部为排名列表
    D2          = [MyRankData|Data], 
    lib_conn:pack_send(PidSender, 21001, [D2]),
    {noreply, State};

%% 获取最高排名 用于阶段奖励
handle_info({my_max_rank, Rid, PidSender}, State) ->
    RewardVal = get_accepted_value(Rid),
    MaxRank = get_max_rank(Rid),
    lib_conn:pack_send(PidSender, 21002, [MaxRank, RewardVal]),
    {noreply, State};

%% 领取奖励
handle_info({accept_reward, Rid, Pid, PidSender}, State) ->
    case get_accepted_value(Rid) of
        1 -> 
            %% ?INFO("你已经领了啦，重领无效", []),
            lib_conn:pack_send(PidSender, 21003, [0]);
        0 ->
            MaxRank = get_max_rank(Rid),
            Gold = case get_rank_reward(MaxRank) of
                undefined -> 
                    ?WARNING("undefined rank reward! [Rid:~w, MaxRank:~w]", [Rid, MaxRank]),
                    0;
                G -> G
            end, 
            ok = srv_cache:set_daily_count({rank_reward, Rid}, 1),
            Pid ! {add_attr, gold, Gold, 1008},
            lib_conn:pack_send(PidSender, 21003, [1]),
            lib_conn:pack_send(PidSender, 11020, [1])
    end,
    {noreply, State};

%% 更新排行榜
handle_info(update_day_rank, State) ->
    case save_rank_data() of
        ok ->
            ?INFO("update_day_rank.....", []),
            update_toprank(1, 1),
            update_toprank(2, 1),
            update_toprank(3, 1),
            update_toprank(1, 3),
            update_toprank(2, 3),
            update_toprank(3, 3),
            update_toprank(4, 3),
            update_myrank(1, 1),
            update_myrank(2, 1),
            update_myrank(3, 1),
            update_myrank(1, 3),
            update_myrank(2, 3),
            update_myrank(3, 3),
            update_myrank(4, 3),
            ok;
        _ -> 
            ok
    end,
    {noreply, State};

handle_info(update_week_rank, State) ->
    case save_rank_data() of
        ok ->
            ?INFO("update_week_rank.....", []),
            update_toprank(1, 2),
            update_toprank(2, 2),
            update_toprank(3, 2),
            update_myrank(1, 2),
            update_myrank(2, 2),
            update_myrank(3, 2),
            ok;
        _ -> 
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    ?ERR("Not matched info: ~w", [_Info]),
    {noreply, State}.

%% 新注册用户
handle_cast({reg, Rid, Name}, State) ->
    Sql = "INSERT INTO `rank` (`id`, `name`) VALUES(~s, ~s);",
    case catch db:execute(Sql, [Rid, Name]) of
        {'EXIT', ERROR} -> ?ERR("~p", [ERROR]);
        _ -> ok
    end,
    {noreply, State};

%% set myrank
handle_cast({set_myrank, Rid, Pid}, State) when is_pid(Pid) ->
    MyRank = get_rank_pos(my, 1, 3, Rid),
    Pid ! {handle_event, 6012, [MyRank]},
    {noreply, State};

%% 更新数据 条件：添加经验 累加胜利 添加金币 升级 lib_role,srv_role
%% update_myrank_data(Rid, Exp, Win, Gold)
%% set_myrank_data(Rid, Lev, Exp, Win)

handle_cast({set_myrank, Rid, Lev, Exp, Win, Power}, State) ->
    set_myrank_data(Rid, Lev, Exp, Win, Power),
    {noreply, State};

handle_cast({add_exp, Rid, X}, State) ->
    update_myrank_data(Rid, X, 0, 0),
    {noreply, State};

handle_cast({add_win, Rid, X}, State) ->
    update_myrank_data(Rid, 0, X, 0),
    {noreply, State};

handle_cast({add_gold, Rid, X}, State) ->
    update_myrank_data(Rid, 0, 0, X),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?ERR("Not matched message: ~w", [_Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---

%% 启动服务器，数据初始化到数据库
init_rank() ->
    case db:get_one("select count(*) from rank") > 0 of
        true -> ok;
        false ->
            ?INFO("Init rank!", []),
            Sql = "select id, name, kvs from role",
            case db:get_all(Sql) of
                [L | T] -> init_rank_to_db([L | T]);
                _ -> []
            end
    end.

init_rank_to_db([]) -> {ok};
init_rank_to_db([L | T]) ->
    [Id, Name, KvsBin] = L,
    Kvs = case KvsBin == <<>> of
        true -> [];
        false -> util:bitstring_to_term(KvsBin)
    end,
    Lev = get_val(?lev, 1, Kvs),
    Gold = get_val(?gold, 0, Kvs),
    {Win, _, _, _} = get_val(?game_count, {0, 0, 0, 0}, Kvs),
    Exp1 = get_val(?exp, 0, Kvs),
    ExpSum = case data_lev:get(Lev) of
        #lev{exp_sum = ExpSum1} -> ExpSum1;
        undefined -> 0
    end,
    Exp = ExpSum + Exp1,
    Sql = lists:concat(["update rank set lev=", Lev, 
            ",exp_all=", Exp,
            ",win_all=", Win,
            ",gold_all=", Gold,
            " where id=", Id]),
    %% ?INFO("Sql:~p", [Sql]), 
    case db:execute(Sql) of
        0 ->
            Sql2 = "select id from rank where id=~s",
            case db:get_one(Sql2, [Id]) of
                null ->
                    Sql3 = "insert into rank (id, lev, name, exp_all, win_all, gold_all)values(~s,~s,~s,~s,~s,~s)",
                    db:execute(Sql3, [Id, Lev, Name, Exp, Win, Gold]);
                _Id ->
                    ok
            end;
        _R -> 
            ok
    end,
    init_rank_to_db(T).

%% 获取奖励区间
get_rank_reward(Rank) ->
    RangeList = data_rank_reward:get(range),
    case get_range(RangeList, Rank) of
        {ok, Range} -> data_rank_reward:get(Range);
        _ -> undefined
    end.

get_range([], _) ->
    undefined;
get_range([L|T], Rank) ->
    %% ?INFO("L:~p", [L]),
    [V1, V2] = L,
    case Rank >= V1 andalso Rank =< V2 of
        true -> {ok, L};
        false -> get_range(T, Rank)
    end.

%% 获取是否已领取奖励的值
get_accepted_value(Rid) ->
    case catch srv_cache:get_daily_count({rank_reward, Rid}) of
        {'EXIT', _} -> 1;
        Reply -> Reply
    end.

%% 获取九种排名中的最高排名最小值，即排在最前 0为没排名
get_max_rank(Rid) ->
    L = [
        get_rank_pos(my, 1, 1, Rid),
        get_rank_pos(my, 1, 2, Rid),
        get_rank_pos(my, 1, 3, Rid),
        get_rank_pos(my, 2, 1, Rid),
        get_rank_pos(my, 2, 2, Rid),
        get_rank_pos(my, 2, 3, Rid),
        get_rank_pos(my, 3, 1, Rid),
        get_rank_pos(my, 3, 2, Rid),
        get_rank_pos(my, 3, 3, Rid)
    ],
    L2 = lists:filter(fun(X) -> X > 0 end, L),
    case L2=:=[] of
        true -> 9999;
        false -> lists:min(L2)
    end.

%% 更新角色信息中的数据 从进程字典中
set_myrank_data(Rid, Lev, Exp, Win, Power) ->
    RankData = get(rank_data),
    ExpSum = case data_lev:get(Lev) of
        #lev{exp_sum = ExpSum1} -> ExpSum1 + Exp;
        undefined -> Exp
    end,
    R1 = case lists:keyfind(Rid, 2, RankData) of
        false -> 
            #myrank_data{
                id           = Rid
                ,lev         = Lev
                ,exp_all     = ExpSum
                ,win_all     = Win
                ,power_all   = Power
            };
        R -> 
            R#myrank_data{
                lev          = Lev
                ,exp_all     = ExpSum
                ,win_all     = Win
                ,power_all   = Power
            }
    end,
    NewRankData = lists:keystore(Rid, 2, RankData, R1),
    put(rank_data, NewRankData),
    ok.

update_myrank_data(Rid, Exp, Win, Gold) ->
    RankData = get(rank_data),
    R1 = case lists:keyfind(Rid, 2, RankData) of
        false -> 
            #myrank_data{
                id           = Rid
                ,exp_day     = Exp
                ,exp_week    = Exp
                ,win_day     = Win
                ,win_week    = Win
                ,gold_day    = Gold
                ,gold_week   = Gold
                ,gold_all    = Gold
            };
        R -> 
            R#myrank_data{
                exp_day      = R#myrank_data.exp_day  + Exp
                ,exp_week    = R#myrank_data.exp_week + Exp
                ,win_day     = R#myrank_data.win_day  + Win
                ,win_week    = R#myrank_data.win_week + Win
                ,gold_day    = R#myrank_data.gold_day + Gold
                ,gold_week   = R#myrank_data.gold_week+ Gold
                ,gold_all    = R#myrank_data.gold_all + Gold
            }
    end,
    NewRankData = lists:keystore(Rid, 2, RankData, R1),
    put(rank_data, NewRankData),
    ok.

%% 添加排名字段
add_rank_pos(Data) ->
    add_rank_pos(Data, 0, []).
add_rank_pos([], _, Rs) ->
    lists:reverse(Rs);
add_rank_pos([R|T], Key, Rs) ->
    Pos = Key + 1,
    add_rank_pos(T, Pos, [[Pos|R]|Rs]).

%% 从进程字典中获取角色的排名信息
get_myrank_info(Rid, Type1, Type2, Lev, Name) ->
    Rank = get_rank(my, Type1, Type2),
    case lists:keyfind(Rid, 2, Rank) of
        false -> {0, Rid, Name, Lev, 0, 2};
        R -> R
    end.

%% 获取排名列表
get_rank(RankType, Type1, Type2) ->
    case get({rank, RankType, Type1, Type2}) of
        undefined -> [];
        RankList -> RankList
    end.

%% 获取排行榜中的名次
get_rank_pos(RankType, Type1, Type2, Rid) ->
    Rank = get_rank(RankType, Type1, Type2),
    case lists:keyfind(Rid, 2, Rank) of
        false -> 0;
        {N, _,_,_,_,_} -> N
    end.

%% 获取排行趋势
get_trend(RankType, Type1, Type2, Rid, Pos) ->
    Rank = get_rank(RankType, Type1, Type2),
    LastPos = case lists:keyfind(Rid, 2, Rank) of
        false -> 100000;
        {N, _,_,_,_,_} -> N
    end,
    if 
        Pos == LastPos -> 2;
        Pos  < LastPos -> 1;
        Pos  > LastPos -> 3
    end.

save_rank_data() ->
    RankData = get(rank_data),
    ?INFO("count save_rank_data:~w~n", [length(RankData)]),
    save_rank_data(RankData).
save_rank_data([]) ->
    put(rank_data, []),
    ok;
save_rank_data([R|T]) ->
    Lev = case R#myrank_data.lev > 0 of
        true -> lists:concat([",lev = ", R#myrank_data.lev]);
        false -> ""
    end,
    ExpAll = case R#myrank_data.exp_all > 0 of
        true -> lists:concat([",exp_all = ", R#myrank_data.exp_all]);
        false -> ""
    end,
    WinAll = case R#myrank_data.win_all > 0 of
        true -> lists:concat([",win_all = ", R#myrank_data.win_all]);
        false -> ""
    end,
    PowerAll = case R#myrank_data.power_all > 0 of
        true -> lists:concat([",power_all = ", R#myrank_data.power_all]);
        false -> ""
    end,
    Sql = lists:concat([
            "update rank set ",
            "exp_day         = exp_day   + ", R#myrank_data.exp_day  ,
            ",exp_week       = exp_week  + ", R#myrank_data.exp_week ,
            ",win_day        = win_day   + ", R#myrank_data.win_day  ,
            ",win_week       = win_week  + ", R#myrank_data.win_week ,
            ",gold_day       = gold_day  + ", R#myrank_data.gold_day ,
            ",gold_week      = gold_week + ", R#myrank_data.gold_week,
            ",gold_all       = gold_all  + ", R#myrank_data.gold_all ,
            Lev, ExpAll, WinAll, PowerAll,
            " where id=", R#myrank_data.id
        ]),
    %% ?INFO("Sql:~s", [Sql]),
    case catch db:execute(Sql) of
        {'EXIT', ERROR} -> 
            ?ERR("~w", [ERROR]),
            put(rank_data, [R|T]),
            ok;
        0 -> 
            %% ?WARNING("Data Unchanged! [Rid:~w]", [R#myrank_data.id]),
            save_rank_data(T);
        _ ->
            save_rank_data(T)
    end.

init_toprank_snapshot() ->
     %% 经验
    put({rank, top,1,1}, get_toprank_snapshot_from_db(1,1)),
    put({rank, top,1,2}, get_toprank_snapshot_from_db(1,2)),
    put({rank, top,1,3}, get_toprank_snapshot_from_db(1,3)),
    %% 胜利
    put({rank, top,2,1}, get_toprank_snapshot_from_db(2,1)),
    put({rank, top,2,2}, get_toprank_snapshot_from_db(2,2)),
    put({rank, top,2,3}, get_toprank_snapshot_from_db(2,3)),
    %% 金币
    put({rank, top,3,1}, get_toprank_snapshot_from_db(3,1)),
    put({rank, top,3,2}, get_toprank_snapshot_from_db(3,2)),
    put({rank, top,3,3}, get_toprank_snapshot_from_db(3,3)),
    %% 战斗力
    put({rank, top,4,3}, get_toprank_snapshot_from_db(4,3)),
    {ok}.

init_myrank_snapshot() ->
    %% 经验
    put({rank, my,1,1}, get_myrank_snapshot_from_db(1,1)),
    put({rank, my,1,2}, get_myrank_snapshot_from_db(1,2)),
    put({rank, my,1,3}, get_myrank_snapshot_from_db(1,3)),
    %% 胜利
    put({rank, my,2,1}, get_myrank_snapshot_from_db(2,1)),
    put({rank, my,2,2}, get_myrank_snapshot_from_db(2,2)),
    put({rank, my,2,3}, get_myrank_snapshot_from_db(2,3)),
    %% 金币
    put({rank, my,3,1}, get_myrank_snapshot_from_db(3,1)),
    put({rank, my,3,2}, get_myrank_snapshot_from_db(3,2)),
    put({rank, my,3,3}, get_myrank_snapshot_from_db(3,3)),
    %% 战斗力
    put({rank, my,4,3}, get_myrank_snapshot_from_db(4,3)),
    {ok}.

get_val(Key, Default, Kvs) ->
    case lists:keyfind(Key, 1, Kvs) of
        false -> Default;
        {_, V} -> V
    end.

%% 读取快照
get_toprank_snapshot_from_db(Type1, Type2) ->
    Sql = "select `rank` from rank_top where type1 = ~s and type2 = ~s order by `time` desc limit 1;",
    case catch db:get_one(Sql, [Type1, Type2]) of
        {'EXIT', _} -> [];
        null -> [];
        Rank -> util:bitstring_to_term(Rank)
    end.

get_myrank_snapshot_from_db(Type1, Type2) ->
    Sql = "select `rank` from rank_my where type1 = ~s and type2 = ~s order by `time` desc limit 1;",
    case catch db:get_one(Sql, [Type1, Type2]) of
        {'EXIT', _} -> [];
        null -> [];
        Rank -> util:bitstring_to_term(Rank)
    end.

%% 更新排行榜
update_toprank(RankType, Type) ->
    %% ?INFO("update_toprank: {~w, ~w}", [RankType, Type]),
    Data = get_toprank_from_db(RankType, Type),
    put({rank, top, RankType, Type}, Data),
    Bin = util:term_to_bitstring(Data),
    Sql = "INSERT INTO `rank_top` (`time`, `day`, `type1`, `type2`, `rank`) VALUES (~s, ~s, ~s, ~s, ~s);",
    Time = util:unixtime(),
    Day = util:get_day_list(now),
    try 
        db:execute(Sql, [Time, Day, RankType, Type, Bin]),
        F = db:execute("delete from `rank_top` where type1 = ~s and type2 = ~s and `time` < ~s;", [RankType, Type, Time]),
        ?INFO("del rank(~w) [~w, ~w]", [F, RankType, Type])
        catch T:X -> ?ERR("[~w:~w]", [T, X]) 
    end.

update_myrank(RankType, Type) ->
    %% ?INFO("update_myrank: {~w, ~w}", [RankType, Type]),
    Data = get_myrank_from_db(RankType, Type),
    put({rank, my, RankType, Type}, Data),
    Bin = util:term_to_bitstring(Data),
    Sql = "INSERT INTO `rank_my` (`time`, `day`, `type1`, `type2`, `rank`) VALUES (~s, ~s, ~s, ~s, ~s);",
    Time = util:unixtime(),
    Day = util:get_day_list(now),
    try 
        db:execute(Sql, [Time, Day, RankType, Type, Bin]),
        F = db:execute("delete from `rank_my` where type1 = ~s and type2 = ~s and `time` < ~s;", [RankType, Type, Time]),
        ?INFO("del rank(~w) [~w, ~w]", [F, RankType, Type])
        catch T:X -> ?ERR("[~w:~w]", [T, X]) 
    end.

get_toprank_from_db(Type1, Type2) ->
    case get_rank_type(Type1, Type2) of
        undefined -> [];
        Var ->
            Sql = lists:concat(["select id, name, lev, ", Var, " from rank where ", Var, " > 0 order by ", Var, " desc limit 0, 100"]),
            %% ?INFO("Sql:~p",[Sql]),
            case db:get_all(Sql) of
                [] -> [];
                Data ->
                    Data1 = add_rank_pos(Data), 
                    F = fun([Pos, Id, Name, Lev, Rank]) ->
                            Trend = get_trend(top, Type1, Type2, Id, Pos),
                            {Pos, Id, Name, Lev, Rank, Trend}
                    end,
                    [F(X) || X <- Data1]
            end
    end.

get_myrank_from_db(Type1, Type2) ->
    case get_rank_type(Type1, Type2) of
        undefined -> [];
        Var ->
            Sql = lists:concat(["select id, name, lev, ", Var, " from rank order by ", Var, " desc limit 0, 5000"]),
            %% ?INFO("Sql:~p",[Sql]),
            case db:get_all(Sql) of
                [] -> [];
                Data ->
                    Data1 = add_rank_pos(Data), 
                    F = fun([Pos, Id, Name, Lev, Rank]) ->
                            Trend = get_trend(my, Type1, Type2, Id, Pos),
                            {Pos, Id, Name, Lev, Rank, Trend}
                    end,
                    [F(X) || X <- Data1]
            end
    end.

get_rank_type(RankType, Type) ->
    case {RankType, Type} of
        {1,1} -> exp_day;
        {1,2} -> exp_week;
        {1,3} -> exp_all;
        {2,1} -> win_day;
        {2,2} -> win_week;
        {2,3} -> win_all;
        {3,1} -> gold_day;
        {3,2} -> gold_week;
        {3,3} -> gold_all;
        {4,3} -> power_all;
        {_,_} -> undefined
    end.
