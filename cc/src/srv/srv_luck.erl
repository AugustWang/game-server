%%----------------------------------------------------
%% 排行榜服务
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_luck).
-behaviour(gen_server).
-export([
        start_link/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-record(state, {
    }
).
 
%% 排行榜数据
-record(myrank_data,
    {
        id              = 0     %% 角色ID
        ,name           = <<>>  %% 角色名字
        ,use_sum        = 0
        ,val_sum        = 0
        ,reward_id      = 0
        ,reward_num     = 0
        ,ctime          = 0
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
    set_rank_from_db(),
    set_recent_from_db(),
    put(rank_data, []),
    {ok, State}.

handle_call(save, _From, State) ->
    permanent_stores(),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% 获取排名列表
handle_info({rank_list, PidSender}, State) ->
    Data = get(rank),
    lib_conn:pack_send(PidSender, 17132, [Data]),
    {noreply, State};

handle_info({recent_list, CardSum, UseSum, ValSum, PidSender}, State) ->
    Data = get(recent),
    lib_conn:pack_send(PidSender, 17131, [CardSum, UseSum, ValSum, Data]),
    %% ?INFO("17131:~w", [[CardSum, UseSum, ValSum, Data]]),
    {noreply, State};

%% 更新排行榜
handle_info(update_rank, State) ->
    case permanent_stores() of
        ok ->
            ?INFO("update_luck_rank.....", []),
            set_rank_from_db(),
            ok;
        _ -> 
            ok
    end,
    {noreply, State};

handle_info(test, State) ->
    ?INFO("DICT:~p", [get()]),
    {noreply, State};

handle_info(_Info, State) ->
    ?ERR("Not matched info: ~w", [_Info]),
    {noreply, State}.

handle_cast({set_myrank, Rid, Name, Val, _CardSum, UseSum, ValSum, RewardId, RewardNum}, State) ->
    %% ?INFO("set_myrank:~w", [[Rid, _CardSum, UseSum, ValSum, RewardId, RewardNum]]),
    set_myrank_data(Rid, Name, Val, UseSum, ValSum, RewardId, RewardNum),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?ERR("Not matched message: ~w", [_Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---

%% 更新角色信息中的数据 从进程字典中
set_myrank_data(Rid, Name, Val, UseSum, ValSum, RewardId, RewardNum) ->
    RankData = get(rank_data),
    Now = util:unixtime(),
    R1 = case lists:keyfind(Rid, 2, RankData) of
        false -> 
            #myrank_data{
                id              = Rid
                ,name           = Name
                ,use_sum        = UseSum
                ,val_sum        = ValSum
                ,reward_id      = RewardId
                ,reward_num     = RewardNum
                ,ctime          = Now
            };
        R -> 
            R#myrank_data{
                name            = Name
                ,use_sum        = UseSum
                ,val_sum        = ValSum
                ,reward_id      = RewardId
                ,reward_num     = RewardNum
                ,ctime          = Now
            }
    end,
    NewRankData = lists:keystore(Rid, 2, RankData, R1),
    put(rank_data, NewRankData),
    case Val >= 500 of
        true ->
            Recent = get(recent),
            Recent1 = lists:sublist(Recent, 1, 19),
            Recent2 = [[Rid, Name, RewardId, RewardNum] | Recent1],
            put(recent, Recent2),
            ok;
        false -> ok
    end.

%% 添加排名字段
%% add_rank_pos(Data) ->
%%     add_rank_pos(Data, 0, []).
%% add_rank_pos([], _, Rs) ->
%%     lists:reverse(Rs);
%% add_rank_pos([R|T], Key, Rs) ->
%%     Pos = Key + 1,
%%     add_rank_pos(T, Pos, [[Pos|R]|Rs]).

permanent_stores() ->
    RankData = get(rank_data),
    ?INFO("count luck rank_data:~w~n", [length(RankData)]),
    permanent_stores(RankData).

permanent_stores([]) ->
    put(rank_data, []),
    ok;
permanent_stores([R|T]) ->
    #myrank_data{
        id              = Id
        ,name           = Name
        ,use_sum        = UseSum
        ,val_sum        = ValSum
        ,reward_id      = RewardId
        ,reward_num     = RewardNum
        ,ctime          = Ctime
    } = R,
    Sql = "UPDATE `rank_luck` SET `use_sum` = ~s,"
    " `val_sum` = ~s, `reward_id` = ~s, `reward_num` = ~s,"
    " `ctime` = ~s WHERE `id` =  ~s;",
    case db:execute(Sql, [UseSum, ValSum, RewardId, RewardNum, Ctime, Id]) of
        {error, _Reason} -> 
            put(rank_data, [R|T]),
            error;
        0 -> 
            Sql2 = "INSERT INTO `rank_luck` "
            "(`id`, `name`, `use_sum`, `val_sum`, `reward_id`, `reward_num`, `ctime`) "
            "VALUES (~s, ~s, ~s, ~s, ~s, ~s, ~s);",
            db:execute(Sql2, [Id, Name, UseSum, ValSum, RewardId, RewardNum, Ctime]),
            permanent_stores(T);
        _ ->
            permanent_stores(T)
    end.

set_rank_from_db() ->
    Sql = lists:concat(["select "
            "id, name, use_sum, val_sum "
            "from rank_luck order by val_sum desc limit 0, 100"]),
    %% ?INFO("Sql:~p",[Sql]),
    Data = db:get_all(Sql),
    %% Data1 = add_rank_pos(Data), 
    put(rank, Data).

set_recent_from_db() ->
    Sql = lists:concat(["select "
            "id, name, reward_id, reward_num "
            "from rank_luck order by ctime desc limit 0, 20"]),
    %% ?INFO("Sql:~p",[Sql]),
    Data = db:get_all(Sql),
    put(recent, Data).
