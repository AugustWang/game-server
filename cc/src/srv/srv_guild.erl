%%----------------------------------------------------
%% Guild
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_guild).
-behaviour(gen_server).
-export([
        start_link/0
        ,save/0
        ,save2/0
        ,start_act/1
        ,fix_invoke/3
        ,permanent_stores2/1
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").

-record(state, {
        guilds = []
        ,act_status = 0 %% act status: 0=no act, 1=in act
    }).

-record(guild, {
        id
        ,name
        ,creater_id
        ,creater_name
        ,roles
        ,roles_num =  0 %% 人数
        ,roles_max = 40 %% 人数上限
        ,exp = 0 %% 帮会经验
        ,lev = 1
        ,change = 0 %% 帮会数据是否有改变
    }).
 
-record(irole, {
        id
        ,name
        ,lev
        ,auth %% 0=申请中, 1=普通会员, 10=会长
        ,login_time = 0
        ,sex = 1
        ,v = 0 %% guild_v
        ,is_online = 0
    }).

%% DICT INFO
%% max_id: 当前最大的工会ID

%% 新建连接
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

save() ->
    gen_server:call(srv_guild, save_guild_to_db).

save2() ->
    gen_server:cast(srv_guild, save_guild_to_db2).

%% Time 单位：分钟
start_act(Time) ->
    gen_server:cast(srv_guild, {start_act, Time}).

%% --- 服务器内部实现 ---

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    put(max_id, get_max_id()),
    Guilds = init_guild(),
    State = #state{
        guilds = Guilds
    },
    {ok, State}.

%% STOP
handle_call(save_guild_to_db, _From, State) ->
    del_guild_from_db(State#state.guilds),
    Reply = permanent_stores(State#state.guilds),
    {reply, Reply, State};

handle_call({buy_benefit_check, Gid, Rid, Bid}, _From, State) ->
    Reply = case g(Gid, State) of
        false -> 
            ?WARNING("Guild not found when buy_benefit! 
                [Gid:~w]", [Gid]),
            false;
        G ->
            case i(Rid, G) of
                false ->
                    ?WARNING("Role not found when buy_benefit! 
                        [Gid:~w, Rid:~w]", [Gid, Rid]),
                    false;
                _I -> do_buy_benefit_check(G, Bid)
            end
    end,
    {reply, Reply, State};

%% 创建
handle_call({create, Rid, Name, Sex, Lev, GuildName}, _From, State) ->
    case creater_check(Rid, GuildName, State) of
        ok ->
            Gid = get_new_id(),
            I = #irole{
                id = Rid
                ,name = Name
                ,lev = Lev
                ,auth = 10
                ,login_time = util:unixtime()
                ,sex = Sex
            },
            Guild = #guild{
                id = Gid
                ,name = GuildName
                ,creater_id = Rid
                ,creater_name = Name
                ,roles_num = 1
                ,roles_max = 40
                ,roles = [I]
            },
            Roles = util:term_to_bitstring(Guild#guild.roles),
            db:execute("INSERT INTO `guild`(`id`, `name`, `creater_id`, `creater_name`, `roles_num`, `roles_max`, `roles`) VALUES (~s, ~s, ~s, ~s, ~s, ~s, ~s)", 
                [Gid, GuildName, Rid, Name, 1, Guild#guild.roles_max, Roles]),
            Guilds1 = [Guild | State#state.guilds],
            State1 = del_apply(Rid, State), %% 删除申请记录
            State2 = State1#state{guilds = Guilds1},
            {reply, {ok, Gid}, State2};
        {error, error_name} -> {reply, {error, error_name}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({role_login, Gid, Rid, Lev, Sex, PidSender}, State) ->
    State1 = case g(Gid, State) of
        false -> 
            set_guild(Rid, 0, <<>>),
            State;
        G ->
            case i(Rid, G) of
                false -> State;
                I ->
                    case State#state.act_status of
                        0 -> ok;
                        1 -> lib_conn:pack_send(PidSender, 22020, [])
                    end,
                    I1 = I#irole{
                        lev = Lev
                        ,login_time = util:unixtime()
                        ,sex = Sex
                        ,is_online = 1
                    },
                    si(I1, G, State)
            end
    end,
    {noreply, State1};

handle_cast({role_logout, Gid, Rid, Lev}, State) ->
    State1 = case g(Gid, State) of
        false -> 
            set_guild(Rid, 0, <<>>),
            State;
        G ->
            case i(Rid, G) of
                false -> State;
                I ->
                    I1 = I#irole{
                        lev = Lev
                        ,is_online = 0
                    },
                    si(I1, G, State)
            end
    end,
    {noreply, State1};

%% 工会列表
handle_cast({guild_list, PidSender}, State) ->
    Data = [[Id, Name, Rid, Rname, Num, Exp, Lev] || #guild{
            id = Id
            ,name = Name
            ,creater_id = Rid
            ,creater_name = Rname
            ,roles_num = Num
            ,roles_max = Max
            ,exp = Exp
            ,lev = Lev
        } <- State#state.guilds, Num < Max],
    lib_conn:pack_send(PidSender, 22005, [Data]),
    {noreply, State};

%% 成员列表
handle_cast({role_list, Gid, PidSender}, State) ->
    case g(Gid, State) of
        false ->
            ?WARNING("Guild not found: ~w", [Gid]),
            ok;
        G ->
            Now = util:unixtime(),
            Data = [
                [Id, Name, Lev, Auth, Now - Time, IsOnline, Sex, V] || #irole{
                    id = Id
                    ,name = Name
                    ,lev = Lev
                    ,auth = Auth
                    ,login_time = Time
                    ,sex = Sex
                    ,v = V
                    ,is_online = IsOnline
                } <- G#guild.roles],
            lib_conn:pack_send(PidSender, 22006, [Data, G#guild.exp, G#guild.lev])
    end,
    {noreply, State};

%% 申请加入
handle_cast({join, Rid, Name, Sex, Lev, Gid, PidSender}, State) ->
    I = #irole{
        id = Rid
        ,name = Name
        ,lev = Lev
        ,auth = 0
        ,login_time = util:unixtime()
        ,sex = Sex
    },
    case join_guild(I, Gid, State) of
        {ok, State1} -> 
            %% ?INFO("join, Rid:~w, Gid:~w", [Rid, Gid]),
            lib_conn:pack_cast(guild, Gid, 11020, [4]),
            lib_conn:pack_send(PidSender, 22008, [0]),
            {noreply, State1};
        {error, already_exists} ->
            lib_conn:pack_send(PidSender, 22008, [3]),
            {noreply, State};
        {error, at_app_full} ->
            lib_conn:pack_send(PidSender, 22008, [5]),
            {noreply, State};
        {error, at_full} ->
            lib_conn:pack_send(PidSender, 22008, [4]),
            {noreply, State};
        {error, Error} ->
            lib_conn:pack_send(PidSender, 22008, [2]),
            ?WARNING("Error when join_guild: ~w, Gid:~w", [Error, Gid]),
            {noreply, State}
    end;

%% 设置权限
handle_cast({chmod, Cid, Rid, Gid, Auth, PidS}, State) ->
    case chmod(Cid, Rid, Gid, State, Auth) of
        {ok, State1} -> 
            lib_conn:send_code(PidS, 22011001),
            lib_conn:pack_cast(guild, Gid, 11020, [4]),
            {noreply, State1};
        {error, Error} ->
            ?WARNING("Error when set_auth: ~w", [Error]),
            {noreply, State}
    end;

%% 审核申请
handle_cast({join_process, Cid, Rid, Gid, Status}, State) ->
    case join_process(Cid, Rid, Gid, State, Status) of
        {ok, State1, GuildName} -> 
            set_guild(Rid, Gid, GuildName),
            lib_conn:pack_cast(guild, Gid, 11020, [4]),
            {noreply, State1};
        {ok, State1} -> 
            {noreply, State1};
        {error, Error} ->
            ?WARNING("Error when join_process: ~w", [Error]),
            {noreply, State}
    end;

%% 退出工会
handle_cast({exit, Cid, Rid, Gid, PidSender}, State) ->
    case exit_guild(Cid, Rid, Gid, State) of
        {ok, State1} -> 
            lib_conn:pack_send(PidSender, 22009, [0, Rid]),
            set_guild(Rid, 0, <<>>),
            {noreply, State1};
        {error, error_rid} ->
            case Cid of
                Rid -> set_guild(Rid, 0, <<>>);
                _ -> ok
            end,
            lib_conn:pack_send(PidSender, 22009, [0, Rid]),
            {noreply, State};
        {error, Error} ->
            ?WARNING("Error when exit_guild: ~w", [Error]),
            lib_conn:pack_send(PidSender, 22009, [2, Rid]),
            {noreply, State}
    end;

handle_cast({add_exp, Gid, Rid, GuildV}, State) ->
    State1 = case g(Gid, State) of
        false -> State;
        G ->
            case i(Rid, G) of
                false -> State;
                I ->
                    G1 = add_guild_exp(GuildV, G),
                    I2 = I#irole{v = I#irole.v + GuildV},
                    si(I2, G1, State)
            end
    end,
    {noreply, State1};

handle_cast({add_guild_v, Gid, Rid, GuildV}, State) ->
    State1 = case g(Gid, State) of
        false -> 
            ?WARNING("Guild not found when reward! [Gid:~w]", [Gid]),
            State;
        G ->
            case i(Rid, G) of
                false ->
                    ?WARNING("Role not found when reward! [Gid:~w, Rid:~w]", [Gid, Rid]),
                    set_guild(Rid, 0, <<>>),
                    State;
                I ->
                    G1 = add_guild_exp(GuildV, G),
                    I2 = I#irole{v = I#irole.v + GuildV},
                    si(I2, G1, State)
            end
    end,
    {noreply, State1};

handle_cast({start_act, Time}, State) ->
    State1 = case State#state.act_status of
        0 -> 
            erlang:send_after(
                Time * 60 * 1000
                ,self()
                ,stop_act
            ),
            ?INFO("Guild act start(~w).", [Time]),
            %% lib_conn:pack_cast(world,
            %%     15002, [<<"公会战已开启！">>]),
            cc_env:set(guild_act, true),
            lib_conn:pack_cast(world, 22020, []),
            State#state{act_status = 1};
        1 -> 
            ?WARNING("start_act when act_status=1", []),
            State
    end,
    {noreply, State1};

handle_cast(save_guild_to_db2, State) ->
    spawn(srv_guild, permanent_stores2, [State#state.guilds]),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?ERR("Not matched message: ~w", [_Msg]),
    {noreply, State}.

handle_info(fix_guild, State) ->
    F = fun(G) ->
            Num = 40 + (G#guild.lev - 1) * 10,
            ?INFO("ID:~w Lev:~w (~w -> ~w)", [G#guild.id, G#guild.lev, G#guild.roles_max, Num]),
            G#guild{roles_max = Num}
    end,
    Guilds = [F(G) || G <- State#state.guilds],
    State1 = State#state{guilds = Guilds},
    {noreply, State1};

handle_info(stop_act, State) ->
    State1 = case State#state.act_status of
        0 -> 
            ?WARNING("stop_act when act_status=0", []),
            State;
        1 -> 
            cc_env:del(guild_act),
            ?INFO("Guild act stop.", []),
            %% lib_conn:pack_cast(world,
            %%     15002, [<<"公会战结束">>]),
            State#state{act_status = 0}
    end,
    {noreply, State1};

handle_info(test, State) ->
    io:format("."),
    db:get_all("SELECT * from role where id = 1"),
    %% D = case get('$d') of
    %%     undefined -> [State];
    %%     DD -> DD
    %% end,
    %% put('$d', D ++ D),
    %% {_, Memory} = erlang:process_info(self(), memory),
    %% ?INFO("Memory: ~wM Guilds: ~w ", [Memory / 1000000, length(State#state.guilds)]),
    erlang:send_after(500, self(), test),
    {noreply, State};

handle_info(info, State) ->
    Memory = erlang:process_info(self(), memory),
    ?INFO("
        Memory: ~p~n
        Guilds: ~p~n
        ", [
        Memory
        ,length(State#state.guilds)
    ]),
    {noreply, State};

handle_info(fix, State) ->
    catch spawn(fun() -> fix(State#state.guilds) end),
    {noreply, State};

handle_info(_Info, State) ->
    ?ERR("Not matched info: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---

%% 私有函数缩写说明：
%% i = get irole
%% g = get guild
%% si = set irole
%% sg = set guild

get_new_id() -> 
    Id = get(max_id) + 1,
    put(max_id, Id),
    Id.

g(Gid, State) ->
    lists:keyfind(Gid, 2, State#state.guilds).

sg(Guild, State) ->
    L = lists:keystore(Guild#guild.id, 2, State#state.guilds, Guild),
    State#state{guilds = L}.

dg(Gid, State) ->
    L = lists:keydelete(Gid, 2, State#state.guilds),
    State#state{guilds = L}.

i(_Rid, false) -> false;
i(Rid, Guild) ->
    lists:keyfind(Rid, 2, Guild#guild.roles).

%% i(Rid, Gid, State) ->
%%     Guild = g(Gid, State),
%%     i(Rid, Guild).

si(Role, Guild) ->
    L = lists:keystore(Role#irole.id, 2, Guild#guild.roles, Role),
    Guild#guild{roles = L}.

si(Role, Guild, State) ->
    L = lists:keystore(Role#irole.id, 2, Guild#guild.roles, Role),
    Guild1 = Guild#guild{roles = L},
    sg(Guild1, State).

di(Rid, Guild) ->
    L = lists:keydelete(Rid, 2, Guild#guild.roles),
    Guild#guild{roles = L}.

di(Rid, Guild, State) ->
    L = lists:keydelete(Rid, 2, Guild#guild.roles),
    Guild1 = Guild#guild{roles = L},
    sg(Guild1, State).

%% 删除申请记录
del_apply(Rid, State) ->
    Guilds = del_apply(Rid, State#state.guilds, []),
    State#state{guilds = Guilds}.

del_apply(Rid, [G | T], Reply) ->
    Reply1 = case i(Rid, G) of
        false -> [G | Reply];
        I when I#irole.auth == 0 ->
            G1 = di(I#irole.id, G),
            [G1 | Reply];
        _ -> [G | Reply]
    end,
    del_apply(Rid, T, Reply1);
del_apply(_, [], Reply) -> Reply.

%% 获取工会人数 
count_member(G) ->
    length([I || I <- G#guild.roles, I#irole.auth > 0]).

%%' 申请加入工会
join_guild(I, Gid, State) ->
    join_guild1(I, g(Gid, State), State).

join_guild1(_I, false, _State) -> {error, error_gid};
join_guild1(_I, Guild, _State) 
when Guild#guild.roles_num >= Guild#guild.roles_max ->
    {error, at_full};
join_guild1(I, Guild, State) ->
    case length(Guild#guild.roles) >= (Guild#guild.roles_max + 20) of
        true -> {error, at_app_full};
        false ->
            join_guild2(I, i(I#irole.id, Guild), Guild, State)
    end.

join_guild2(I, false, Guild, State) ->
    Guild1 = si(I, Guild),
    State1 = sg(Guild1, State),
    {ok, State1};
join_guild2(_I, _I2, _Guild, _State) -> {error, already_exists}.
%%.

%%' 处理申请
join_process(Cid, Rid, Gid, State, Status) ->
    Guild = g(Gid, State),
    case find_myguild(Rid, State) of
        false -> join_process1(Cid, Rid, Guild, State, Status);
        _ -> join_process1(Cid, Rid, Guild, State, 0)
    end.

join_process1(_Cid, _Rid, false, _State, _Status) -> {error, error_gid};
%% join_process1(_Cid, _Rid, Guild, _State, _Status) 
%% when Guild#guild.roles_num >= Guild#guild.roles_max -> {error, full};
join_process1(Cid, Rid, Guild, State, Status) when Cid == Guild#guild.creater_id ->
    I = i(Rid, Guild),
    Status1 = case Guild#guild.roles_num >= Guild#guild.roles_max of
        true -> 0;
        false -> Status
    end,
    join_process2(I, Guild, State, Status1);
join_process1(_Cid, _Rid, _Guild, _State, _Status) -> {error, no_auth}.

join_process2(false, _Guild, _State, _Status) -> {error, error_rid};
join_process2(I, Guild, State, 1) when I#irole.auth == 0 ->
    %% 同意，并删除在其它帮会中的申请记录
    I1 = I#irole{auth = 1, is_online = lib_role:is_online(I#irole.id)},
    Num = count_member(Guild) + 1,
    Guild1 = Guild#guild{roles_num = Num, change = 1},
    State1 = si(I1, Guild1, State),
    State2 = del_apply(I#irole.id, State1),
    {ok, State2, Guild#guild.name};
join_process2(I, Guild, State, 0) when I#irole.auth == 0 ->
    %% 拒绝
    State1 = di(I#irole.id, Guild, State),
    {ok, State1};
join_process2(_I, _Guild, _State, _Status) -> {error, already_processed}.
%%.

%%' 处理权限
chmod(Cid, Rid, Gid, State, Auth) ->
    G = g(Gid, State),
    I1 = i(Cid, G),
    I2 = i(Rid, G),
    if
        I1 == false ->
            {error, error_cid};
        I2 == false ->
            {error, error_rid};
        true ->
            chmod1(I1, I2, G, State, Auth)
    end.

chmod1(I1, _I2, _G, _State, Auth) 
when Auth == 10 andalso I1#irole.auth =/= 10 ->
    {error, no_auth};

chmod1(_I1, I2, _G, _State, Auth) 
when Auth == 10 andalso I2#irole.auth == 0 ->
    {error, error_rid};

%% 转让会长
chmod1(I1, I2, G, State, Auth) when Auth == 10 ->
    G1 = G#guild{
        creater_id = I2#irole.id
        ,creater_name = I2#irole.name
    },
    I11 = I1#irole{auth = 1},
    I22 = I2#irole{auth = Auth},
    G2 = si(I11, G1),
    G3 = si(I22, G2),
    State1 = sg(G3, State),
    srv_cache:send_event(I22#irole.id, {?EVENT_SEND_CODE, 22011002}),
    {ok, State1};
chmod1(_I1, _I2, _G, _State, _Auth) ->
    {error, error}.
%%.

%%' 退出工会
exit_guild(Cid, Rid, Gid, State) ->
    Guild = g(Gid, State),
    exit_guild1(Cid, Rid, Guild, State).

%% 找不到工会
exit_guild1(_Cid, _Rid, false, _State) -> {error, error_gid};
%% 自己退出
exit_guild1(Rid, Rid, Guild, State) ->
    I = i(Rid, Guild),
    exit_guild2(I, Guild, State);
%% 踢人
exit_guild1(Cid, Rid, Guild, State) when Cid == Guild#guild.creater_id ->
    I = i(Rid, Guild),
    exit_guild2(I, Guild, State);
%% 权限不足
exit_guild1(_Cid, _Rid, _Guild, _State) -> {error, no_auth}.

%% 找不到成员
exit_guild2(false, _Guild, _State) -> {error, error_rid};
%% 会长退出，解散
exit_guild2(I, G, State) when I#irole.id == G#guild.creater_id ->
    F = fun(I2) ->
            case I2#irole.auth > 0 of
                true -> set_guild(I2#irole.id, 0, <<>>);
                false -> ok
            end
    end,
    lists:foreach(F, G#guild.roles),
    %% ?INFO("解散了1：~w", [State#state.guilds]),
    State1 = dg(G#guild.id, State),
    %% ?INFO("解散了2：~w", [State1#state.guilds]),
    {ok, State1};
%% 会员退出
exit_guild2(I, Guild, State) ->
    %% ?INFO("会员退出：~w", [State#state.guilds]),
    Num = count_member(Guild) - 1,
    Guild1 = Guild#guild{roles_num = Num, change = 1},
    State1 = di(I#irole.id, Guild1, State),
    {ok, State1}.
%%.

init_guild() ->
    case db:get_all("SELECT `id`, `name`, `creater_id`, `creater_name`, `roles_num`, `roles_max`, `exp`, `lev`, `roles` FROM `guild`") of
        [] -> [];
        Rows -> init_guild(Rows, [])
    end.

get_max_id() ->
    case db:get_one("SELECT max(id) FROM `guild` limit 1") of
        Num when is_integer(Num) -> Num;
        _ -> 0
    end.

init_guild([H|T], Reply) ->
    [Id, Name, Cid, Cname, RolesNum, RolesMax, Exp, Lev, Roles1] = H,
    Roles = util:bitstring_to_term(Roles1),
    Lev1 = case Lev of
        0 -> 1;
        _ -> Lev
    end,
    G = #guild{
        id = Id
        ,name = Name
        ,creater_id = Cid
        ,creater_name = Cname
        ,roles_num = RolesNum
        ,roles_max = RolesMax
        ,roles = unzip(Roles)
        ,exp = Exp
        ,lev = Lev1
    },
    Reply1 = [G | Reply],
    init_guild(T, Reply1);
init_guild([], Reply) -> Reply.

%%' 从数据库中删除帮会
del_guild_from_db(Guilds) ->
    Ids = case db:get_all("select id from guild") of
        {error, _} -> [];
        I -> I
    end,
    del_guild_from_db(Ids, Guilds).

del_guild_from_db([[Id] | T], Guilds) ->
    case lists:keyfind(Id, 2, Guilds) of
        false -> 
            ?INFO("Delete Guild(~w)!", [Id]),
            db:execute("delete from guild where id = ~s", [Id]);
        _ -> ok 
    end,
    del_guild_from_db(T, Guilds);
del_guild_from_db([], _) -> ok.
%%.

set_guild(Rid, GuildId, GuildName) ->
    srv_cache:send_event(Rid, {?EVENT_SET_GUILD, GuildId, GuildName}).

creater_check(Rid, GuildName, State) ->
    creater_check1(Rid, GuildName, State#state.guilds).

creater_check1(Rid, GuildName, [G|T]) ->
    if
        G#guild.name == GuildName -> {error, error_name};
        Rid == G#guild.creater_id -> {error, {created, G#guild.id, G#guild.name}};
        true ->
            creater_check1(Rid, GuildName, T)
    end;
creater_check1(_, _, []) -> ok.

find_myguild(Rid, State) ->
    find_myguild1(Rid, State#state.guilds).

find_myguild1(Rid, [G|T]) ->
    case i(Rid, G) of
        false -> find_myguild1(Rid, T);
        I when I#irole.auth > 0 -> G;
        _ -> find_myguild1(Rid, T)
    end;
find_myguild1(_, []) -> false.

add_guild_exp(AddExp, G) ->
    case data_guild_exp:get(G#guild.lev) of
        undefined -> 
            ?INFO("No guild exp data! [Lev:~w]", [G#guild.lev]),
            G;
        Data ->
            UpExp = util:get_val(exp, Data, 0),
            add_guild_exp2(AddExp, G, UpExp)
    end.

add_guild_exp2(_AddExp, G, 0) -> G;
add_guild_exp2(AddExp, G, UpExp) -> 
    Exp = G#guild.exp + AddExp,
    case Exp >= UpExp of
        true ->
            AddExp1 = Exp - UpExp,
            Lev = G#guild.lev + 1,
            RolesMax = 40 + (Lev - 1) * 10,
            G1 = G#guild{lev = Lev, exp = 0, change = 1, roles_max = RolesMax},
            add_guild_exp(AddExp1, G1);
        false ->
            G#guild{exp = Exp}
    end.

do_buy_benefit_check(G, Bid) ->
    Data = data_guild_exp:get(G#guild.lev),
    BenefitId = util:get_val(benefit_id, Data, []),
    case lists:member(Bid, BenefitId) of
        true -> do_buy_benefit_check2(Bid);
        false -> 
            ?INFO("福利未开放：Lev:~w, Bid:~w", [G#guild.lev, Bid]),
            false
    end.

do_buy_benefit_check2(Bid) ->
    case data_guild_benefit:get(Bid) of
        undefined -> false;
        Data -> {true, Data}
    end.

%%' permanent_stores
permanent_stores([G|T]) ->
    #guild{
        id = Id
        ,name = Name
        ,creater_id = Cid
        ,creater_name = Cname
        ,roles_num = RolesNum
        ,roles_max = RolesMax
        ,exp = Exp
        ,lev = Lev
        ,roles = Roles
    } = G,
    Roles1 = zip(Roles),
    Roles2 = util:term_to_bitstring(Roles1),
    db:execute("UPDATE `guild` SET `name`=~s,`creater_id`=~s,`creater_name`=~s,`roles_num`=~s,`roles_max`=~s,`exp`=~s,`lev`=~s,`roles`=~s WHERE id = ~s", [Name, Cid, Cname, RolesNum, RolesMax, Exp, Lev, Roles2, Id]),
    permanent_stores(T);
permanent_stores([]) -> ok.

permanent_stores2([G|T]) when G#guild.change == 1 ->
    #guild{
        id = Id
        ,name = Name
        ,creater_id = Cid
        ,creater_name = Cname
        ,roles_num = RolesNum
        ,roles_max = RolesMax
        ,exp = Exp
        ,lev = Lev
        ,roles = Roles
    } = G,
    Roles1 = zip(Roles),
    Roles2 = util:term_to_bitstring(Roles1),
    db:execute("UPDATE `guild` SET `name`=~s,`creater_id`=~s,`creater_name`=~s,`roles_num`=~s,`roles_max`=~s,`exp`=~s,`lev`=~s,`roles`=~s WHERE id = ~s", [Name, Cid, Cname, RolesNum, RolesMax, Exp, Lev, Roles2, Id]),
    %% ?INFO("save guild:~w", [Id]),
    util:sleep(10000),
    permanent_stores2(T);
permanent_stores2([_G|T]) -> 
    %% ?INFO("unchanged guild:~w", [G#guild.id]),
    permanent_stores2(T);
permanent_stores2([]) -> ok.
%%.

%%' zip/unzip
zip(L) -> zip(L, []).
zip([I | T], Reply) ->
    #irole{
        id         = X1
        ,name      = X2
        ,lev       = X3
        ,auth      = X4
        ,login_time= X5
        ,sex       = X6
        ,v         = X7
    } = I,
    zip(T, [{X1, X2, X3, X4, X5, X6, X7} | Reply]);
zip([], Reply) -> Reply.

unzip(L) -> unzip(L, []).
unzip([{X1, X2, X3, X4, X5, X6, X7} | T], Reply) ->
    I = #irole{
        id         = X1
        ,name      = X2
        ,lev       = X3
        ,auth      = X4
        ,login_time= X5
        ,sex       = X6
        ,v         = X7
    },
    unzip(T, [I | Reply]);
unzip([{irole, X1, X2, X3, X4, X5, X6, X7} | T], Reply) ->
    I = #irole{
        id         = X1
        ,name      = X2
        ,lev       = X3
        ,auth      = X4
        ,login_time= X5
        ,sex       = X6
        ,v         = X7
    },
    unzip(T, [I | Reply]);
unzip([{irole, X1, X2, X3, X4, X5, X6, X7, _} | T], Reply) ->
    I = #irole{
        id         = X1
        ,name      = X2
        ,lev       = X3
        ,auth      = X4
        ,login_time= X5
        ,sex       = X6
        ,v         = X7
    },
    unzip(T, [I | Reply]);
unzip([X | T], Reply) ->
    ?INFO(" *** undefined Data:~w", [X]),
    unzip(T, Reply);
unzip([], Reply) -> Reply.
%%.

fix([Guild | T]) ->
    fix2(Guild#guild.id, Guild#guild.name, Guild#guild.roles),
    fix(T);
fix([]) -> ok.

fix2(Gid, GuildName, [I | T]) when I#irole.auth > 0 ->
    M = srv_guild,
    F = fix_invoke,
    A = [Gid, GuildName],
    case lib_authen:get_role_pid(role_id, I#irole.id) of
        false -> ok;
        Pid ->
            Pid ! {handle_event, 6001, {?EVENT_INVOKE, M, F, A}},
            util:sleep(500),
            io:format(".")
    end,
    fix2(Gid, GuildName, T);
fix2(Gid, GuildName, [_I | T]) ->
    fix2(Gid, GuildName, T);
fix2(_Gid, _GuildName, []) -> ok.

fix_invoke(Rs, GuildId, GuildName) ->
    Status = case Rs#role.status of
        0 -> 
            self() ! {reset_stop_timer, 1000},
            1;
        _ -> Rs#role.status
    end,
    case Rs#role.guild_id of
        GuildId -> {ok};
        _ ->
            case GuildId > Rs#role.guild_id of
                true ->
                    ?INFO("Fix guild, Gid: ~w -> ~w", [Rs#role.guild_id, GuildId]),
                    Rs1 = Rs#role{
                        guild_id = GuildId 
                        ,guild_name = GuildName
                    },
                    lib_conn:pack_send(
                        Rs#role.pid_sender
                        ,22012
                        ,[GuildId, GuildName]
                    ),
                    {ok, Rs1#role{status = Status}};
                false ->
                    {ok}
            end
    end.
