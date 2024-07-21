%%----------------------------------------------------
%% 工会
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_guild).
-export([handle/3]).

-include("common.hrl").

%% 工会列表
handle(22005, [], Rs) ->
    gen_server:cast(srv_guild, {guild_list, Rs#role.pid_sender}),
    {ok};

%% 工会成员列表
handle(22006, [0], Rs) when Rs#role.guild_id > 0 ->
    handle(22006, [Rs#role.guild_id], Rs);

handle(22006, [Gid], Rs) ->
    gen_server:cast(srv_guild, {role_list, Gid, Rs#role.pid_sender}),
    {ok};

%% 创建工会
handle(22007, [_GuildName], Rs) when Rs#role.guild_id > 0 ->
    {ok, [4]};
handle(22007, [_GuildName], Rs) when Rs#role.lev < 6 ->
    {ok, [3]};

handle(22007, [GuildName], Rs) ->
    case lib_role:spend(gold, 10000, Rs) of
        {ok, Rs1} ->
            case lib_role:spend(card, 500, Rs1) of
                {ok, Rs2} ->
                    case catch gen_server:call(srv_guild, {create, Rs#role.id, Rs#role.name, Rs#role.sex, Rs#role.lev, GuildName}) of
                        {ok, Gid} -> 
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            self() ! {task, ?TASK_JOIN_GUILD, {add, 1}},
                            ?LOG_GOLD(Rs, Rs2, 2002),
                            ?LOG_CARD(Rs, Rs2, 2002),
                            {ok, [0], Rs2#role{guild_id = Gid, guild_name = GuildName}};
                        {error, error_name} -> {ok, [5]};
                        {error, {created, Gid1, GuildName1}} -> 
                            {ok, [0], Rs#role{guild_id = Gid1, guild_name = GuildName1}};
                        {error, Error} -> 
                            ?ERR("ERROR when create guild:~w", [Error]),
                            {ok, [4]}
                    end;
                {error, _} -> {ok, [1]}
            end;
        {error, _} -> {ok, [2]}
    end;

%% 申请加入
handle(22008, [_], Rs) when Rs#role.guild_id > 0 ->
    {ok, [1]};
handle(22008, [Gid], Rs) ->
    gen_server:cast(srv_guild, {join, Rs#role.id, Rs#role.name, Rs#role.sex, Rs#role.lev, Gid, Rs#role.pid_sender}),
    {ok};

%% 退出工会
handle(22009, [_], Rs) when Rs#role.guild_id == 0 ->
    ?INFO("guild_id=0 when exit_guild", []),
    {ok, [1, 0]};

handle(22009, [0], Rs) ->
    handle(22009, [Rs#role.id], Rs);

handle(22009, [Rid], Rs) ->
    %% ?INFO("Rid:~w, guild_id:~w", [Rid, Rs#role.guild_id]),
    gen_server:cast(srv_guild, {exit, Rs#role.id, Rid, Rs#role.guild_id, Rs#role.pid_sender}),
    {ok};

handle(22010, [Rid, Status], Rs) ->
    gen_server:cast(srv_guild, {join_process, Rs#role.id, Rid, Rs#role.guild_id, Status}),
    {ok};

%% 转让会长
handle(22011, [Rid], Rs) ->
    gen_server:cast(srv_guild, {chmod, Rs#role.id, Rid, Rs#role.guild_id, 10, Rs#role.pid_sender}),
    {ok};

%% 个人贡献值
handle(22014, [], Rs) ->
    GuildB = [[Id, Time] || {Id, _, _, Time} <- Rs#role.guild_b],
    {ok, [Rs#role.guild_v, GuildB]};

%% 购买公会福利
handle(22015, [Bid, Days], Rs) ->
    case gen_server:call(srv_guild, 
            {buy_benefit_check, Rs#role.guild_id, Rs#role.id, Bid}) of
        false ->
            {ok, [1]};
        {true, Data} ->
            case add_benefit(Data, Bid, Days, Rs) of
                {error, Code} -> {ok, [Code]};
                {ok, Rs1} -> {ok, [0], Rs1}
            end
    end;

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

add_benefit([{gold, GoldRate} | T], Bid, Days, Rs) ->
    EndTime = util:unixtime() + Days * 24 * 3600,
    GuildB = lists:keystore(gold, 2, Rs#role.guild_b, {Bid, gold, GoldRate, EndTime}),
    Rs1 = Rs#role{guild_b = lib_role:fix_benefit(gold, GuildB, [])},
    add_benefit(T, Bid, Days, Rs1);
add_benefit([{exp, ExpRate} | T], Bid, Days, Rs) ->
    EndTime = util:unixtime() + Days * 24 * 3600,
    GuildB = lists:keystore(exp, 2, Rs#role.guild_b, {Bid, exp, ExpRate, EndTime}),
    Rs1 = Rs#role{guild_b = lib_role:fix_benefit(exp, GuildB, [])},
    add_benefit(T, Bid, Days, Rs1);
add_benefit([{price, 0} | _T], _Bid, _Days, _Rs) ->
    ?INFO("price=0 when add_benefit!", []),
    {error, 1};
add_benefit([{price, Price} | T], Bid, Days, Rs) ->
    case lib_role:spend(guild_v, Price * Days, Rs) of
        {ok, Rs1} -> add_benefit(T, Bid, Days, Rs1);
        {error, _} -> {error, 2}
    end;
add_benefit([H | T], Bid, Days, Rs) ->
    ?WARNING("unexpected benefit data: ~w", [H]),
    add_benefit(T, Bid, Days, Rs);
add_benefit([], _Bid, _Days, Rs) -> {ok, Rs}.
