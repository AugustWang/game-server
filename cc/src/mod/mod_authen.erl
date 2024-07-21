%%----------------------------------------------------
%% 登陆认证
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_authen).
-export([handle/3]).

-include("common.hrl").
-include("lev.hrl").

%% 测试
%% handle(10000, [Bin], Rs) ->
%%     lib_test:gm(Bin, Rs);

%% 处理角色登录
%% Rid = RoleId,角色ID
%% Rs  = RoleState 角色进程状态
handle(10003, [Bin], Rs) ->
    %% ?INFO("10003:~w", [Bin]),
    {ok, VersionType} = application:get_env(cc, version_type),
    login(VersionType, [Bin], Rs);

%% 创建角色
handle(10004, [Name, Sex], Rs) ->
    %% ?INFO("10004 RECV:~s", [Name]),
    #role{account_id = AccountId, ip = Ip} = Rs,
    IsReged1 = db:get_one("select id from role where account_id = ~s limit 1", [AccountId]),
    IsReged2 = db:get_one("select id from role where name = ~s limit 1", [Name]),
    if
        AccountId =:= undefined -> 
            ?INFO("No login!", []),
            %% 没有登陆
            {ok, [5]};
        IsReged1 =/= null ->
            ?INFO("Repeat reg! account_id:~w", [AccountId]),
            %% 已注册
            {ok, [6]};
        IsReged2 =/= null ->
            %% 名称已被占用
            {ok, [1]};
        true ->
            db:execute("insert into role(account_id, name, account_name, lev)
                values(~s, ~s, ~s, ~s)", 
            [AccountId, Name, Name, 1]),
            Lev = data_lev:get(1),
            Rs1 = Rs#role{
                sex = Sex 
                ,gold = data_config:get(init_gold)
                ,card = data_config:get(init_card)
                ,exp_max = Lev#lev.exp_max
                ,growth = 1
            },
            {ok, Reply, NewRs} = case lib_authen:login(AccountId, Rs1, 1) of
                {ok, X} -> {ok, X, Rs1};
                {ok, X1, X2} -> {ok, X1, X2};
                _X -> 
                    ?ERR("unexpected value: ~w", [_X]),
                    {ok, [0, 0, 0, 0], Rs1}
            end,
            lib_conn:pack_send(Rs#role.pid_sender, 10003, Reply),
            ?LOG({reg, AccountId, Ip}),
            {ok, [0], NewRs}
    end;

%% 验证角色名字
handle(10005, [Name], _Rs) ->
    ?INFO("10005:~s", [Name]),
    case db:get_one("select id from role where name = ~s", [Name]) of
        null -> {ok, [0]};
        _ -> {ok, [1]}
    end;

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%% 登陆验证 开发
login(dev, [Bin], Rs) ->
    Qs = util:parse_qs(Bin),
    %% ?INFO("~s", [Bin]),
    case lists:keyfind("account_id", 1, Qs) of
        {_, AccountId} -> lib_authen:login(AccountId, Rs);
        _Other -> {ok, [0, 0, 0, 0]}
    end;

%% 登陆验证 正式
login(release, [Bin], Rs) ->
    Qs = util:parse_qs(Bin),
    {_, ServerKey} = application:get_env(cc, server_key),
    {_, AccountId} = lists:keyfind("account_id", 1, Qs),
    {_, ServerId} = lists:keyfind("serverid", 1, Qs),
    {_, Ltime} = lists:keyfind("time", 1, Qs),
    {_, Stoken} = lists:keyfind("signature", 1, Qs),
    Stoken3 = list_to_binary(Stoken),
    Lconcat = lists:concat([Ltime, AccountId, ServerId, ServerKey]),
    Stoken2 = util:md5(Lconcat),
    case Stoken2 of
        Stoken3 ->
            ExprieTime = util:unixtime() - list_to_integer(Ltime),
            case ExprieTime < (14400 * 1) of
                true -> lib_authen:login(AccountId, Rs);
                false -> 
                    ?INFO("1 :~s", [Bin]),
                    {ok, [0, 0, 0, 0]}
            end;
        _ -> 
            ?INFO("2 :~s", [Bin]),
            {ok, [0, 0, 0, 0]}
    end.
