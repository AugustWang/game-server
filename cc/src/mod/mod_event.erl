%%----------------------------------------------------
%% 60内部协议 - 角色事件处理
%% 
%% @author Rolong<erlang6@qq.com>
%%
%%----------------------------------------------------

-module(mod_event).
-export([handle/3]).

-include("common.hrl").
-include("shop.hrl").

%% 事件处理
handle(6001, {?EVENT_REQUEST_LEV, FromRid}, Rs) ->
    ReplyEvent = {?EVENT_RESPOND_LEV, Rs#role.id, Rs#role.lev},
    srv_cache:send_event(FromRid, ReplyEvent),
    {ok};

handle(6001, {?EVENT_RESPOND_LEV, FromRid, Lev}, _Rs) ->
    lib_sns:update_friend_lev(FromRid, Lev),
    {ok};

handle(6001, {?EVENT_ADD_FANS, FansId}, _Rs) ->
    lib_sns:add_fans(FansId),
    {ok};

handle(6001, {?EVENT_DEL_FANS, FansId}, _Rs) ->
    lib_sns:del_fans(FansId),
    {ok};

handle(6001, {?EVENT_SET_GUILD, GuildId, GuildName}, Rs) ->
    Rs1 = set_guild(GuildId, GuildName, Rs),
    {ok, Rs1};

handle(6001, {?EVENT_TASK, CType, Arg}, Rs) ->
    process_task(CType, Arg, Rs),
    {ok};

handle(6001, {?EVENT_SHOPPING, Tid, Num}, Rs) ->
    shopping(Tid, Num, Rs);

handle(6001, {?EVENT_ADD_ITEM, Tid, Num}, Rs) ->
    ?INFO("add_item: ~w * ~w", [Tid, Num]),
    add_item(Tid, Num, Rs),
    {ok};

handle(6001, {?EVENT_ADD_GOLD, Num}, Rs) when Num > 0 ->
    ?INFO("Rid:~w, add_gold:~w", [Rs#role.id, Num]),
    Rs1 = lib_role:add_attr(Rs, gold, Num),
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
    {ok, Rs1};

handle(6001, {?EVENT_ADD_GOLD, Num, LogType}, Rs) when Num > 0 ->
    ?INFO("Rid:~w, add_gold:~w", [Rs#role.id, Num]),
    Rs1 = lib_role:add_attr(Rs, gold, Num, LogType),
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
    {ok, Rs1};

handle(6001, {?EVENT_ADD_CARD, Num}, Rs) when Num > 0 ->
    ?INFO("Rid:~w, add_card:~w", [Rs#role.id, Num]),
    Rs1 = lib_role:add_attr(Rs, card, Num),
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
    {ok, Rs1};

handle(6001, {?EVENT_ADD_CARD, Num, LogType}, Rs) when Num > 0 ->
    ?INFO("Rid:~w, add_card:~w", [Rs#role.id, Num]),
    Rs1 = lib_role:add_attr(Rs, card, Num, LogType),
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
    {ok, Rs1};

handle(6001, {?EVENT_INVOKE, M, F, A}, Rs) ->
    erlang:apply(M, F, [Rs | A]);

handle(6001, {?EVENT_SEND_CODE, Code}, Rs) ->
    lib_conn:send_code(Rs#role.pid_sender, Code),
    {ok};

handle(6001, {?EVENT_BAN_CHAT, Status}, Rs) ->
    BitData = case Status of
        0 -> lib_role:bit_del(Rs#role.bit_data, ?BIT_BAN_CHAT); 
        1 -> lib_role:bit_set(Rs#role.bit_data, ?BIT_BAN_CHAT); 
        _ -> 
            ?WARNING("EVENT_BAN_CHAT: error Status(~w)", [Status]),
            Rs#role.bit_data
    end,
    {ok, Rs#role{bit_data = BitData}};

handle(6001, Event, Rs) ->
    ?WARNING("undefined event, Rid:~w, Event:~w", [Rs#role.id, Event]),
    {ok};

%% 登陆完成后处理离线事件
handle(6002, [do_events, Events], Rs) ->
    %% ?INFO("do_events:~w", [Events]),
    Events1 = lists:reverse(Events),
    do_events(Rs, Events1),
    {ok};

%% 服务启动后第一次登陆
handle(6003, [], Rs) ->
    %% lib_sns:request_friends_lev(Rs#role.id),
    case env:get(platform) of
        id -> ok;
        _ ->
            lib_conn:pack_send(Rs#role.pid_sender, 15010, []),
            ok
    end,
    {ok};

%% 升级
handle(6010, [], Rs) ->
    #role{id = Id, lev = Lev, exp = Exp, pid_room = PidRoom} = Rs,
    %% 重新计算角色属性
    MyItems = lib_item:get_myitems(),
    Rs1 = lib_role:calc_attrs(Rs, MyItems),
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]), %% 通知属性数据过期
    lib_conn:pack_send(Rs#role.pid_sender, 11024, [Lev, Exp]), %% 升级通知
    %% 触发任务
    self() ! {task, ?TASK_UP_LEV, {to, Lev}},
    catch db:execute("update role set lev = ~s where id = ~s", [Lev, Id]),
    ?LOG({upgrade, Rs#role.id, Lev}),
    Rs2 = lib_skill:init_skill(Rs1),
    NewSkill = Rs2#role.skill,
    case Rs1#role.skill of
        NewSkill -> ok;
        _ -> lib_conn:pack_send(Rs#role.pid_sender, 17006, [Rs2#role.skill])
    end,
    gen_server:cast(PidRoom, {set_lev, Rs#role.id, Lev}),
    lib_sns:respond_fans_lev(Id, Lev),
    Rs3 = case Lev > 5 of
        true ->
            Kvs = lib_role:get_kvs(Rs2),
            Save = [{myitems, MyItems}, {kvs, Kvs}],
            Rs2#role{save = Save};
        false -> Rs2
    end,
    {ok, Rs3};

%% 登陆后更新我的总经验排名
handle(6012, [MyRank], Rs) ->
    {ok, Rs#role{myrank = MyRank}};

%% 刷新任务
handle(6016, [], Rs) ->
    Rs1 = case Rs#role.status > 0 of
        true -> 
            ?INFO("6016 :~w", [Rs#role.id]),
            lib_conn:pack_send(Rs#role.pid_sender, 11020, [3]),
            lib_task:refresh_mytask(Rs);
        false -> 
            Rs
    end,
    {ok, Rs1};

%% 零点整
handle(6018, [], Rs) ->
    lib_rank:set_myrank(Rs),
    {ok, Rs};

%% 接任务，测试用
handle(6090, [Tid], Rs) ->
    MyTasks = lib_task:get_mytasks(),
    T = lib_task:init_mytask(Tid, 0),
    lib_task:put_mytasks([T | MyTasks]),
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [3]),
    {ok};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%%' === 私有函数 ============

do_events(Rs, [Event | Events]) ->
    self() ! {handle_event, 6001, Event},
    do_events(Rs, Events);
do_events(_Rs, []) -> ok.

shopping(ShopId, Num, Rs) ->
    ?INFO("shopping: ~w * ~w, Rid:~w", [ShopId, Num, Rs#role.id]),
    case data_shop:get(ShopId) of
        undefined -> 
            ?WARNING("ShopId undefined:~w", [ShopId]),
            {ok};
        #shop{
            item_id = Tid
        } ->
            case Tid of
                1 -> 
                    Rs1 = lib_role:add_attr(Rs, gold, Num),
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    {ok, Rs1};
                2 -> 
                    Rs1 = lib_role:add_attr(Rs, card, Num),
                    self() ! {task, ?TASK_CHARGE, {add, Num}},
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    {ok, Rs1};
                _ ->
                    add_item(Tid, Num, Rs),
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    {ok}
            end
    end.

add_item(Tid, Num, Rs) ->
    MyItems = lib_item:get_myitems(),
    case lib_item:add_item(Tid, Num, MyItems) of
        {ok, MyItems1, My} ->
            lib_item:add_item_notice(My, Rs#role.pid_sender),
            lib_item:put_myitems(MyItems1),
            ok;
        {error, Reason} -> 
            ?WARNING("error when add_item: ~w, Rid:~w, Tid:~w, Num:~w", 
                [Reason, Rs#role.id, Tid, Num]),
            {error, Reason}
    end.

set_guild(GuildId, GuildName, Rs) ->
    GuildV = case GuildId of
        0 -> 
            %% 退出工会
            lib_conn:pack_send(
                Rs#role.pid_sender
                ,22013
                ,[]
            ),
            0;
        _ -> 
            %% 被同意加入公会
            lib_conn:pack_send(
                Rs#role.pid_sender
                ,22012
                ,[GuildId, GuildName]
            ),
            self() ! {task, ?TASK_JOIN_GUILD, {add, 1}},
            Rs#role.guild_v
    end,
    Rs#role{
        guild_id = GuildId 
        ,guild_name = GuildName
        ,guild_v = GuildV
    }.

process_task(CType, Arg, Rs) ->
    MyTasks = lib_task:get_mytasks(),
    case lib_task:process(CType, Arg, MyTasks) of
        {ok} -> ok;
        {ok, NewMyTasks} ->
            lib_task:put_mytasks(NewMyTasks),
            lib_conn:pack_send(Rs#role.pid_sender, 11020, [3])
    end.
%%.

%% vim: set foldmethod=marker foldmarker=%%',%%.:
