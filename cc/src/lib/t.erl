%%---------------------------------------------------
%% tools
%%
%% @author rolong@vip.qq.com
%%----------------------------------------------------

-module(t).
-export([
        add_gold/3
        ,add_card/3
        ,add_item/4
        ,online_num/0
        ,set_online_max/1
        ,robot_info/0
        ,t/2
        ,t/0
        ,shopping/3
        ,del_role/1
        ,ban_chat/2
        ,cmd/2
    ]
).
-include_lib("kernel/include/file.hrl").
-include("common.hrl").

%% TEST
t() ->
    ?INFO("~w", [<<"♂╰LuCky╮♀">>]).

t(0, _Time) -> ok;
t(T, Time) ->
    util:sleep(Time),
    overload:request(),
    t(T-1, Time).

%% tools
online_num() ->
    ets:info(online, size) - length(robot_lib:get_names()).

set_online_max(Max) ->
    srv_cache ! {online_max, Max}.

robot_info() ->
    robot ! info.

add_gold(Type, Rid, Val) ->
    LogType = case Type of
        0 -> 1012;
        1 -> 1013;
        _ -> 0
    end,
    srv_cache:send_event(Rid, {?EVENT_ADD_GOLD, Val, LogType}),
    ?LOG({gm_send_gold, Type, Rid, Val}),
    true.

add_card(Type, Rid, Val) ->
    case Type of
        1 -> srv_cache:send_event(Rid, {?EVENT_TASK, ?TASK_CHARGE, {add, Val}});
        0 -> ok
    end,
    LogType = case Type of
        0 -> 1012;
        1 -> 1013;
        _ -> 0
    end,
    srv_cache:send_event(Rid, {?EVENT_ADD_CARD, Val, LogType}),
    ?LOG({gm_send_card, Type, Rid, Val}),
    true.

add_item(Type, Rid, Tid, Num) ->
    srv_cache:send_event(Rid, {?EVENT_ADD_ITEM, Tid, Num}),
    ?LOG({gm_send_item, Type, Rid, Tid, Num}),
    true.

shopping(Rid, ShopId, Num) ->
    srv_cache:send_event(Rid, {?EVENT_SHOPPING, ShopId, Num}),
    ?LOG({gm_send_item, 2, Rid, ShopId, Num}),
    true.

del_role(Rid) ->
    Sql = lists:concat(["select id, account_id from role "
            "where id = '", Rid, "' limit 10"]),
    case db:get_all(Sql) of 
        [[Rid, Aid]] ->
            Pid = lib_authen:get_role_pid(role_id, Rid),
            gen_server:cast(Pid, {handle_event, 22009, [0]}),
            lib_role:kick(Rid, <<>>),
            db:execute("delete from role where id = ~s", [Rid]),
            db:execute("delete from rank where id = ~s", [Rid]),
            db:execute("delete from log_login where role_id = ~s", [Rid]),
            db:execute("delete from log_upgrade where role_id = ~s", [Rid]),
            db:execute("delete from log_upgrade where role_id = ~s", [Rid]),
            db:execute("delete from log_reg where account_id = ~s", [Aid]),
            db:execute("delete from log_enhance where role_id = ~s", [Rid]),
            db:execute("delete from log_del_item where role_id = ~s", [Rid]),
            db:execute("delete from log_combine where role_id = ~s", [Rid]),
            db:execute("delete from log_buy where role_id = ~s", [Rid]),
            ok;
        Else -> ?INFO("Role not found: ~w", [Else])
    end.

%% Status = 1 禁言
%% Status = 0 取消禁言
ban_chat(Rid, Status) ->
    srv_cache:send_event(Rid, {?EVENT_BAN_CHAT, Status}),
    true.

cmd(X1, X2) ->
    lib_test:cmd(X1, X2).
