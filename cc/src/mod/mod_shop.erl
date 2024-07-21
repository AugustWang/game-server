%%----------------------------------------------------
%% shop
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_shop).
-export([handle/3]).

-include("common.hrl").
-include("shop.hrl").
-include("daoju.hrl").

%% 购买物品
%% 状态（0=成功,1=金币不足,2=点券不足,3=商品不存在,4=添加物品出错,5=没有购买任务物品）
handle(20001, [BuyList], Rs) ->
    case BuyList of
        [] -> {ok, [5]};
        _ ->
            MyItems = lib_item:get_myitems(),
            case buy(BuyList, Rs, MyItems, [], []) of
                {ok, Rs1, MyItems1, AddItems, Logs} ->
                    %% ?INFO("20001 AddItems:~w (Gold: ~w->~w)(card: ~w->~w)", [AddItems, Rs#role.gold, Rs1#role.gold, Rs#role.card, Rs1#role.card]),
                    lib_item:put_myitems(MyItems1),
                    lib_item:add_item_notice(AddItems, Rs#role.pid_sender),
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    do_buy_task(AddItems),
                    %% case lists:keyfind(170001, #myitem.tid, AddItems) of
                    %%     false -> ok;
                    %%     My170001 ->
                    %%         self() ! {task, ?TASK_BUY, {add, 170001, My170001#myitem.num}}
                    %% end,
                    ?LOG({buy, Logs}),
                    Kvs = lib_role:get_kvs(Rs1),
                    Save = [{myitems, MyItems1}, {kvs, Kvs}],
                    ?LOG_GOLD(Rs, Rs1, 2001),
                    ?LOG_CARD(Rs, Rs1, 2001),
                    {ok, [0], Rs1#role{save = Save}};
                {error, Code} -> 
                    %% ?INFO("20001 ERR:~w", [Code]),
                    {ok, [Code]}
            end
    end;

%% 购买道具
handle(20003, [Tid], Rs) ->
    Daoju = data_daoju:get(Tid),
    if
        Daoju == undefined ->
            ?INFO("道具不存在：~w", [Tid]),
            {ok, [1]};
        Daoju#daoju.gold =< 0 ->
            ?INFO("此道具不可出售：~w", [Tid]),
            {ok, [2]};
        true ->
            case lib_role:spend(gold, Daoju#daoju.gold, Rs) of
                {ok, Rs1} ->
                    case lib_item:add_mydaoju(Tid) of
                        {ok, MyDaoju} -> 
                            ?LOG({buy, Rs#role.id, 2, Tid, 1, Daoju#daoju.gold, 0}),
                            lib_conn:pack_send(Rs#role.pid_sender, 17002, [MyDaoju]),
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            ?LOG_GOLD(Rs, Rs1, 2009),
                            {ok, [0], Rs1};
                        {error, at_full} -> {ok, [4]};
                        {error, _} -> {ok, [4]}
                    end;
                {error, _} -> {ok, [3]}
            end
    end;

handle(20005, [], Rs) ->
    gen_server:cast(srv_cache, {send_rebate_shop, Rs#role.pid_sender}),
    {ok};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%%' 私有函数

buy([[_, 0] | T], Rs, MyItems, AddItems, Logs) ->
    buy(T, Rs, MyItems, AddItems, Logs);
buy([[0, _] | T], Rs, MyItems, AddItems, Logs) ->
    buy(T, Rs, MyItems, AddItems, Logs);
buy([[ShopId, Num] | T], Rs, MyItems, AddItems, Logs) ->
    case data_shop:get(ShopId) of
        undefined -> 
            ?WARNING("ShopId undefined:~w", [ShopId]),
            {error, 3};
        #shop{
            item_id = ItemId
            , card = CardTmp
            , gold = Gold
            , rebate = Rebate
        } ->
            Card = case Rebate > 0 andalso Rebate < 10 of
                true ->
                    case gen_server:call(srv_cache, {is_rebate_shop, ShopId}) of
                        true -> util:ceil(CardTmp / 10 * Rebate);
                        false -> CardTmp
                    end;
                false -> CardTmp
            end,
            Card1 = Card * Num,
            Gold1 = Gold * Num,
            case lib_role:spend(card, Card1, Rs) of
                {ok, Rs1} ->
                    case lib_role:spend(gold, Gold1, Rs1) of
                        {ok, Rs2} ->
                            case lib_item:add_item(ItemId, Num, MyItems) of
                                {ok, MyItems1, Add} ->
                                    Log = {buy, Rs#role.id, 1, ItemId, Num, Gold1, Card1},
                                    buy(T, Rs2, MyItems1, Add ++ AddItems, [Log | Logs]);
                                {error, _} ->
                                    {error, 4}
                            end;
                        {error, _} -> {error, 1}
                    end;
                {error, _} -> {error, 2}
            end
    end;
buy([], Rs, MyItems, AddItems, Logs) -> 
    {ok, Rs, MyItems, AddItems, Logs}.

do_buy_task([#myitem{tid = Tid, num = Num} | T]) ->
    self() ! {task, ?TASK_BUY, {add, Tid, Num}},
    do_buy_task(T);
do_buy_task([]) -> ok.

%%.

%% vim: set foldmethod=marker foldmarker=%%',%%.:
