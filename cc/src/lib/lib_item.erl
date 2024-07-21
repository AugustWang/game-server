%%----------------------------------------------------
%% 物品
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_item).
-export([
        init_myitems/2
        ,init_myitems/3
        ,get_myitems/0
        ,get_myitem/1
        ,get_myitem/2
        ,move_item/3
        ,move_item/4
        ,sort_myitems/0
        ,test_add_item/3
        ,add_item/3
        ,add_item/4
        ,add_items/2
        ,add_item_notice/2
        %% ,clear_myitems/0
        ,set_myitem/2
        ,put_myitems/1
        ,del_item/4
        ,del_items/3
        %% ,get_equipped_info/1
        ,update_myitem_notice/2
        ,get_weapon_id/1
        ,get_mydaojus/0
        ,put_mydaojus/1
        ,add_mydaoju/1
        ,use_daoju/1
        ,get_pos/2
        ,calc_etime/1
        ,zip/1
        ,clear_mydaojus/0
    ]
).

-include("common.hrl").

%% get_myitems() -> [#myitem{}, ...]
get_myitems() -> 
    get(myitems).

put_myitems(MyItems) -> 
    put(myitems, MyItems).

set_myitem(MyItem, MyItems) ->
    lists:keystore(MyItem#myitem.id, 2, MyItems, MyItem).

%% clear_myitems() -> put(myitems, []).

get_new_id() -> 
    Id = get(myitem_max_id) + 1,
    put(myitem_max_id, Id),
    Id.

init_etime(0) -> 0;
init_etime(Period) -> util:unixtime() + Period.

calc_etime(0) -> 999999999;
calc_etime(Etime) -> 
    Rest = Etime - util:unixtime(),
    case Rest > 0 of
        true -> Rest;
        false -> 0
    end.

init_myitems(MyItemsBin, Growth) ->
    init_myitems(self, MyItemsBin, Growth).

%% 修改时要慎重考虑！
init_myitems(Type, MyItemsBin, Growth) ->
    MyItems = case util:bitstring_to_term(MyItemsBin) of
        undefined ->
            NewId = case Type of
                self -> 
                    put(myitem_max_id, 0),
                    get_new_id();
                _ -> 1
            end,
            case Growth > 1 of
                true ->
                    ?WARNING("Myitems is empty! [MyItemsBin:~w]", [MyItemsBin]),
                    [];
                false -> 
                    %% 默认物品
                    Tid = data_config:get(init_items),
                    Item = data_item:get(Tid),
                    My = #myitem{
                        id = NewId
                        ,tid = Tid
                        ,tab = Item#item.tab
                        ,sort = Item#item.sort
                        ,pos = 97
                        ,max_num = Item#item.max_num
                        ,num = 1
                        ,attr = #equ_attr{
                            hp_max = Item#item.attr#equ_attr.hp_max
                            ,dmg = Item#item.attr#equ_attr.dmg
                            ,dmg_speed = Item#item.attr#equ_attr.dmg_speed
                            ,move_speed = Item#item.attr#equ_attr.move_speed
                            ,crit = Item#item.attr#equ_attr.crit
                            ,attack   = Item#item.attr#equ_attr.attack
                            ,fb_attack   = Item#item.attr#equ_attr.fb_attack
                        }
                        ,etime = init_etime(Item#item.period)
                    },
                    [My]
            end;
        MyItemsData -> 
            MyItems1 = unzip(MyItemsData),
            %% F = fun 
            %%     ({myitem, X1, X2, X3, X4, X5, X6, X7, X8, X9}) -> 
            %%         {myitem, X1, X2, X3, X4, X5, X6, X7, X8, X9, 0};
            %%     (My) -> 
            %%         Attr = case My#myitem.attr of
            %%             {equ_attr, X1, X2, X3, X4, X5, X6} -> 
            %%                 {equ_attr, X1, X2, X3, X4, X5, X6, 0};
            %%             A -> A
            %%         end,
            %%         My#myitem{attr = Attr}
            %% end,
            %% 转换老数据
            %% MyItems1 = [F(X) || X <- MyItems2],
            case Type of
                self -> 
                    Ids = [Id || #myitem{id = Id} <- MyItems1],
                    MaxId = case Ids of
                        [] -> 0;
                        _ -> lists:max(Ids)
                    end,
                    put(myitem_max_id, MaxId);
                _ -> ok
            end,
            MyItems1
    end,
    case Type of
        self -> put_myitems(MyItems);
        _ -> ok
    end,
    MyItems.

get_myitem(Id) ->
    MyItems = get_myitems(),
    get_myitem(Id, MyItems).
get_myitem(Id, MyItems) ->
    lists:keyfind(Id, 2, MyItems).

del_items(by_id, Dels, MyItems) ->
    del_items(by_id, Dels, MyItems, []);
del_items(by_tid, Dels, MyItems) ->
    del_items(by_tid, Dels, MyItems, []).

del_items(by_id, [{Id, Num} | T], MyItems, AccDels) ->
    del_items(by_id, [[Id, Num] | T], MyItems, AccDels);
del_items(by_id, [[Id, Num] | T], MyItems, AccDels) 
when Id =< 0; Num =< 0 ->
    del_items(by_id, T, MyItems, AccDels);
del_items(by_id, [[Id, Num] | T], MyItems, AccDels) ->
    case del_item(by_id, Id, Num, MyItems) of
        {ok, MyItems1, Dels} ->
            del_items(by_id, T, MyItems1, Dels ++ AccDels);
        {error, Reason} ->
            {error, Reason}
    end;
del_items(by_id, [], MyItems, AccDels) ->
    {ok, MyItems, AccDels};

del_items(by_tid, [{Id, Num} | T], MyItems, AccDels) ->
    del_items(by_tid, [[Id, Num] | T], MyItems, AccDels);
del_items(by_tid, [[Id, Num] | T], MyItems, AccDels) 
when Id =< 0; Num =< 0 ->
    del_items(by_tid, T, MyItems, AccDels);
del_items(by_tid, [[Id, Num] | T], MyItems, AccDels) ->
    case del_item(by_tid, Id, Num, MyItems) of
        {ok, MyItems1, Dels} ->
            del_items(by_tid, T, MyItems1, Dels ++ AccDels);
        {error, Reason} ->
            {error, Reason}
    end;
del_items(by_tid, [], MyItems, AccDels) ->
    {ok, MyItems, AccDels}.

%% @spec () -> {ok, MyItems1} | {error, Reason}
del_item(by_id, Id, Num, MyItems) ->
    case lists:keyfind(Id, 2, MyItems) of
        false -> {error, no_item};
        MyItem -> 
            if
                Num == MyItem#myitem.num ->
                    MyItems1 = lists:keydelete(MyItem#myitem.id, 2, MyItems),
                    {ok, MyItems1, [[Id, Num]]};
                Num < MyItem#myitem.num ->
                    MyItem1 = MyItem#myitem{num = MyItem#myitem.num - Num},
                    MyItems1 = lists:keyreplace(MyItem1#myitem.id, 2, MyItems, MyItem1),
                    {ok, MyItems1, [[Id, Num]]};
                Num > MyItem#myitem.num ->
                    %% ?INFO("删除数量不正确：~w ! ~w", [Num, MyItem#myitem.num]),
                    {error, no_item}
            end
    end;

%% @spec () -> {ok, MyItems1, Dels} | {error, Reason}
del_item(by_tid, Tid, Num, MyItems) ->
    del_item(by_tid, Tid, Num, MyItems, []).
del_item(by_tid, Tid, Num, MyItems, Dels) ->
    case lists:keyfind(Tid, #myitem.tid, MyItems) of
        false -> {error, no_item};
        MyItem -> 
            if
                Num == MyItem#myitem.num ->
                    MyItems1 = lists:keydelete(MyItem#myitem.id, 2, MyItems),
                    Dels1 = [[MyItem#myitem.id, Num] | Dels],
                    {ok, MyItems1, Dels1};
                Num < MyItem#myitem.num ->
                    MyItem1 = MyItem#myitem{num = MyItem#myitem.num - Num},
                    MyItems1 = lists:keyreplace(MyItem1#myitem.id, 2, MyItems, MyItem1),
                    Dels1 = [[MyItem#myitem.id, Num] | Dels],
                    {ok, MyItems1, Dels1};
                Num > MyItem#myitem.num ->
                    Num1 = Num - MyItem#myitem.num,
                    MyItems1 = lists:keydelete(MyItem#myitem.id, 2, MyItems),
                    Dels1 = [[MyItem#myitem.id, MyItem#myitem.num] | Dels],
                    del_item(by_tid, Tid, Num1, MyItems1, Dels1)
            end
    end.

move_item(Id, Pos, RoleLev) ->
    MyItems = get_myitems(),
    move_item(Id, Pos, RoleLev, MyItems).
move_item(Id, Pos, RoleLev, MyItems) ->
    My1 = lists:keyfind(Id, 2, MyItems),
    My2 = lists:keyfind(Pos, #myitem.pos, MyItems),
    {OldPos, Sort} = case My1 of
        false -> {0, 0};
        _ -> {My1#myitem.pos, My1#myitem.sort}
    end,
    Disable1 = case lists:member(Pos, ?EQU_POS) andalso My1 =/= false of
        true -> 
            DataItem1 = data_item:get(My1#myitem.tid),
            %% ?INFO("RoleLev:~w, lev_min:~w", [RoleLev, DataItem1#item.lev_min]),
            RoleLev < DataItem1#item.lev_min;
        false -> false
    end,
    Disable2 = case My2 =/= false andalso My1 =/= false 
        andalso lists:member(My1#myitem.pos, ?EQU_POS) of
        true -> 
            DataItem2 = data_item:get(My2#myitem.tid),
            %% ?INFO("RoleLev:~w, lev_min:~w", [RoleLev, DataItem2#item.lev_min]),
            RoleLev < DataItem2#item.lev_min;
        false -> false
    end,
    if
        My1 == false -> 
            ?WARNING("Illigal moving! Id:~w, Pos:~w", [Id, Pos]),
            {error, error};
        Disable1 orelse Disable2 ->
            %% ?WARNING("Illigal moving! Disable 1:~w, 2:~w", [Disable1, Disable2]),
            {error, lev_min};
        Pos < 0 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        (OldPos >= 1 andalso OldPos =< 36) andalso (Pos >= 49 andalso Pos =< 84) ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        (OldPos >= 49 andalso OldPos =< 84) andalso (Pos < 49 orelse Pos > 84) ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        OldPos >= 97 andalso Pos > 36 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        Pos == 97 andalso Sort /= 1 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        Pos == 98 andalso Sort /= 11 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        Pos == 99 andalso Sort /= 5 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        Pos == 100 andalso Sort /= 6 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        Pos == 101 andalso Sort /= 4 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        Pos == 102 andalso Sort /= 12 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        Pos == 103 andalso Sort /= 15 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        Pos == 104 andalso Sort /= 20 ->
            ?WARNING("Illigal moving! ~w[~w -> ~w]", [My1#myitem.tid, OldPos, Pos]),
            {error, error};
        My2 == false ->
            %% 直接移动
            My11 = My1#myitem{pos = Pos},
            MyItems1 = lists:keyreplace(Id, 2, MyItems, My11),
            {ok, My1#myitem.pos, MyItems1};
        true ->
            %% 交换位置
            My11 = My1#myitem{pos = Pos},
            My21 = My2#myitem{pos = My1#myitem.pos},
            MyItems1 = lists:keyreplace(Id, 2, MyItems, My11),
            MyItems2 = lists:keyreplace(My2#myitem.id, 2, MyItems1, My21),
            {ok, My1#myitem.pos, MyItems2}
    end.

sort_myitems() ->
    MyItems = get_myitems(),
    %% ?INFO("sort_myitems1:~p", [MyItems]),
    Combined = combine(MyItems, []),
    Tabs = lists:foldl(fun sort_tab/2, [[], [], []], Combined),
    [My1_36, My49_84, My97_101] = [sort_tid(sort_id(X)) || X <- Tabs],
    M1 = set_pos(My1_36, [], 1, 36),
    M2 = set_pos(My49_84, [], 49, 84),
    M3 = My97_101,
    MyItems2 = M1 ++ M2 ++ M3,
    %% ?INFO("sort_myitems2:~p", [MyItems2]),
    put_myitems(MyItems2).


sort_tab(I, [I1, I2, I3]) ->
    if
        I#myitem.pos >=1 andalso I#myitem.pos =< 36 ->
            [[I | I1], I2, I3];
        I#myitem.pos >=49 andalso I#myitem.pos =< 84 ->
            [I1, [I | I2], I3];
        I#myitem.pos >=97 ->
            [I1, I2, [I | I3]];
        true -> [I1, I2, I3]
    end.

set_pos([H | T], Result, Min, Max) when Min =< Max ->
    NewResult = [H#myitem{pos = Min} | Result],
    set_pos(T, NewResult, Min + 1, Max);
set_pos([], Result, _Min, _Max) -> Result;
set_pos(_, Result, Min, Max) when Min > Max ->
    ?ERR("set_pos error"),
    Result.

%% 取得整理后的包裹列表
combine([], Result) -> Result;
combine(Items, Result) ->
    [H | T] = Items,
    case H#myitem.max_num =< 1 orelse H#myitem.num >= H#myitem.max_num of
        true -> 
            %% 将不能合并的放入结果集中
            combine(T, [H | Result]);
        false ->
            L = [X || X <- Items, X#myitem.tid =:= H#myitem.tid],
            CL = combine1(L, []), 
            L1 = [X || X <- Items, X#myitem.tid =/= H#myitem.tid],
            combine(L1, lists:append([CL, Result]))
    end.

%% 合并同类型的物品
%% 优先保留ID较小的物品
combine1([H], Sorted) ->
    case H#myitem.num =:= 0 of
        true  -> Sorted;
        false -> [H | Sorted]
    end;
combine1([H | T], Sorted) ->
    [H1 | T1] = T,
    {My1, My2} = case H#myitem.id < H1#myitem.id of 
        true -> {H, H1};
        false -> {H1, H}
    end,
    N = My1#myitem.num + My2#myitem.num,
    case N >= My1#myitem.max_num of
        true ->
            My11 = My1#myitem{num = My1#myitem.max_num},
            My21 = My2#myitem{num = N - My1#myitem.max_num},
            combine1([My21 | T1], [My11 | Sorted]);
        false ->
            My11 = My1#myitem{num = N},
            combine1([My11 | T1], Sorted)
    end;
combine1([], Sorted) -> Sorted.

sort_id([]) -> [];
sort_id([RR | T]) ->
    sort_id([R || R <- T, R#myitem.id < RR#myitem.id])
    ++ [RR] ++
    sort_id([R || R <- T, R#myitem.id >= RR#myitem.id]).

sort_tid([]) -> [];
sort_tid([RR | T]) ->
    sort_tid([R || R <- T, R#myitem.tid < RR#myitem.tid])
     ++[RR] ++
    sort_tid([R || R <- T, R#myitem.tid >= RR#myitem.tid]).

get_pos(MyItems, Tab) ->
    case Tab of
        1 -> get_pos(MyItems, 1, 36);
        _ -> get_pos(MyItems, 49, 84)
    end.
get_pos(MyItems, Min, Max) when Min =< Max ->
    case lists:keymember(Min, #myitem.pos, MyItems) of
        true -> get_pos(MyItems, Min + 1, Max);
        false -> Min
    end;
get_pos(_, Min, Max) when Min > Max -> 0.

%% 测试用
test_add_item(Tid, Num, PidSender) ->
    Item = data_item:get(Tid),
    MyItems = get_myitems(),
    Pos = case Item of 
        undefined -> 0;
        _ -> get_pos(MyItems, Item#item.tab)
    end,
    if
        Item == undefined ->
            {false, <<"物品有误">>};
        Pos == 0 ->
            {false, <<"背包已满">>};
        true ->
            Attr = case lists:member(Item#item.sort, ?EQU_TYPE) of
                true -> #equ_attr{
                        hp_max = Item#item.attr#equ_attr.hp_max
                        ,dmg = Item#item.attr#equ_attr.dmg
                        ,dmg_speed = Item#item.attr#equ_attr.dmg_speed
                        ,move_speed = Item#item.attr#equ_attr.move_speed
                        ,crit = Item#item.attr#equ_attr.crit
                        ,attack   = Item#item.attr#equ_attr.attack
                        ,fb_attack   = Item#item.attr#equ_attr.fb_attack
                    };
                false -> undefined
            end,
            Num1 = case Num > Item#item.max_num of
                true -> Item#item.max_num;
                false -> Num
            end,
            My = #myitem{
                id = get_new_id()
                ,tid = Tid
                ,tab = Item#item.tab
                ,sort = Item#item.sort
                ,pos = Pos
                ,max_num = Item#item.max_num
                ,num = Num1
                ,attr = Attr
                ,etime = init_etime(Item#item.period)
            },
            put_myitems([My | MyItems]),
            ?INFO("test_add_item Pos:~w, Tid:~w, Id:~w", [Pos, Tid, My#myitem.id]),
            Data = [
                My#myitem.id 
                ,My#myitem.tid
                ,My#myitem.tab
                ,My#myitem.sort
                ,My#myitem.pos 
                ,My#myitem.num 
                ,My#myitem.lev 
                ,calc_etime(My#myitem.etime) 
            ],
            lib_conn:pack_send(PidSender, 17111, Data),
            true
    end.

add_items(L, MyItems) ->
    add_items(L, MyItems, []).

add_items([{Id, Num} | T], MyItems, Add) ->
    case add_item(Id, Num, MyItems) of
        {ok, MyItems1, Add1} ->
            add_items(T, MyItems1, Add1 ++ Add);
        {error, Reason} -> {error, Reason}
    end;
add_items([], MyItems, Add) -> {ok, MyItems, Add}.

add_item(Tid, Num, MyItems) ->
    add_item(Tid, Num, 0, MyItems).
add_item(Tid, Num, EnLev, MyItems) ->
    add_item(Tid, Num, EnLev, MyItems, []).
add_item(Tid, Num, EnLev, MyItems, AddMy) ->
    Item = data_item:get(Tid),
    Pos = get_pos(MyItems, Item#item.tab),
    if
        Item == undefined ->
            {error, incorrect_id};
        Pos == 0 ->
            {error, at_full};
        true ->
            Sort = Item#item.sort,
            Attr = case lists:member(Sort, ?EQU_TYPE) of
                true -> 
                    case Item#item.attr of
                        undefined ->
                            ?INFO("Error Item(Tid:~w):~w", [Tid, Item]),
                            undefined;
                        #equ_attr{} ->
                            #equ_attr{
                                hp_max = Item#item.attr#equ_attr.hp_max
                                ,dmg = Item#item.attr#equ_attr.dmg
                                ,dmg_speed = Item#item.attr#equ_attr.dmg_speed
                                ,move_speed = Item#item.attr#equ_attr.move_speed
                                ,crit = Item#item.attr#equ_attr.crit
                                ,fb_attack   = Item#item.attr#equ_attr.fb_attack
                                ,attack   = Item#item.attr#equ_attr.attack
                            }
                    end;
                false -> undefined
            end,
            case Num =< Item#item.max_num of
                true ->
                    My = #myitem{
                        id = get_new_id()
                        ,tid = Tid
                        ,tab = Item#item.tab
                        ,sort = Item#item.sort
                        ,pos = Pos
                        ,max_num = Item#item.max_num
                        ,num = Num 
                        ,attr = Attr
                        ,etime = init_etime(Item#item.period)
                    },
                    {ok, [My | MyItems], [My | AddMy]};
                false ->
                    Num1 = Num - Item#item.max_num,
                    My = #myitem{
                        id = get_new_id()
                        ,tid = Tid
                        ,tab = Item#item.tab
                        ,sort = Item#item.sort
                        ,pos = Pos
                        ,max_num = Item#item.max_num
                        ,num = Item#item.max_num 
                        ,attr = Attr
                    },
                    MyItems1 = [My | MyItems],
                    AddMy1 = [My | AddMy],
                    add_item(Tid, Num1, EnLev, MyItems1, AddMy1)
            end
    end.

add_item_notice([My | T], PidSender) ->
    Data = [
        My#myitem.id 
        ,My#myitem.tid
        ,My#myitem.tab
        ,My#myitem.sort
        ,My#myitem.pos 
        ,My#myitem.num 
        ,My#myitem.lev 
        ,calc_etime(My#myitem.etime)
    ],
    lib_conn:pack_send(PidSender, 17111, Data),
    add_item_notice(T, PidSender);
add_item_notice([], _PidSender) -> ok.

update_myitem_notice(MyItem, PidSender) ->
    Attr = case lists:member(MyItem#myitem.sort, ?EQU_TYPE) of
        true ->
            #equ_attr{
                crit = Crit
                ,hp_max = HpMax
                ,move_speed = MoveSpeed
                ,dmg_speed = DmgSpeed
                ,attack = Attack
                ,fb_attack = FbAttack
            } = MyItem#myitem.attr,
            [[Crit, 0, HpMax, MoveSpeed, DmgSpeed, Attack, FbAttack]];
        false -> []
    end,
    Data = [
        MyItem#myitem.id 
        ,MyItem#myitem.tid
        ,MyItem#myitem.tab
        ,MyItem#myitem.sort
        ,MyItem#myitem.pos 
        ,MyItem#myitem.num 
        ,MyItem#myitem.lev 
        ,Attr
        ,calc_etime(MyItem#myitem.etime) 
    ],
    lib_conn:pack_send(PidSender, 17117, Data).


get_weapon_id(MyItems) ->
    case lists:keyfind(97, #myitem.pos, MyItems) of
        false -> 0;
        MyItem -> MyItem#myitem.tid
    end.

%% 战斗道具
clear_mydaojus() ->
    erase(mydaoju).

get_mydaojus() ->
    case get(mydaoju) of
        undefined -> [];
        D -> D
    end.

put_mydaojus(Daoju) ->
    put(mydaoju, Daoju).

add_mydaoju(Tid) ->
    Daoju = get_mydaojus(),
    Pos = get_daoju_pos(Daoju),
    case Pos == 0 of
        true -> {error, at_full};
        false -> 
            Daoju1 = [{Pos, Tid} | Daoju],
            put(mydaoju, Daoju1),
            {ok, Daoju1}
    end.

get_daoju_pos(Daoju) ->
    get_daoju_pos(1, Daoju).
get_daoju_pos(Index, Daoju) when Index =< ?MYDAOJU_MAX ->
    case lists:keymember(Index, 1, Daoju) of
        true -> get_daoju_pos(Index + 1, Daoju);
        false -> Index
    end;
get_daoju_pos(Index, _Daoju) when Index > ?MYDAOJU_MAX -> 0.

use_daoju(Pos) ->
    Daoju = get_mydaojus(),
    case lists:keyfind(Pos, 1, Daoju) of
        false -> 0;
        {_, Tid} ->
            Daoju1 = lists:keydelete(Pos, 1, Daoju),
            put(mydaoju, Daoju1),
            Tid
    end.

%%' zip/unzip
zip(L) -> zip(L, []).
zip([I | T], Reply) ->
    #myitem{
        id          = X1
        ,tid        = X2
        ,tab        = X3
        ,sort       = X4
        ,pos        = X5
        ,max_num    = X6
        ,num        = X7
        ,lev        = X8      
        ,attr       = X9
        ,etime      = X10
    } = I,
    X900 = case X9 of
        undefined -> u;
        #equ_attr{
            hp_max      = X901
            ,dmg        = X902
            ,move_speed = X903
            ,dmg_speed  = X904
            ,crit       = X905
            ,fb_attack  = X906
            ,attack     = X907
        } -> {e, X901, X902, X903, X904, X905, X906, X907}
    end,
    zip(T, [{X1, X2, X3, X4, X5, X6, X7, X8, X900, X10} | Reply]);
zip([], Reply) -> Reply.

unzip(L) -> unzip(L, []).
unzip([{X1, X2, X3, X4, X5, X6, X7, X8, X9, X10} | T], Reply) ->
    X900 = case X9 of
        u -> undefined;
        {e, X901, X902, X903, X904, X905, X906, X907} ->
            #equ_attr{
                hp_max      = X901
                ,dmg        = X902
                ,move_speed = X903
                ,dmg_speed  = X904
                ,crit       = X905
                ,fb_attack  = fix_fb_attack(X906, X2, X4, X8)
                ,attack     = X907
            }
    end,
    MyItem = #myitem{
        id          = X1
        ,tid        = X2
        ,tab        = X3
        ,sort       = X4
        ,pos        = X5
        ,max_num    = X6
        ,num        = X7
        ,lev        = X8      
        ,attr       = X900
        ,etime      = X10
    },
    unzip(T, [MyItem | Reply]);
unzip([MyItem = #myitem{tid = Tid, lev = Lev, attr = Attr, sort = Sort} | T], Reply) ->
    Attr1 = case Attr of
        undefined -> undefined;
        #equ_attr{
            fb_attack  = FbAttack
        } -> Attr#equ_attr{fb_attack = fix_fb_attack(FbAttack, Tid, Sort, Lev)}
    end,
    MyItem1 = MyItem#myitem{attr = Attr1},
    unzip(T, [MyItem1 | Reply]);
unzip([X | T], Reply) ->
    ?INFO(" *** undefined Data:~w", [X]),
    unzip(T, Reply);
unzip([], Reply) -> Reply.
%%.

fix_fb_attack(0, Tid, 1, Lev) ->
    V = fix_fb_attack_base(Tid) + fix_fb_attack_en(Lev),
    %% ?INFO("fix_fb_attack: ~w -> ~w, Lev:~w", [Tid, V, Lev]),
    V;
fix_fb_attack(FbAttack, _Tid, _Sort, _Lev) ->
    FbAttack.

fix_fb_attack_base(Tid) ->
    L = [
         {110001,  25}
        ,{110016,  30}
        ,{110017,  35}
        ,{110018,  40}
        ,{110038,  10}
        ,{110002,  40}
        ,{110007,  50}
        ,{110019,  60}
        ,{110020,  70}
        ,{110034,  40}
        ,{110035,  70}
        ,{110036,  70}
        ,{110037,  70}
        ,{110039,  70}
        ,{110026,  80}
        ,{110027,  90}
        ,{110028, 110}
        ,{110029, 120}
        ,{110030,  80} 
        ,{110031, 120}
        ,{110032, 120}
        ,{110033, 120}
        ,{110040, 120}
        ,{110003, 130}
        ,{110008, 160}
        ,{110009, 190}
        ,{110021, 220}
        ,{110022, 130}
        ,{110023, 220}
        ,{110024, 220}
        ,{110025, 220}
        ,{110004, 230}
        ,{110010, 250}
        ,{110011, 280}
        ,{110012, 310}
        ,{110005, 320}
        ,{110013, 350}
        ,{110014, 380}
        ,{110015, 400}
        ,{110006, 220}
        ,{110041, 220}
        ,{110042, 220}
        ,{110043, 120}
        ,{110044, 120}
    ],
    util:get_val(Tid, L, 0).

fix_fb_attack_en(Lev) ->
    L = [
         {1 ,  10} 
        ,{2 ,  20}
        ,{3 ,  30}
        ,{4 ,  40}
        ,{5 ,  50}
        ,{6 ,  65}
        ,{7 ,  80}
        ,{8 ,  95}
        ,{9 , 110}
        ,{10, 125}
        ,{11, 145}
        ,{12, 165}
        ,{13, 185}
        ,{14, 205}
        ,{15, 225}
        ,{16, 255}
        ,{17, 285}
        ,{18, 315}
        ,{19, 345}
        ,{20, 375}
    ],
    util:get_val(Lev, L, 0).


