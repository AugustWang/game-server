%%----------------------------------------------------
%% 物品
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_item).
-export([handle/3]).

-include("common.hrl").
-include("enhance.hrl").
-include("daoju.hrl").
-include("combine1.hrl").
-include("equ_upgrade.hrl").
-include("identify.hrl").
-include("renew.hrl").

%% 道具栏信息
%% handle(17001, [], _Rs) ->
%%     MyDaoju = lib_item:get_mydaoju(),
%%     {ok, [MyDaoju]};

handle(17001, [], Rs) ->
    case Rs#role.status of
        3 -> gen_server:cast(Rs#role.pid_room,
                {send_mydaojus, Rs#role.id, Rs#role.pid_sender}),
            {ok};
        _ -> 
            MyDaoju = lib_item:get_mydaojus(),
            {ok, [MyDaoju]}
    end;

%% handle(17003, [ItemId], Rs) ->
%%     %% ?INFO("use_daoju:~w", [ItemId]),
%%     case Rs#role.status of
%%         3 ->
%%             case lib_item:use_daoju(ItemId) of
%%                 0 -> 
%%                     %% ?INFO("没有道具可被使用! ItemId:~w, Rid:~w", [ItemId, Rs#role.id]),
%%                     ok;
%%                 Tid ->
%%                     self() ! {task, ?TASK_USE_DAOJU, {add, Tid, 1}},
%%                     gen_server:cast(Rs#role.pid_room, 
%%                         {use_mydaoju, Rs#role.id, Rs#role.pid_sender, ItemId, Tid})
%%             end;
%%         _ -> 
%%             %% ?INFO("Error status(~w) when use_daoju!", [Rs#role.status])
%%             ok
%%     end,
%%     {ok};

handle(17003, [Pos], Rs) ->
    %% ?INFO("use_daoju:~w", [ItemId]),
    case Rs#role.status of
        3 ->
            gen_server:cast(Rs#role.pid_room, 
                {use_mydaoju, Rs#role.id, Rs#role.pid_sender, Pos});
        _ -> ok
    end,
    {ok};

%% 技能列表
handle(17006, [], Rs) ->
    %% ?INFO("skill:~w", [Rs#role.skill]),
    {ok, [Rs#role.skill]};

%% 升级技能
handle(17007, [SkillId, ItemId], Rs) ->
    MyItems = lib_item:get_myitems(),
    case lib_item:get_myitem(ItemId, MyItems) of
        false -> 
            ?ERR("物品不存在 ItemId:~w", [ItemId]),
            {ok, [1, SkillId, 0, 0]};
        MyItem ->
            Item = data_item:get(MyItem#myitem.tid),
            AddExp = Item#item.ctl1,
            case Item#item.sort == 10 andalso AddExp > 0 of
                true ->
                    case lib_item:del_items(by_id, [[ItemId, 1]], MyItems) of
                        {ok, MyItems1, Dels} ->
                            case lib_skill:add_exp(Rs, SkillId, AddExp) of
                                {ok, Rs1, SkillId1, AddExp1} -> 
                                    SkillLev = SkillId1 rem 10,
                                    self() ! {task, ?TASK_UPGRADE_SKILL, {to, SkillLev}},
                                    lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                                    lib_item:put_myitems(MyItems1),
                                    {ok, [0, SkillId, SkillId1, AddExp1], Rs1};
                                {error, top_lev} -> 
                                    {ok, [2, SkillId, 0, 0]};
                                {error, _} -> 
                                    {ok, [3, SkillId, 0, 0]}
                            end;
                        {error, Reason} ->
                            ?ERR("物品不存在 Reason:~w", [Reason]),
                            {ok, [1, SkillId, 0, 0]}
                    end;
                false -> 
                    ?ERR("物品不正确 ItemId:~w", [ItemId]),
                    {ok, [1, SkillId, 0, 0]}
            end
    end;

%% 携带技能
handle(17008, [SkillId, Pos], Rs) ->
    %% ?INFO("SkillId:~w, Pos:~w, Skills:~w", [SkillId, Pos, Rs#role.skill]),
    case lib_skill:set_pos(Rs, SkillId, Pos) of
        {error, _} -> {ok, [1, SkillId]};
        {ok, Rs1} -> {ok, [0, SkillId], Rs1}
    end;

%% 使用技能
handle(17009, [SkillId], Rs) ->
    case Rs#role.status of
        3 ->
            case lib_skill:check(Rs, SkillId) of
                true ->
                    gen_server:cast(Rs#role.pid_room, 
                        {use_skill, Rs#role.id, Rs#role.pid_sender, SkillId}),
                    {ok};
                false -> 
                    ?INFO("check false，SkillId:~w", [SkillId]),
                    {ok, [SkillId, 0, 0]}
            end;
        _ -> 
            %% ?INFO("error status:~w，SkillId:~w", [Rs#role.status, SkillId]),
            {ok, [SkillId, 0, 0]}
    end;

%% 取消携带
handle(17010, [SkillId], Rs) ->
    case lib_skill:set_pos(Rs, SkillId, 0) of
        {error, _} -> 
            ?INFO("取消携带失败：~w", [SkillId]),
            {ok};
        {ok, Rs1} -> {ok, Rs1}
    end;

%% 物品续费
handle(17021, [ItemId, Time], Rs) ->
    MyItems = lib_item:get_myitems(),
    case lib_item:get_myitem(ItemId, MyItems) of
        false -> {ok, [4]};
        MyItem ->
            case renew(MyItem#myitem.tid, Time, Rs) of
                {ok, Rs1} ->
                    Now = util:unixtime(),
                    Rest = MyItem#myitem.etime - Now,
                    Rest1 = case Rest > 0 of
                        true -> Rest;
                        false -> 0
                    end,
                    Etime = util:unixtime() + Time + Rest1,
                    MyItem1 = MyItem#myitem{etime = Etime},
                    MyItems2 = lib_item:set_myitem(MyItem1, MyItems),
                    Rs2 = lib_role:calc_attrs(Rs1, MyItems2),
                    lib_item:put_myitems(MyItems2),
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    lib_item:update_myitem_notice(MyItem1, Rs#role.pid_sender),
                    Save = [{myitems, MyItems2}, {kvs, lib_role:get_kvs(Rs2)}],
                    ?LOG_GOLD(Rs, Rs2, 2007),
                    ?LOG_CARD(Rs, Rs2, 2007),
                    {ok, [0], Rs2#role{save = Save}};
                {error, Code} -> {ok, [Code]}
            end
    end;

handle(17101, [], Rs) ->
    MyItems = lib_item:get_myitems(),
    Data = transit_items(MyItems, []),
    Rs1 = lib_role:calc_attrs(Rs, MyItems),
    %% ?INFO("MyItems:~w", [Data]),
    {ok, [Data], Rs1};

handle(17103, [Id, Pos], Rs) ->
    MyItems = lib_item:get_myitems(),
    RoleLev = Rs#role.lev,
    case lib_item:move_item(Id, Pos, RoleLev, MyItems) of
        {ok, OldPos, MyItems1} ->
            lib_item:put_myitems(MyItems1),
            case lists:member(Pos, ?EQU_POS) 
                orelse lists:member(OldPos, ?EQU_POS) of
                true ->
                    case lists:member(Pos, ?EQU_POS) of
                        true ->
                            %% 装上了装备
                            case lists:keyfind(Id, 2, MyItems) of
                                false -> ok;
                                M -> self() ! {task, ?TASK_WEAR_EQU, {to, M#myitem.tid}}
                            end;
                        false -> ok
                    end,
                    Rs1 = lib_role:calc_attrs(Rs, MyItems1),
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    {ok, [Id, Pos], Rs1};
                false ->
                    {ok, [Id, Pos]}
            end;
        {error, _} -> 
            {ok, [0, 0]}
    end;

handle(17105, [_Tab], _Rs) ->
    lib_item:sort_myitems(),
    {ok};

%% 出卖物品
handle(17107, [Id], Rs) ->
    MyItems = lib_item:get_myitems(),
    case lib_item:get_myitem(Id, MyItems) of
        false -> 
            ?INFO("Item not found when sell:~w", [Id]),
            {ok, [1]};
        MyItem ->
            case lib_item:del_items(by_id, [[MyItem#myitem.id, MyItem#myitem.num]], MyItems) of
                {ok, MyItems1, Dels} ->
                    Item = data_item:get(MyItem#myitem.tid),
                    lib_item:put_myitems(MyItems1),
                    Rs1 = lib_role:add_attr(Rs, gold, Item#item.gold * MyItem#myitem.num, 1010),
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                    ?LOG({del_item, Rs#role.id, MyItem#myitem.tid}),
                    {ok, [0], Rs1};
                {error, _} ->
                    {ok, [1]}
            end
    end;

handle(17108, [ItemId], _Rs) ->
    case lib_item:use_daoju(ItemId) of
        0 -> {ok, [3, 0]};
        Tid ->
            Daoju = data_daoju:get(Tid),
            if
                Daoju == undefined ->
                    ?INFO("道具不存在：~w", [Tid]),
                    {ok, [1, 0]};
                true ->
                    {ok, [0, ItemId]}
            end
    end;

%% handle(17108, [ItemId], Rs) ->
%%     case lib_item:use_daoju(ItemId) of
%%         0 -> {ok, [3, 0]};
%%         Tid ->
%%             Daoju = data_daoju:get(Tid),
%%             if
%%                 Daoju == undefined ->
%%                     ?INFO("道具不存在：~w", [Tid]),
%%                     {ok, [1, 0]};
%%                 Daoju#daoju.gold =< 0 ->
%%                     ?INFO("此道具不可出售：~w", [Tid]),
%%                     {ok, [2, 0]};
%%                 true ->
%%                     Rs1 = lib_role:add_attr(Rs, gold, Daoju#daoju.gold),
%%                     {ok, [0, ItemId], Rs1}
%%             end
%%     end;

%% 删除物品
handle(17109, [InDels], _Rs) ->
    MyItems = lib_item:get_myitems(),
    case lib_item:del_items(by_id, InDels, MyItems) of
        {ok, MyItems1, Dels} ->
            lib_item:put_myitems(MyItems1),
            %% ?INFO("17109-物品出错：~w", [Dels]),
            {ok, [Dels]};
        {error, _} ->
            ?INFO("删除物品出错：~w", [InDels]),
            {ok}
    end;

%% 强化
handle(17113, [ItemId, PartId1, PartId2, PartId3, LuckId0, ProtecteId0], Rs) ->
    MyItems = lib_item:get_myitems(),
    MyItem = lib_item:get_myitem(ItemId, MyItems),
    {LuckId, Luck} = calc_erate_luck(LuckId0),
    Rate1 = calc_erate([PartId1, PartId2, PartId3]) * Luck,
    Enhance = enhance(MyItem),
    ProtecteId = check_protecte(ProtecteId0),
    case {MyItem, Rate1, Enhance} of
        {false, _, _         } -> {ok, [3, ItemId, 0]};
        {_    , 0, _         } -> {ok, [1, ItemId, 0]};
        {_    , _, {error, _}} -> {ok, [4, ItemId, 0]};
        {_    , _, {ok, MyItem1, Reply1}} ->
            Gold = util:get_val(gold, Reply1),
            Rate2 = util:get_val(success_rate, Reply1),
            case lib_role:spend(gold, Gold, Rs) of
                {ok, Rs1} ->
                    Need = [
                        {PartId1, 1}
                        ,{PartId2, 1}
                        ,{PartId3, 1}
                        ,{LuckId, 1}
                        ,{ProtecteId, 1}
                    ],
                    case lib_item:del_items(by_tid, Need, MyItems) of
                        {ok, MyItems1, Dels} ->
                            %% ?INFO("~w / ~w = ~w", [Rate1, Rate2, Rate1 / Rate2 * 100]),
                            {MyItem2, Code} = case util:rate(Rate1 / Rate2 * 100) of
                                true -> 
                                    NewLev1 = MyItem1#myitem.lev,
                                    ?LOG({enhance, Rs#role.id, MyItem#myitem.tid, NewLev1}),
                                    case NewLev1 > 5 of
                                        true ->
                                            lib_conn:pack_cast(world, 15007, 
                                                [Rs#role.id, Rs#role.name, MyItem#myitem.tid, NewLev1]),
                                            ok;
                                        false -> ok
                                    end,
                                    {MyItem1, 0};
                                false -> {enhance_failure(MyItem, ProtecteId), 10}
                            end,
                            NewLev = MyItem2#myitem.lev,
                            self() ! {task, ?TASK_UPGRADE_EQU, {to, NewLev}},
                            self() ! {task, ?TASK_ENHANCE, {to, MyItem#myitem.tid, NewLev}},
                            MyItems2 = lib_item:set_myitem(MyItem2, MyItems1),
                            Rs2 = lib_role:calc_attrs(Rs1, MyItems2),
                            lib_item:put_myitems(MyItems2),
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            lib_item:update_myitem_notice(MyItem2, Rs#role.pid_sender),
                            Save = [{myitems, MyItems2}, {kvs, lib_role:get_kvs(Rs2)}],
                            ?LOG_GOLD(Rs, Rs2, 2003),
                            {ok, [Code, ItemId, MyItem2#myitem.lev], Rs2#role{save = Save}};
                        {error, _} ->
                            %% 材料不足
                            {ok, [1, ItemId, 0]}
                    end;
                {error, _} ->
                    %% 金币不足
                    {ok, [2, ItemId, 0]}
            end
    end;

%% 合成
handle(17115, [ItemId], Rs) ->
    %% ?INFO("17115:~w", [ItemId]),
    MyItems = lib_item:get_myitems(),
    case lib_item:get_myitem(ItemId, MyItems) of
        false -> 
            %% ?ERR("物品不存在 ItemId:~w", [ItemId]),
            {ok, [1]};
        MyItem ->
            case data_combine1:get(MyItem#myitem.tid) of
                undefined ->
                    ?ERR("此物品在合成表中未定义：~w", [MyItem#myitem.tid]),
                    {ok, [1]};
                Com1 ->
                    case lib_role:spend(gold, Com1#combine1.gold, Rs) of
                        {ok, Rs1} ->
                            case lib_item:del_item(by_tid, MyItem#myitem.tid, Com1#combine1.num, MyItems) of
                                {ok, MyItems1, Dels} ->
                                    case lib_item:add_item(Com1#combine1.tid, 1, MyItems1) of
                                        {ok, MyItems2, My} ->
                                            lib_item:add_item_notice(My, Rs1#role.pid_sender),
                                            lib_conn:pack_send(Rs1#role.pid_sender, 17109, [Dels]),
                                            lib_item:put_myitems(MyItems2),
                                            self() ! {task, ?TASK_COMBINE, {add, Com1#combine1.tid, 1}},
                                            Save = [{myitems, MyItems2}],
                                            ?LOG({combine, Rs1#role.id, MyItem#myitem.tid}),
                                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                                            ?LOG_GOLD(Rs, Rs1, 2004),
                                            {ok, [0], Rs1#role{save = Save}};
                                        {error, at_full} -> 
                                            {ok, [2]};
                                        {error, Reason} -> 
                                            ?ERR("error: ~w", [Reason]),
                                            {ok, [1]}
                                    end;
                                {error, _} ->
                                    %% ?INFO("材料不足", []),
                                    {ok, [1]}
                            end;
                        {error, _} ->
                            %% ?INFO("金币不足", []),
                            {ok, [3]}
                    end
            end
    end;

%% 使用物品
handle(17119, [ItemId], Rs) ->
    %% ?INFO("17119 ItemId:~w", [ItemId]),
    MyItems = lib_item:get_myitems(),
    case lib_item:get_myitem(ItemId, MyItems) of
        false -> 
            ?WARNING("Item not found:~w", [ItemId]),
            {ok, [ItemId, 0, 1]};
        MyItem ->
            #myitem{tid = Tid, sort = Sort} = MyItem,
            %% ?INFO("use ItemId, Sort:~w, Tid:~w", [Sort, Tid]),
            case {Sort, Tid} of
                {18, _} ->
                    %% 经验卡
                    case lib_item:del_items(by_id, [[ItemId, 1]], MyItems) of
                        {ok, MyItems1, Dels} ->
                            #item{ctl1 = Exp2Time1} = data_item:get(Tid),
                            Exp2Time2 = lib_role:get_exp2_time(Rs#role.exp2_etime),
                            Exp2ETime = util:unixtime() + Exp2Time1 + Exp2Time2,
                            Rs1 = Rs#role{exp2_etime = Exp2ETime},
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            lib_item:put_myitems(MyItems1),
                            %% ?INFO("use ok, Sort:~w, Tid:~w", [Sort, Tid]),
                            %% ?INFO("Exp2Time1:~w, Exp2Time2:~w", [Exp2Time1, Exp2Time2]),
                            {ok, [ItemId, Tid, 0], Rs1};
                        {error, Reason} ->
                            ?WARNING("error when del item: ~w", [Reason]),
                            {ok, [ItemId, Tid, 1]}
                    end;
                {13, _} ->
                    %% 礼包
                    case lib_item:del_items(by_id, [[ItemId, 1]], MyItems) of
                        {ok, MyItems1, Dels} ->
                            #item{lev_min = LevMin, ctl1 = Ctl1, ctl2 = Ctl2}
                            = data_item:get(Tid),
                            case Rs#role.lev >= LevMin of
                                true ->
                                    AddList = lists:zip(Ctl1, Ctl2),
                                    case lib_item:add_items(AddList, MyItems1) of
                                        {ok, MyItems2, My} ->
                                            lib_item:add_item_notice(My, Rs#role.pid_sender),
                                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                                            AddNotice = [[4, IdX, NumX] || {IdX, NumX} <- AddList],
                                            lib_conn:pack_send(Rs#role.pid_sender, 11030, [AddNotice]),
                                            Save = [{myitems, MyItems2}],
                                            lib_item:put_myitems(MyItems2),
                                            {ok, [ItemId, Tid, 0], Rs#role{save = Save}};
                                        {error, at_full} ->
                                            lib_conn:send_code(Rs#role.pid_sender, 17000101),
                                            {ok, [ItemId, Tid, 1]};
                                        {error, Reason} ->
                                            ?WARNING("error when add item: ~w", [Reason]),
                                            {ok, [ItemId, Tid, 1]}
                                    end;
                                false -> {ok, [ItemId, Tid, 2]}
                            end;
                        {error, Reason} ->
                            ?WARNING("error when del item: ~w", [Reason]),
                            {ok, [ItemId, Tid, 1]}
                    end;
                {_, 190001} ->
                    %% 金币箱
                    case lib_item:del_items(by_id, [[MyItem#myitem.id, 1]], MyItems) of
                        {ok, MyItems1, Dels} ->
                            AddGold = 10000,
                            Rs1 = lib_role:add_attr(Rs, gold, AddGold, 1007),
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            %% ?INFO("17119 del items: ~w", [Dels]),
                            lib_conn:pack_send(Rs#role.pid_sender, 11030, [[[1, AddGold, 0]]]),
                            Save = [{myitems, MyItems1}],
                            lib_item:put_myitems(MyItems1),
                            {ok, [ItemId, Tid, 0], Rs1#role{save = Save}};
                        {error, Reason} ->
                            ?WARNING("error when del item: ~w", [Reason]),
                            {ok, [ItemId, Tid, 1]}
                    end;
                {_, 190002} ->
                    %% 金币箱
                    case lib_item:del_items(by_id, [[MyItem#myitem.id, 1]], MyItems) of
                        {ok, MyItems1, Dels} ->
                            AddGold = 100000,
                            Rs1 = lib_role:add_attr(Rs, gold, AddGold, 1007),
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            %% ?INFO("17119 del items: ~w", [Dels]),
                            lib_conn:pack_send(Rs#role.pid_sender, 11030, [[[1, AddGold, 0]]]),
                            Save = [{myitems, MyItems1}],
                            lib_item:put_myitems(MyItems1),
                            {ok, [ItemId, Tid, 0], Rs1#role{save = Save}};
                        {error, Reason} ->
                            ?WARNING("error when del item: ~w", [Reason]),
                            {ok, [ItemId, Tid, 1]}
                    end;
                {_, 190003} ->
                    %% 点券
                    case lib_item:del_items(by_id, [[MyItem#myitem.id, 1]], MyItems) of
                        {ok, MyItems1, Dels} ->
                            AddCard = 500,
                            Rs1 = lib_role:add_attr(Rs, card, AddCard, 1007),
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            %% ?INFO("17119 del items: ~w", [Dels]),
                            lib_conn:pack_send(Rs#role.pid_sender, 11030, [[[2, AddCard, 0]]]),
                            Save = [{myitems, MyItems1}],
                            lib_item:put_myitems(MyItems1),
                            {ok, [ItemId, Tid, 0], Rs1#role{save = Save}};
                        {error, Reason} ->
                            ?WARNING("error when del item: ~w", [Reason]),
                            {ok, [ItemId, Tid, 1]}
                    end;
                {_, _Tid} -> 
                    ?WARNING("item use type undefined: ~w", [_Tid]),
                    {ok, [ItemId, Tid, 0]}
            end
    end;

%% 使用卷轴
handle(17121, [ItemId], Rs) ->
    MyItems = lib_item:get_myitems(),
    case lib_item:get_myitem(ItemId, MyItems) of
        false -> 
            ?WARNING("卷轴物品不存在 ItemId:~w", [ItemId]),
            {ok, [2]};
        MyItem ->
            case data_equ_upgrade:get(MyItem#myitem.tid) of
                false -> 
                    ?WARNING("找不到升级数据 ID:~w", [MyItem#myitem.tid]),
                    {ok, [4]};
                #equ_upgrade{
                    need_item = NeedItem
                    ,need_num = NeedNum
                    ,to = To
                } ->
                    Need = lists:zip(NeedItem, NeedNum),
                    case lib_item:del_items(by_tid, Need, MyItems) of
                        {ok, MyItems1, Dels1} ->
                            case lib_item:del_items(by_id, [[ItemId, 1]], MyItems1) of
                                {ok, MyItems2, Dels2} ->
                                    case lib_item:add_item(To, 1, MyItems2) of
                                        {ok, MyItems3, NewEqu} ->
                                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels1 ++ Dels2]),
                                            lib_item:add_item_notice(NewEqu, Rs#role.pid_sender),
                                            lib_item:put_myitems(MyItems3),
                                            Save = [{myitems, MyItems3}],
                                            {ok, [0], Rs#role{save = Save}};
                                        {error, at_full} -> 
                                            {ok, [5]};
                                        {error, _Reason} -> 
                                            ?WARNING("添加物品时出错:~w", [_Reason]),
                                            {ok, [5]}
                                    end;
                                {error, Reason} ->
                                    ?WARNING("物品不存在 Reason:~w", [Reason]),
                                    {ok, [1]}
                            end;
                        {error, Reason} ->
                            ?WARNING("error when del item: ~w", [Reason]),
                            {ok, [1]}
                    end
            end
    end;

%% 丢弃游戏中拾取的道具
handle(17123, [ItemId], _Rs) ->
    lib_item:use_daoju(ItemId),
    {ok};

%% 属性转移
handle(17114, [ItemId1, ItemId2], Rs) ->
    MyItems = lib_item:get_myitems(),
    case transfer_attr(ItemId1, ItemId2, MyItems) of
        {ok, MyItems1, MyItem1, MyItem2, LevMax} ->
            SpendGold = data_config:get(change_gold),
            case lib_role:spend(gold, SpendGold * LevMax, Rs) of
                {ok, Rs1} ->
                    lib_item:put_myitems(MyItems1),
                    Rs2 = lib_role:calc_attrs(Rs1, MyItems1),
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    lib_item:update_myitem_notice(MyItem1, Rs#role.pid_sender),
                    lib_item:update_myitem_notice(MyItem2, Rs#role.pid_sender),
                    self() ! {task, ?TASK_TRANSFER_ATTR},
                    ?LOG_GOLD(Rs, Rs2, 2005),
                    {ok, [0], Rs2};
                {error, _} -> {ok, [5]}
            end;
        {error, Code} -> {ok, [Code]}
    end;

%% 鉴定物品
handle(17116, [ItemId], Rs) ->
    SpendGold = data_config:get(confirm_gold),
    case lib_role:spend(gold, SpendGold, Rs) of
        {ok, Rs1} ->
            MyItems = lib_item:get_myitems(),
            MyItem = lib_item:get_myitem(ItemId, MyItems),
            case identify(MyItem) of
                {ok, MyItem1} ->
                    MyItems1 = lib_item:set_myitem(MyItem1, MyItems),
                    %% ?INFO("MyItem :~p~nMyItem1:~p~n", [MyItem, MyItem1]),
                    lib_item:put_myitems(MyItems1),
                    lib_item:update_myitem_notice(MyItem1, Rs#role.pid_sender),
                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                    ?LOG_GOLD(Rs, Rs1, 2006),
                    {ok, [0], Rs1};
                {error, Code} -> {ok, [Code]}
            end;
        {error, _} -> {ok, [2]}
    end;

%% 活动兑换物品
handle(17125, [Id], Rs) ->
    case data_festival:get(Id) of
        undefined -> {ok, [1]};
        Data ->
            Tid = util:get_val(tid, Data),
            Num = util:get_val(num, Data),
            IsShow = util:get_val(is_show, Data, 0),
            MyItems = lib_item:get_myitems(),
            case lib_item:del_item(by_tid, Tid, Num, MyItems) of
                {ok, MyItems1, Dels} ->
                    case lib_item:add_item(Id, 1, MyItems1) of
                        {ok, MyItems2, My} ->
                            lib_item:add_item_notice(My, Rs#role.pid_sender),
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            lib_item:put_myitems(MyItems2),
                            %% self() ! {task, ?TASK_COMBINE, {add, Com1#combine1.tid, 1}},
                            case IsShow of
                                1 ->
                                    lib_conn:pack_cast(world, 15008, [2, Rs#role.id, Rs#role.name, Id]),
                                    ok;
                                _ -> ok
                            end,
                            Save = [{myitems, MyItems2}],
                            %% ?LOG({combine, Rs#role.id, MyItem#myitem.tid}),
                            {ok, [0], Rs#role{save = Save}};
                        {error, at_full} -> 
                            {ok, [2]};
                        {error, Reason} -> 
                            ?ERR("error: ~w", [Reason]),
                            {ok, [1]}
                    end;
                {error, _} -> {ok, [3]}
            end
    end;

%% 转盘
%% luck = {1000, 0, 0} = {累积的点券,使用物品数量,累计价值}
handle(17130, [], Rs) ->
    case data_luck:get(1) of
        [] -> {ok, [1, 0, 0, 0, 0, 0]};
        LuckData ->
            SpendTid = 320001,
            SpendNum = 1,
            MyItems = lib_item:get_myitems(),
            #role{id = Rid, name = Name, luck = {CardSum, UseSum, ValSum}} = Rs,
            case lib_item:del_item(by_tid, SpendTid, SpendNum, MyItems) of
                {ok, MyItems1, Dels} ->
                    Rand = util:rand(1, 100000),
                    case luck_process(Rand, LuckData) of
                        {1, Num, Val} ->
                            Rs1 = lib_role:add_attr(Rs, gold, Num, 1005),
                            CardSum1 = CardSum + 25, 
                            UseSum1 = UseSum + 1, 
                            ValSum1 = ValSum + Val,
                            Luck1 = {CardSum1, UseSum1, ValSum1},
                            Rs2 = Rs1#role{luck = Luck1},
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            gen_server:cast(srv_luck, {set_myrank, Rid, Name, Val, 
                                    CardSum1, UseSum1, ValSum1, 1, Num}),
                            lib_item:put_myitems(MyItems1),
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            {ok, [0, CardSum1, UseSum1, ValSum1, 1, Num], Rs2};
                        {2, Num, Val} ->
                            Rs1 = lib_role:add_attr(Rs, card, Num, 1005),
                            CardSum1 = CardSum + 25, 
                            UseSum1 = UseSum + 1, 
                            ValSum1 = ValSum + Val,
                            Luck1 = {CardSum1, UseSum1, ValSum1},
                            Rs2 = Rs1#role{luck = Luck1},
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            gen_server:cast(srv_luck, {set_myrank, Rid, Name, Val,
                                    CardSum1, UseSum1, ValSum1, 2, Num}),
                            lib_item:put_myitems(MyItems1),
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            {ok, [0, CardSum1, UseSum1, ValSum1, 2, Num], Rs2};
                        {4, Num, Val} ->
                            Rs1 = lib_role:add_attr(Rs, card, CardSum, 1006),
                            CardSum1 = 1000,
                            UseSum1 = UseSum + 1, 
                            ValSum1 = ValSum + Val,
                            Luck1 = {CardSum1, UseSum1, ValSum1},
                            Rs2 = Rs1#role{luck = Luck1},
                            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                            gen_server:cast(srv_luck, {set_myrank, Rid, Name, Val,
                                    CardSum1, UseSum1, ValSum1, 4, Num}),
                            lib_item:put_myitems(MyItems1),
                            lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                            {ok, [0, CardSum1, UseSum1, ValSum1, 4, Num], Rs2};
                        {RewardTid, Num, Val} ->
                            case lib_item:add_item(RewardTid, Num, MyItems1) of
                                {ok, MyItems2, My} ->
                                    lib_item:add_item_notice(My, Rs#role.pid_sender),
                                    Save = [{myitems, MyItems2}],
                                    CardSum1 = CardSum + 25,
                                    UseSum1 = UseSum + 1, 
                                    ValSum1 = ValSum + Val,
                                    Luck1 = {CardSum1, UseSum1, ValSum1},
                                    Rs2 = Rs#role{luck = Luck1, save = Save},
                                    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
                                    lib_item:put_myitems(MyItems2),
                                    lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                                    gen_server:cast(srv_luck, {set_myrank, Rid, Name, Val,
                                            CardSum1, UseSum1, ValSum1, RewardTid, Num}),
                                    {ok, [0, CardSum1, UseSum1, ValSum1, RewardTid, Num], Rs2};
                                {error, at_full} ->
                                    %% lib_conn:send_code(Rs#role.pid_sender, 17000101),
                                    {ok, [3, CardSum, UseSum, ValSum, 0, 0]};
                                {error, Reason} -> 
                                    ?ERR("error: ~w", [Reason]),
                                    {ok, [1, CardSum, UseSum, ValSum, 0, 0]}
                            end
                    end;
                {error, _} -> {ok, [2, CardSum, UseSum, ValSum, 0, 0]}
            end
    end;

handle(17131, [], Rs) ->
    {CardSum, UseSum, ValSum} = Rs#role.luck,
    srv_luck ! {recent_list, CardSum, UseSum, ValSum,
        Rs#role.pid_sender},
    {ok};

handle(17132, [], Rs) ->
    srv_luck ! {rank_list, Rs#role.pid_sender},
    {ok};

%% 领取成长礼包
handle(17140, [], Rs) ->
    Bs0 = Rs#role.bool_sign,
    case util:get_bool_sign(1, Bs0) of
        0 ->
            MyItems = lib_item:get_myitems(),
            case lib_item:add_item(250012, 1, MyItems) of
                {ok, MyItems2, My} ->
                    lib_item:add_item_notice(My, Rs#role.pid_sender),
                    lib_item:put_myitems(MyItems2),
                    Bs = util:set_bool_sign(1, 1, Bs0),
                    %% ?INFO("~s", [Rs#role.name]),
                    %% util:print_bit(Bs0),
                    %% util:print_bit(Bs),
                    {ok, [0], Rs#role{bool_sign = Bs}};
                {error, at_full} -> 
                    {ok, [2]};
                {error, Reason} -> 
                    ?ERR("error: ~w", [Reason]),
                    {ok, [1]}
            end;
        1 -> {ok, [3]}
    end;

handle(_Cmd, _Data, _RoleState) ->
    ?INFO("bad_request! [Cmd:~w, Data:~w]", [_Cmd, _Data]),
    {error, bad_request}.

%% 转盘
luck_process(Rand, [{Id, Num, Value, {Min, Max}} | T]) ->
    case Rand >= Min andalso Rand =< Max of
        true -> {Id, Num, Value};
        false -> luck_process(Rand, T)
    end;
luck_process(_, []) -> {0, 0, 0}.

%%' 鉴定
identify(false) -> {error, 3};
identify(MyItem) ->
    Data = data_identify:get(MyItem#myitem.tid),
    identify1(MyItem, Data).

identify1(_MyItem, undefined) -> {error, 2};
identify1(MyItem, Data) ->
    {Tid, Attr} = gen_equ_data(Data),
    Item = data_item:get(Tid),
    MyItem1 = MyItem#myitem{
        tid = Tid 
        ,tab = Item#item.tab
        ,sort = Item#item.sort
        ,max_num = Item#item.max_num
        ,num = 1
        ,attr = Attr},
    {ok, MyItem1}.

%% get_identify_Id(Rand) ->
%%     Val1 = util:rand(1, 1000),
%%     [Id] = [V || {Min, Max, V}<- Rand, Val1 >= Min, Val1 =< Max],
%%     Id.

%% 1 = 攻击
%% 2 = 血量
%% 3 = 移动速度
%% 4 = 攻击速度
%% 5 = 暴击几率
%% 6 = 为魔法攻击
gen_equ_data(Data) ->
    #identify{
        rand = Rand
        ,attr1 = AttrList1
        ,attr2 = AttrList2
        ,attr3 = AttrList3
        ,attr4 = AttrList4
        ,items = Items
    } = Data,
    Num = get_attr_num(Rand),
    AttrList = case Num of
        1 -> AttrList1;
        2 -> AttrList2;
        3 -> AttrList3;
        4 -> AttrList4;
        _ -> AttrList1
    end,
    {_, Tid} = lists:keyfind(Num, 1, Items),
    Attr = get_attrs(Num, AttrList, #equ_attr{}),
    {Tid, Attr}.

get_attrs(Num, AttrList, Attr) when Num > 0 ->
    A = util:rand_element(AttrList),
    Attr1 = case A of
        {?doc_dmg       , {S, E}} -> Attr#equ_attr{dmg        = util:rand(S, E)};
        {?doc_hp_max    , {S, E}} -> Attr#equ_attr{hp_max     = util:rand(S, E)};
        {?doc_move_speed, {S, E}} -> Attr#equ_attr{move_speed = util:rand(S, E)};
        {?doc_dmg_speed , {S, E}} -> Attr#equ_attr{dmg_speed  = util:rand(S, E)};
        {?doc_crit      , {S, E}} -> Attr#equ_attr{crit       = util:rand(S, E)};
        {?doc_attack    , {S, E}} -> Attr#equ_attr{attack     = util:rand(S, E)};
        _Else -> 
            ?WARNING("Unexpected identify data: ~w", [_Else]),
            Attr
    end,
    AttrList1 = lists:delete(A, AttrList),
    get_attrs(Num - 1, AttrList1, Attr1);
get_attrs(_Num, _AttrList, Attr) ->
    Attr.

get_attr_num(Rand) ->
    get_attr_num(Rand, 0).

get_attr_num([H | T], Num) ->
    case util:rate1000(H) of
        true -> get_attr_num(T, Num + 1);
        false -> get_attr_num(T, Num)
    end;
get_attr_num([], Num) -> Num.

%% get_identify_val(Rand) ->
%%     Val1 = util:rand(1, 1000),
%%     case [{Tid, V} || {Min, Max, Tid, V}<- Rand, Val1 >= Min, Val1 =< Max] of
%%         [] -> 
%%             ?INFO("ERROR when get_identify_val, Val:~w, Rand:~w", [Val1, Rand]),
%%             {0, 0};
%%         [{Tid1, {Min1, Max1}}] -> {Tid1, util:rand(Min1, Max1)}
%%     end.
%%.

transit_items([MyItem | T], Reply) ->
    Attr = case lists:member(MyItem#myitem.sort, ?EQU_TYPE) of
        true ->
            case MyItem#myitem.attr of
                undefined -> 
                    ?WARNING("error myitem [Tid:~w, Pos:~w]", [MyItem#myitem.tid, MyItem#myitem.pos]),
                    [];
                #equ_attr{
                    hp_max = HpMax
                    ,move_speed = MoveSpeed
                    ,dmg_speed = DmgSpeed
                    ,crit = Crit
                    ,attack = Attack
                    ,fb_attack = FbAttack
                } ->
                    [[Crit, 0, HpMax, MoveSpeed, DmgSpeed, Attack, FbAttack]]
            end;
        false -> []
    end,
    R = [
        MyItem#myitem.id
        ,MyItem#myitem.tid
        ,MyItem#myitem.tab
        ,MyItem#myitem.sort
        ,MyItem#myitem.pos 
        ,MyItem#myitem.num
        ,MyItem#myitem.lev 
        ,Attr
        ,lib_item:calc_etime(MyItem#myitem.etime)
    ],
    Reply1 = [R | Reply], 
    transit_items(T, Reply1);
transit_items([], Reply) -> Reply.

%%' 属性转移
transfer_attr(ItemId1, ItemId2, MyItems) ->
    I1 = lib_item:get_myitem(ItemId1, MyItems),
    I2 = lib_item:get_myitem(ItemId2, MyItems),
    transfer_attr1(I1, I2, MyItems).

transfer_attr1(false, _, _) -> {error, 1};
transfer_attr1(_, false, _) -> {error, 2};
transfer_attr1(MyItem1, MyItem2, _MyItems) 
when MyItem1#myitem.sort =/= MyItem2#myitem.sort -> {error, 3};
transfer_attr1(MyItem1, MyItem2, MyItems) ->
    transfer_attr2(
        enhance(MyItem2#myitem.lev, MyItem1), 
        enhance(MyItem1#myitem.lev, MyItem2), 
        MyItems 
    ).
transfer_attr2({ok, MyItem1, _}, {ok, MyItem2, _}, MyItems) ->
    MyItems1 = lib_item:set_myitem(MyItem1, MyItems),
    MyItems2 = lib_item:set_myitem(MyItem2, MyItems1),
    LevMax = lists:max([MyItem1#myitem.lev, MyItem2#myitem.lev]),
    {ok, MyItems2, MyItem1, MyItem2, LevMax};
transfer_attr2(_, _, _) -> {error, 4}.
%%.

%%' 强化
enhance(false) -> {error, 2};
enhance(MyItem) ->
    enhance(MyItem#myitem.lev + 1, MyItem, []).

enhance(ToLev, MyItem) ->
    enhance(ToLev, MyItem, []).

enhance(ToLev, MyItem, Reply) when MyItem#myitem.lev == ToLev -> {ok, MyItem, Reply};
enhance(ToLev, MyItem, Reply) when ToLev < 0 -> {ok, MyItem, Reply};
enhance(ToLev, MyItem, Reply) when MyItem#myitem.lev < ToLev ->
    enhance1(ToLev, MyItem, Reply, 1, MyItem#myitem.lev + 1);
enhance(ToLev, MyItem, Reply) when MyItem#myitem.lev > ToLev ->
    enhance1(ToLev, MyItem, Reply, -1, MyItem#myitem.lev).

enhance1(ToLev, MyItem, Reply, Sign, Lev) ->
    E = data_enhance:get({MyItem#myitem.sort, Lev}),
    enhance2(ToLev, MyItem, Reply, Sign, E).

enhance2(_ToLev, _MyItem, _Reply, _Sign, undefined) -> 
    {error, 1};
enhance2(ToLev, MyItem, Reply, Sign, E) -> 
    #enhance{
        success_rate = SuccessRate
        ,gold = Gold1
        ,a_name = AName
        ,a_val = AVal
    } = E,
    %% ?INFO("AName:~w, Tid:~w, AVal1:~w, Sign:~w", [AName, Tid, AVal1, Sign]),
    %% AVal = AVal1 * Sign,
    Attr = enhance_add_attr(AName, AVal, Sign, MyItem#myitem.attr),
    %% Attr = case AName of
    %%     1 -> MyItem#myitem.attr#equ_attr{dmg = MyItem#myitem.attr#equ_attr.dmg + AVal};
    %%     2 -> MyItem#myitem.attr#equ_attr{hp_max = MyItem#myitem.attr#equ_attr.hp_max + AVal};
    %%     3 -> MyItem#myitem.attr#equ_attr{move_speed = MyItem#myitem.attr#equ_attr.move_speed + AVal};
    %%     4 -> MyItem#myitem.attr#equ_attr{dmg_speed = MyItem#myitem.attr#equ_attr.dmg_speed + AVal};
    %%     5 -> MyItem#myitem.attr#equ_attr{crit = MyItem#myitem.attr#equ_attr.crit + AVal};
    %%     6 -> MyItem#myitem.attr#equ_attr{attack = MyItem#myitem.attr#equ_attr.attack + AVal};
    %%     OtherAName -> 
    %%         ?WARNING("undefined attr name:~w", [OtherAName]),
    %%         MyItem#myitem.attr
    %% end,
    MyItem1 = MyItem#myitem{attr = Attr, lev = MyItem#myitem.lev + Sign},
    SuccessRate1 = case lists:keyfind(SuccessRate, 1, Reply) of
        false -> SuccessRate;
        {_, SR} -> SuccessRate + SR
    end,
    Reply1 = lists:keystore(success_rate, 1, Reply, {success_rate, SuccessRate1}),
    Reply2 = case Gold1 > 0 of
        true ->
            Gold = case lists:keyfind(gold, 1, Reply1) of
                false -> Gold1;
                {_, Gold2} -> Gold1 + Gold2
            end,
            lists:keystore(gold, 1, Reply1, {gold, Gold});
        false -> Reply1
    end,
    enhance(ToLev, MyItem1, Reply2).

%% 1: $aVal = "dmg"; break;
%% 2: $aVal = "hp_max"; break;
%% 3: $aVal = "move_speed"; break;
%% 4: $aVal = "dmg_speed"; break;
%% 5: $aVal = "crit"; break;
%% 6: $aVal = "attack"; break;
%% 7: $aVal = "fb_attack"; break;
enhance_add_attr([Key | KeyT], [Val | ValT], Sign, Attr) ->
    AVal = Val * Sign,
    Attr1 = case Key of
        1 -> Attr#equ_attr{dmg        = Attr#equ_attr.dmg        + AVal};
        2 -> Attr#equ_attr{hp_max     = Attr#equ_attr.hp_max     + AVal};
        3 -> Attr#equ_attr{move_speed = Attr#equ_attr.move_speed + AVal};
        4 -> Attr#equ_attr{dmg_speed  = Attr#equ_attr.dmg_speed  + AVal};
        5 -> Attr#equ_attr{crit       = Attr#equ_attr.crit       + AVal};
        6 -> Attr#equ_attr{attack     = Attr#equ_attr.attack     + AVal};
        7 -> Attr#equ_attr{fb_attack  = Attr#equ_attr.fb_attack  + AVal};
        OtherAName -> 
            ?WARNING("undefined attr name:~w", [OtherAName]),
            Attr
    end,
    enhance_add_attr(KeyT, ValT, Sign, Attr1);
enhance_add_attr([], [], _Sign, Attr) -> Attr.

enhance_failure(MyItem, ProtecteId) ->
    MinLev = data_config:get(strengthen_reduce_lv),
    Lev = MyItem#myitem.lev,
    case ProtecteId > 0 orelse Lev < MinLev of
        true -> MyItem;
        false ->
            case enhance(Lev - 1, MyItem) of
                {ok, MyItem1, _} -> MyItem1;
                {error, Reason} ->
                    ?ERR("ERROR:~w", [Reason]),
                    MyItem
            end
    end.

calc_erate(L) ->
    calc_erate(L, 0).

calc_erate([PartId | T], Reply) when PartId > 0 ->
    Reply1 = case data_item:get(PartId) of
        #item{sort = 7, ctl1 = V} -> Reply + V;
        _ -> Reply
    end,
    calc_erate(T, Reply1);
calc_erate([_PartId | T], Reply) ->
    calc_erate(T, Reply);
calc_erate([], Reply) -> 
    Reply.

check_protecte(0) -> 0;
check_protecte(Id) ->
    case data_item:get(Id) of
        #item{sort = 16} -> Id;
        Else -> 
            ?WARNING("Invalid ProtecteId:~w", [Else]),
            0
    end.

calc_erate_luck(0) -> {0, 1};
calc_erate_luck(LuckId) -> 
    case data_item:get(LuckId) of
        #item{sort = 17, ctl1 = V} -> 
            {LuckId, V / 100 + 1};
        Else -> 
            ?WARNING("Invalid LuckId:~w", [Else]),
            {0, 0}
    end.
%%.

%% 续费
renew(Tid, Time, Rs) ->
    case data_renew:get({Tid, Time}) of
        #renew{gold = Gold, card = Card} ->
            renew1(Gold, Card, Rs);
        undefined -> {error, 1}
    end.

renew1(0, Card, Rs) ->
    renew2(Card, Rs);
renew1(Gold, Card, Rs) ->
    case lib_role:spend(gold, Gold, Rs) of
        {ok, Rs1} -> renew2(Card, Rs1);
        {error, _} -> {error, 2}
    end.

renew2(0, Rs) -> {ok, Rs};
renew2(Card, Rs) ->
    case lib_role:spend(card, Card, Rs) of
        {ok, Rs1} -> {ok, Rs1};
        {error, _} -> {error, 3}
    end.
%%.
