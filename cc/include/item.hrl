%%----------------------------------------------------
%% item
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

%% 物品基础信息
-record(item, {
        tid
        ,tab = 2
        ,sort
        ,max_num %% 最大堆叠数
        ,attr %% 物品附加属性
        ,gold = 0
        ,avatar_id = 0
        ,lev_min = 1
        ,ctl1
        ,ctl2
        ,period = 0
    }).

%% 个人物品信息
-record(myitem, {
        id
        ,tid
        ,tab = 2
        ,sort
        ,pos = 0
        ,max_num
        ,num = 1 %% 堆叠数量
        ,lev = 0
        ,attr %% 物品附加属性
        ,etime = 0 %% 到期时间
    }).

%% 装备属性
-record(equ_attr, {
        hp_max = 0
        ,dmg = 0
        ,move_speed = 0
        ,dmg_speed = 0
        ,crit = 0
        ,fb_attack = 0
        ,attack = 0
    }).
