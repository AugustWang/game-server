%%----------------------------------------------------
%% Game
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-include("common.hrl").
-include("npc.hrl").
-include("daoju.hrl").
-include("buff.hrl").
-include("skill.hrl").

-record(state, {
        type
        ,map_id
        ,roles = []
        ,npcs = []
        ,senders = []
        ,start_time = 0
        ,end_time
        ,role_avg_lev
        ,falled_hp = 0 %% 角色掉下来时扣的血量
        ,role_hit_drop_hp = 0  %% 怪攻击角色时扣的血量
        ,width
        ,height
        ,brick_num
        ,avg_lev
        ,min_lev
        ,team1_score = 0
        ,team2_score = 0
        ,npc_pro_rate = [] %% NPC产出概率
        ,carbon_id
        ,pass
    }
).

%% info of role
-record(irole, {
        id
        ,score           = 0 %% 玩家得分
        ,x0              = 0 %% 上一个X轴
        ,y0              = 0 %% 上一个Y轴
        ,x               = 0 %% 当前X轴
        ,y               = 0 %% 当前Y轴
        ,v               = 0 %% 当前坐标值
        ,break_ref           %% 砖块破碎计时器
        ,hp = 1
        ,hp_max = 1
        ,dmg = 10
        ,dmg_speed      = 0
        ,hit_speed      = 0 %% 毫秒/次
        ,attack = 0
        ,move_speed = 0
        ,room_id       %% 玩家房间ID
        ,sex = 1
        ,name = <<>>          %% 玩家名字
        ,pid 
        ,pid_sender
        ,pid_room1
        ,row_dmg = 0
        ,col_dmg = 0
        %% ,opposite
        ,team = 0        %% 队伍
        ,exp_card = 1
        ,lev = 0
        ,gold = 0
        ,crit = 0
        ,weapon_id = 0 %% 武器ID
        ,mydaoju = []
        ,pos102 = 0
        ,pos104 = 0
        %% ,attack_all = 0
        %% ,power_pill = 0 %% 大力丸
        ,win_rate = 0
        ,skilled = []
        ,stop_move = false
        ,guild_id = 0
        ,hold_time = 0
        ,fb_attack = 0
    }
).

%% info of npc
-record(inpc, {
        id
        ,type
        ,name = <<>>
        ,pid 
        ,x = 0
        ,y = 0
        ,action = hidden
        ,score
        % -- 副本怪 --
        ,hp
        ,hp_max
        ,ref
        ,sort
    }
).

%% info of item
%% -record(iitem, {
%%         pos
%%         ,tid
%%         ,faller = 0
%%     }
%% ).

%% info of item
-record(ibuff, {
        id    %% BUFF ID
        ,ctl1 %% 控制字段1，一般是BUFF结束时间
        ,ctl2 %% 控制字段2，一般是BUFF有效次数
        ,del_ref
    }
).

-record(iskill, {
        id
        ,rid
        ,attack
        ,sort
        ,time_ef
        ,use_time
        ,ctl1
        ,ctl2
        ,ctl3
    }
).
