%%----------------------------------------------------
%% 角色相关数据结构定义
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

%% 角色进程状态数据结构
-record(role, {
        % --- int ---
         id             = 0     %% 角色ID
        ,account_id
        ,room_id        = 0     %% 房间ID
        ,room_type      = 0
        ,lev            = 1
        ,exp            = 0
        ,exp_max        = 0
        ,gold           = 0     %% 金币
        ,card           = 0     %% 点券
        ,status         = 0     %% 0=末登陆,1=大厅,2=准备游戏,3=游戏中
        ,score          = 0
        ,ticket         = 0
        ,hp             = 0
        ,hp_max         = 0
        ,crit           = 0     %% 暴击率
        ,dmg            = 0
        ,dmg_speed      = 0
        ,move_speed     = 0
        ,port
        ,sex            = 1
        ,map_id         = 0
        ,growth         = 0     %% 角色成长进度
        ,login_time     = 0     %% 登陆时间
        ,online_time    = 0     %% 总在线时间
        ,attack         = 0     %% 魔法攻击
        ,upgrade_show   = 1
        ,guild_id       = 0     %% 工会ID
        ,guild_v        = 0     %% 公会贡献值
        ,continue_win   = 0     %% 连胜
        ,myrank         = 0     %% 个人总经验排行
        ,exp2_etime     = 0     %% 双倍经验卡过期时间
        ,is_win         = 0     %% 上次游戏结果 败=-1 平=0 胜=1
        ,pos102         = 0     %% 装备102格的类型ID
        ,pos104         = 0     %% 装备104格的类型ID
        ,dtask_time     = 0     %% 上次接日常任务的时间
        ,power          = 0     %% 战斗力
        ,fb_attack      = 0
        ,bit_data       = 0     %% 位标记
        % --- pid ---
        ,pid            = 0     %% 角色主进程ID
        ,pid_conn       = 0     %% 连接处理进程ID
        ,pid_sender     = 0     %% Socket数据发包进程
        ,pid_room               %% srv_room1 | srv_room2
        ,pid_room1              %% srv_room1
        ,win_rate       = 0
        % --- port ---
        ,socket         = 0     %% socket
        % --- binary ---
        ,ip
        ,name           = <<>>  %% 角色名称
        ,guild_name     = <<>>  %% 工会名称
        ,bool_sign      = <<16#00000000:32>> %% 32位布尔标记
        % --- tuple ---
        ,game_count     = {0, 0, 0, 0}   %% game_count:{Win, Lost, Draw, Escape}
        ,online_reward  = {1, 0, 0}      %% 在线奖励 {第几次, 开始时间, 已在线时间}，开始时间为0时即为当前登陆时间
        ,sign_reward    = {{0, 0, 0}, 0} %% {{Y,M,D}, Nth}
        ,invite_reward  = {{0, 0, 0}, 0, 0} %% {{Y,M,D}, 过期的邀请人数, 已领取的邀请奖励次数}
        ,luck           = {1000, 0, 0}  %% 转盘{累积的点券,使用物品数量,累计价值}
        % --- list ---
        ,guild_b        = []    %% 公会福利 [{Bid, Key, RateVal, 结束时间}]
        ,task_tail      = []
        ,enable_dtask   = []    %% [TaskId, ...] 可接的日常任务
        ,save           = []    %% 回存数据到数据库[{myitems, MyItems}, ...]
        ,game_senders   = []    %% 一起游戏的所有人的pid_sender
        ,skill          = []
        ,equ_info       = []    %% 装备信息 [{pos, tid, lev}]
    }
).
