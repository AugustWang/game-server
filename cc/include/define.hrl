%%----------------------------------------------------
%% 宏定义
%% 
%% @author rolong@vip.qq.com
%%----------------------------------------------------

-define(DB, mysql_conn_poll).
-define(FL_POLICY_REQ, <<"<policy-file-request/>\0">>).

-define(LOG(Msg), cc_logger:log(Msg)).
-define(LOG_GOLD(R1, R2, Type), cc_logger:log(
        {gold, R1#role.id, Type, R2#role.gold - R1#role.gold, R2#role.gold}
    )).
-define(LOG_CARD(R1, R2, Type), cc_logger:log(
        {card, R1#role.id, Type, R2#role.card - R1#role.card, R2#role.card}
    )).


-define(INFO(Msg), util:info(Msg, [], ?MODULE, ?LINE)).
-define(DEBUG(Msg), cc_logger:notify(debug, Msg, [], ?MODULE, ?LINE)).
%% -define(DEBUG(Msg), util:debug(Msg, [], ?MODULE, ?LINE)).
-define(ERR(Msg), cc_logger:notify(error, Msg, [], ?MODULE, ?LINE)).
-define(INFO(F, A), util:info(F, A, ?MODULE, ?LINE)).
-define(DEBUG(F, A), cc_logger:notify(debug, F, A, ?MODULE, ?LINE)).
%% -define(DEBUG(F, A), util:debug(F, A, ?MODULE, ?LINE)).
-define(WARN(F, A), cc_logger:notify(warning, F, A, ?MODULE, ?LINE)).
-define(WARNING(F, A), cc_logger:notify(warning, F, A, ?MODULE, ?LINE)).
-define(ERR(F, A), cc_logger:notify(error, F, A, ?MODULE, ?LINE)).
-define(REC_SET(R, T, T2, K, V), (R#T.T2)#T2{K=V}).
-define(REC_SET(R, T, T2, Kvs), lists:foldl(fun({K, V}) -> ?REC_SET(R, T, T2, K, V) end, R, Kvs).
-define(REC_GET(R, T, T2, K), (R#T.T2)#T2.K).

-define(EQU_POS, [97, 98, 99, 100, 101, 102, 103, 104]).
-define(EQU_TYPE, [1, 4, 5, 6, 11, 12, 15, 20]).

-define(GAME_MODES, [1, 2, 101, 102, 103]).

-define(ETS_MAP_POS, map_pos).
-define(T(Text), Text).

-define(MYDAOJU_MAX, 2).

-define(OFFLINE_CACHE_TIME, 600000).

-define(BIT_MAX, 16#ffffffff).

-define(BIT_BAN_CHAT, 2#1).

%% 1.升到[ctl2]级
%% 2.积分战获胜[ctl2]场
%% 3.参加x场积分战
%% 4.杀死怪x只
%% 5.强化x到y级
%% 6.合成x个y物品
%% 7.購買x物品y個
%% 8.参加竞技战x场
%% 9.竞技战获胜x场
%% 10.使用xx道具y次
%% 11.參觀xx建築y次
%% 12.使用xx技能y次
%% 13.穿上xx裝備y次
%% 14.升级一个技能到x级
%% 15.积分战斗中击杀x人
%% 16.对战模式中击杀x人
%% 17.强化任意装备到x级

%% 18.加入或創建一個公會
%% 19.添加x個好友
%% 20.与异性玩家一起战斗x场
%% 21.杀死x个敌人
%% 22.连胜x场
%% 23.弹跳的瞬间使用弹簧
%% 24.充值x点券
%% 25.分享心情
%% 26.与公会成员一起战斗x场
%% 27.与人组队一起战斗x场
%% 28.通关XX副本

-define(TASK_UP_LEV, 1).
-define(TASK_WIN1, 2).
-define(TASK_JOIN1, 3).
-define(TASK_KILL_MONSTER, 4).
-define(TASK_ENHANCE, 5).
-define(TASK_COMBINE, 6).
-define(TASK_BUY, 7).
-define(TASK_JOIN2, 8).
-define(TASK_WIN2, 9).
-define(TASK_USE_DAOJU, 10).
-define(TASK_OPEN_BUILDING, 11).
-define(TASK_USE_SKILL, 12).
-define(TASK_WEAR_EQU, 13).
-define(TASK_UPGRADE_SKILL, 14).
-define(TASK_KILL_ROLE1, 15).
-define(TASK_KILL_ROLE2, 16).
-define(TASK_UPGRADE_EQU, 17).
-define(TASK_JOIN_GUILD, 18).
-define(TASK_ADD_FRIEND, 19).
-define(TASK_WITH_OSEX, 20).
-define(TASK_KILL_ROLE, 21).
-define(TASK_CONTINUE_WIN, 22).
-define(TASK_UP_REALIVE, 23).
-define(TASK_CHARGE, 24).
-define(TASK_SHARE, 25).
-define(TASK_WITH_GUILD, 26).
-define(TASK_WITH_TEAM, 27).
-define(TASK_TRANSFER_ATTR, 28).
-define(TASK_CARBON_PASS, 29).

%% 定义事件
-define(EVENT_REQUEST_LEV, 1).
-define(EVENT_RESPOND_LEV, 2).
-define(EVENT_ADD_FANS, 3).
-define(EVENT_DEL_FANS, 4).
-define(EVENT_SET_GUILD, 5).
-define(EVENT_TASK, 6).
-define(EVENT_SHOPPING, 7).
-define(EVENT_ADD_ITEM, 8).
-define(EVENT_ADD_GOLD, 9).
-define(EVENT_ADD_CARD, 10).
-define(EVENT_INVOKE, 11).
-define(EVENT_SEND_CODE, 12).
-define(EVENT_BAN_CHAT, 13).

%% 定义属性代码
-define(hp           , 1).
-define(exp          , 2).
-define(exp_max      , 3).
-define(game_count   , 4).
-define(task_tail    , 5).
-define(enable_dtask , 6).
-define(dtask_time   , 7).
-define(sex          , 8).
-define(card         , 9).
-define(gold         , 10).
-define(score        , 11).
-define(lev          , 12).
-define(growth       , 13). %% 成长进度
-define(online_time  , 14). %% 总在线时间
-define(online_reward, 15). %% 在线奖励
-define(sign_reward  , 16). %% 七天奖励
-define(skill        , 17). %% 技能 [{技能ID, 技能经验, 携带位置}, ...]
-define(upgrade_show , 18). %% 用于播放升级动画...
-define(guild_id     , 19).
-define(guild_name   , 20).
-define(invite_reward, 21).
-define(guild_v      , 22).
-define(guild_b      , 23).
-define(continue_win , 24).
-define(exp2_etime   , 25).
-define(bool_sign    , 26).
-define(luck         , 27).
-define(bit_data     , 28).

%% 定义文案中的属性代码
-define(doc_dmg       , 1).
-define(doc_hp_max    , 2).
-define(doc_move_speed, 3).
-define(doc_dmg_speed , 4).
-define(doc_crit      , 5).
-define(doc_attack    , 6).
