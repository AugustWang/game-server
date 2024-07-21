%%----------------------------------------------------
%% 角色相关
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_role).
-export([
        get_rid/1
        ,is_online/1
        ,get_online_info/1
        ,get_list/2
        ,add_attr/2
        ,add_attr/3
        ,add_attr/4
        ,set_attr/2
        ,set_attr/3
        ,spend_score/2
        ,spend_gold/2
        ,add_exp/2
        ,calc_attrs/2
        ,init_kvs/2
        ,spend/3
        ,spend/4
        ,load_role_data/2
        ,stop_all/0
        ,kick/2
        ,get_kvs/1
        ,get_online_time/1
        ,reset_online_reward/1
        ,guild_benefit_addition/3
        ,get_exp2_time/1
        ,fix_benefit/3
        ,bit_chk/2
        ,bit_set/2
        ,bit_del/2
    ]
).

-include("common.hrl").
-include("offline.hrl").
-include("lev.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%' Get role id
get_rid(Name) when is_list(Name) ->
    Sql = lists:concat(["select id from role where name like '", Name, "' limit 10"]),
    get_rid1(Sql);

get_rid(Name1) when is_binary(Name1) ->
    Name = binary_to_list(Name1),
    Sql = lists:concat(["select id from role where name like '", Name, "' limit 10"]),
    get_rid1(Sql);

get_rid(Aid) when is_integer(Aid) ->
    Sql = lists:concat(["select id from role where account_id = '", Aid, "' limit 10"]),
    get_rid1(Sql);

get_rid({aid, Aid}) ->
    Sql = lists:concat(["select id from role where account_id = '", Aid, "' limit 10"]),
    get_rid1(Sql).

get_rid1(Sql) -> 
    Data = db:get_all(Sql),
    get_rid2(Data).

get_rid2([[Id]]) -> Id;
get_rid2([]) -> 0;
get_rid2(Data) -> 
    ?WARNING("unexpected data when get_rid/1. (~w)]", [Data]),
    0.
%%.

%%' @spec (Rid::int()) -> false | #online{}
get_online_info(Rid) ->
    case ets:lookup(online, Rid) of
        [] -> false;
        [R] ->
            case is_pid(R#online.pid) andalso is_process_alive(R#online.pid) of
                true -> R;
                false ->
                    %% 角色进程已经挂了，
                    %% 但因某些原因没有清掉数据，现在删除掉。
                    %% TODO:也许这时应该回写数据，或是做其它特殊处理
                    ?WARNING("Not alive Pid: ~w", [R#online.pid]),
                    ets:delete(online, Rid),
                    false
            end
    end.
%%.

is_online(Rid) -> 
    case ets:member(online, Rid) of
        true -> 1;
        false -> 0
    end.
            
stop_all() ->
    ?INFO("logout ", []),
    L = ets:tab2list(online),
    do_stop_all(L),
    L2 = ets:tab2list(offline),
    do_stop_all2(L2),
    catch db:execute("UPDATE `log_login` SET `event`= 2, `logout_time`= ~s WHERE event = 0", [util:unixtime()]).

do_stop_all([]) -> 
    util:sleep(1000),
    L = ets:tab2list(online),
    Len = length(L),
    case Len > 0 of
        true -> 
            io:format("Rest: ~w!", [Len]),
            util:sleep(5000),
            %% do_stop_all([]);
            ok;
        false -> 
            %% io:format("done!", []),
            ok
    end;
do_stop_all([H | T]) ->
    H#online.pid ! shutdown,
    io:format("."),
    util:sleep(100),
    do_stop_all(T).

do_stop_all2([]) -> 
    util:sleep(1000),
    L = ets:tab2list(offline),
    Len = length(L),
    case Len > 0 of
        true -> 
            io:format("Rest2: ~w!", [Len]),
            util:sleep(5000),
            %% do_stop_all2([]);
            ok;
        false -> 
            io:format("done!", []),
            ok
    end;
do_stop_all2([H | T]) ->
    H#offline.pid ! shutdown,
    io:format("."),
    util:sleep(10),
    do_stop_all2(T).


get_list(by_room, RoomId) ->
    ets:match_object(online, #online{room_id = RoomId, _ = '_'}).

add_attr(R, [{K, V}|T]) ->
    NewR = add_attr(R, K, V),
    add_attr(NewR, T);
add_attr(R, []) -> R.

add_attr(R, hp, V) -> 
    Hp = R#role.hp + V,
    NewHp = if
        Hp < 0 -> 0;
        Hp > R#role.hp_max -> R#role.hp_max;
        true -> Hp 
    end,
    R#role{hp = NewHp};
add_attr(R, hp_max, V) -> 
    R#role{hp_max = R#role.hp_max + V};
add_attr(R, move_speed, V) -> 
    R#role{move_speed = R#role.move_speed + V};
add_attr(R, gold, 0) -> R;
add_attr(R, gold, V) ->
    add_attr(R, gold, V, 0);
add_attr(R, card, V) -> 
    add_attr(R, card, V, 0);
add_attr(R, score, V) -> 
    R#role{score = R#role.score + V};
add_attr(R, exp, V) -> 
    %% 更新排行榜数据 经验
    gen_server:cast(srv_rank, {add_exp, R#role.id, V}),
    %% 加经验必须用add_exp/2
    add_exp(R, V);
add_attr(R, dmg_speed, V) -> 
    DmgSpeedMax = data_config:get(dmg_speed_max),
    DmgSpeedMin = data_config:get(dmg_speed_min),
    DmgSpeed = R#role.dmg_speed + V,
    DmgSpeed1 = if
        DmgSpeed > DmgSpeedMax -> DmgSpeedMax;
        DmgSpeed < DmgSpeedMin -> DmgSpeedMin;
        true -> DmgSpeed
    end,
    R#role{dmg_speed = DmgSpeed1};
add_attr(R, K, V) -> 
    ?WARNING("[add_attr] undefined kvs: ~w-~w", [K, V]),
    R.

add_attr(R, gold, 0, _LogType) -> R;
add_attr(R, gold, V, LogType) ->
    %% 更新排行榜数据 金币
    gen_server:cast(srv_rank, {add_gold, R#role.id, V}),
    Gold = R#role.gold + V,
    case LogType > 0 of
        true -> ?LOG({gold, R#role.id, LogType, V, Gold});
        false -> ok
    end,
    R#role{gold = Gold};
add_attr(R, card, 0, _LogType) -> R;
add_attr(R, card, V, LogType) -> 
    Card = R#role.card + V,
    case LogType > 0 of
        true -> ?LOG({card, R#role.id, LogType, V, Card});
        false -> ok
    end,
    R#role{card = Card};
add_attr(R, K, V, _LogType) -> 
    ?WARNING("[add_attr] undefined kvs: ~w-~w", [K, V]),
    R.

set_attr(R, [{K, V}|T]) ->
    NewR = set_attr(R, K, V),
    set_attr(NewR, T);
set_attr(R, []) -> R.

set_attr(R, hp, V) -> R#role{hp = V};
set_attr(R, hp_max, V) -> R#role{hp_max = V};
set_attr(R, pid_room, V) -> R#role{pid_room = V};
set_attr(R, pid_room1, V) -> R#role{pid_room1 = V, pid_room = V};
set_attr(R, status, V) -> R#role{status = V};
set_attr(R, K, V) -> 
    ?WARNING("[set_attr] undefined kvs: ~w-~w", [K, V]),
    R.

spend_gold(R, Val) ->
    ?INFO("Use spend/3 instead!"),
    Gold = R#role.gold - Val,
    case Gold >= 0 of
        true -> {ok, R#role{gold = Gold}};
        false -> {error, <<"金币不足">>}
    end.

spend_score(R, Val) ->
    ?INFO("Use spend/3 instead!"),
    NewScore = R#role.score - Val,
    case NewScore >= 0 of
        true -> {ok, R#role{score = NewScore}};
        false -> {error, <<"积分不足">>}
    end.

spend(card, Val, R) ->
    Val1 = R#role.card - Val,
    case Val1 >= 0 of
        true -> {ok, R#role{card = Val1}};
        false -> {error, card}
    end;
spend(gold, Val, R) ->
    Val1 = R#role.gold - Val,
    case Val1 >= 0 of
        true -> {ok, R#role{gold = Val1}};
        false -> {error, gold}
    end;
spend(guild_v, Val, R) ->
    Val1 = R#role.guild_v - Val,
    case Val1 >= 0 of
        true -> {ok, R#role{guild_v = Val1}};
        false -> {error, guild_v}
    end.

spend(card, Val, R, LogType) ->
    Val1 = R#role.card - Val,
    case Val1 >= 0 of
        true -> 
            case LogType > 0 of
                true -> ?LOG({card, R#role.id, LogType, -Val, Val1});
                false -> ok
            end,
            {ok, R#role{card = Val1}};
        false -> {error, card}
    end;
spend(gold, Val, R, LogType) ->
    Val1 = R#role.gold - Val,
    case Val1 >= 0 of
        true -> 
            case LogType > 0 of
                true -> ?LOG({gold, R#role.id, LogType, -Val, Val1});
                false -> ok
            end,
            {ok, R#role{gold = Val1}};
        false -> {error, gold}
    end.

%% 加经验，会自动触发升级
%% (R, AddExp) -> NewR
add_exp(R, AddExp) ->
    %% ?INFO("[Rid~w, Lev:~w, Exp:~w]", [R#role.id, R#role.lev, AddExp]),
    Exp = R#role.exp + AddExp,
    MaxLev = data_lev:get(max),
    case Exp >= R#role.exp_max andalso R#role.lev < MaxLev of
        true ->
            %% 升一级
            Lev = R#role.lev + 1,
            RestExp = Exp - R#role.exp_max,
            ExpMax = case data_lev:get(Lev) of
                #lev{exp_max = ExpMax1} -> ExpMax1;
                undefined -> 
                    ?WARNING("找不到等级数据 [Lev:~w, Rid~w]", [Lev, R#role.id]),
                    999999999
            end,
            R1 = R#role{
                lev = Lev
                ,exp = RestExp
                ,exp_max = ExpMax
            },
            self() ! {handle_event, 6010, []},
            %% 更新排行榜数据 等级
            add_exp(R1, 0);
        false ->
            %% 无升级
            R#role{exp = R#role.exp + AddExp}
    end.

%% @spec () -> Rs1
%% @doc 计算角色属性
calc_attrs(Rs, MyItems) ->
    BaseDmg   = data_config:get(init_dmg),
    HpMax     = data_config:get(init_hp_max),
    DmgSpeed  = data_config:get(init_dmg_speed),
    MoveSpeed = data_config:get(init_move_speed),
    NowTime   = util:unixtime(),
    {LevDmg, LevHpMax} = case data_lev:get(Rs#role.lev) of
        #lev{dmg = Dmg1, hp_max = HpMax1} ->
            {Dmg1, HpMax1};
        undefined -> 
            {0, 0}
    end,
    {#equ_attr{
            dmg         = Dmg2        
            ,crit       = Crit   
            ,hp_max     = HpMax2    
            ,dmg_speed  = DmgSpeed2 
            ,move_speed = MoveSpeed2
            ,fb_attack  = FbAttack
            ,attack     = Attack
        }, EquInfo} = get_equipped_attrs(NowTime, ?EQU_POS, MyItems, #equ_attr{}, []),
    Pos102  = case lists:keyfind(102, 1, EquInfo) of
        false -> 0;
        {_, Tid102, _} -> Tid102
    end,
    Pos104  = case lists:keyfind(104, 1, EquInfo) of
        false -> 0;
        {_, Tid104, _} -> Tid104
    end,
    %% ?INFO("
    %%     dmg         = ~p~n 
    %%     ,crit       = ~p~n
    %%     ,hp_max     = ~p~n
    %%     ,dmg_speed  = ~p~n
    %%     ,move_speed = ~p~n
    %%     ,attack     = ~p~n
    %%     ", [
    %%     Dmg2       
    %%     ,Crit   
    %%     ,HpMax2    
    %%     ,DmgSpeed2 
    %%     ,MoveSpeed2
    %%     ,Attack    
    %% ]),
    BaseDmgNew   = BaseDmg   + Dmg2       + LevDmg,
    CritNew      = Crit,
    HpMaxNew     = HpMax     + HpMax2     + LevHpMax,
    DmgSpeedNew  = DmgSpeed  + DmgSpeed2,
    MoveSpeedNew = MoveSpeed + MoveSpeed2,
    FbAttackNew  = FbAttack,
    AttackNew    = Attack,
    Pos102New    = Pos102,
    Pos104New    = Pos104,
    EquInfoNew   = EquInfo,
    Power = util:floor(AttackNew * 3 
        + FbAttackNew * 3
        + HpMaxNew * 1.5 
        + (MoveSpeedNew - 1000) * 5
        + (DmgSpeedNew - 1000) * 5
        + CritNew * 5),
    Rs#role{
        dmg         = BaseDmgNew  
        ,crit       = CritNew     
        ,hp_max     = HpMaxNew    
        ,dmg_speed  = DmgSpeedNew 
        ,move_speed = MoveSpeedNew
        ,attack     = AttackNew   
        ,pos102     = Pos102New   
        ,pos104     = Pos104New   
        ,equ_info   = EquInfoNew  
        ,power      = Power
        ,fb_attack  = FbAttackNew
    }.

get_equipped_attrs(NowTime, [Pos | T], MyItems, EquAttr, EquInfo) ->
    {EquAttr1, EquInfo1} = case lists:keyfind(Pos, #myitem.pos, MyItems) of
        false -> {EquAttr, EquInfo};
        MyItem ->
            if
                MyItem#myitem.attr == undefined -> 
                    {EquAttr, EquInfo};
                MyItem#myitem.etime > 0, MyItem#myitem.etime < NowTime ->
                    {EquAttr, EquInfo};
                true ->
                    #equ_attr{
                        dmg         =  Dmg1       
                        ,crit       =  Crit1  
                        ,hp_max     =  HpMax1    
                        ,dmg_speed  =  DmgSpeed1 
                        ,move_speed =  MoveSpeed1
                        ,fb_attack  =  FbAttack1
                        ,attack     =  Attack1
                    } = EquAttr,
                    #equ_attr{
                        dmg         =  Dmg2       
                        ,crit       =  Crit2  
                        ,hp_max     =  HpMax2    
                        ,dmg_speed  =  DmgSpeed2 
                        ,move_speed =  MoveSpeed2
                        ,fb_attack  =  FbAttack2
                        ,attack     =  Attack2
                    } = MyItem#myitem.attr,
                    {#equ_attr{
                        dmg         = Dmg1       + Dmg2      
                        ,crit       = Crit1      + Crit2 
                        ,hp_max     = HpMax1     + HpMax2    
                        ,dmg_speed  = DmgSpeed1  + DmgSpeed2 
                        ,move_speed = MoveSpeed1 + MoveSpeed2
                        ,fb_attack  = FbAttack1  + FbAttack2
                        ,attack     = Attack1    + Attack2
                    }, [{Pos, MyItem#myitem.tid, MyItem#myitem.lev} | EquInfo]}
            end
    end,
    get_equipped_attrs(NowTime, T, MyItems, EquAttr1, EquInfo1);
get_equipped_attrs(_NowTime, [], _MyItems, EquAttr, EquInfo) ->
    {EquAttr, EquInfo}.

load_role_data(by_id, Id) ->
    case db:get_row("select name, kvs from role where id = ~s", [Id]) of 
        [] -> undefined;
        [Name, KvsBin] ->
            Kvs = case util:bitstring_to_term(KvsBin) of
                undefined -> [];
                Kvs1 -> Kvs1
            end,
            Rs = #role{
                id = Id 
                ,name = Name 
                ,status  = 0
            },
            lib_role:init_kvs(Kvs, Rs)
    end;
load_role_data(by_name, Name) ->
    case db:get_row("select id, kvs from role where name = ~s", [Name]) of 
        [] -> undefined;
        [Id, KvsBin] ->
            Kvs = case util:bitstring_to_term(KvsBin) of
                undefined -> [];
                Kvs1 -> Kvs1
            end,
            Rs = #role{
                id = Id 
                ,name = Name 
                ,status  = 0
            },
            lib_role:init_kvs(Kvs, Rs)
    end.

%% @spec kick(int(), binary()) -> boolean()
%% @doc 踢除某个角色
%% Rid 角色id
%% Reason 踢出原因
kick(Rid, Reason) ->
    case lib_authen:get_pid_from_ets(role_id, Rid) of 
        false -> false;
        {_, Pid} -> 
            try gen_server:call(Pid, {force_logout, Reason}) of
                _Any -> true
            catch
                _T:_X -> false
            end
    end.

get_online_time(Rs) ->
    AddOnlineTime = util:unixtime() - Rs#role.login_time,
    case AddOnlineTime > 0 andalso Rs#role.login_time > 0 of
        true -> Rs#role.online_time + AddOnlineTime;
        false -> Rs#role.online_time
    end.

reset_online_reward(Rs) ->
    case srv_cache:get_daily_count({online_reward, Rs#role.id}) of
        0 ->
            %% 今天还没有重置过，现在重置
            srv_cache:set_daily_count({online_reward, Rs#role.id}, 1),
            Rs#role{online_reward = {1, 0, 0}};
        1 -> Rs
    end.

%%' KVS DATA

%% 增加kvs存储要修改三个地方：
%% 1. define.hrl
%% 2. get_kvs/1
%% 3. init_kvs/2

get_kvs(Rs) ->
    Rs0 = #role{},
    L = [
        {?exp           , Rs#role.exp          , Rs0#role.exp          }
        ,{?exp_max      , Rs#role.exp_max      , Rs0#role.exp_max      }
        ,{?game_count   , Rs#role.game_count   , Rs0#role.game_count   }
        ,{?task_tail    , Rs#role.task_tail    , Rs0#role.task_tail    }
        ,{?enable_dtask , Rs#role.enable_dtask , Rs0#role.enable_dtask }
        ,{?dtask_time   , Rs#role.dtask_time   , Rs0#role.dtask_time   }
        ,{?sex          , Rs#role.sex          , Rs0#role.sex          }
        ,{?card         , Rs#role.card         , Rs0#role.card         }
        ,{?gold         , Rs#role.gold         , Rs0#role.gold         }
        ,{?score        , Rs#role.score        , Rs0#role.score        }
        ,{?lev          , Rs#role.lev          , Rs0#role.lev          }
        ,{?growth       , Rs#role.growth       , Rs0#role.growth       }
        ,{?online_time  , Rs#role.online_time  , Rs0#role.online_time  }
        ,{?online_reward, Rs#role.online_reward, Rs0#role.online_reward}
        ,{?sign_reward  , Rs#role.sign_reward  , Rs0#role.sign_reward  }
        ,{?skill        , Rs#role.skill        , Rs0#role.skill        }
        ,{?upgrade_show , Rs#role.upgrade_show , Rs0#role.upgrade_show }
        ,{?guild_id     , Rs#role.guild_id     , Rs0#role.guild_id     }
        ,{?guild_name   , Rs#role.guild_name   , Rs0#role.guild_name   }
        ,{?invite_reward, Rs#role.invite_reward, Rs0#role.invite_reward}
        ,{?guild_v      , Rs#role.guild_v      , Rs0#role.guild_v      }
        ,{?guild_b      , Rs#role.guild_b      , Rs0#role.guild_b      }
        ,{?continue_win , Rs#role.continue_win , Rs0#role.continue_win }
        ,{?exp2_etime   , Rs#role.exp2_etime   , Rs0#role.exp2_etime   }
        ,{?bool_sign    , Rs#role.bool_sign    , Rs0#role.bool_sign    }
        ,{?luck         , Rs#role.luck         , Rs0#role.luck    }
        ,{?bit_data     , Rs#role.bit_data     , Rs0#role.bit_data     }
    ],
    L1 = lists:foldl(fun
            ({_V1, V2, V2}, Reply) -> Reply;
            ({V1, V2, _}, Reply) -> [{V1, V2} | Reply]
        end, [], L),
    %% ?INFO("~w~n~w", [L, L1]),
    L1.

init_kvs([{?lev, V} | T], Rs)          -> init_kvs(T, Rs#role{lev = V});
init_kvs([{?exp, V} | T], Rs)          -> init_kvs(T, Rs#role{exp = V});
init_kvs([{?exp_max, V} | T], Rs)      -> init_kvs(T, Rs#role{exp_max = V});
init_kvs([{?game_count, V} | T], Rs)   -> init_kvs(T, Rs#role{game_count = V});
init_kvs([{?task_tail, V} | T], Rs)    -> init_kvs(T, Rs#role{task_tail = V});
init_kvs([{?enable_dtask, V} | T], Rs) -> init_kvs(T, Rs#role{enable_dtask = V});
init_kvs([{?dtask_time, V} | T], Rs)   -> init_kvs(T, Rs#role{dtask_time = V});
init_kvs([{?sex, V} | T], Rs)          -> init_kvs(T, Rs#role{sex = V});
init_kvs([{?gold, V} | T], Rs)         -> init_kvs(T, Rs#role{gold = V});
init_kvs([{?card, V} | T], Rs)         -> init_kvs(T, Rs#role{card = V});
init_kvs([{?score, V} | T], Rs)        -> init_kvs(T, Rs#role{score = V});
init_kvs([{?growth, V} | T], Rs)       -> init_kvs(T, Rs#role{growth = V});
init_kvs([{?online_time, V} | T], Rs)  -> init_kvs(T, Rs#role{online_time = V});
init_kvs([{?online_reward, V} | T], Rs)-> init_kvs(T, Rs#role{online_reward = V});
init_kvs([{?sign_reward, V} | T], Rs)  -> init_kvs(T, Rs#role{sign_reward = V});
init_kvs([{?skill, V} | T], Rs)        -> init_kvs(T, Rs#role{skill = V});
init_kvs([{?upgrade_show, V} | T], Rs) -> init_kvs(T, Rs#role{upgrade_show = V});
init_kvs([{?guild_id, V} | T], Rs)     -> init_kvs(T, Rs#role{guild_id = V});
init_kvs([{?guild_name, V} | T], Rs)   -> init_kvs(T, Rs#role{guild_name = V});
init_kvs([{?invite_reward, V} | T], Rs)-> init_kvs(T, Rs#role{invite_reward = V});
init_kvs([{?guild_v, V} | T], Rs)      -> init_kvs(T, Rs#role{guild_v = V});
init_kvs([{?guild_b, V} | T], Rs)      -> init_kvs(T, Rs#role{guild_b = V});
init_kvs([{?continue_win, V} | T], Rs) -> init_kvs(T, Rs#role{continue_win = V});
init_kvs([{?exp2_etime, V} | T], Rs)   -> init_kvs(T, Rs#role{exp2_etime = V});
init_kvs([{?bool_sign, V} | T], Rs)    -> init_kvs(T, Rs#role{bool_sign = V});
init_kvs([{?luck, V} | T], Rs)         -> init_kvs(T, Rs#role{luck = V});
init_kvs([{?bit_data, V} | T], Rs)     -> init_kvs(T, Rs#role{bit_data = V});
init_kvs([{lev, V} | T], Rs)           -> init_kvs(T, Rs#role{lev = V});
init_kvs([{exp, V} | T], Rs)           -> init_kvs(T, Rs#role{exp = V});
init_kvs([{exp_max, V} | T], Rs)       -> init_kvs(T, Rs#role{exp_max = V});
init_kvs([{game_count, V} | T], Rs)    -> init_kvs(T, Rs#role{game_count = V});
init_kvs([{task_tail, V} | T], Rs)     -> init_kvs(T, Rs#role{task_tail = V});
init_kvs([{enable_dtask, V} | T], Rs)  -> init_kvs(T, Rs#role{enable_dtask = V});
init_kvs([{dtask_time, V} | T], Rs)    -> init_kvs(T, Rs#role{dtask_time = V});
init_kvs([{sex, V} | T], Rs)           -> init_kvs(T, Rs#role{sex = V});
init_kvs([{gold, V} | T], Rs)          -> init_kvs(T, Rs#role{gold = V});
init_kvs([{card, V} | T], Rs)          -> init_kvs(T, Rs#role{card = V});
init_kvs([{score, V} | T], Rs)         -> init_kvs(T, Rs#role{score = V});
init_kvs([{growth, V} | T], Rs)        -> init_kvs(T, Rs#role{growth = V});
init_kvs([{online_time, V} | T], Rs)   -> init_kvs(T, Rs#role{online_time = V});
init_kvs([{K, V} | T], Rs)  -> 
    ?WARNING("[init_kvs] undefined kvs: ~w-~w", [K, V]),
    init_kvs(T, Rs);
init_kvs([], Rs) -> Rs;
init_kvs(undefined, Rs) -> Rs.

%%.

fix_benefit(Key, [{Bid, Key, Rate, EndTime} | T], Reply) ->
    case util:unixtime() =< EndTime of
        true -> 
            Reply1 = [{Bid, Key, Rate, EndTime} | Reply],
            fix_benefit(Key, T, Reply1);
        false -> 
            fix_benefit(Key, T, Reply)
    end;
fix_benefit(Key, [H | T], Reply) -> fix_benefit(Key, T, [H | Reply]);
fix_benefit(_Key, [], Reply) -> Reply.

guild_benefit_addition(_Key, _Val, []) -> 0;
guild_benefit_addition(_Key, 0, _) -> 0;
guild_benefit_addition(Key, Val, GuildB1) ->
    GuildB = fix_benefit(Key, GuildB1, []),
    %% ?INFO("GuildB:~w -> ~w", [GuildB1, GuildB]),
    case lists:keyfind(Key, 2, GuildB) of
        false -> 0;
        {_, _, Rate, EndTime} ->
            case util:unixtime() =< EndTime of
                true -> 
                    V = util:ceil(Val * Rate / 100),
                    %% ?INFO("guild_benefit: ~w(~w + ~w)", [Key, Val, V]),
                    V;
                false -> 0
            end
    end.

get_exp2_time(ETime) ->
    Exp2Time = ETime - util:unixtime(),
    case Exp2Time > 0 of
        true -> Exp2Time;
        false -> 0
    end.

bit_chk(Bits, V) -> Bits band V.
bit_set(Bits, V) -> Bits bor V.
bit_del(Bits, V) -> Bits band (?BIT_MAX bxor V).

%% vim: set foldmethod=marker foldmarker=%%',%%.:
