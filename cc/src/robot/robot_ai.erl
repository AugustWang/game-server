%%----------------------------------------------------
%% AI
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(robot_ai).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1]).
-compile(export_all).
-include("common.hrl").
-include("rs.hrl").

-define(I(_Msg), ok).
-define(I(_F, _A), ok).
%% -define(I(Msg), util:info(Msg, [], ?MODULE, ?LINE)).
%% -define(I(F, A), util:info(F, A, ?MODULE, ?LINE)).

%% 新建连接
start(Rs) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Rs, []),
    Pid.

%% --- 服务器内部实现 ---

init(Rs) ->  
    Ref = erlang:send_after(3800, self(), move),
    FindEnemy = case Rs#rs.type of
        1 -> 0;
        _ -> 1
    end,
    Rs1 = Rs#rs{
        speed_row = 500
        ,speed_col = 500
        ,move_ref = Ref
        ,dir = ?DIR_LEFT
        ,find_enemy = FindEnemy 
        ,find_npc = 1
        ,myscore = 0
        ,opscore = 0
    },
    self() ! update_speed,
    %% 缓存敌方角色
    put(enemy, Rs#rs.enemy),
    %% erlang:send_after(util:rand(3000, 10000), self(), use_skill),
    %% 五分钟后让本进程结束
    erlang:send_after(5 * 60 * 1000, self(), stop),
    %% erlang:send_after(util:rand(3000, 10000), self(), test),
    {ok, Rs1}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    ?ERR("Not matched message: ~w", [_Msg]),
    {noreply, State}.

handle_info(move, #rs{dir = Dir} = Rs) when Dir == ?DIR_UP orelse Dir == ?DIR_DOWN ->
    {noreply, move(Rs)};

handle_info(move, Rs) ->
    %% pick_item(Rs),
    %% Rs1 = move(Rs),
    %% case Rs#rs.id == 175 of
    %%     true -> ?WARN("Rs~p, DICT:~p", [Rs, get()]);
    %%     false -> ok
    %% end,
    Rs1 = case leave_enemy(Rs) orelse 
        (Rs#rs.must_lost == 1 andalso Rs#rs.myscore >= (Rs#rs.opscore - 3)) of
        true -> move(Rs);
        false ->
            case hit(Rs) of
                true -> 
                    [S, E] = Rs#rs.ai#ai.hit_reaction,
                    T = util:rand(S, E),
                    next(Rs#rs{last_dir = stop}, Rs#rs.speed_hit + T);
                false -> move(Rs)
            end
    end,
    {noreply, Rs1};

handle_info({moving, Rid, X, Y}, Rs) ->
    Enemy = lists:keyreplace(Rid, 1, Rs#rs.enemy, {Rid, {X, Y}}),
    Rs1 = Rs#rs{enemy = Enemy},
    {noreply, Rs1};

handle_info(use_skill, Rs) ->
    use_skill(Rs),
    erlang:send_after(util:rand(30000, 60000), self(), use_skill),
    {noreply, Rs};

%% Npc 协议
%% int32 怪物id号
%% int8  动作(2=切换,1=冒出,0=缩进)
%% int8  手持物品(0=无, 1=炸弹, 2=金币)
%% int8  X轴(格子)   
%% int8  Y轴(格子)
handle_info({npc_ai, NpcId, Action, Action2, X, Y}, Rs) ->
    Npcs = case get(npcs) of
        undefined -> [];
        N -> N
    end,
    Npcs1 = case {Action, Action2} of
        {0, _} -> 
            erase({npc, NpcId}),
            lists:delete({X, Y}, Npcs);
        {_, 1} -> 
            erase({npc, NpcId}),
            lists:delete({X, Y}, Npcs);
        _ -> 
            put({npc, NpcId}, {X, Y}),
            N1 = lists:delete({X, Y}, Npcs),
            [{X, Y} | N1]
    end,
    put(npcs, Npcs1),
    {noreply, Rs};

handle_info({npc_decay, NpcId, Hp}, Rs) ->
    case Hp =< 0 of
        true ->
            case get({npc, NpcId}) of
                undefined -> skip;
                Xy -> 
                    Npcs = case get(npcs) of
                        undefined -> [];
                        N -> N
                    end,
                    erase({npc, NpcId}),
                    Npcs1 = lists:delete(Xy, Npcs),
                    put(npcs, Npcs1)
            end;
        false ->
            ok
    end,
    {noreply, Rs};

handle_info({break, X, Y, Depth, _Type}, Rs) ->
    MapId = Rs#rs.map_id,
    MaxDepth = data_map_depth:get({MapId, X, Y}),
    case MaxDepth > 0 of
        true ->
            put({break, X, Y}, Depth),
            case Depth >= MaxDepth of 
                true -> 
                    %% ?INFO("broken_pos: ~w, ~w", [X, Y]),
                    put({broken_pos, X, Y}, true);
                false -> erase({broken_pos, X, Y})
            end;
        false -> 
            ?INFO("error pos", []),
            ok
    end,
    {noreply, Rs};

handle_info({fall_item, Id, X, Y}, Rs) ->
    put({item, X, Y}, Id),
    {noreply, Rs};

handle_info({picked_item, X, Y}, Rs) ->
    erase({item, X, Y}),
    {noreply, Rs};

handle_info({update_rs, Rs1}, Rs) ->
    ?I("update_rs, move_speed:~w, dmg_speed:~w", [Rs1#rs.move_speed, Rs1#rs.dmg_speed]),
    Rs2 = Rs#rs{
        hp = Rs1#rs.hp
        ,move_speed = Rs1#rs.move_speed
        ,dmg_speed = Rs1#rs.dmg_speed
    },
    self() ! update_speed,
    {noreply, Rs2};

handle_info(update_speed, Rs) ->
    %% 50 / (客户端基础速度*(人物当前速度/服务端基础速度))
    ServerSpeed = data_config:get(init_move_speed),
    ClientSpeed = data_config:get(int_client_move_speed),
    Speed = ClientSpeed * (Rs#rs.move_speed / ServerSpeed),
    SpeedRow = util:floor(50 / Speed),
    SpeedCol = util:floor(36 / Speed),
    %% 客户端基础速度/(人物当前速度/服务端基础速度)
    ServerDmgSpeed = data_config:get(init_dmg_speed),
    ClientDmgSpeed = data_config:get(init_client_dmg_speed),
    SpeedHit1 = ClientDmgSpeed / (Rs#rs.dmg_speed / ServerDmgSpeed),
    SpeedHit = util:ceil(SpeedHit1 * 8),
    Rs1 = Rs#rs{speed_row = SpeedRow, speed_col = SpeedCol, speed_hit = SpeedHit, 
    hit_delay = util:ceil(SpeedHit1 * 3)},
    %% case SpeedRow =< 250 of
    %%     true ->
    %%         ?INFO("update_speed, SpeedRow:~w, SpeedCol:~w", 
    %%             [SpeedRow, SpeedCol]),
    %%         ok;
    %%     false ->
    %%         ok
    %% end,
    {noreply, Rs1};

handle_info({set_score, Rid, Score}, Rs) ->
    Rs1 = case Rs#rs.id of
        Rid -> Rs#rs{myscore = Score};
        _   -> Rs#rs{opscore = Score}
    end,
    {noreply, Rs1};

handle_info({stop_move, Rid}, Rs) ->
    case Rs#rs.id of
        Rid ->
            erlang:cancel_timer(Rs#rs.move_ref),
            {noreply, Rs};
        _ ->
            Enemy = lists:keydelete(Rid, 1, Rs#rs.enemy),
            Rs1 = Rs#rs{enemy = Enemy},
            {noreply, Rs1}
    end;

handle_info({re_alive, Rid, X, Y}, Rs) ->
    case Rs#rs.id of
        Rid ->
            Rs1 = Rs#rs{x = X, y = Y, last_dir = stop},
            {noreply, next(Rs1)};
        _ ->
            case lists:keymember(Rid, 1, get(enemy)) of
                true ->
                    Enemy = lists:keystore(Rid, 1, Rs#rs.enemy, {Rid, {X, Y}}),
                    Rs1 = Rs#rs{enemy = Enemy},
                    {noreply, Rs1};
                false -> {noreply, Rs}
            end
    end;

handle_info({falling, Rid, X, Y}, Rs) ->
    case Rs#rs.id of
        Rid ->
            Rs1 = Rs#rs{x = X, y = Y, last_dir = stop},
            erlang:cancel_timer(Rs1#rs.move_ref),
            {noreply, Rs1};
        _ ->
            Enemy = lists:keyreplace(Rid, 1, Rs#rs.enemy, {Rid, {X, Y}}),
            Rs1 = Rs#rs{enemy = Enemy},
            {noreply, Rs1}
    end;

handle_info(re_move, Rs) ->
    %% ?INFO("re_move", []),
    Rs1 = Rs#rs{last_dir = stop},
    {noreply, next(Rs1, 200)};

handle_info({set_xy, Rid, X, Y, Time}, Rs) ->
    case Rs#rs.id of
        Rid ->
            Rs1 = Rs#rs{x = X, y = Y, last_dir = stop},
            case Time of
                0 -> {noreply, next(Rs1)};
                _ -> {noreply, next(Rs1, Time)}
            end;
        _ ->
            Enemy = lists:keyreplace(Rid, 1, Rs#rs.enemy, {Rid, {X, Y}}),
            Rs1 = Rs#rs{enemy = Enemy},
            {noreply, Rs1}
    end;

%% 发呆
handle_info({stay, Status}, Rs) ->
    %% ?INFO("stay, Status:~w: ~w", 
    %% [Status, [Rs#rs.dir, Rs#rs.x, Rs#rs.y]]),
    %% Time = case Status of
    %%     1 -> 
    %%         Rs#rs.pid ! {walk, [0, Rs#rs.dir, 2, Rs#rs.x, Rs#rs.y]},
    %%         60000;
    %%     0 -> 0
    %% end,
    %% {noreply, next(Rs#rs{last_dir = stop}, Time)};
    case Status of
        1 -> 
            {noreply, next(Rs, 60000)};
        0 -> {noreply, next(Rs#rs{last_dir = stop}, 100)}
    end;

%% update_enemy
handle_info({update_enemy, Rid, 1}, Rs) ->
    Enemy = lists:keydelete(Rid, 1, Rs#rs.enemy),
    {noreply, Rs#rs{enemy = Enemy}};

handle_info({update_enemy, Rid, 0}, Rs) ->
    case lists:keymember(Rid, 1, get(enemy)) of
        true ->
            Enemy = lists:keystore(Rid, 1, Rs#rs.enemy, {Rid, {0, 0}}),
            Rs1 = Rs#rs{enemy = Enemy},
            {noreply, Rs1};
        false -> {noreply, Rs}
    end;

handle_info({update_enemys, 1}, Rs) ->
    {noreply, Rs#rs{enemy = []}};

handle_info({update_enemys, 0}, Rs) ->
    Enemy = [{Rid, {0, 0}} || {Rid, _} <- get(enemy)],
    Rs1 = Rs#rs{enemy = Enemy},
    {noreply, Rs1};

handle_info({buff, Rid, BuffId, Status}, Rs) ->
    case {Rs#rs.id, BuffId, Status} of
        {Rid, 11, _} -> 
            %% ?INFO("ice ~w", [[Rid, BuffId, Status]]),
            self() ! {stay, Status}; %% 冰冻
        {Rid, 2, _} -> ok;
        {_, 2, _} -> self() ! {update_enemy, Rid, Status};
        {Rid, 8, _} -> self() ! {update_enemys, Status};
        _ -> ok
    end,
    {noreply, Rs};

handle_info(test, Rs) ->
    #rs{speed_row = SpeedRow, speed_col = SpeedCol, speed_hit = SpeedHit} = Rs, 
    case SpeedRow < 300 of
        true ->
            ?INFO(" SpeedRow:~w SpeedCol:~w SpeedHit:~w ", 
                [SpeedRow ,SpeedCol ,SpeedHit]),
            ok;
        false -> ok
    end,
    erlang:send_after(util:rand(10000, 20000), self(), test),
    {noreply, Rs};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    ?ERR("Not matched info: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---

move(Rs) when Rs#rs.dir == undefined ->
    Rs#rs.pid ! {walk, [0, ?DIR_LEFT, 3, Rs#rs.x, Rs#rs.y]},
    Rs1 = Rs#rs{dir = ?DIR_LEFT},
    next(Rs1);

move(Rs) ->
    #rs{x = X, y = Y0, dir = Dir, map_id = MapId, last_dir = LastDir, pid = Pid} = Rs,
    Y = set_y(Y0),
    XL = X - 1,
    XR = X + 1,
    YU0 = Y - 1,
    YD0 = Y + 1,
    YU = set_y(YU0),
    YD = set_y(YD0),
    %% 顺着方向走，下一个坐标点
    {XX, YY} = case Dir of
        ?DIR_LEFT -> {XL, Y};
        ?DIR_RIGHT -> {XR, Y};
        ?DIR_UP -> {X, YU};
        ?DIR_DOWN -> {X, YD}
    end,
    MoveType = if
        LastDir == ?DIR_DOWN andalso Y == 1 -> 3;
        LastDir == ?DIR_UP andalso Y == 18 -> 3;
        LastDir =/= Dir -> 3;
        true -> 98
    end,
    Val  = data_map_pos:get({MapId, X, Y}),
    ValN = data_map_pos:get({MapId, XX, YY}),
    CanN = is_broken(XX, YY) == false andalso ValN > 0,
    ValL = data_map_pos:get({MapId, XL, Y}),
    ValR = data_map_pos:get({MapId, XR, Y}),
    ValU = data_map_pos:get({MapId, X, YU}),
    ValD = data_map_pos:get({MapId, X, YD}),
    CanL = is_broken(XL, Y) == false andalso ValL > 0,
    CanR = is_broken(XR, Y) == false andalso ValR > 0,
    CanU = is_broken(X, Y - 1) == false andalso ValU > 0,
    CanD = is_broken(X, Y + 1) == false andalso ValD > 0,
    %% {PriorL, PriorR, PriorU, PriorD} = find_enemy(Rs),
    {PriorL, PriorR, PriorU, PriorD} = case find_enemy(Rs) of
        {0, 0, 0, 0} ->
            case find_npc(Rs) of
                {0, 0, 0, 0} ->
                    case is_row(Dir) of
                        true ->
                            case util:rand(1, 2) of
                                1 -> {0, 0, 1, 0};
                                2 -> {0, 0, 0, 1}
                            end;
                        false ->
                            case util:rand(1, 2) of
                                1 -> {1, 0, 0, 0};
                                2 -> {0, 1, 0, 0}
                            end
                    end;
                P -> P
            end;
        P -> P
    end,
    ?I("val:~w, valN:~w, valD:~w, X:~w, Y:~w, Dir:~w, LDIR:~w, Can:~w, P:~w", [Val, ValN, ValD, X, Y, Dir, LastDir, {CanL, CanR, CanU, CanD}, {PriorL, PriorR, PriorU, PriorD}]),
    EnableCol = not ((LastDir =/= ?DIR_LEFT andalso LastDir =/= ?DIR_RIGHT)
        andalso (CanL orelse CanR)),
    if
        CanL == false andalso CanR == false andalso CanU == false andalso CanD == false ->
            ?I("走投无路"),
            Pid ! {use_daoju, 120018},
            next(Rs, 1000);
        CanN == false ->
            ?I("前方不可走"),
            %% @前方不可走，寻找新方向，此时是一定有一个可走方向的，优后往回走
            NextDir = case {Dir, {CanL, CanR, CanU, CanD}, {PriorL, PriorR, PriorU, PriorD}} of
                %% 防止不断上下走
                {?DIR_UP,    {true,  true,  false, _}, {_, _, _, _}} -> rand(row);
                {?DIR_UP,    {true,  false, false, _}, {_, _, _, _}} -> ?DIR_LEFT;
                {?DIR_UP,    {false, true,  false, _}, {_, _, _, _}} -> ?DIR_RIGHT;
                {?DIR_DOWN,  {true,  true,  _,     false}, {_, _, _, _}} -> rand(row);
                {?DIR_DOWN,  {true,  false, _,     false}, {_, _, _, _}} -> ?DIR_LEFT;
                {?DIR_DOWN,  {false, true,  _,     false}, {_, _, _, _}} -> ?DIR_RIGHT;
                %% 优先方向可走，则优先走
                {_, {true, _, _, _}, {1, _, _, _}} -> ?DIR_LEFT;
                {_, {_, true, _, _}, {_, 1, _, _}} -> ?DIR_RIGHT;
                {_, {_, _, true, _}, {_, _, 1, _}} -> ?DIR_UP;
                {_, {_, _, _, true}, {_, _, _, 1}} -> ?DIR_DOWN;
                %% Sence: 只有一个方向可走时忽略优先级
                {_, {true, false, false, false}, _} -> ?DIR_LEFT;
                {_, {false, true, false, false}, _} -> ?DIR_RIGHT;
                {_, {false, false, true, false}, _} -> ?DIR_UP;
                {_, {false, false, false, true}, _} -> ?DIR_DOWN;
                %% Sence: 当前方向和优先方向相同，前方点不能走
                {?DIR_LEFT,  {false, _,     true,  true}, {1, _, _, _}} -> rand(col);
                {?DIR_LEFT,  {false, _,     true,  false}, {1, _, _, _}} -> ?DIR_UP;
                {?DIR_LEFT,  {false, _,     false, true}, {1, _, _, _}} -> ?DIR_DOWN;
                {?DIR_RIGHT, {_,     false, true,  true}, {_, 1, _, _}} -> rand(col);
                {?DIR_RIGHT, {_,     false, true,  false}, {_, 1, _, _}} -> ?DIR_UP;
                {?DIR_RIGHT, {_,     false, false, true}, {_, 1, _, _}} -> ?DIR_DOWN;
                %% 走可走方向
                {_, {true, _, _, _}, _} -> ?DIR_LEFT;
                {_, {_, true, _, _}, _} -> ?DIR_RIGHT;
                {_, {_, _, true, _}, _} -> ?DIR_UP;
                {_, {_, _, _, true}, _} -> ?DIR_DOWN;
                %% 反方向走，此项优后
                {?DIR_LEFT , {_, true, _, _}, _} -> ?DIR_RIGHT;
                {?DIR_RIGHT, {true, _, _, _}, _} -> ?DIR_LEFT;
                {?DIR_UP   , {_, _, _, true}, _} -> ?DIR_DOWN;
                {?DIR_DOWN , {_, _, true, _}, _} -> ?DIR_UP;
                Msg ->
                    ?INFO("Unexpected message: ~w", [Msg]),
                    ?DIR_LEFT
            end,
            Pid ! {walk, [0, NextDir, 3, X, Y]},
            next(next_pos(Rs, Dir, NextDir));
        Dir == ?DIR_UP andalso ValN == 4 ->
            ?I("进楼梯-上"),
            %% @进楼梯-上 (与 @楼梯口 的顺序不能调换)
            Rs#rs.pid ! {walk, [0, Rs#rs.dir, MoveType, X, Y]},
            Rs1 = Rs#rs{last_dir = Rs#rs.dir, y = YY},
            next(Rs1);
        Dir == ?DIR_DOWN andalso ValN == 3 ->
            ?I("进楼梯-下"),
            %% @进楼梯-下 (与 @楼梯口 的顺序不能调换)
            Rs#rs.pid ! {walk, [0, Rs#rs.dir, MoveType, X, Y]},
            Rs1 = Rs#rs{last_dir = Rs#rs.dir, y = YY},
            next(Rs1);
        (Dir == ?DIR_UP orelse Dir == ?DIR_DOWN) andalso (Val == 3 orelse Val == 4) ->
            ?I("楼梯中"),
            Rs#rs.pid ! {walk, [0, Rs#rs.dir, MoveType, X, Y]},
            Rs1 = Rs#rs{last_dir = Rs#rs.dir, y = YY},
            next(Rs1);
        (Dir == ?DIR_UP orelse Dir == ?DIR_DOWN) andalso (Val =/= 3 orelse Val =/= 4) ->
            ?I("楼梯口-出"),
            NextDir = case {Dir, {CanL, CanR}, {PriorL, PriorR, PriorU, PriorD}} of
                {?DIR_UP,   {_, _}, {_, _, 1, _}} -> ?I("1"), ?DIR_UP;
                %% {?DIR_UP,   {_, _}, {_, _, _, 1}} -> ?I("2"), ?DIR_DOWN;
                {?DIR_DOWN, {_, _}, {_, _, _, 1}} -> ?I("3"), ?DIR_DOWN;
                %% {?DIR_DOWN, {_, _}, {_, _, 1, _}} -> ?I("4"), ?DIR_UP;
                {_, {true, true}, {1, _, _, _}}   -> ?I("5"), ?DIR_LEFT;
                {_, {true, true}, {_, 1, _, _}}   -> ?I("6"), ?DIR_RIGHT;
                {_, {true, true}, {_, _, _, _}}   -> ?I("7"), rand(row);
                {_, {true, false}, {_, _, _, _}}  -> ?I("8"), ?DIR_LEFT;
                {_, {false, true}, {_, _, _, _}}  -> ?I("9"), ?DIR_RIGHT;
                {_, {false, false}, {_, _, _, _}} -> ?I("10"), Dir
            end,
            Pid ! {walk, [0, NextDir, 3, X, Y]},
            next(next_pos(Rs, Dir, NextDir));
            %% case is_row(NextDir) of
            %%     true -> 
            %%         %% 在楼梯口换方向时，Y坐标值不要改变
            %%         Rs#rs.pid ! {walk, [0, NextDir, 3, X, Y]},
            %%         XXX = case NextDir of
            %%             ?DIR_LEFT -> X - 1;
            %%             ?DIR_RIGHT -> X + 1
            %%         end,
            %%         Rs1 = Rs#rs{last_dir = NextDir, x = XXX, dir = NextDir},
            %%         next(Rs1);
            %%     false -> 
            %%         Rs1 = Rs#rs{last_dir = Rs#rs.dir, y = YY, dir = NextDir},
            %%         next(Rs1, 0)
            %% end;
        CanL == false andalso CanR == false ->
            ?I("左右都不能走"),
            %% 左右都不能走
            case {LastDir, ValU, ValD, PriorU, PriorD} of
                {?DIR_UP, _, 4, _, 1} ->
                    %% 向上走着时发现下面有目标，并且可以往下走
                    %% 反向，向下走
                    Rs#rs.pid ! {walk, [0, ?DIR_DOWN, 3, X, Y]},
                    Rs1 = Rs#rs{last_dir = ?DIR_DOWN, dir = ?DIR_DOWN},
                    next(Rs1);
                {?DIR_DOWN, 3, _, 1, _} ->
                    %% 向下走着时发现上面有目标，并且可以往上走
                    %% 反向，向上走
                    Rs#rs.pid ! {walk, [0, ?DIR_UP, 3, X, Y]},
                    Rs1 = Rs#rs{last_dir = ?DIR_UP, dir = ?DIR_UP},
                    next(Rs1);
                {?DIR_UP, 3, _, _, _} ->
                    %% 顺着向上走
                    Rs#rs.pid ! {walk, [0, ?DIR_UP, 98, X, Y]},
                    next(next_pos(Rs, Dir, ?DIR_UP));
                {?DIR_DOWN, _, 4, _, _} ->
                    %% 顺着向下走
                    Rs#rs.pid ! {walk, [0, ?DIR_DOWN, 98, X, Y]},
                    next(next_pos(Rs, Dir, ?DIR_DOWN));
                {_, 3, _, _, _} ->
                    %% 反向，向上走
                    Rs#rs.pid ! {walk, [0, ?DIR_UP, 3, X, Y]},
                    Rs1 = Rs#rs{last_dir = ?DIR_UP, dir = ?DIR_UP},
                    next(Rs1);
                {_, _, 4, _, _} ->
                    %% 反向，向下走
                    Rs#rs.pid ! {walk, [0, ?DIR_DOWN, 3, X, Y]},
                    Rs1 = Rs#rs{last_dir = ?DIR_DOWN, dir = ?DIR_DOWN},
                    next(Rs1);
                {_, _, _, _, _} ->
                    ?INFO("Unexpected message!", []),
                    next(Rs, 1000)
            end;
        ValU == 3 andalso PriorU == 1 andalso EnableCol ->
            ?I("往上走梯子"),
            %% 往上走梯子
            Rs#rs.pid ! {walk, [0, ?DIR_UP, 3, X, Y]},
            next(next_pos(Rs, Dir, ?DIR_UP));
        ValD == 4 andalso PriorD == 1 andalso EnableCol ->
            ?I("往下走梯子"),
            %% 往下走梯子
            Rs#rs.pid ! {walk, [0, ?DIR_DOWN, 3, X, Y]},
            next(next_pos(Rs, Dir, ?DIR_DOWN));
        Rs#rs.dir == ?DIR_LEFT andalso CanL == false andalso CanR ->
            ?I("顺方向已走不通，改向右走"),
            %% 顺方向已走不通，改向右走
            Rs#rs.pid ! {walk, [0, ?DIR_RIGHT, 3, X, Y]},
            next(next_pos(Rs, Dir, ?DIR_RIGHT));
        Rs#rs.dir == ?DIR_RIGHT andalso CanR == false andalso CanL ->
            ?I("顺方向已走不通，改向左走"),
            %% 顺方向已走不通，改向左走
            Rs#rs.pid ! {walk, [0, ?DIR_LEFT, 3, X, Y]},
            next(next_pos(Rs, Dir, ?DIR_LEFT));
        true ->
            %% 顺方向走
            %% Rs#rs.pid ! {walk, [0, Rs#rs.dir, MoveType, X, Y]},
            %% next(next_pos(Rs, Dir, Dir))
            case {Dir, {CanL, CanR}, {PriorL, PriorR, PriorU, PriorD}} of
                {?DIR_LEFT,   {_, true}, {_, 1, _, _}} -> 
                    ?I("顺方向走-转右寻目标"),
                    Pid ! {walk, [0, ?DIR_RIGHT, 3, X, Y]},
                    next(next_pos(Rs, ?DIR_RIGHT, ?DIR_RIGHT));
                {?DIR_RIGHT,   {true, _}, {1, _, _, _}} -> 
                    ?I("顺方向走-转左寻目标"),
                    Pid ! {walk, [0, ?DIR_LEFT, 3, X, Y]},
                    next(next_pos(Rs, ?DIR_LEFT, ?DIR_LEFT));
                _ ->
                    ?I("顺方向走"),
                    Rs#rs.pid ! {walk, [0, Rs#rs.dir, MoveType, X, Y]},
                    next(next_pos(Rs, Dir, Dir))
            end
    end.

next(Rs) ->
    case is_row(Rs#rs.dir) of
        true -> next(Rs, Rs#rs.speed_row);
        false -> next(Rs, Rs#rs.speed_col)
    end.

next(Rs, 0) ->
    erlang:cancel_timer(Rs#rs.move_ref),
    self() ! move,
    Rs;
next(Rs, Time) ->
    erlang:cancel_timer(Rs#rs.move_ref),
    Ref = erlang:send_after(Time, self(), move),
    Rs#rs{move_ref = Ref}.

pick_item(Rs) ->
    pick_item(Rs, Rs#rs.x, Rs#rs.y).
pick_item(Rs, X, Y) ->
    case get({item, X, Y}) of
        undefined -> false;
        ItemId -> Rs#rs.pid_sender ! {cmd, 11009, [ItemId, X, Y]}
    end.

%% reverse_dir(?DIR_LEFT) -> ?DIR_RIGHT;
%% reverse_dir(?DIR_RIGHT) -> ?DIR_LEFT;
%% reverse_dir(?DIR_UP) -> ?DIR_DOWN;
%% reverse_dir(?DIR_DOWN) -> ?DIR_UP.

is_npc(X, Y) ->
    Npcs = case get(npcs) of
        undefined -> [];
        N -> N
    end,
    lists:member({X, Y}, Npcs).

is_broken(X, Y) ->
    case get({broken_pos, X, Y}) of
        true -> true;
        _ -> false
    end.

is_enemy(Rs, X, Y) ->
    lists:keymember({X, Y}, 2, Rs#rs.enemy).

gen_dirs([{Dir, R} | T], Reply) ->
    Reply1 = case util:rate(R) of
        true -> [Dir | Reply];
        false -> Reply
    end,
    gen_dirs(T, Reply1);
gen_dirs([], Reply) -> Reply.

hit(Rs) ->
    #ai{
        hit_front = Front
        ,hit_back = Back
        ,hit_up = Up
        ,hit_down = Down
    } = Rs#rs.ai,
    #rs{x = X, y = Y, dir = Dir} = Rs,
    {Left, Right} = case Dir of
        ?DIR_LEFT -> {Front, Back};
        ?DIR_RIGHT -> {Back, Front};
        _ -> 
            ?INFO("error dir:~w", [Dir]),
            {Back, Front}
    end,
    DR = [{left, Left}, {right, Right}, {up, Up}, {down, Down}],
    Dirs = gen_dirs(DR, []),
    hit(Dirs, Rs, X, Y).

hit([left | T], Rs, X, Y) ->
    case is_npc(X - 1, Y) orelse is_enemy(Rs, X - 1, Y) of
        true ->
            Rs#rs.pid ! {walk, [?ACT_HIT, ?DIR_LEFT, 2, X, Y]},
            erlang:send_after(Rs#rs.hit_delay, Rs#rs.pid_sender, 
                {cmd, 11001, [X, Y, X - 1, Y, 0]}),
            true;
        false -> hit(T, Rs, X, Y)
    end;

hit([right | T], Rs, X, Y) ->
    case is_npc(X + 1, Y) orelse is_enemy(Rs, X + 1, Y) of
        true ->
            Rs#rs.pid ! {walk, [?ACT_HIT, ?DIR_RIGHT, 2, X, Y]},
            erlang:send_after(Rs#rs.hit_delay, Rs#rs.pid_sender, 
                {cmd, 11001, [X, Y, X + 1, Y, 0]}),
            true;
        false -> hit(T, Rs, X, Y)
    end;

hit([up | T], Rs, X, Y) ->
    case is_npc(X, Y - 3) orelse is_enemy(Rs, X, Y - 3) of
        true ->
            Rs#rs.pid ! {walk, [?ACT_HIT, ?DIR_UP, 2, X, Y]},
            erlang:send_after(Rs#rs.hit_delay, Rs#rs.pid_sender, 
                {cmd, 11001, [X, Y, X, Y - 3, 0]}),
            true;
        false -> hit(T, Rs, X, Y)
    end;

hit([down | T], Rs, X, Y) ->
    case is_npc(X, Y + 3) orelse is_enemy(Rs, X, Y + 3) of
        true ->
            Rs#rs.pid ! {walk, [?ACT_HIT, ?DIR_DOWN, 2, X, Y]},
            erlang:send_after(Rs#rs.hit_delay, Rs#rs.pid_sender, 
                {cmd, 11001, [X, Y, X, Y + 3, 0]}),
            true;
        false -> hit(T, Rs, X, Y)
    end;

hit([], _Rs, _X, _Y) -> false.

find_enemy(Rs) when Rs#rs.find_enemy == 0 ->
    case util:rate(Rs#rs.ai#ai.find_enemy) of
        true -> find_enemy(Rs#rs{find_enemy = 1});
        false -> {0, 0, 0, 0}
    end;
find_enemy(Rs) ->
    #rs{map_id = MapId, enemy = Enemy, x = X, y = Y} = Rs,
    EnemyXY = [{X1, Y1} || {_, {X1, Y1}} <- Enemy, X1 > 0],
    find_enemy(MapId, X, Y, EnemyXY, 0).

%% return : [L, R, U, D]
find_enemy(_MapId, _X, _Y, [], _) -> {0, 0, 0, 0};
find_enemy(MapId, X, Y, EnemyXY, 0) ->
    case lists:keyfind(Y, 2, EnemyXY) of
        {X1, _} when X < X1 -> 
            case pass(MapId, X, X1, Y) of
                true -> {0, 1, 0, 0};
                false -> find_enemy(MapId, X, Y, EnemyXY, 1)
            end;
        {X1, _} when X > X1 -> 
            case pass(MapId, X1, X, Y) of
                true -> {1, 0, 0, 0};
                false -> find_enemy(MapId, X, Y, EnemyXY, 1)
            end;
        {X1, _} when X == X1 -> {0, 0, 0, 0};
        _ -> find_enemy(MapId, X, Y, EnemyXY, 1)
    end;
find_enemy(_MapId, _X, Y, EnemyXY, 1) ->
    case EnemyXY of
        [] -> {0, 0, 0, 0};
        _ ->
            Abs = [{Y1, erlang:abs(Y1 - Y)} || {_, Y1} <- EnemyXY],
            [{Y1, Abs1} | _] = sort_y(Abs),
            if
                Abs1 >= 3 andalso Y1 rem 3 == 0 ->
                    case Y < Y1 of
                        true -> {0, 0, 0, 1};
                        false -> {0, 0, 1, 0}
                    end;
                true -> {0, 0, 0, 0}
            end
    end.

find_npc(Rs) when Rs#rs.ai#ai.hit_back == 100 ->
    case util:rate(Rs#rs.ai#ai.find_npc) of
        true -> 
            #rs{map_id = MapId, x = X, y = Y} = Rs,
            EnemyXY = [XY || XY <- get_npcs()],
            %% ?INFO("E:~w", [EnemyXY]),
            find_npc(MapId, X, Y, EnemyXY, 0);
        false -> {0, 0, 0, 0}
    end;
find_npc(_Rs) -> {0, 0, 0, 0}.

%% return : [L, R, U, D]
find_npc(MapId, X, Y, EnemyXY, 0) ->
    case lists:keyfind(Y, 2, EnemyXY) of
        {X1, _} when X < X1 -> 
            case pass(MapId, X, X1, Y) of
                true -> 
                    ?I("向右寻"),
                    {0, 1, 0, 0};
                false -> 
                    ?I("向右寻－不通"),
                    find_npc(MapId, X, Y, EnemyXY, 1)
            end;
        {X1, _} when X > X1 -> 
            case pass(MapId, X1, X, Y) of
                true -> 
                    ?I("向左寻"),
                    {1, 0, 0, 0};
                false -> 
                    ?I("向左寻－不通"),
                    find_npc(MapId, X, Y, EnemyXY, 1)
            end;
        {X1, _} when X == X1 -> {0, 0, 0, 0};
        _ -> find_npc(MapId, X, Y, EnemyXY, 1)
    end;
find_npc(_MapId, _X, Y, EnemyXY, 1) ->
    case EnemyXY of
        [] -> 
            ?I("find_npc1"),
            {0, 0, 0, 0};
        _ ->
            Abs = [{Y1, erlang:abs(Y1 - Y)} || {_, Y1} <- EnemyXY],
            [{Y1, _Abs1} | _] = sort_y(Abs),
            case Y < Y1 of
                true -> 
                    ?I("向下寻"),
                    {0, 0, 0, 1};
                false -> 
                    ?I("向上寻"),
                    {0, 0, 1, 0}
            end
    end.

pass(MapId, X1, X2, Y) when X1 < X2 ->
    case data_map_pos:get({MapId, X1, Y}) > 0 andalso is_broken(X1, Y) == false of
        true -> pass(MapId, X1 + 1, X2, Y);
        false -> false
    end;
pass(_MapId, _X1, _X2, _Y) -> true.


leave_enemy(Rs) ->
    case util:rate(Rs#rs.ai#ai.leave_enemy) of
        true ->
            #rs{map_id = MapId, enemy = Enemy, x = X, y = Y} = Rs,
            MaxDepth = data_map_depth:get({MapId, X, Y}),
            Depth = case get({break, X, Y}) of
                undefined -> 0;
                D -> D
            end,
            case (MaxDepth - Depth) =< 1 of
                true ->
                    EnemyXY = [XY || {_, XY} <- Enemy],
                    leave_enemy(MapId, X, Y, EnemyXY, 0);
                false -> false
            end;
        false -> false
    end.

leave_enemy(MapId, X, Y, EnemyXY, 0) ->
    case lists:member({X, Y + 3}, EnemyXY) of
        true -> true;
        false -> leave_enemy(MapId, X, Y, EnemyXY, 1)
    end;
leave_enemy(_MapId, X, Y, EnemyXY, 1) ->
    case lists:member({X, Y - 3}, EnemyXY) of
        true -> true;
        false -> false
    end.

sort_y([]) -> [];
sort_y([{YY, Abs} | T]) ->
    sort_y([{Y, A} || {Y, A} <- T, A < Abs])
    ++ [{YY, Abs}] ++
    sort_y([{Y, A} || {Y, A} <- T, A >= Abs]).

rand(row) ->
    case util:rand(1, 2) of
        1 -> ?DIR_LEFT;
        2 -> ?DIR_RIGHT
    end;
rand(col) ->
    case util:rand(1, 2) of
        1 -> ?DIR_UP;
        2 -> ?DIR_DOWN
    end.

is_row(Dir) -> Dir == ?DIR_LEFT orelse Dir == ?DIR_RIGHT.
is_col(Dir) -> Dir == ?DIR_UP orelse Dir == ?DIR_DOWN.

%% 下一点坐标点
next_pos(Rs, Dir, NextDir) ->
    #rs{x = X, y = Y} = Rs,
    {X1, Y1} = case is_col(Dir) andalso is_row(NextDir) of
        true -> 
            %% 在楼梯口换横向时，Y坐标值不要改变
            XXX = case NextDir of
                ?DIR_LEFT -> X - 1;
                ?DIR_RIGHT -> X + 1
            end,
            {XXX, Y};
        false -> 
            case is_col(NextDir) andalso is_row(Dir) of
                true -> 
                    %% 在楼梯口换横向时，X坐标值不要改变
                    YYY = case NextDir of
                        ?DIR_UP -> Y - 1;
                        ?DIR_DOWN -> Y + 1
                    end,
                    {X, YYY};
                false -> 
                    case NextDir of
                        ?DIR_LEFT -> {X - 1, Y};
                        ?DIR_RIGHT -> {X + 1, Y};
                        ?DIR_UP -> {X, Y - 1};
                        ?DIR_DOWN -> {X, Y + 1}
                    end
            end
    end,
    Y2 = if
        Y1 > 18 -> Y1 - 18;
        Y1 =< 0 -> Y1 + 18;
        true -> Y1
    end,
    Rs#rs{last_dir = NextDir, dir = NextDir, x = X1, y = Y2}.

set_y(Y0) ->
    if
        Y0 > 18 -> Y0 - 18;
        Y0 =< 0 -> Y0 + 18;
        true -> Y0
    end.

get_npcs() ->
    case get(npcs) of
        undefined -> [];
        N -> N
    end.

%% 使用技能
use_skill(Rs) ->
    Data1 = Rs#rs.skills,
    case util:rand(1, 2) of
        1 ->
            case lists:keyfind(1, 3, Data1) of
                false -> ok;
                {SkillId, _, _} ->
                    erlang:send_after(util:rand(500, 2000), 
                        Rs#rs.pid_sender, 
                        {cmd, 17009, [SkillId]})
            end;
        2 ->
            case lists:keyfind(2, 3, Data1) of
                false -> ok;
                {SkillId, _, _} ->
                    erlang:send_after(util:rand(500, 2000), 
                        Rs#rs.pid_sender, 
                        {cmd, 17009, [SkillId]})
            end;
        _ -> ok
    end.
