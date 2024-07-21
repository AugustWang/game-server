%%----------------------------------------------------
%% Test 
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(robot_handle).
-compile(export_all).

-include("common.hrl").
-include("rs.hrl").

%%' 10协议_登陆认证

handle(10003, [IsOk, IsCreated, Rid, _], #rs{pid_sender = Sender} = Rs) ->
    case IsOk of
        1 -> 
            case IsCreated of
                0 ->
                    %% 创建角色
                    {Sex, Name} = case Rs#rs.sex > 0 andalso Rs#rs.name =/= <<>> of 
                        true -> 
                            {Rs#rs.sex, Rs#rs.name};
                        false -> 
                            %% robot_lib:get_name(test, Rs#rs.acc_id)
                            robot_lib:get_name(Rs#rs.acc_id)
                    end,
                    SexS = case Sex of
                        1 -> <<"男">>;
                        2 -> <<"女">>;
                        _ -> <<"未知">>
                    end,
                    ?INFO("Reg:~s - ~s", [SexS, Name]),
                    Sender ! {cmd, 10004, [Name, Sex]},
                    Rs#rs{sex = Sex, name = Name};
                _ -> 
                    %% ?INFO("Login:~w - ~w - ~s", [Rs#rs.acc_id, Rs#rs.id, Rs#rs.name]),
                    %% 正常登陆
                    io:format("+"),
                    Rs1 = Rs#rs{id = Rid, pid = self()},
                    Rs1
            end;
        0 -> 
            robot ! {logout, Rs#rs.acc_id},
            io:format("login failure! ~w~n", [[IsOk, IsCreated, Rid]]),
            Rs
    end;
handle(10003, Data, _Rs) ->
    ?WARNING("10003 - Error Data: ~w", [Data]);

handle(10004, [0], Rs) ->
    %% 创建角色成功
    Rs;

handle(10004, [1], Rs) ->
    {Sex, Name} = robot_lib:get_name(Rs#rs.acc_id),
    ?INFO("rename! [~s -> ~s]", [Rs#rs.name, Name]),
    Rs#rs.pid_sender ! {cmd, 10004, [Name, Sex]},
    ok;

handle(10004, [Status], Rs) ->
    robot ! {logout, Rs#rs.acc_id},
    ?INFO("create_role failure! [Status:~w, Rs:~w]", [Status, Rs]),
    Rs;
%%.
%%' 11协议_角色相关

handle(11003, [Id, _, _, _, _, X, Y], Rs) ->
    case is_pid(Rs#rs.pid_ai) of
        true -> Rs#rs.pid_ai ! {moving, Id, util:floor(X/50), util:floor(Y/36)};
        false -> ok
    end;

handle(11003, Data, _Rs) ->
    ?INFO("Recv Data(11003): ~w", [Data]),
    ok;

handle(11005, [Rid, Score, _, _], Rs) ->
    case is_pid(Rs#rs.pid_ai) of
        true -> Rs#rs.pid_ai ! {set_score, Rid, Score};
        false -> ok
    end;

handle(11006, [
         Id 
        ,Name 
        ,Lev 
        ,Sex 
        ,Gold 
        ,Card 
        ,_Hp 
        ,_HpMax 
        ,_Dmg 
        ,_FbAttack
        ,_Attack
        ,DmgSpeed
        ,MoveSpeed 
        ,_Exp
        ,_
        ,_
        ,GuildId
        | _
    ], Rs) when is_record(Rs, rs) ->
    Rs1 = Rs#rs{
        id = Id
        ,move_speed = MoveSpeed 
        ,dmg_speed = DmgSpeed 
        ,guild_id = GuildId 
        ,lev = Lev 
        ,gold = Gold
        ,card = Card
        ,sex = Sex
        ,name = Name
    },
    %% 登陆成功
    robot ! {login_ok, Rs#rs.acc_id, Id, self(), Lev},
    Rs#rs.pid_sender ! {cmd, 11026, []},
    %% case is_pid(Rs#rs.pid_ai) andalso is_process_alive(Rs#rs.pid_ai) of
    %%     true -> Rs#rs.pid_ai ! {update_rs, Rs1};
    %%     false -> ok
    %% end,
    case application:get_env(cc, version_type) of
        {ok, dev} ->
            %% 测试
            %% 公会
            %% case GuildId == 0 of
            %%     true ->
            %%         erlang:send_after(2000, Rs#rs.pid_sender, {cmd, 22005, []});
            %%     false -> ok
            %% end,
            %% erlang:send_after(util:rand(1000, 3000), Rs#rs.pid_sender, {cmd, 11026, []}),
            %% Rs#rs.pid_sender ! {cmd, 11026, []},
            ok;
        _ -> 
            %% 非开发服忽略邀请
            ok
    end,
    Rs1;

handle(11006, Data, Rs) ->
    ?INFO("Recv Data(11006): ~w, Rs:~w", [Data, Rs]),
    ok;

%% handle(11007, [[[
%%                 Id 
%%                 ,_Name 
%%                 ,_Lev 
%%                 ,_Sex 
%%                 ,_Gold 
%%                 ,_Card 
%%                 ,_Hp 
%%                 ,_HpMax 
%%                 ,_Dmg 
%%                 ,_Attack
%%                 ,DmgSpeed
%%                 ,MoveSpeed 
%%                 ,_Exp
%%                 ,_
%%                 ,_
%%                 ,_GuildId
%%                 | _
%%             ]]], Rs) ->
%%     case Id == Rs#rs.id andalso is_pid(Rs#rs.pid_ai) of
%%         true -> 
%%             Rs1 = Rs#rs{move_speed = MoveSpeed, dmg_speed = DmgSpeed},
%%             %% case is_pid(Rs#rs.pid_ai) andalso is_process_alive(Rs#rs.pid_ai) of
%%             %%     true -> 
%%             %%         Rs#rs.pid_ai ! {update_rs, Rs1};
%%             %%     false -> ok
%%             %% end,
%%             Rs1;
%%         false -> ok
%%     end;

handle(11007, Data, _Rs) ->
    ?INFO("Recv Data(11007): ~w", [Data]),
    ok;

handle(11008, [Rid, Key, Val], Rs = #rs{id = Rid}) ->
    Rs1 = case Key of
        1 -> Rs#rs{hp = Val};
        2 -> Rs#rs{move_speed = Val};
        3 -> Rs#rs{dmg_speed = Val}
    end,
    case is_pid(Rs#rs.pid_ai) andalso 
        is_process_alive(Rs#rs.pid_ai) of
        true -> Rs#rs.pid_ai ! {update_rs, Rs1};
        false -> ok
    end,
    Rs1;
handle(11008, [_Rid, _Key, _Val], _Rs) ->
    ok;
handle(11008, Data, _Rs) ->
    ?INFO("Recv Data(11008): ~w", [Data]);


%% handle(11033, [], Rs) ->
%%     #rs{id = Id, pid_sender = S} = Rs,
%%     case get(ping) of
%%         undefined ->
%%             S ! {cmd, 11033, []},
%%             put(ping, erlang:now()),
%%             ok;
%%         T ->
%%             DT = timer:now_diff(erlang:now(), T) / 1000,
%%             case DT > 0 andalso Id == 1 of
%%                 true ->
%%                     %% ?INFO("PingS:~w (Id:~w)", [util:ceil(DT), Id]),
%%                     ?INFO("Ping Server:~w (Id:~w)", [DT, Id]),
%%                     ok;
%%                 false -> skip
%%             end,
%%             erlang:send_after(5000, S, {cmd, 11033, []}),
%%             erase(ping),
%%             ok
%%     end,
%%     ok;
%% 
%% handle(11034, [], Rs) ->
%%     #rs{pid_sender = S} = Rs,
%%     {Y1, Y2, Y3} = erlang:now(),
%%     S ! {cmd, 11034, [Y1, Y2, Y3]},
%%     ok;

%% handle(11018, [Rid, Hp, _HpMax, _], #rs{pid_ai = PidWalk}) when is_pid(PidWalk) ->
%%     PidWalk ! {update_hp, Rid, Hp};

handle(11019, [Rid, _, _, _], #rs{pid_ai = PidWalk}) when is_pid(PidWalk) ->
    PidWalk ! {stop_move, Rid};

handle(11036, [X1, X2, X3], Rs) ->
    #rs{id = Id} = Rs,
    {Y1, Y2, Y3} = erlang:now(),
    DT = timer:now_diff({X1, X2, X3}, {Y1, Y2, Y3}) / 1000,
    case DT > 0 andalso Id ==1 of
        true ->
            ?INFO("Reach Client:~w (~w)", [DT, Id]),
            ok;
        false -> skip
    end,
    ok;

handle(11009, [_Status, _Rid, _ItemId, X, Y], Rs) ->
    Rs#rs.pid_ai ! {picked_item, X, Y},
    Rs;

%%.
%%' 12协议_NPC

%% 怪被打
handle(12005, [NpcId, _Type, X, Y], Rs) ->
    case Rs#rs.type of
        2 ->
            case is_pid(Rs#rs.pid_ai) of
                true -> Rs#rs.pid_ai ! {npc_ai, NpcId, 1, 0, X, Y};
                false -> ok
            end;
        _ -> ok
    end;

%% 怪被打
handle(12009, [NpcId, _HpMax, Hp, _], Rs) ->
    case is_pid(Rs#rs.pid_ai) of
        true -> Rs#rs.pid_ai ! {npc_decay, NpcId, Hp};
        false -> ok
    end;

%% 怪物AI
handle(12011, [NpcId, Action, Action2, X, Y], Rs) ->
    case is_pid(Rs#rs.pid_ai) of
        true -> Rs#rs.pid_ai ! {npc_ai, NpcId, Action, Action2, X, Y};
        false -> ok
    end;

%%.
%%' 13协议_大厅

%% 房间列表为空，创建一个房间或稍后再请求列表
handle(13001, [Data], #rs{pid_sender = Sender} = Rs) ->
    %% io:format("13001: ~w~n", [Rooms]),
    %% 创建
    %% erlang:send_after(util:rand(500, 1000), Sender, {cmd, 13005, [1]});
    case util:rand(1, 1) of
        1 ->
            case util:rand(1, 1) of
                1 ->
                    %% 创建
                    erlang:send_after(util:rand(10, 100), Sender, {cmd, 13005, [1]});
                _ ->
                    %% 继续请求
                    erlang:send_after(util:rand(10000, 20000), Sender, {cmd, 13001, [1]})
            end;
        _ ->
            Rooms = [RoomId || [RoomId, _, _, _, _, 0, 0] <- Data],
            case Rooms of
                [] ->
                    %% 继续请求
                    erlang:send_after(util:rand(10000, 15000), Sender, {cmd, 13001, [1]});
                [Id | _] ->
                    Sender ! {cmd, 13003, [1, Id]}
            end
    end,
    Rs;

%% 加入房间成功
handle(13003, [0, _RoomId, Type, _Name, _Map, _, _], #rs{pid_sender = Sender} = Rs) ->
    %% ?INFO("join_room_ok ~w", [Rs#rs.id]),
    %% 成功进入房间, 准备游戏
    Sender ! {cmd, 14005, [1]},
    case Rs#rs.is_invited of
        1 -> robot ! {set_robot, at_game, Rs#rs.acc_id, join_room_ok};
        0 -> ok
    end,
    Rs#rs{type = Type, is_host = 0, status = 2};

%% 创建房间成功
handle(13005, [0, _Type, _Name], _Rs) ->
    ?INFO("创建房间失败", []),
    ok;

%% 创建房间成功
handle(13005, [RoomId, Type, _Name], Rs) ->
    %% ?INFO("recv create ok:~w", [Rs#rs.id]),
    S = Rs#rs.pid_sender,
    case Rs#rs.action of
        {create_room, Type, 1} ->
            %% 房主点开始
            S ! {cmd, 14026, [1, Rs#rs.to_room_id]},
            ok;
        {create_room, Type, 2} -> 
            Ref1 = erlang:send_after(util:rand(2000, 3000), S, {cmd, 14026, [1, Rs#rs.to_room_id]}),
            reset_start_ref(Ref1),
            robot ! {join_room, Rs#rs.trigger, Type, RoomId, Rs#rs.avg_lev};
        {create_room, Type, 3} -> 
            S ! {cmd, 14018, [2, 1]},
            erlang:send_after(1000, robot, {join_room, Rs#rs.trigger, Type, RoomId, Rs#rs.avg_lev}),
            erlang:send_after(1100, robot, {join_room, Rs#rs.trigger, Type, RoomId, Rs#rs.avg_lev}),
            Ref1 = erlang:send_after(util:rand(2000, 3000), S, {cmd, 14026, [1, Rs#rs.to_room_id]}),
            reset_start_ref(Ref1),
            ok;
        _ -> ok
    end,
    Rs#rs{type = Type, is_host = 1, status = 2};

%%.
%%' 15协议_错误处理

handle(15001, [Code], #rs{pid_ai = Ai, pid_sender = Sender, to_room_id = ToRoomId}) ->
    case Code of
        11000101 ->
            case is_pid(Ai) andalso is_process_alive(Ai) of
                true -> 
                    Ai ! stop,
                    self() ! stop;
                false -> ok
            end;
        13003101 -> 
            %% 房间人数已满
            erlang:send_after(util:rand(10000, 18000), Sender, {cmd, 13001, [1]});
        13003102 -> 
            %% 这个房间正在游戏中
            erlang:send_after(util:rand(10000, 18000), Sender, {cmd, 13001, [1]});
        13003103 -> 
            %% 这个房间已经删除
            erlang:send_after(util:rand(10000, 18000), Sender, {cmd, 13001, [1]});
        13003104 ->
            %% 稍后再开始匹配
            Ref = erlang:send_after(util:rand(5000, 10000), Sender, {cmd, 14026, [1, ToRoomId]}),
            reset_start_ref(Ref),
            ok;
        17000101 -> 
            %% 背包已满，进行整理
            Sender ! {cmd, 17105, [1]},
            Sender ! {cmd, 17105, [2]},
            ok;
        100 ->
            ok;
        _ ->
            io:format("Recv Error Code: ~w~n", [Code])
    end,
    ok;

%%.
%%' 14协议_房间

handle(14004, [Rid], #rs{id = Id, acc_id = AccId} = Rs) ->
    %% 退出
    case Rid == Id of
        true ->
            %% 被踢出来了，取消房主点击开始的计时
            case get(start_ref) of
                undefined -> skip;
                Ref ->
                    erlang:cancel_timer(Ref),
                    erase(start_ref),
                    ok
            end,
            robot ! {set_robot, at_hall, AccId, exit_room},
            Rs#rs{is_invited = 0, is_host = 0, status = 1};
        false ->
            ok
    end;

%% 自己准备好了
handle(14005, [Rid, 1], #rs{id = Rid}) ->
    ok;

%% %% 别人已经准备好了
handle(14005, [Rid, 1], Rs) ->
    case application:get_env(cc, version_type) of
        {ok, dev} ->
            %% 加好友
            Rs#rs.pid_sender ! {cmd, 18007, [Rid]},
            ok;
        _ -> 
            %% 非开发服忽略邀请
            ok
    end;

%% 游戏结束
%% game over
handle(14008, [_Status, _], Rs) ->
    case is_pid(Rs#rs.pid_ai) of
        true -> Rs#rs.pid_ai ! stop;
        false -> ok
    end,
    case Rs#rs.is_invited of
        1 -> Rs#rs.pid_sender ! {cmd, 14005, [1]};
        0 -> Rs#rs.pid_sender ! {cmd, 14004, []}
    end,
    case util:rand(1, 5) of
        1 ->
            %% 查看背包
            erlang:send_after(util:rand(100, 1000), Rs#rs.pid_sender, {cmd, 17101, []}),
            ok;
        2 ->
            %% 查看任务
            erlang:send_after(util:rand(100, 1000), Rs#rs.pid_sender, {cmd, 19001, []}),
            ok;
        3 ->
            %% 查看技能
            erlang:send_after(util:rand(100, 1000), Rs#rs.pid_sender, {cmd, 17006, []}),
            ok;
        _ -> ok
    end,
    Rs#rs{is_battle = 0, status = 2};
handle(14008, Data, _Rs) ->
    ?WARNING("14008 - Error Data: ~w", [Data]);

%% 加载资源
handle(14009, [MapId, _, _, Roles], #rs{id = Rid, pid_sender = Sender} = Rs) ->
    %% 完成加载
    Ref14023 = erlang:send_after(util:rand(1000, 2000), Sender, {cmd, 14023, []}),
    put(ref_14023, Ref14023),
    MyData =  [[Team, X, Y, Ds, Ms] || [Rid1, Team, X, Y, _Name, _Sex, _Hp, _HpMax, _Dmg, Ds, Ms | _] <- Roles, Rid1 == Rid],
    case MyData of
        [[Team, X, Y, Ds, Ms]] ->
            Enemy =  [{Rid2, {X2, Y2}} || [Rid2, Team1, X2, Y2 | _] <- Roles, Team =/= Team1],
            Rs#rs{team = Team, x = X, y = Y, map_id = MapId, enemy = Enemy, dmg_speed = Ds, move_speed = Ms, is_battle = 1, status = 3};
        _D ->
            io:format("Recv Error 14009: ~w~n", [_D]),
            Rs
    end;

%% 玩家退出
handle(14012, [Rid], #rs{id = Rid, pid_ai = PidWalk} = Rs) when is_pid(PidWalk) ->
    %% ?INFO("退出:~w", [Rid]),
    PidWalk ! {stop_move, Rid},
    Rs#rs{is_battle = 0, is_invited = 0, is_host = 0};
handle(14012, [Rid], #rs{pid_ai = PidWalk}) when is_pid(PidWalk) ->
    PidWalk ! {stop_move, Rid};

%% 自己成了新房主
handle(14013, [Rid], Rs = #rs{id = Rid, pid_sender = Sender, to_room_id = ToRoomId}) ->
    %% ?INFO("自己成了新房主:~w", [Rid]),
    case Rs#rs.is_invited == 1 of
        true ->
            %% 被邀请的，自己成了房主后则退出房间
            Rs#rs.pid_sender ! {cmd, 14004, []},
            %% erlang:send_after(util:rand(800, 1000), Rs#rs.pid_sender, {cmd, 14004, []});
            Rs#rs{is_invited = 0};
        false ->
            %% 房主点开始
            Ref1 = erlang:send_after(util:rand(1000, 5000), Sender, {cmd, 14026, [1, ToRoomId]}),
            reset_start_ref(Ref1),
            Rs#rs{is_host = 1}
    end;

%% 踢人
handle(14015, [Rid], #rs{id = Rid} = Rs) ->
    robot ! {set_robot, at_hall, Rs#rs.acc_id, be_kicked},
    Rs#rs{status = 1};

%% handle(14018, [Pos, Status], Rs) ->
%%     ?INFO("开启或关闭房间, Rid:~w, Pos:~w Status:~w", [Rs#rs.id, Pos, Status]);

%% 游戏正式开始
%% game start
handle(14027, [], Rs) ->
    case get(ref_14023) of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    %% 寻路
    AvgLev = case Rs#rs.avg_lev > 0 of
        true -> Rs#rs.avg_lev;
        false -> Rs#rs.lev
    end,
    Ai = data_config_ai:get(AvgLev),
    %% ?INFO("{game start, Id:~w, Trigger:~w, AvgLev:~w, Lev:~w}", [Rs#rs.id, Rs#rs.trigger, Rs#rs.avg_lev, Rs#rs.lev]),
    Rs1 = Rs#rs{ai = Ai},
    case is_pid(Rs#rs.pid_ai) andalso is_process_alive(Rs#rs.pid_ai) of
        true -> Rs#rs.pid_ai ! stop;
        false -> ok
    end,
    Pid = robot_ai:start(Rs1),
    Rs1#rs{pid_ai = Pid, status = 3};

%% 收到邀请（当前人数为1时进入房间）
handle(14029, [_, Type, RoomId, _, _], Rs) when Rs#rs.status == 1 ->
    case application:get_env(cc, version_type) of
        {ok, dev} ->
            Rs#rs.pid_sender ! {cmd, 13003, [Type, RoomId]},
            Rs#rs{is_invited = 1};
        _ -> 
            %% 非开发服忽略邀请
            ok
    end;

%% 复活(格子)
handle(14031, [Rid, X, Y], #rs{pid_ai = PidWalk}) ->
    PidWalk ! {re_alive, Rid, X, Y};

%% BUFF
handle(14033, [Rid, BuffId, Status], #rs{pid_ai = Ai}) when is_pid(Ai) ->
    Ai ! {buff, Rid, BuffId, Status};

%%.
%%' 16协议_地图

%% 摔落(格子)
handle(16003, [Rid, _, _X, _Y, X, Y], #rs{pid_ai = PidWalk}) when is_pid(PidWalk) ->
    PidWalk ! {falling, Rid, X, Y};

handle(16004, [], #rs{pid_ai = PidWalk}) when is_pid(PidWalk) ->
    PidWalk ! re_move;

%% 物品掉落
handle(16005, [Id, _Tid, _, X, Y], #rs{pid_ai = PidWalk}) when is_pid(PidWalk) ->
    PidWalk ! {fall_item, Id, X, Y},
    ok;

%% 破碎
handle(16001, [X, Y, DepTh, Type], #rs{pid_ai = Ai}) ->
    case is_pid(Ai) of
        true -> Ai ! {break, X, Y, DepTh, Type};
        false -> ok
    end;

%% 走到了破碎点上数次，回到上一个点
handle(16009, [Rid, X, Y], #rs{pid_ai = Ai}) when is_pid(Ai) ->
    Ai ! {set_xy, Rid, X, Y, 0};

%% 人往上弹 
handle(16010, [Rid, _X, _Y, X, Y], #rs{id = Rid, pid_ai = Ai}) when is_pid(Ai) ->
    Ai ! {set_xy, Rid, X, Y, 800};

%% 类型:
%% * 1: 单向移动
%% * 2: 双向移动
%% * 3: 瀑布 
%% 协议中的动作值:
%% * 1=停留在目标点，此时目标点有[云]
%% * 2=从起始点开始移动，此时起始点[云]消失
handle(16015, [_, _, _X1, _Y1, _X2, _Y2, []], _) ->
    ok;
handle(16015, [2, 2, _X1, _Y1, _X2, _Y2, Ids], 
    #rs{pid_ai = Ai}) when is_pid(Ai) ->
    %% ?INFO("at_cloud:~w", [Ids]),
    [Ai ! {stop_move, Id} || Id <- Ids];
%%.
%%' 17协议_道具

%% 使用道具
handle(17001, [L], Rs) ->
    #rs{pid_sender = S} = Rs,
    Enable = case util:rate(Rs#rs.ai#ai.use_daoju) of
        true ->
            [
                120001
                ,120002
                ,120003
                ,120004
                ,120005
                ,120006
                ,120007
                ,120008
                ,120010
                ,120011
                ,120014
                ,120016
                ,120017
                ,120018 %% 弹簧
                ,120019
                ,120020
                ,120021
                ,120022
            ];
        false ->
            []
    end,
    F = fun([Pos, Tid]) ->
            case lists:member(Tid, Enable) of
                true ->
                    if
                        Tid == 120018 ->
                            %% 先不使用弹簧
                            robot_lib:add_daoju(Pos, Tid),
                            ok;
                        true ->
                            erlang:send_after(util:rand(10, 1000), S, {cmd, 17003, [Pos]})
                    end;
                false -> 
                    S ! {cmd, 17123, [Pos]}
                    %% erlang:send_after(util:rand(10, 1000), S, {cmd, 17003, [Pos]})
            end
    end,
    lists:foreach(F, L),
    case L of
        [[_, 120018], [_, 120018], [_, 120018]] ->
            %% 三个都是弹簧，先使用一个
            robot_lib:use_daoju(120018, Rs);
        _ -> ok
    end,
    Rs;

handle(17006, [Data], Rs) ->
    Data1 = [list_to_tuple(X) || X <- Data],
    case lists:keyfind(1, 3, Data1) of
        false ->
            case lists:keyfind(0, 3, Data1) of
                false -> ok;
                {SkillId, _, _} -> 
                    %% 携带技能1
                    %% ?INFO("携带技能1:~w", [SkillId]),
                    Rs#rs.pid_sender ! {cmd, 17008, [SkillId, 1]}
            end;
        _ ->
            case lists:keyfind(2, 3, Data1) of
                false ->
                    case lists:keyfind(0, 3, Data1) of
                        false -> ok;
                        {SkillId, _, _} -> 
                            %% 携带技能2
                            %% ?INFO("携带技能2:~w", [SkillId]),
                            Rs#rs.pid_sender ! {cmd, 17008, [SkillId, 2]}
                    end;
                _ -> ok
            end
    end,
    Rs#rs{skills = Data1};

handle(17101, [Data], Rs) ->
    case util:rate(20) of
        true ->
            %% 整理背包
            erlang:send_after(200, Rs#rs.pid_sender, {cmd, 17105, [0]});
        false ->
            Data1 = [list_to_tuple(X) || X <- Data],
            S = Rs#rs.pid_sender,
            F = fun
                ([Id, 190001, _Tab, 7, _Pos, _Num, _Lev | _]) ->
                    S ! {cmd, 17119, [Id]};
                ([Id, _Tid, _Tab, 10, _Pos, _Num, _Lev | _]) ->
                    %% 技能书
                    case Rs#rs.skills of
                        [{SkillId, _, _} | _] -> S ! {cmd, 17007, [SkillId, Id]};
                        _ -> ok
                    end;
                ([Id, Tid, _Tab, 7, _Pos, _Num, _Lev | _]) ->
                    %% 合成
                    case util:rand(1, 8) of
                        1 ->
                            case data_combine1:get(Tid) of
                                undefined -> ok;
                                _ -> 
                                    %% ?INFO("合成：~w", [Tid]),
                                    erlang:send_after(util:rand(100, 5000), S, {cmd, 17115, [Id]})
                            end;
                        _ -> ok
                    end;
                ([Id, _Tid, _Tab, Sort, Pos, _Num, Lev | _]) when Pos >= 97 ->
                    %% 强化
                    case data_enhance:get({Sort, Lev + 1}) of
                        undefined -> ok;
                        _ -> 
                            %% ?INFO("强化：~w", [_Tid]),
                            %% erlang:send_after(util:rand(10, 2000), S, {cmd, 17113, [Id]})
                            ok
                    end;
                ([Id, Tid, _Tab, Sort, Pos, _Num, Lev | _]) ->
                    robot_lib:process_item(Id, Tid, Sort, Pos, Lev, Data1, S),
                    ok
            end,
            lists:map(F, Data),
            ok
    end;

%%.

%%'
handle(19001, [TaskList], Rs) ->
    F = fun
        ([Id, _, _, Process, ProcessOk | _]) when Process == ProcessOk ->
            Rs#rs.pid_sender ! {cmd, 19003, [Id, 0]};
        (_) -> ok
    end,
    lists:foreach(F, TaskList),
    %% erlang:send_after(100, Rs#rs.pid_sender, {cmd, 11006, []}),
    ok;
%%.

%%'
handle(22005, [[]], Rs) ->
    N = list_to_binary("TEST-" ++ integer_to_list(util:rand(1, 10000))),
    erlang:send_after(1000, Rs#rs.pid_sender, {cmd, 22007, [N]}),
    erlang:send_after(10000, Rs#rs.pid_sender, {cmd, 22005, []});

handle(22005, [Data], Rs) ->
    D = [G || [G| _] <- Data],
    GuildId = util:rand_element(D),
    %% ?INFO("GuildId:~w", [GuildId]),
    erlang:send_after(2000, Rs#rs.pid_sender, {cmd, 22008, [GuildId]});

handle(22007, [0], _Rs) ->
    ?INFO("create guild ok!", []);
%%.

handle(_Cmd, _Data, Rs) ->
    %% io:format("recv:[cmd:~w data:~w, Rs:~w]~n", [Cmd, Data, Rs]),
    Rs.

reset_start_ref(Ref) ->
    case get(start_ref) of
        undefined -> ok;
        Ref1 -> erlang:cancel_timer(Ref1)
    end,
    put(start_ref, Ref).

%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
