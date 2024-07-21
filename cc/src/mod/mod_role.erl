%%----------------------------------------------------
%% 角色相关
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_role).
-export([handle/3]).

-include("common.hrl").
-include("enhance.hrl").
-include("activity_reward.hrl").
-include("friend.hrl").

%% 敲锤子
handle(11001, [X, Y, X2, Y2, Type], Rs) ->
    #role{pid_room = PidRoom, id= Rid, status = Status} = Rs,
    case Status of
        3 -> gen_server:cast(PidRoom, {hit, Rid, X, Y, X2, Y2, Type});
        _ -> ok
    end,
    {ok};

%% 角色移动 
handle(11003, [Act, Dir, MoveType, Z, XX, YY], Rs) ->
    #role{id = Id, pid_room = PidRoom, status = Status} = Rs,
    case Status of
        3 ->
            X = util:floor(XX/50),
            Y1 = util:floor(YY/36),
            Y = case Y1 of
                0 -> 18;
                _ -> Y1
            end,
            %% case Rs#role.id == 18 of
            %%     true ->
            %%         ?INFO("[Act:~w, Dir:~w, Type:~w, Z:~w, (~w,~w) (~w,~w)", 
            %%             [Act, Dir, MoveType, Z, X, Y, XX, YY]);
            %%     false -> ok
            %% end,
            PidRoom ! {move, Id, Act, Dir, MoveType, Z, XX, YY, X, Y};
        _ -> ok
    end,
    {ok};

%% 角色属性
handle(11006, [Id], Rs) when Id > 0 ->
    case Id =:= Rs#role.id of 
        true -> lib_sender:send(11006, Rs);
        false -> 
            case lib_authen:get_role_pid(role_id, Id) of
                false -> ok;
                Pid -> Pid ! {send_attr, {11006, Rs#role.pid_sender}}
            end
    end,
    {ok};

%% 角色属性
%% handle(11007, [Ids], Rs) ->
%%     case Ids =:= [[Rs#role.id]] of 
%%         true -> self() ! {send_attr, self};
%%         false ->
%%             Pid = spawn(lib_sender, send, [11007, Rs#role.pid_sender, Ids, []]),
%%             Pid ! start
%%     end,
%%     {ok};

%% 拾取物品
handle(11009, _, _Rs) -> 
    ?INFO("recv pick_daoju 11009"),
    {ok};
%% handle(11009, [ItemId, X, Y], Rs) ->
%%     #role{pid_room = PidRoom, id= Rid, pid_sender = PidSender, status = Status} = Rs,
%%     case Status of
%%         3 -> PidRoom ! {pick_daoju, Rid, PidSender, ItemId, X, Y};
%%         _ -> ok
%%     end,
%%     {ok};

%% 查看个人信息
handle(11022, [Id], Rs) ->
    Info = {send_attr, {11022, Rs#role.pid_sender}},
    case Id =:= Rs#role.id of 
        true -> self() ! Info;
        false ->
            lib_sender:send_info(Id, Info)
            %% case lib_role:get_online_info(Id) of 
            %%     false -> lib_sender:send(11022, Rs#role.pid_sender, Id);
            %%     R -> R#online.pid ! Info
            %% end
    end,
    {ok};

%% 客户端初始化完毕
handle(11026, [], #role{id = Rid, status = Status, pid_sender = PidSender} = Rs) ->
    %% ?INFO("init_ok, Status:~w", [Status]),
    case Status of
        3 ->
            gen_server:cast(Rs#role.pid_room, {send_element, Rs#role.pid_sender}),
            {ok};
        2 -> 
            {ok, Rs#role{status = 1}};
        _ -> 
            %% 发送公告
            srv_notice:send_notice(PidSender),
            gen_server:cast(srv_cache, {login, Rid, self()}),
            case Rs#role.guild_id > 0 of
                true ->
                    gen_server:cast(srv_guild, {
                            role_login 
                            ,Rs#role.guild_id
                            ,Rs#role.id
                            ,Rs#role.lev
                            ,Rs#role.sex
                            ,Rs#role.pid_sender 
                        });
                false -> ok
            end,
            %% 活动
            case lib_festival:at(trade_time) of
                true ->
                    lib_conn:pack_send(PidSender, 11050, []),
                    ok;
                false ->
                    ok
            end,
            %% ?INFO("~s", [Rs#role.name]),
            %% util:print_bit(Rs#role.bool_sign),
            lib_conn:pack_send(PidSender, 15015, [Rs#role.bool_sign]),
            %% Rs1 = lib_role:reset_online_reward(Rs),
            %% SignReward = init_sign_reward(Rs1#role.sign_reward),
            %% {ok, Rs1#role{sign_reward = SignReward}}
            SignReward = init_sign_reward(Rs#role.sign_reward),
            {ok, Rs#role{sign_reward = SignReward}}
    end;

%% Ping
handle(11033, [], _Rs) ->
    %% case Rs#role.id == 1 of
    %%     true ->
    %%         #role{pid_sender = S} = Rs,
    %%         {Y1, Y2, Y3} = erlang:now(),
    %%         {ok, Bin} = pack:p(11036, [Y1, Y2, Y3]),
    %%         S ! {data, Bin};
    %%     false -> ok
    %% end,
    {ok, []};

%% handle(11034, [X1, X2, X3], Rs) ->
%%     #role{id = Id, pid_sender = S} = Rs,
%%     {Y1, Y2, Y3} = erlang:now(),
%%     {ok, Bin} = protocol:pack(11034, []),
%%     case get(ping) of
%%         undefined ->
%%             S ! {data, Bin},
%%             put(ping, {Y1, Y2, Y3}),
%%             ok;
%%         T ->
%%             DT = timer:now_diff({Y1, Y2, Y3}, T) / 1000,
%%             ST = timer:now_diff({X1, X2, X3}, {Y1, Y2, Y3}) / 1000,
%%             case DT > 0 andalso Id == 1 of
%%                 true ->
%%                     %% ?INFO("PingC:~w (Id:~w)", [util:ceil(DT), Id]),
%%                     ?INFO("Ping Client:~w (Id:~w)Reach Server:~w", [DT, Id, ST]),
%%                     ok;
%%                 false -> skip
%%             end,
%%             erlang:send_after(5000, S, {data, Bin}),
%%             erase(ping),
%%             ok
%%     end,
%%     {ok};

%% 角色成长进度
handle(11035, [Nth], Rs) ->
    {ok, Rs#role{growth = Nth}};

%% 用于播放升级动画
handle(11036, [Nth], Rs) ->
    {ok, Rs#role{upgrade_show = Nth}};

handle(11037, [], Rs) ->
    Rs1 = lib_role:reset_online_reward(Rs),
    online_reward(Rs1),
    {ok, Rs1};

handle(11038, [], Rs) ->
    case Rs#role.online_reward of
        {0, 0, 0} -> 
            ?WARNING("There is no online_reward!", []),
            {ok};
        {Nth, StartTime, SumTime} ->
            StartTime1 = case StartTime > 0 of
                true -> StartTime;
                false -> Rs#role.login_time
            end,
            Now = util:unixtime(),
            SumTime1 = SumTime + (Now - StartTime1),
            case data_activity_reward:get({1, Nth}) of
                undefined -> 
                    ?WARNING("undefined nth:~w", [Nth]),
                    {ok};
                ActReward ->
                    case SumTime1 >= ActReward#activity_reward.condition of
                        true ->
                            %% 可领取
                            MyItems = lib_item:get_myitems(),
                            Rs0 = case data_activity_reward:get({1, Nth + 1}) of
                                undefined -> 
                                    %% 没有下一次了
                                    lib_conn:pack_send(Rs#role.pid_sender, 11037, [0, 0]),
                                    Rs#role{online_reward = {0, 0, 0}};
                                AR1 -> 
                                    %% 还有下一次
                                    lib_conn:pack_send(Rs#role.pid_sender, 11037, [Nth + 1, AR1#activity_reward.condition]),
                                    %% ?INFO("--> 11037 [Nth:~w, RemainTime:~w]", [Nth + 1, AR1#activity_reward.condition]),
                                    Rs#role{online_reward = {Nth + 1, Now, 0}}
                            end,
                            Rs1 = lib_role:add_attr(Rs0, gold, ActReward#activity_reward.gold),
                            Rs2 = lib_role:add_attr(Rs1, card, ActReward#activity_reward.card),
                            case ActReward#activity_reward.gold > 0 orelse ActReward#activity_reward.card > 0 of
                                true -> lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]);
                                false -> ok
                            end,
                            case add_items(ActReward#activity_reward.item, 
                                    ActReward#activity_reward.num, MyItems) of
                                {ok, MyItems1, AddItems} ->
                                    lib_item:put_myitems(MyItems1),
                                    lib_item:add_item_notice(AddItems, Rs#role.pid_sender),
                                    Save = [{myitems, MyItems1}],
                                    Rs3 = Rs2#role{save = Save},
                                    ?LOG_GOLD(Rs, Rs3, 1002),
                                    ?LOG_CARD(Rs, Rs3, 1002),
                                    {ok, [0], Rs3};
                                {error, empty} ->
                                    ?LOG_GOLD(Rs, Rs2, 1002),
                                    ?LOG_CARD(Rs, Rs2, 1002),
                                    {ok, [0], Rs2};
                                {error, at_full} ->
                                    lib_conn:send_code(Rs#role.pid_sender, 17000101),
                                    {ok, [0]};
                                {error, Error} ->
                                    ?WARNING("Error:~w", [Error]),
                                    {ok}
                            end;
                        false ->
                            RemainTime = ActReward#activity_reward.condition - SumTime1,
                            {ok, [RemainTime]}
                    end
            end
    end;

handle(11040, [], Rs) ->
    sign_reward(Rs),
    {ok};

handle(11041, [], Rs) ->
    {Date, _} = erlang:localtime(),
    {Date1, Nth} = Rs#role.sign_reward,
    case Date of
        Date1 ->
            %% 日期相同，表示已领取过了
            {ok, [1]};
        _ ->
            %% 可领取
            Nth1 = case Nth >= 7 of
                true -> 1;
                false -> Nth + 1
            end,
            case data_activity_reward:get({2, Nth1}) of
                undefined -> 
                    ?WARNING("undefined nth:~w", [Nth1]),
                    {ok, [2]};
                ActReward ->
                    Rs0 = Rs#role{sign_reward = {Date, Nth1}},
                    Rs1 = lib_role:add_attr(Rs0, gold, ActReward#activity_reward.gold),
                    Rs2 = lib_role:add_attr(Rs1, card, ActReward#activity_reward.card),
                    case ActReward#activity_reward.gold > 0 orelse ActReward#activity_reward.card > 0 of
                        true -> lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]);
                        false -> ok
                    end,
                    MyItems = lib_item:get_myitems(),
                    case add_items(ActReward#activity_reward.item, 
                            ActReward#activity_reward.num, MyItems) of
                        {ok, MyItems1, AddItems} ->
                            lib_item:put_myitems(MyItems1),
                            lib_item:add_item_notice(AddItems, Rs#role.pid_sender),
                            Save = [{myitems, MyItems1}],
                            Rs3 = Rs2#role{save = Save},
                            %% ?INFO("Rid:~w, SignReward:~w", [Rs#role.id, Rs3#role.sign_reward]),
                            ?LOG_GOLD(Rs, Rs3, 1003),
                            ?LOG_CARD(Rs, Rs3, 1003),
                            {ok, [0], Rs3};
                        {error, empty} ->
                            ?LOG_GOLD(Rs, Rs2, 1003),
                            ?LOG_CARD(Rs, Rs2, 1003),
                            {ok, [0], Rs2};
                        {error, at_full} ->
                            lib_conn:send_code(Rs#role.pid_sender, 17000101),
                            {ok};
                        {error, Error} ->
                            ?WARNING("Error:~w", [Error]),
                            {ok}
                    end
            end
    end;

handle(11044, [], Rs) ->
    %% {Count1, Nth} = Rs#role.invite_reward,
    %% ?INFO("Count1:~w, Nth:~w", [Count1, Nth]),
    %% case get_invite_count(Rs#role.account_id) of
    %%     {ok, Count} when Count > Count1 ->
    %%         Rs1 = Rs#role{invite_reward = {Count, Nth}},
    %%         Can = case Nth >= Count orelse Nth >= 5 of
    %%             true -> 0;
    %%             false -> 1
    %%         end,
    %%         %% ?INFO("发Nth:~w Can:~w", [Nth + 1, Can]),
    %%         {ok, [0, Count, Nth+1, Can], Rs1};
    %%     {ok, Count} ->
    %%         Can = case Nth >= Count orelse Nth >= 5 of
    %%             true -> 0;
    %%             false -> 1
    %%         end,
    %%         %% ?INFO("发Nth:~w Can:~w", [Nth + 1, Can]),
    %%         {ok, [0, Count, Nth+1, Can]};
    %%     {error, _} -> 
    %%         {ok, [1, 0, 0, 0]}
    %% end;
    case get_invite_count(Rs#role.account_id) of
        {ok, Count} ->
            {Date, _} = erlang:localtime(),
            {OldNum2, Nth2} = case Rs#role.invite_reward of
                {Date, OldNum, Nth} -> 
                    %% 今天已经刷新过了
                    {OldNum, Nth};
                _ -> 
                    %% 今天还没有刷新
                    {Count, 0}
            end,
            Count2 = Count - OldNum2,
            put('today_invite_count', Count2),
            Rs1 = Rs#role{invite_reward = {Date, OldNum2, Nth2}},
            Can = case Nth2 >= Count2 orelse Nth2 >= 5 of
                true -> 0;
                false -> 1
            end,
            %% ?INFO("Nth2:~w Can:~w", [Nth2 + 1, Can]),
            {ok, [0, Count, Nth2+1, Can], Rs1};
        {error, _} -> 
            {ok, [1, 0, 0, 0]}
    end;

%% 领取邀请奖励
handle(11045, [], Rs) ->
    {Date, OldNum, Nth} = Rs#role.invite_reward,
    Nth1 = Nth + 1,
    Count = case get('today_invite_count') of
        undefined -> 0;
        CountTmp -> CountTmp
    end,
    %% ?INFO("领Nth:~w", [Nth1]),
    case data_activity_reward:get({3, Nth1}) of
        undefined -> 
            ?WARNING("undefined nth:~w", [Nth1]),
            {ok, [2]};
        ActReward when Count >= ActReward#activity_reward.condition ->
            Rs0 = Rs#role{invite_reward = {Date, OldNum, Nth1}},
            Rs1 = lib_role:add_attr(Rs0, gold, ActReward#activity_reward.gold),
            Rs2 = lib_role:add_attr(Rs1, card, ActReward#activity_reward.card),
            case ActReward#activity_reward.gold > 0 orelse ActReward#activity_reward.card > 0 of
                true -> lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]);
                false -> ok
            end,
            MyItems = lib_item:get_myitems(),
            case add_items(ActReward#activity_reward.item, 
                    ActReward#activity_reward.num, MyItems) of
                {ok, MyItems1, AddItems} ->
                    lib_item:put_myitems(MyItems1),
                    lib_item:add_item_notice(AddItems, Rs#role.pid_sender),
                    Save = [{myitems, MyItems1}],
                    Rs3 = Rs2#role{save = Save},
                    %% ?INFO("Rid:~w, SignReward:~w", [Rs#role.id, Rs3#role.sign_reward]),
                    ?LOG_GOLD(Rs, Rs3, 1004),
                    ?LOG_CARD(Rs, Rs3, 1004),
                    {ok, [0], Rs3};
                {error, empty} ->
                    ?LOG_GOLD(Rs, Rs2, 1004),
                    ?LOG_CARD(Rs, Rs2, 1004),
                    {ok, [0], Rs2};
                {error, at_full} ->
                    lib_conn:send_code(Rs#role.pid_sender, 17000101),
                    {ok};
                {error, Error} ->
                    ?WARNING("Error:~w", [Error]),
                    {ok}
            end;
        _ -> {ok, [1]}
    end;

handle(11052, [], _Rs) ->
    self() ! {task, ?TASK_SHARE},
    {ok};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

online_reward(Rs) ->
    case Rs#role.online_reward of
        {0, 0, 0} -> 
            %% ?INFO("online_reward {0, 0, 0} Id:~w", [Rs#role.id]),
            lib_conn:pack_send(Rs#role.pid_sender, 11037, [0, 0]),
            ok;
        {Nth, StartTime, SumTime} ->
            StartTime1 = case StartTime > 0 of
                true -> StartTime;
                false -> Rs#role.login_time
            end,
            Now = util:unixtime(),
            SumTime1 = SumTime + (Now - StartTime1),
            case data_activity_reward:get({1, Nth}) of
                undefined -> 
                    ?INFO("online_reward [Nth:~w, undefined data_activity_reward] ~s", [Nth, Rs#role.name]),
                    lib_conn:pack_send(Rs#role.pid_sender, 11037, [0, 0]),
                    ok;
                ActReward ->
                    RemainTime = case SumTime1 >= ActReward#activity_reward.condition of
                        true -> 0;
                        false -> ActReward#activity_reward.condition - SumTime1
                    end,
                    %% ?INFO("online_reward [Nth:~w, RemainTime:~w] ~s", [Nth, RemainTime, Rs#role.name]),
                    lib_conn:pack_send(Rs#role.pid_sender, 11037, [Nth, RemainTime])
            end
    end.

sign_reward(Rs) ->
    {Date, _} = erlang:localtime(),
    {Date1, Nth} = Rs#role.sign_reward,
    %% ?INFO("SignReward:~w", [Rs#role.sign_reward]),
    case Date of
        Date1 ->
            %% 日期相同，表示已领取过了
            lib_conn:pack_send(Rs#role.pid_sender, 11040, [Nth, 1]);
        _ ->
            %% 今天还没有领取
            lib_conn:pack_send(Rs#role.pid_sender, 11040, [Nth + 1, 0])
    end.

add_items([], [], _MyItems) -> {error, empty};
add_items(Items, Nums, MyItems) ->
    add_items(Items, Nums, MyItems, []).

add_items([ItemId | T1], [Num | T2], MyItems, AddItems) ->
    case lib_item:add_item(ItemId, Num, MyItems) of
        {ok, MyItems1, Add} ->
            add_items(T1, T2, MyItems1, Add ++ AddItems);
        {error, Error} -> {error, Error}
    end;
add_items([], [], MyItems, AddItems) -> 
    {ok, MyItems, AddItems}.

%% 100003361740763
get_invite_count(AccoutId1) ->
    AccoutId = if
        is_integer(AccoutId1) -> integer_to_list(AccoutId1);
        is_binary(AccoutId1) -> binary_to_list(AccoutId1);
        true -> AccoutId1
    end,
    Query = case {env:get(version_type), env:get(platform)} of
        {dev, _} -> "http://test1.hammer01.com/t/get_invite_count.php";
        {_, tx} -> 
            case env:get(server_id) of
                1 -> "http://s2.app100666392.qqopenapp.com/get_invite_count.php?openid=" ++ AccoutId;
                2 -> "http://s3.app100666392.qqopenapp.com/get_invite_count.php?openid=" ++ AccoutId;
                _ -> "http://s2.app100666392.qqopenapp.com/get_invite_count.php?openid=" ++ AccoutId
            end;
        {_, id} -> "http://fb-yn.hammer01.com/get_invite_count.php?fbuid=" ++ AccoutId;
        {_, fb} -> "http://www.hammer01.com/sso/endpoint.php?member.get_invite_count?fbuid=" ++ AccoutId;

        _ -> "http://abc" ++ AccoutId
    end,
    %% ?INFO("Query:~s", [Query]),
    try httpc:request(get, {Query, []}, [{timeout, 3000}], []) of
        {error, _Reason} ->
            ?WARNING("Error when get_invite_count! [AccoutId:~w]", [AccoutId]),
            {error, _Reason};
        {ok, {_, _, Body}} ->
            case rfc4627:decode(Body) of
                {ok, {obj, Data}, _} ->
                    case lists:keyfind("c", 1, Data) of
                        false ->
                            ?WARNING("Error json data:~w", [Data]),
                            {error, error_data};
                        {_, C} when is_integer(C) ->
                            %% ?INFO("Count:~w", [C]),
                            {ok, C};
                        {_, C} when is_binary(C) ->
                            %% ?INFO("Count:~w", [C]),
                            C1 = list_to_integer(binary_to_list(C)),
                            {ok, C1}
                    end;
                _Error -> 
                    ?WARNING("rfc4627:decode error:~w", [_Error]),
                    {error, error}
            end;
        _Error ->
            ?WARNING("error data:~w", [_Error]),
            {error, error}
        catch X:Y ->
            ?WARNING("ERROR ~p : ~p", [X, Y]),
            {error, error}
    end.

init_sign_reward(SignReward) ->
    %% 初始化七日登陆奖励
    {Today, _} = erlang:localtime(),
    case SignReward of
        {{0, 0, 0}, _} -> 
            %% ?INFO("** 1 ** ~w", [SignReward]),
            SignReward;
        {Today, _} -> 
            %% ?INFO("** 2 ** 领过了 ~w", [SignReward]),
            SignReward; %% 今天领取过了
        {Date, N} -> 
            case (N >= 7) orelse 
                (util:mktime({Date, {0, 0, 0}}) < 
                    util:unixtime(yesterday)) of
                true -> 
                    %% ?INFO("** 3 ** 重置 ~w", [SignReward]),
                    {{0, 0, 0}, 0};
                false -> 
                    %% ?INFO("** 3 ** 不重置 ~w", [SignReward]),
                    SignReward
            end
    end.
