%%----------------------------------------------------
%% 房间
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_room).
-export([handle/3]).

-include("common.hrl").

handle(14001, [], Rs) ->
    gen_server:cast(Rs#role.pid_room, {send_roles, Rs#role.pid_sender}),
    {ok};

handle(14004, [], #role{id = Id, pid_room = PidRoom}) ->
    gen_server:cast(PidRoom, {role_exit, Id}),
    {ok};

handle(14005, [Status], Rs) ->
    gen_server:cast(Rs#role.pid_room1, {ready, Rs#role.id, Status}),
    {ok};

%% handle(14006, [], Rs) ->
%%     gen_server:cast(Rs#role.pid_room, {start, Rs#role.id}),
%%     {ok};

%% handle(14009, [], Rs) ->
%%     gen_server:cast(Rs#role.pid_room, {send_element, Rs#role.pid_sender}),
%%     {ok};

%% 从游戏退出到房间
%% 返回状态(0=不能进，1=能进)
handle(14011, [], #role{id = Id, pid_room1 = PidRoom}) ->
    case is_pid(PidRoom) andalso is_process_alive(PidRoom) of
        true ->
            Reply = gen_server:call(PidRoom, {enter_check, Id}),
            %% ?INFO("从游戏退出到房间,检查是否能重新进入：~w", [Reply]),
            {ok, [Reply]};
        false -> {ok, [0]}
    end;

handle(14012, [], #role{id = Id, pid_sender = PidSerder, status = Status, pid_room = PidRoom} = Rs) when Status == 3 ->
    gen_server:cast(PidRoom, {role_exit, Id}),
    %% 重新计算角色属性
    MyItems = lib_item:get_myitems(),
    Rs1 = lib_role:calc_attrs(Rs, MyItems),
    lib_conn:pack_send(PidSerder, 11020, [1]), %% 通知属性数据过期
    {ok, Rs1};

handle(14012, [], #role{id = Id, status = Status}) ->
    ?WARNING("Recv 14012 when status is ~w(Rid:~w).", [Status, Id]),
    {ok};

handle(14015, [Kid], Rs) ->
    gen_server:cast(Rs#role.pid_room, {kick, Rs#role.id, Kid}),
    {ok};

handle(14017, [MapId, RoomName, Password], Rs) ->
    gen_server:cast(Rs#role.pid_room, {mod_info, Rs#role.id, MapId, RoomName, Password}),
    {ok};

handle(14018, [Pos, Status], Rs) ->
    gen_server:cast(Rs#role.pid_room, {set_pos, Rs#role.id, Pos, Status}),
    {ok};

%% 加载游戏资源进度
handle(14022, [Rate], Rs) when Rs#role.status == 3 ->
    Rs#role.pid_room ! {loading, Rs#role.id, Rate},
    {ok};
handle(14022, [_], _Rs) -> {ok};

%% 加载游戏资源完成
handle(14023, [], Rs) when Rs#role.status == 3 ->
    Rs#role.pid_room ! {loaded, Rs#role.id, Rs#role.pid_sender},
    {ok};
handle(14023, [], _Rs) -> {ok};

%% 房主准备开始游戏
handle(14025, [Status], Rs) when Rs#role.status == 2 ->
    %% ?INFO("14025: ~w, is_process_alive(pid_room):~w", [Rs#role.status, is_process_alive(Rs#role.pid_room)]),
    gen_server:cast(Rs#role.pid_room, {start, Rs#role.id, Status, 0}),
    {ok};
handle(14025, [_Status], Rs) ->
    ?INFO("14025: ~w", [Rs#role.status]),
    {ok};

handle(14026, [Status, ToRoomId], Rs) when Rs#role.status == 2 ->
    %% ?INFO("14026: ~w , ~w", [Status, ToRoomId]),
    gen_server:cast(Rs#role.pid_room, {start, Rs#role.id, Status, ToRoomId}),
    {ok};

handle(14026, [Status, ToRoomId], Rs) ->
    ?WARNING("14026:[~w, ~w], Rid:~w, Status:~w", [Status, ToRoomId, Rs#role.id, Rs#role.status]),
    {ok};

%% 邀请
handle(14029, [Rid], Rs) ->
    gen_server:cast(Rs#role.pid_room, {invite, Rid, Rs#role.name}),
    {ok};

handle(14035, [Type], #role{id = Id, status = Status, pid_room1 = PidRoom}) ->
    case Status of
        2 ->
            gen_server:cast(PidRoom, {set_type, Id, Type});
        _ -> 
            ?WARNING("Recv 14035 when status is ~w.", [Status]),
            ok
    end,
    {ok};

%% 战斗奖励
handle(14037, [IsWin, GameMode, GuildV], Rs) ->
    DropLv = data_config:get(drop_lv),
    MyDropLv = find_dorp_lv(Rs#role.lev, DropLv),
    RewardType = GameMode * 10000 + MyDropLv,
    DataReward = data_reward:get(RewardType),
    Rand = util:rand(1, 100000),
    {Id, Num, Tips} = reward_process(Rand, DataReward),
    AtFestival = lib_festival:at(pro_time),
    {ItemReward1, FestivalReward} = case {AtFestival, IsWin} of
        {true, 1} -> {[{280002, 1}], 1};
        {true, _} -> 
            case util:rate(30) of
                true -> {[{280002, 1}], 1};
                false -> {[], 0}
            end;
        _ -> {[], 0}
    end,
    ItemReward2 = case Id > 1000 of
        true -> [{Id, Num}];
        false -> []
    end,
    {Rs1, AddGold1} = if
        Id == 1 ->
            %% ?INFO("reward Gold ~w! Name:~s", [Num, Rs#role.name]),
            AddGold = lib_role:guild_benefit_addition(gold, Num, Rs#role.guild_b),
            {lib_role:add_attr(Rs, gold, Num + AddGold), AddGold};
        Id == 2 ->
            %% ?INFO("reward Card ~w! Name:~s", [Num, Rs#role.name]),
            {lib_role:add_attr(Rs, card, Num), 0};
        true -> 
            {Rs, 0}
    end,
    ItemReward = ItemReward1 ++ ItemReward2,
    Rs2 = case ItemReward of
        [] -> Rs1;
        _ ->
            MyItems = lib_item:get_myitems(),
            case lib_item:add_items(ItemReward, MyItems) of
                {ok, MyItems1, My} ->
                    %% ?INFO("reward Item ~w! Name:~s", [{Id, Num}, Rs1#role.name]),
                    lib_item:put_myitems(MyItems1),
                    lib_item:add_item_notice(My, Rs1#role.pid_sender),
                    case Tips of
                        1 ->
                            lib_conn:pack_cast(world, 15008, [1, Rs1#role.id, Rs1#role.name, Id]),
                            Rs1#role{save = [{myitems, MyItems1}]};
                        _ ->
                            Rs1
                    end;
                {error, at_full} ->
                    lib_conn:send_code(Rs1#role.pid_sender, 17000101),
                    Rs1;
                {error, Error} ->
                    ?WARNING("handle 14037 error: ~w", [Error]),
                    Rs1
            end
    end,
    Rs3 = Rs2#role{guild_v = Rs2#role.guild_v + GuildV},
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
    {ok, [Id, Num, AddGold1, GuildV, FestivalReward], Rs3};

%% handle(14039, [CarbonId], Rs) ->
%%     gen_server:cast(Rs#role.pid_room, {select_carbon, Rs#role.id, 
%%             Rs#role.pid_sender, CarbonId}),
%%     {ok};

%% 加载游戏资源完成
handle(14062, [], Rs) when Rs#role.status == 3 ->
    Rs#role.pid_room ! {pass_loaded, Rs#role.id, Rs#role.pid_sender},
    {ok};
handle(14062, [], _Rs) -> {ok};

handle(14066, [Nth], Rs) when Rs#role.status == 3 ->
    Rs#role.pid_room ! {get_reward, Rs#role.id, Nth},
    {ok};
handle(14066, [_], _Rs) -> {ok};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%% 私有函数

reward_process(Rand, [{Id, Num, {Min, Max}, {tips, Tips}, _} | T]) ->
    case Rand >= Min andalso Rand =< Max of
        true -> {Id, Num, Tips};
        false -> reward_process(Rand, T)
    end;
reward_process(_, []) -> {0, 0, 0}.

find_dorp_lv(Lev, [{Id, {Min, Max}} | T]) ->
    case Lev >= Min andalso Lev =< Max of
        true -> Id;
        false -> find_dorp_lv(Lev, T)
    end;
find_dorp_lv(_, []) -> 0.
