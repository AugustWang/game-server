%%----------------------------------------------------
%% 社交相关
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_sns).
-export([handle/3]).

-include("common.hrl").
-include("friend.hrl").

%%' === 内部协议 ===

%% 被加好友
handle(1802, [_FromRid, FromPid], Rs) ->
    Friend = #friend{ 
        id = Rs#role.id 
        ,name = Rs#role.name 
        ,lev = Rs#role.lev 
        ,sex = Rs#role.sex 
    },
    FromPid ! {handle_event, 1803, [Friend]},
    {ok};

%% 加好友
handle(1803, [Friend], Rs) ->
    case add_friend(Friend) of
        true -> 
            %% 加好友成功
            gen_server:cast(srv_cache, {
                    add_event 
                    ,Friend#friend.id 
                    ,{?EVENT_ADD_FANS, Rs#role.id} 
                }),
            self() ! {task, ?TASK_ADD_FRIEND, {add, 1}},
            lib_conn:pack_send(Rs#role.pid_sender, 18008, [0]);
        false -> 
            %% 已经是好友
            lib_conn:pack_send(Rs#role.pid_sender, 18008, [2])
    end,
    {ok};

%%. ================

%% 聊天
handle(18001, [Ch, Msg], Rs) ->
    #role{
        id = Id
        ,name = Name 
        ,sex = Sex
        ,status = Status 
        ,pid_room = PidRoom
        ,pid_room1 = PidRoom1
        ,guild_id = Gid
        ,bit_data = BitData
    } = Rs,
    Data = [Id, Name, Msg, Sex, Ch],
    Ban = lib_role:bit_chk(BitData, ?BIT_BAN_CHAT),
    if
        Ban == 1 ->
            ?INFO("bind:~w", [Id]),
            ok;
        Ch == 0 -> 
            %% 当前频道
            case Status of
                3 ->
                    case is_pid(PidRoom) andalso is_process_alive(PidRoom) of
                        true -> PidRoom ! {pack_cast, 18001, Data};
                        false -> 
                            ?WARNING("chat WARNING", []),
                            ok
                    end;
                _ -> 
                    NowTime = util:unixtime(),
                    LastChatTime = case get(lastchattime) of
                        undefined -> 0;
                        LC -> LC
                    end,
                    put(lastchattime, NowTime),
                    case ((NowTime - LastChatTime) * 1000) < data_config:get(chat_interval_time) of
                        true -> ok;
                        false -> lib_conn:pack_cast(nogame, 18001, Data)
                    end
            end;
            %% case Status of
            %%     1 -> 
            %%         %% 当前在大厅
            %%         NowTime = util:unixtime(),
            %%         LastChatTime = case get(lastchattime) of
            %%             undefined -> 0;
            %%             LC -> LC
            %%         end,
            %%         put(lastchattime, NowTime),
            %%         case ((NowTime - LastChatTime) * 1000) < data_config:get(chat_interval_time) of
            %%             true -> ok;
            %%             false -> lib_conn:pack_cast(nogame, 18001, Data)
            %%         end;	
            %%     _ ->
            %%         case is_pid(PidRoom) andalso is_process_alive(PidRoom) of
            %%             true -> PidRoom ! {pack_cast, 18001, Data};
            %%             false -> 
            %%                 ?WARNING("chat WARNING", []),
            %%                 ok
            %%         end
            %% end;
        Ch == 1 -> 
            %% 公会
            case Gid > 0 of
                true -> lib_conn:pack_cast(guild, Gid, 18001, Data);
                false -> ok
            end;
        Ch == 2 -> 
            %% 队伍
            case is_pid(PidRoom1) andalso is_process_alive(PidRoom1) of
                true -> PidRoom1 ! {pack_cast, 18001, Data};
                false -> ok
            end,
            ok;
        Ch == 3 -> 
            %% 喇叭
            MyItems = lib_item:get_myitems(),
            case lib_item:del_item(by_tid, 180001, 1, MyItems) of
                {ok, MyItems1, Dels} ->
                    lib_item:put_myitems(MyItems1),
                    lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
                    lib_conn:pack_cast(world, 18001, Data);
                {error, _Reason} -> 
                    %% ?INFO("删除出错：~w", [Reason])
                    ok
            end,
            ok;
        true ->
            ?WARNING("Chat Ch undefined: ~w", [Ch]),
            ok
    end,
    {ok, Vtype} = application:get_env(cc, version_type),
    case Vtype == dev of
        true ->
            case erlang:bitstring_to_list(Msg) of
                [36 | T] -> 
                    case Id > 0 of
                        true ->
                            T1 = re:replace(T,"\r","",[{return,list}]),
                            lib_test:gm(T1, Rs);
                        false -> 
                            lib_conn:pack_send(Rs#role.pid_sender,
                                15002, [<<"此号禁止使用命令">>]),
                            {ok}
                    end;
                _ -> {ok}
            end;
        false -> {ok}
    end;

handle(18003, [Rid, Msg], Rs) ->
    case get({chat_to_one, Rid}) of
        undefined ->
            case lib_role:get_online_info(Rid) of
                false -> skip;
                Online ->
                    NewFriend = #friend{
                        id = Rid 
                        ,name = Online#online.name
                        ,lev = Online#online.lev
                        ,sex = Online#online.sex
                    },
                    add_friend(NewFriend, 2),
                    ok
            end,
            put({chat_to_one, Rid}, true);
        _Data -> skip
    end,
    Reply = [Rs#role.id, Rs#role.name, Msg, Rs#role.sex],
    lib_conn:pack_send(Rid, 18003, Reply),
    {ok, Reply};

handle(18005, [], Rs) ->
    lib_sns:send_friends(Rs#role.pid_sender),
    {ok};

%% -> 1802 -> 1803
handle(18007, [Rid], Rs) ->
    %% ?INFO("add_friend:~w, ~w", [Rid, self()]),
    Self = self(),
    case lib_authen:get_role_pid(role_id, Rid) of
        false -> {ok, [1]};
        Self -> 
            %% ?INFO("不能添加自己：~w", [Rid]),
            {ok, [1]};
        Pid -> 
            Pid ! {handle_event, 1802, [Rs#role.id, self()]},
            {ok}
    end;

%% -> 1802 -> 1803
handle(18008, [Name], Rs) ->
    ?INFO("add_friend:~s, ~w", [Name, self()]),
    Self = self(),
    case lib_authen:get_role_pid(name, Name) of
        false -> {ok, [1]};
        Self -> 
            %% ?INFO("不能添加自己：~s", [Name]),
            {ok, [3]};
        Pid -> 
            Pid ! {handle_event, 1802, [Rs#role.id, self()]},
            {ok}
    end;

%% handle(18007, [Rid], Rs) ->
%%     case ets:match(online, #online{id=Rid, name='$1', lev='$2', sex='$3', _='_'}) of
%%         {error, _Reason} -> 
%%             ?INFO("添加失败：~w", [Rid]),
%%             {ok, [1]};
%%         [] -> 
%%             case lib_role:load_role_data(by_id, Rid) of
%%                 undefined -> {ok, [1]};
%%                 RsF ->
%%                     NewFriend = #friend{id = RsF#role.id, name = RsF#role.name, lev = RsF#role.lev, sex = RsF#role.sex},
%%                     case add_friend(NewFriend) of
%%                         true -> {ok, [0]};
%%                         false -> {ok, [1]}
%%                     end
%%             end;
%%         [[Name, Lev, Sex]] when Rid =/= Rs#role.id -> 
%%             NewFriend = #friend{id = Rid, name = Name, lev = Lev, sex = Sex},
%%             case add_friend(NewFriend) of
%%                 true -> {ok, [0]};
%%                 false -> {ok, [2]}
%%             end;
%%         _ ->
%%             ?INFO("不能添加自己：~w", [Rid]),
%%             {ok, [3]}
%%     end;

%% handle(18008, [Name], Rs) ->
%%     case ets:match(online, #online{id='$1', lev='$2', sex='$3', name = Name, _='_'}) of
%%         {error, _Reason} -> 
%%             ?INFO("添加失败：~s", [Name]),
%%             {ok, [1]};
%%         [] -> 
%%             case lib_role:load_role_data(by_name, Name) of
%%                 undefined -> {ok, [1]};
%%                 RsF ->
%%                     NewFriend = #friend{id = RsF#role.id, name = RsF#role.name, lev = RsF#role.lev, sex = RsF#role.sex},
%%                     case add_friend(NewFriend) of
%%                         true -> {ok, [0]};
%%                         false -> {ok, [1]}
%%                     end
%%             end;
%%         [[Rid, Lev, Sex]] when Rid =/= Rs#role.id -> 
%%             NewFriend = #friend{id = Rid, name = Name, lev = Lev, sex = Sex},
%%             case add_friend(NewFriend) of
%%                 true -> {ok, [0]};
%%                 false -> {ok, [2]}
%%             end;
%%         _ ->
%%             ?INFO("不能添加自己：~s", [Name]),
%%             {ok, [3]}
%%     end;

handle(18009, [Id], Rs) ->
    Friends = lib_sns:get_friends(1),
    Friends1 = lib_sns:del_friend(Id, Friends),
    lib_sns:put_friends(1, Friends1),
    gen_server:cast(srv_cache, {add_event, Id, {?EVENT_DEL_FANS, Rs#role.id}}),
    {ok, [1]};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

add_friend(NewFriend) ->
    add_friend(NewFriend, 1).
add_friend(NewFriend, Type) ->
    Friends = lib_sns:get_friends(Type),
    case lib_sns:add_friend(Type, NewFriend, Friends) of
        {ok, Friends1} -> 
            lib_sns:put_friends(Type, Friends1),
            true;
        {error, _} -> false
    end.
