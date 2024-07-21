%%----------------------------------------------------
%% 大厅
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_hall).
-export([handle/3]).

-include("common.hrl").

%% 房间列表
handle(13001, [Type], Rs) ->
    case lists:member(Type, ?GAME_MODES) of
        true ->
            srv_hall:room_list(Type, Rs#role.pid_sender),
            ok;
        false -> 
            ?WARNING("invalid game mode(~w).", [Type]),
            ok
    end,
    {ok};

%% 进入房间
handle(13003, [RoomType, RoomId], #role{status = Status, pid_sender = PidSender, pid_room = PidRoom} = Rs) ->
    case Status == 2 andalso RoomId == 0 of
        true ->
            %% 断线重连后，游戏结束回房间时，客户端发起进入房间请求，RoomId=0
            gen_server:cast(PidRoom, {send_13003, PidSender}),
            {ok};
        false ->
            case Status =/= 1 andalso is_pid(PidRoom) andalso is_process_alive(PidRoom) of
                true -> gen_server:cast(PidRoom, {role_exit, Rs#role.id});
                false -> ok
            end,
            case RoomType > 100 of
                true -> lib_item:clear_mydaojus();
                false -> ok
            end,
            srv_hall:join_room(RoomType, RoomId, <<>>, Rs),
            {ok}
    end;

%% 进入房间
handle(13004, [RoomType, RoomId, Password], Rs) ->
    #role{status = Status, pid_room = PidRoom} = Rs,
    case Status =/= 1 andalso is_pid(PidRoom) andalso is_process_alive(PidRoom) of
        true -> gen_server:cast(PidRoom, {role_exit, Rs#role.id});
        false -> ok
    end,
    %% ?INFO("RoomType:~w, RoomId:~w, Password:~w", [RoomType, RoomId, Password]),
    case RoomType > 100 of
        true -> lib_item:clear_mydaojus();
        false -> ok
    end,
    srv_hall:join_room(RoomType, RoomId, Password, Rs),
    {ok};

%% 创建房间
handle(13005, [Type], Rs) ->
    #role{status = Status, pid_room = PidRoom} = Rs,
    case Status =/= 1 andalso is_pid(PidRoom) andalso is_process_alive(PidRoom) of
        true -> gen_server:cast(PidRoom, {role_exit, Rs#role.id});
        false -> ok
    end,
    case lists:member(Type, ?GAME_MODES) of
        true ->
            case Type > 100 of
                true -> lib_item:clear_mydaojus();
                false -> ok
            end,
            srv_hall:create_room(Type, Rs),
            ok;
        false -> 
            ?WARNING("Error Type(~w) when create_room", [Type]),
            ok
    end,
    {ok};

%% 在线列表
handle(13007, [], _Rs) ->
    case ets:select(online, [{#online{id='$1', name='$2', sex='$3', lev='$4', status = '$5', _ = '_'}, [{'==','$5',1}], [['$1','$2','$3','$4']]}], 20) of
        {L, _} -> 
            {ok, [L]};
        '$end_of_table' -> 
            {ok};
        _Other -> 
            ?WARNING("unexpected data:~w", [_Other]),
            {ok}
    end;

%% 进入指定玩家的房间
handle(13009, [ToRid], Rs) ->
    case lib_authen:get_online_pid(role_id, ToRid) of
        false -> {ok, [2]};
        ToPid -> 
            ToPid ! {handle_event, 6115, [Rs#role.pid, Rs#role.pid_sender]},
            {ok}
    end;

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.
