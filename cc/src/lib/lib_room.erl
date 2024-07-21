%%----------------------------------------------------
%% 房间
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_room).
-export([
        get_roles/1
    ]
).

-include("common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

get_roles(RoomId) ->
    List = ets:match_object(online, #online{room_id = RoomId, _ = '_'}),
    [{O#online.id, O#online.name} || O <- List].
