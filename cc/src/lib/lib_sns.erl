%%----------------------------------------------------
%% SNS
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_sns).
-export([
        send_friends/1
        ,get_friends/1
        ,put_friends/2
        ,add_friend/3
        ,get_friend/2
        ,set_friend/2
        ,del_friend/2
        ,init_friends/1
        ,get_myfriends/0
        ,update_friend_lev/2
        ,request_friends_lev/1
        ,add_fans/1
        ,del_fans/1
        ,respond_fans_lev/2
    ]
).

-include("common.hrl").
-include("friend.hrl").

%% 1 = 好友
%% 2 = 最近
%% 3 = Fans
init_friends(Friends) ->
    case Friends of
        [Friends1, Friends2] ->
            put_friends(1, Friends1),
            put_friends(2, Friends2),
            put_friends(3, []);
        [Friends1, Friends2, Fans] ->
            put_friends(1, Friends1),
            put_friends(2, Friends2),
            case check_fans(Fans) of
                true -> put_friends(3, Fans);
                false -> 
                    put_friends(3, []),
                    ?INFO("error fans: ~w", [Fans])
            end;
        _ -> skip
    end.

check_fans([Id | Ids]) when is_integer(Id) ->
    check_fans(Ids);
check_fans([_Id | _Ids]) -> false;
check_fans([]) -> true.

is_online(Rid) -> 
    case ets:member(online, Rid) of
        true -> 1;
        false -> 0
    end.
            
send_friends(PidSender) ->
    %% 好友
    Data1 = get_friends(1),
    Data2 = [[Fr#friend.id, Fr#friend.name, 
            Fr#friend.lev, Fr#friend.sex, is_online(Fr#friend.id)]|| Fr <- Data1],
    %% 最近
    Data3 = get_friends(2),
    Data4 = [[Fr#friend.id, Fr#friend.name, 
            Fr#friend.lev, Fr#friend.sex, is_online(Fr#friend.id)]|| Fr <- Data3],
    lib_conn:pack_send(PidSender, 18005, [Data2, Data4]).

request_friends_lev(Rid) ->
    case lib_sns:get_friends(1) of
        [] -> ok;
        FriendData1 ->
            FriendIds = [Fr#friend.id || Fr <- FriendData1],
            Event = {?EVENT_REQUEST_LEV, Rid},
            srv_cache:send_event(FriendIds, Event)
    end.

respond_fans_lev(Rid, Lev) ->
    case lib_sns:get_friends(3) of
        [] -> ok;
        Fans ->
            Event = {?EVENT_RESPOND_LEV, Rid, Lev},
            srv_cache:send_event(Fans, Event)
    end.

get_myfriends() ->
    [get_friends(1), get_friends(2), get_friends(3)].

get_friends(Type) ->
    case get({Type, friends}) of
        undefined -> [];
        Friends -> Friends 
    end.

put_friends(Type, Friends) ->
    put({Type, friends}, Friends).

add_friend(Type, NewFriend, Friends) ->
    case Type of
        1 -> 
            case get_friend(NewFriend#friend.id, Friends) of
                false -> 
                    Friends1 = [NewFriend | Friends],
                    Friends2 = lists:sublist(Friends1, 150),
                    {ok, Friends2};
                _ -> {error, be_friend}
            end;
        2 ->
            Friends1 = del_friend(NewFriend#friend.id, Friends),
            Friends2 = lists:sublist(Friends1, 15),
            {ok, [NewFriend | Friends2]}
    end.

add_fans(Rid) ->
    Fans = get_friends(3),
    case lists:member(Rid, Fans) of
        true -> ok;
        false -> 
            Fans1 = [Rid | Fans],
            %% ?INFO("add_fans:~w, ~w fans:~w", [Rid, self(), Fans]),
            put_friends(3, Fans1)
    end.

del_fans(Rid) ->
    Fans = get_friends(3),
    case lists:member(Rid, Fans) of
        true -> 
            Fans1 = lists:delete(Rid, Fans),
            %% ?INFO("del_fans:~w, ~w fans:~w", [Rid, self(), Fans]),
            put_friends(3, Fans1),
            ok;
        false -> 
            ok
    end.

get_friend(Rid, Friends) ->
    lists:keyfind(Rid, 2, Friends).

set_friend(Friend, Friends) ->
    lists:keyreplace(Friend#friend.id, 2, Friends, Friend).

del_friend(Rid, Friends) ->
    lists:keydelete(Rid, 2, Friends).

update_friend_lev(Rid, Lev) ->
    update_friend_lev(Rid, Lev, 1),
    update_friend_lev(Rid, Lev, 2).

update_friend_lev(Rid, Lev, Type) ->
    Friends = get_friends(Type),
    case get_friend(Rid, Friends) of
        false -> ok;
        Friend ->
            %% ?INFO("update_friend_lev, Type:~w, Rid:~w, Lev:~w", [Type, Rid, Lev]),
            Friend1 = Friend#friend{lev = Lev},
            Friends1 = set_friend(Friend1, Friends),
            put_friends(Type, Friends1)
    end.
