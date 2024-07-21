%%----------------------------------------------------
%% 消息发送器
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_sender).
-export([
        send/4
        ,send/3
        ,send/2
        ,to_role_info/1
        ,to_role_equ/1
        ,send_info/2
    ]
).

-include("common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

send_info(Rid, Info) ->
    case lib_authen:get_role_pid(role_id, Rid) of
        false -> 
            ?WARNING("send failure! [~w]", [Info]),
            ok;
        Pid -> 
            %% catch Pid ! Info
            Pid ! Info
    end.

%% 转换成可以发送给客户端的角色数据
to_role_info(Rs) when is_record(Rs, role) ->
    Exp2Time = lib_role:get_exp2_time(Rs#role.exp2_etime),
    [
        Rs#role.id
        ,Rs#role.name
        ,Rs#role.lev
        ,Rs#role.sex
        ,Rs#role.gold
        ,Rs#role.card
        ,Rs#role.hp
        ,Rs#role.hp_max
        ,Rs#role.dmg
        ,Rs#role.attack
        ,Rs#role.fb_attack
        ,Rs#role.dmg_speed
        ,Rs#role.move_speed
        ,Rs#role.exp
        ,Rs#role.crit
        ,Rs#role.win_rate
        ,Rs#role.guild_id
        ,Rs#role.guild_name
        ,Exp2Time
    ].

%% 转换成可以发送给客户端的装备数据
to_role_equ(MyItems) ->
    to_role_equ(?EQU_POS, MyItems, []).

to_role_equ([Pos | T], MyItems, Reply) ->
    Reply1 = case lists:keyfind(Pos, #myitem.pos, MyItems) of
        false -> Reply;
        MyItem -> 
            case MyItem#myitem.attr of 
                undefined -> 
                    ?WARNING("error myitem [Tid:~w, Pos:~w]", [MyItem#myitem.tid, Pos]),
                    Reply;
                _ -> 
                    [[
                            MyItem#myitem.tid 
                            ,Pos 
                            ,MyItem#myitem.lev 
                            ,MyItem#myitem.attr#equ_attr.crit 
                            ,0
                            ,MyItem#myitem.attr#equ_attr.hp_max
                            ,MyItem#myitem.attr#equ_attr.move_speed 
                            ,MyItem#myitem.attr#equ_attr.dmg_speed 
                            ,MyItem#myitem.attr#equ_attr.attack 
                            ,MyItem#myitem.attr#equ_attr.fb_attack 
                            ,lib_item:calc_etime(MyItem#myitem.etime) 
                        ] | Reply]
            end
    end,
    to_role_equ(T, MyItems, Reply1);
to_role_equ([], _MyItems, Reply) -> Reply.

send(11006, Rs) ->
    Data = to_role_info(Rs) ++ [Rs#role.skill],
    %% ?INFO("11006:~w", [Data]),
    lib_conn:pack_send(Rs#role.pid_sender, 11006, Data).

send(11022, PidSender, Rs, MyItems) ->
    Attr = to_role_info(Rs),
    EquInfo = to_role_equ(MyItems),
    Attr1 = Attr ++ [Rs#role.skill] ++ [EquInfo],
    lib_conn:pack_send(PidSender, 11022, Attr1).

send(11006, PidSender, Rs) ->
    Data = to_role_info(Rs) ++ [Rs#role.skill],
    %% ?INFO("11006:~w", [Data]),
    lib_conn:pack_send(PidSender, 11006, Data).
