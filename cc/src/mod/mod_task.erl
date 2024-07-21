%%----------------------------------------------------
%% Task
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_task).
-export([handle/3]).

-include("common.hrl").
-include("task.hrl").

handle(19001, [], _Rs) ->
    MyTasks = lib_task:get_mytasks(),
    Data = transit_tasks(MyTasks, []),
    {ok, [Data]};

handle(19003, [TaskId, SelTid], Rs) ->
    MyTasks = lib_task:get_mytasks(),
    case lib_task:get_task_reward(TaskId, MyTasks) of
        {ok, MyTask, MyTasks1} ->
            AttrReward = attr_reward(MyTask#mytask.reward, []),
            ItemReward = case MyTask#mytask.reward_item of
                [] -> [];
                Tmp ->
                    case MyTask#mytask.select_item == 1 of
                        true -> [[Tid, Num, ELev] || [Tid, Num, ELev] <- Tmp, Tid == SelTid];
                        false -> Tmp
                    end
            end,
            %% ?INFO("type:~w, loop:~w, ItemReward:~w", [MyTask#mytask.type, MyTask#mytask.loop, ItemReward]),
            Save1 = case (ItemReward == []) orelse (MyTask#mytask.type == 2 andalso MyTask#mytask.loop > 0)  of
                true -> [];
                false ->
                    %% 物品奖励
                    MyItems = lib_item:get_myitems(),
                    {ok, MyItems1, AddItem} = item_reward(ItemReward, MyItems, []),
                    lib_item:add_item_notice(AddItem, Rs#role.pid_sender),
                    lib_item:put_myitems(MyItems1),
                    [{myitems, MyItems1}]
            end,
            %% 属性奖励
            Rs1 = lib_role:add_attr(Rs, AttrReward),
            lib_task:put_mytasks(MyTasks1),
            %% F = fun
            %%     ({gold, N}) -> [1, N, 0];
            %%     ({card, N}) -> [2, N, 0];
            %%     ({exp, N}) -> [3, N, 0]
            %% end,
            %% TipsData1 = [F(X) || X <- AttrReward],
            %% TipsData2 = [[4, Tid, Num] || [Tid, Num, _] <- ItemReward],
            %% TipsData = TipsData1 ++ TipsData2,
            %% lib_conn:pack_send(Rs#role.pid_sender, 11030, [TipsData]),
            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
            lib_conn:pack_send(Rs#role.pid_sender, 11020, [3]),
            Save = Save1 ++ [{mytasks, MyTasks}, {kvs, lib_role:get_kvs(Rs1)}],
            ?LOG_GOLD(Rs, Rs1, 1009),
            ?LOG_CARD(Rs, Rs1, 1009),
            {ok, [MyTask#mytask.tid, 0], Rs1#role{save = Save}};
        {error, _} -> {ok, [0, 1]}
    end;

handle(_Cmd, _Data, _Rs) ->
    {error, bad_request}.

transit_tasks([My| T], Reply) ->
    RewardItem = case My#mytask.type == 2 andalso My#mytask.loop > 0 of
        true -> [];
        false -> My#mytask.reward_item 
    end,
    R = [
         My#mytask.id 
        ,My#mytask.tid 
        ,My#mytask.type
        ,My#mytask.process
        ,My#mytask.ctl2
        ,util:ceil(My#mytask.rfactor * 100)
        ,My#mytask.reward 
        ,My#mytask.select_item 
        ,RewardItem
    ],
    Reply1 = [R | Reply], 
    transit_tasks(T, Reply1);
transit_tasks([], Reply) -> Reply.

item_reward([[Tid, Num, ELev] | T], MyItems, Add) ->
    case lib_item:add_item(Tid, Num, ELev, MyItems) of
        {ok, MyItems1, My} ->
            item_reward(T, MyItems1, My ++ Add);
        {error, _} -> item_reward(T, MyItems, Add)
    end;
item_reward([], MyItems, Add) -> {ok, MyItems, Add}.

attr_reward([[1, X] | T], Reply) ->
    attr_reward(T, [{gold, X} | Reply]);
attr_reward([[2, X] | T], Reply) ->
    attr_reward(T, [{card, X} | Reply]);
attr_reward([[3, X] | T], Reply) ->
    attr_reward(T, [{exp, X} | Reply]);
attr_reward([], Reply) -> Reply.
