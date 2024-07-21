%%----------------------------------------------------
%% Task
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_task).
-export([
        init_mytasks/5
        ,init_mytask/2
        ,get_mytasks/0
        ,put_mytasks/1
        ,process/3
        ,get_task_reward/2
        ,gen_data/0
        ,fix_mytasks1/1
        ,fix_mytasks2/0
        ,refresh_mytask/1
        ,zip/1
    ]
).

-include("common.hrl").
-include("task.hrl").
-include("task_reward.hrl").

%% @spec () -> MyTasks
get_mytasks() ->
    get(mytasks).

%% @spec () -> put()
put_mytasks(MyTasks) ->
    %% ?INFO("MyTasks:~w", [MyTasks]),
    put(mytasks, MyTasks).

get_mytask_id() -> 
    Id = get(mytask_max_id) + 1,
    put(mytask_max_id, Id),
    Id.

%% @spec (CType, MyTasks) -> {ok} | {ok, NewMyTasks}
%% @doc 角色进程内部调用
process(CType, Arg, MyTasks) ->
    %% ?INFO("CType:~w, Arg:~w, MyTasks:~w", [CType, Arg, MyTasks]),
    process(CType, Arg, MyTasks, [], false).

%% 进度增加 [Num]
process(CType, {add, Num}, [#mytask{ctype = CType} = MyTask | T], Reply, Change) ->
    {MyTask1, Change1} = process1(MyTask, Num, Change),
    process(CType, {add, Num}, T, [MyTask1 | Reply], Change1);

%% [ctl1] 进度增加 [Num]
process(CType, {add, Ctl1, Num} = Arg, [#mytask{ctype = CType, ctl1 = Ctl1} = MyTask | T], Reply, Change) ->
    {MyTask1, Change1} = process1(MyTask, Num, Change),
    process(CType, Arg, T, [MyTask1 | Reply], Change1);

%% 已经达到 Num
process(CType, {to, Num} = Arg, [#mytask{ctype = CType} = MyTask | T], Reply, Change) ->
    {MyTask1, Change1} = process0(MyTask, Num, Change),
    process(CType, Arg, T, [MyTask1 | Reply], Change1);

%% [Ctl1] 已经达到 [Num]
process(CType, {to, Ctl1, Num}, [#mytask{ctype = CType, ctl1 = Ctl1} = MyTask | T], Reply, Change) ->
    {MyTask1, Change1} = process0(MyTask, Num, Change),
    process(CType, {to, Ctl1, Num}, T, [MyTask1 | Reply], Change1);

%% 设置达到 Num
process(CType, {set_to, Num} = Arg, [#mytask{ctype = CType} = MyTask | T], Reply, Change) ->
    {MyTask1, Change1} = process2(MyTask, Num, Change),
    process(CType, Arg, T, [MyTask1 | Reply], Change1);

%% 与[本次触发]无关的任务,直接跳过
process(CType, Arg, [MyTask | T], Reply, Change) ->
    process(CType, Arg, T, [MyTask | Reply], Change);

%% 检查结束，返回
process(_, _, [], Reply, Change) -> 
    case Change of 
        true -> {ok, Reply};
        false -> {ok}
    end.

%% 无中间进度 (只有两个状态：未完成 OR 完成)
process0(MyTask, ProcessAdd, Change) ->
    Process = MyTask#mytask.process,
    Process1 = Process + ProcessAdd,
    ProcessOk = MyTask#mytask.ctl2,
    if
        Process1 >= ProcessOk ->
            %% 完成任务
            {MyTask#mytask{process = ProcessOk}, true};
        true ->
            {MyTask, Change}
    end.

%% 有中间进度
process1(MyTask, ProcessAdd, Change) ->
    Process = MyTask#mytask.process,
    Process1 = Process + ProcessAdd,
    ProcessOk = MyTask#mytask.ctl2,
    if
        Process >= ProcessOk ->
            %% 已经完成的任务
            {MyTask, Change};
        Process1 >= ProcessOk ->
            %% 完成任务
            {MyTask#mytask{process = ProcessOk}, true};
        true ->
            {MyTask#mytask{process = Process1}, true}
    end.

%% 设置进度
process2(MyTask, SetToProcess, Change) ->
    Process = MyTask#mytask.process,
    ProcessOk = MyTask#mytask.ctl2,
    if
        Process >= ProcessOk ->
            %% 已经完成的任务
            {MyTask, Change};
        true ->
            {MyTask#mytask{process = SetToProcess}, true}
    end.

%% 重置日常任务
%% @spec (Lev, MyTasks) -> NewMyTasks
reset_dtask(Lev, MyTasks, EnableDTask, DTaskTime1) ->
    Now = util:unixtime(),
    DTaskTime = case is_integer(DTaskTime1) of
        false -> 0;
        true -> DTaskTime1
    end,
    %% ?INFO("Lev:~w, ~w, ~w, Time:~w", [Lev, DTaskTime, DTaskTime1, Time]),
    case DTaskTime > 0 andalso Now < DTaskTime of
        true -> {MyTasks, DTaskTime};
        false ->
            DailyTaskIds = data_task:daily(Lev),
            F = fun(Id) -> lists:member(Id, EnableDTask) end,
            DailyTaskIds1 = lists:filter(F, DailyTaskIds),
            MyTasks1 = reset_dtask1(DailyTaskIds1, MyTasks),
            Time = util:unixtime(tomorrow) + 6 * 3600,
            {MyTasks1, Time}
    end.

reset_dtask1([TaskId | T], MyTasks) ->
    F = fun(MyTask) ->
            MyTask#mytask.tid == TaskId andalso 
            MyTask#mytask.process < MyTask#mytask.ctl2 
    end,
    case lists:filter(F, MyTasks) of
        [] ->
            AddMyTask = init_mytask(TaskId, 0),
            reset_dtask1(T, [AddMyTask | MyTasks]);
        [My | _] ->
            %% 找到未完成的日常任务，并保留
            Task = data_task:get(TaskId),
            Loop = 0,
            MyTasks1 = set_mytask(My#mytask{
                    loop = Loop
                    ,rfactor = lists:nth(Loop + 1, Task#task.rfactor)
                    ,reward = init_reward(Task, Loop)
                }, MyTasks),
            reset_dtask1(T, MyTasks1)
    end;
reset_dtask1([], MyTasks) -> MyTasks.

init_mytasks(MyTasksBin, Lev, TaskTail, EnableDTask, DTaskTime) ->
    %% ?INFO("init_mytasks, Lev:~w", [Lev]),
    MyTasks = case util:bitstring_to_term(MyTasksBin) of
        undefined -> 
            put(mytask_max_id, 0),
            [
                init_mytask(1, 0)
            ];
        [] ->
            ?WARNING("Unexpect empty task when init_mytasks, Lev:~w", [Lev]),
            put(mytask_max_id, 0),
            case Lev of
                5 -> [init_mytask(12, 0)];
                _ -> []
            end;
        MyTasksData -> 
            M1 = unzip(MyTasksData),
            Ids = [Id || #mytask{id = Id} <- M1],
            MaxId = case Ids of
                [] -> 0;
                _ -> lists:max(Ids)
            end,
            put(mytask_max_id, MaxId),
            %% M1
            %% 删掉血瓶任务
            lists:keydelete(24, #mytask.tid, M1)
    end,
    {MyTasks1, DTaskTime1} = reset_dtask(Lev, MyTasks, EnableDTask, DTaskTime),
    {ok, MyTasks2, TaskTail1} = init_tail(TaskTail, [], []),
    put_mytasks(MyTasks1 ++ MyTasks2),
    {TaskTail1, DTaskTime1}.

refresh_mytask(Rs) ->
    #role{lev = Lev, enable_dtask = EnableDTask,
    dtask_time = DTaskTime, task_tail = TaskTail} = Rs,
    MyTasks = get_mytasks(),
    {MyTasks1, DTaskTime1} = reset_dtask(Lev, MyTasks, EnableDTask, DTaskTime),
    {ok, MyTasks2, TaskTail1} = init_tail(TaskTail, [], []),
    put_mytasks(MyTasks1 ++ MyTasks2),
    Rs#role{task_tail = TaskTail1, dtask_time = DTaskTime1}.

init_mytask(TaskId, Loop) ->
    case data_task:get(TaskId) of
        undefined -> 
            ?ERR("undefined task id:~w", [TaskId]),
            #mytask{};
        Task ->
            {Sel, RewardItem} = init_reward_item(TaskId),
            case Task#task.condition == ?TASK_UP_LEV of
                true -> self() ! {task, ?TASK_UP_LEV};
                false -> skip
            end,
            case Task#task.type of
                2 -> self() ! {enable_dtask, TaskId};
                _ -> skip
            end,
            #mytask{
                id = get_mytask_id()
                ,tid = TaskId
                ,type = Task#task.type
                ,ctype = Task#task.condition
                ,ctl1 = Task#task.ctl1
                ,ctl2 = Task#task.ctl2
                ,loop = Loop
                ,loop_max = Task#task.loop
                ,rfactor = lists:nth(Loop + 1, Task#task.rfactor)
                ,reward = init_reward(Task, Loop)
                ,select_item = Sel
                ,reward_item = RewardItem
            }
    end.

init_tail([Tid | T], MyTasks, Tail) ->
    Task = data_task:get(Tid),
    if
        Task == undefined ->
            init_tail(T, MyTasks, Tail);
        Task#task.next == [0] ->
            init_tail(T, MyTasks, Tail);
        Task#task.next == [] ->
            init_tail(T, MyTasks, [Tid | Tail]);
        true ->
            My = init_mytask(Tid, 0),
            init_tail(T, [My | MyTasks], Tail)
    end;
init_tail([], MyTasks, Tail) -> {ok, MyTasks, Tail}.

init_reward(Task, Loop) ->
    Rfactor = lists:nth(Loop + 1, Task#task.rfactor),
    init_reward(Task, Rfactor, []).

init_reward(#task{rgold = X} = Task, Rfactor, Reply) when X > 0 ->
    init_reward(Task#task{rgold = 0}, Rfactor, [[1, util:ceil(X * Rfactor)] | Reply]);
init_reward(#task{rcard = X} = Task, Rfactor, Reply) when X > 0 ->
    init_reward(Task#task{rcard = 0}, Rfactor, [[2, util:ceil(X * Rfactor)] | Reply]);
init_reward(#task{rexp = X} = Task, Rfactor, Reply) when X > 0 ->
    init_reward(Task#task{rexp = 0}, Rfactor, [[3, util:ceil(X * Rfactor)] | Reply]);
init_reward(_Task, _Rfactor, Reply) -> Reply.

init_reward_item(TaskId) ->
    case get_reward_items(TaskId, 1, []) of
        [] -> {0, []};
        Items ->
            [#task_reward{option = SelectItem} | _] = Items,
            Items1 = [[R#task_reward.tid, R#task_reward.num, R#task_reward.elev]
                || R <- Items],
            {SelectItem, Items1}
    end.

get_reward_items(TaskId, Index, Reply) ->
    case data_task_reward:get({TaskId, Index}) of
        undefined -> Reply;
        R -> 
            Reply1 = [R | Reply],
            get_reward_items(TaskId, Index + 1, Reply1)
    end.

get_task_reward(Id, MyTasks) ->
    case get_mytask(Id, MyTasks) of
        false -> {error, not_finish};
        MyTask ->
            case MyTask#mytask.process >= MyTask#mytask.ctl2 of
                true ->
                    Loop = MyTask#mytask.loop + 1,
                    MyTasks1 = del_mytask(Id, MyTasks),
                    case MyTask#mytask.type == 2 andalso 
                        lists:keymember(MyTask#mytask.tid, #mytask.tid, MyTasks1) of
                        true ->
                            {ok, MyTask, MyTasks1};
                        false -> 
                            case Loop >= MyTask#mytask.loop_max of
                                true ->
                                    Task = data_task:get(MyTask#mytask.tid),
                                    AddMyTasks = case Task#task.next of
                                        [] -> 
                                            self() ! {task_tail, MyTask#mytask.tid},
                                            [];
                                        [0] -> [];
                                        Next -> 
                                            F = fun(N) ->
                                                    not lists:keymember(N, #mytask.tid, MyTasks1)
                                            end,
                                            Next1 = lists:filter(F, Next),
                                            [init_mytask(I, 0) || I <- Next1]
                                    end,
                                    {ok, MyTask, AddMyTasks ++ MyTasks1};
                                false ->
                                    AddMyTask = init_mytask(MyTask#mytask.tid, Loop),
                                    {ok, MyTask, [AddMyTask | MyTasks1]}
                            end
                    end;
                false ->
                    {error, not_finish}
            end
    end.

get_mytask(Id, MyTasks) ->
    lists:keyfind(Id, 2, MyTasks).

del_mytask(Id, MyTasks) ->
    lists:keydelete(Id, 2, MyTasks).

set_mytask(MyTask, MyTasks) ->
    lists:keyreplace(MyTask#mytask.id, 2, MyTasks, MyTask).

%% gen_data
gen_data() ->
    MainTasks = gen_next_tasks([1], []),
    io:format("~nMainTasks length: ~w", [length(MainTasks)]),
    MainTasks1 = [{X, gen_pre_tasks([X], [])} || X <- MainTasks],
    MainTasks2 = gen_tasks_string(MainTasks1),
    FileName = "src/data/data_pre_task.erl",
    Code = "-module(data_pre_task).
-export([get/1]).

" ++ MainTasks2 ++ "
get(_) -> undefined.
",
    io:format("~nGenerate ~s ok!", [FileName]),
    file:write_file(FileName, Code, []),
    ok.

gen_tasks_string(L) ->
    gen_tasks_string(L, "").
gen_tasks_string([{Id, Pre} | T], Reply) ->
    Pre1 = util:term_to_bitstring(Pre),
    Pre2 = binary_to_list(Pre1),
    Reply1 = lists:concat([Reply, "get(", Id, ") -> ", Pre2, ";\n"]),
    gen_tasks_string(T, Reply1);
gen_tasks_string([], Reply) -> Reply.

gen_pre_tasks([0 | IdT], Reply) ->
    gen_pre_tasks(IdT, Reply);
gen_pre_tasks([Id | IdT], Reply) ->
    case data_task:get(Id) of 
        #task{type = 1, before = Before} ->
            Reply1 = Reply ++ Before,
            gen_pre_tasks(IdT ++ Before, Reply1);
        undefined -> 
            ?INFO("undefined task id: ~w", [Id]),
            Reply;
        _ -> 
            gen_pre_tasks(IdT, Reply)
    end;
gen_pre_tasks([], Reply) -> Reply.

gen_next_tasks([0 | IdT], Reply) ->
    gen_next_tasks(IdT, Reply);
gen_next_tasks([Id | IdT], Reply) ->
    case data_task:get(Id) of 
        %% #task{type = 1, next = Next}
        %% when Next =/= [], Next =/= [0] ->
        #task{type = 1, next = Next} ->
            Reply1 = Reply ++ [Id],
            gen_next_tasks(IdT ++ Next, Reply1);
        undefined -> 
            ?INFO("undefined task id: ~w", [Id]),
            Reply;
        _ -> 
            gen_next_tasks(IdT, Reply)
    end;
gen_next_tasks([], Reply) -> Reply.
%%.

%%' FIX
fix_task_data() ->
    [
        {12, [119, 120, 133, 150]}
        ,{15, [123]}
        ,{50, [126]}
        ,{20, [130]}
    ].

fix_mytasks1(_Name) ->
    MyTasks = get_mytasks(),
    MainIds = [X#mytask.tid || X <- MyTasks, X#mytask.type == 1],
    DoneIds = lists:foldl(
        fun(A, AccIn) -> 
                case data_pre_task:get(A) of
                    undefined ->
                        %% ?INFO("undefined data, [Rid:~w, MainIds:~w, Id:~w]", [Rid, MainIds, A]),
                        AccIn;
                    Pre -> Pre ++ AccIn 
                end
        end
        ,[]
        ,MainIds
    ),
    AddIds = lists:foldl(
        fun({DoneId, AddList}, AccIn) -> 
                case lists:member(DoneId, DoneIds) of
                    true -> AccIn ++ AddList;
                    false -> AccIn
                end
        end
        ,[]
        ,fix_task_data()
    ),
    AddIds1 = lists:filter(
        fun(X) -> 
                case lists:keymember(X, #mytask.tid, MyTasks) of
                    true ->
                        io:format("*~w", [X]),
                        false;
                    false -> true
                end
        end
        ,AddIds
    ),
    case AddIds1 of
        [] -> false;
        _ -> 
            AddMyTasks = [init_mytask(I, 0) || I <- AddIds1],
            MyTasks1 = MyTasks ++ AddMyTasks,
            put_mytasks(MyTasks1),
            %% ?INFO("~s:~w", [Name, AddIds1])
            io:format("."),
            true
    end.

fix_mytasks2() ->
    MyTasks = 
    [
        {mytask,247,89,2,2,0,3,0,1,3,1,[[3,3500],[1,800]],0,[]},
        {mytask,246,88,2,3,0,6,1,1,3,1,[[3,2500],[1,900]],0,[]},
        {mytask,244,83,2,2,0,2,1,2,3,0.5,[[3,1500],[1,300]],0,[]},
        {mytask,243,92,2,9,0,3,0,0,3,2,[[3,7000],[1,1400]],0,[]},
        {mytask,242,91,2,8,0,6,0,0,3,2,[[3,5000],[1,540]],0,[]},
        {mytask,204,41,1,17,0,15,0,0,1,1,[[3,5000],[1,500]],0,[]},
        {mytask,144,26,1,14,0,4,0,0,1,1,[[3,40000],[1,700]],0,[]},
        {mytask,231,90,2,15,0,6,4,0,3,2,[[3,8000],[1,1600]],0,[]},
        {mytask,228,18,1,12,5,5,0,0,1,1,[[3,15000],[1,500]],0,[[220001,4,0]]},
        {mytask,227,57,1,1,0,16,0,0,1,1,[[3,15000],[1,850]],0,[]},
        {mytask,235,84,2,15,0,4,1,1,3,1,[[3,3500],[1,350]],0,[]},
        {mytask,241,93,2,16,0,6,5,0,3,2,[[3,8000],[1,1400]],0,[]}
    ],
    MainIds = [X#mytask.tid || X <- MyTasks, X#mytask.type == 1],
    ?INFO("MainIds:~w", [MainIds]),
    DoneIds = lists:foldl(
        fun(A, AccIn) -> 
                case data_pre_task:get(A) of
                    undefined ->
                        %% ?INFO("undefined data, [Rid:~w, MainIds:~w, Id:~w]", [Rid, MainIds, A]),
                        AccIn;
                    Pre -> Pre ++ AccIn 
                end
        end
        ,[]
        ,MainIds
    ),
    ?INFO("DoneIds:~w", [DoneIds]),
    AddIds = lists:foldl(
        fun({DoneId, AddList}, AccIn) -> 
                case lists:member(DoneId, DoneIds) of
                    true -> AccIn ++ AddList;
                    false -> AccIn
                end
        end
        ,[]
        ,fix_task_data()
    ),
    ?INFO("AddIds:~w", [AddIds]),
    AddIds1 = lists:filter(
        fun(X) -> 
                case lists:keymember(X, #mytask.tid, MyTasks) of
                    true ->
                        io:format("*~w", [X]),
                        false;
                    false -> true
                end
        end
        ,AddIds
    ),
    ?INFO("AddIds1:~w", [AddIds1]),
    ok.
%%.

%%' zip/unzip
zip(L) -> zip(L, []).
zip([I | T], Reply) ->
    #mytask{
        id           = X1
        ,tid         = X2
        ,type        = X3
        ,ctype       = X4
        ,ctl1        = X5
        ,ctl2        = X6
        ,process     = X7
        ,loop        = X8 
        ,loop_max    = X9
        ,rfactor     = X10
        ,reward      = X11
        ,select_item = X12
        ,reward_item = X13
    } = I,
    zip(T, [{X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13} | Reply]);
zip([], Reply) -> Reply.

unzip(L) -> unzip(L, []).
unzip([{X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13} | T], Reply) ->
    I = #mytask{
        id           = X1
        ,tid         = X2
        ,type        = X3
        ,ctype       = X4
        ,ctl1        = X5
        ,ctl2        = X6
        ,process     = X7
        ,loop        = X8 
        ,loop_max    = X9
        ,rfactor     = X10
        ,reward      = X11
        ,select_item = X12
        ,reward_item = X13
    },
    unzip(T, [I | Reply]);
unzip([I = #mytask{} | T], Reply) ->
    unzip(T, [I | Reply]);
unzip([X | T], Reply) ->
    ?INFO(" *** undefined Data:~w", [X]),
    unzip(T, Reply);
unzip([], Reply) -> Reply.
%%.
