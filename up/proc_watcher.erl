-module(proc_watcher).
-export([start/0, spawn_and_watch/4, proc_watcher_loop/0]).

% 原理是通过process_flag(trap_exit, true)让父进程可以接收到子进程
% 退出时的{'EXIT', Pid, Reason}消息，然后通过Pid找到子进程的入口函数，
% 重新spawn以实现重启子进程的目的

start () ->
    register(proc_watcher, spawn(?MODULE, proc_watcher_loop, [])).

spawn_and_watch (ProcName, Module, Function, Args) ->
    proc_watcher ! {spawn, self(), {ProcName, Module, Function, Args}},
    receive
        {spawn_ok, Pid} -> {ok, Pid}
    end.

proc_watcher_loop () ->
    process_flag(trap_exit, true),
    receive

        {spawn, From, {ProcName, Module, Function, Args}} ->
            case catch spawn_link(Module, Function, Args) of

                {'EXIT', Reason} ->
                    From ! {spanw_error, Reason},
                    proc_watcher_loop();

                Pid ->
                    put(Pid, {ProcName, Module, Function, Args}),
                    register(ProcName, Pid),
                    From ! {spawn_ok, Pid},
                    proc_watcher_loop()

            end;

        {'EXIT', From, _} ->
            case erase(From) of

                undefined ->
                    proc_watcher_loop();

                {ProcName, Module, Function, Args} ->
                    Pid = spawn_link(Module, Function, Args),
                    register(ProcName, Pid),
                    put(Pid, {ProcName, Module, Function, Args}),
                    proc_watcher_loop()

            end
    end.