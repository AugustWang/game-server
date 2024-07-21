-module(proc_watcher_test).
-export([test_proc/0]).

% {add, A, B}当A,B类型不合法时退出，hi自然退出；
test_proc () ->
    receive
        {add, A, B} ->
            io:format("A + B = ~p~n", [A + B]),
            test_proc();

        hi ->
            io:format("Hi! I'm ~p~n", [self()])
    end.