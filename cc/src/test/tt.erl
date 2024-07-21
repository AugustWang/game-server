%%----------------------------------------------------
%% Test
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(tt).
-compile(export_all).

%%'读代码与读进程字典比较
%% t:t1(1000000).
%% "Read File" [total: 93(94)ms avg: 0.093(0.094)us]
%% "Read Dict" [total: 3838(3837)ms avg: 3.838(3.837)us]
%% ========================================================================
%% Read File            =     93.00ms [  100.00%]     94.00ms [  100.00%]
%% Read Dict            =   3838.00ms [ 4126.88%]   3837.00ms [ 4081.91%]
%% ========================================================================
t1(N)->
    F1 = fun(_I) ->
            data_lev:get(20)
    end,
    F2 = fun(_I)->
            srv_cache:get({lev, 20})
    end,
    tester:run(N, [
            {"Read File", F1}
            ,{"Read Dict", F2}
        ]
    ).

%%.

%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
