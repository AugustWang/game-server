%%----------------------------------------------------
%% log
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_log).
-export([
        online_num/0
    ]
).

-include("common.hrl").

online_num() ->
    try
        %% {ok, [Start, End]} = application:get_env(cc, robot_accounts),
        %% Size = ets:select_count(online, [{#online{id='$1', _ = '_'}, [{'>','$1', 200}], [true]}]),
        %% ?INFO("online:~w", [Size]),
        Robots = gen_server:call(robot, count_robots),
        Size = ets:info(online, size) - Robots,
        io:format("[online:~w(~w)]", [Size, Robots]),
        db:execute("INSERT INTO `log_online_num`(`ctime`, `num`) VALUES (~s, ~s)", [util:unixtime(), Size]),
        ok
    catch
        T:X -> ?ERR("online_num[~w:~w]", [T, X])
    end.
