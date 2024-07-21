%%---------------------------------------------------
%% fix
%%
%% @author rolong@vip.qq.com
%%----------------------------------------------------

-module(lib_fix).
-compile(export_all).

-include("common.hrl").

tasks() ->
    spawn(?MODULE, tasks, [1]).

tasks(Nth) ->
    PageSize = 100,
    S = (Nth - 1) * PageSize,
    Sql = lists:concat(["select id from role where lev >= 8 "
            "order by id asc limit ", S, ", ", PageSize, ";"]),
    Ids = db:get_all(Sql),
    Len = length(Ids),
    %% ?INFO("~nSql:~s Ids:~w:~w~n", [Sql, Len, Ids]),
    do_fix(Ids),
    case Len > 0 of
        true -> 
            util:sleep(1000),
            tasks(Nth + 1);
        false -> 
            ?INFO("Fix Done!"),
            ok
    end.

do_fix([[Id] | Ids]) ->
    Pid = lib_authen:get_role_pid(role_id, Id),
    Pid ! {handle_event, 6199, []},
    util:sleep(100),
    do_fix(Ids);
do_fix([]) -> ok.
