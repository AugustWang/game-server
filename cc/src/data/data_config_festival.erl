-module(data_config_festival).
-export([get/1]).
get(exp_mul) -> 2;
get(gold_mul) -> 2;
get(pro_time) -> [{2012,12,24,0,0,0},{2013,1,8,23,59,59}];
get(trade_time) -> [{2012,12,24,0,0,0},{2013,1,8,23,59,59}];
get(exp_time) -> [{2012,12,24,0,0,0},{2012,12,27,23,59,59}];
get(gold_time) -> [{2012,12,24,0,0,0},{2012,12,27,23,59,59}];
get(festival) -> christmas;
get(Key) ->
    io:format("\n[~w:~w] Key(~w) undefined!\n", [?MODULE, ?LINE, Key]),
    undefined.