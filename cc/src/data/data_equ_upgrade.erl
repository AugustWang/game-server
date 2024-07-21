-module(data_equ_upgrade).
-export([get/1]).
-include("equ_upgrade.hrl").
get(200001) -> #equ_upgrade{need_item=[210008,210005,210006,210007], need_num=[4,12,12,12], to=110038};
get(200002) -> #equ_upgrade{need_item=[210009,210005,210006,210007], need_num=[10,30,30,30], to=110039};
get(200003) -> #equ_upgrade{need_item=[210010,210005,210006,210007], need_num=[20,60,60,60], to=110040};
get(200004) -> #equ_upgrade{need_item=[210011,210005,210006,210007], need_num=[30,90,90,90], to=110042};
get(Key) ->
    io:format("\n[~w:~w] Key(~w) undefined!\n", [?MODULE, ?LINE, Key]),
    undefined.