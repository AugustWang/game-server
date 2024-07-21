-module(data_buff).
-export([get/1]).
-include("buff.hrl").
get(1) -> #buff{ieffect=1};
get(2) -> #buff{};
get(3) -> #buff{remove=[7]};
get(4) -> #buff{disabled=[5]};
get(5) -> #buff{remove=[4,6,8]};
get(6) -> #buff{disabled=[5]};
get(7) -> #buff{remove=[3], ieffect=1};
get(8) -> #buff{disabled=[5]};
get(9) -> #buff{};
get(15) -> #buff{};
get(16) -> #buff{};
get(18) -> #buff{};
get(10) -> #buff{disabled=[5], remove=[11]};
get(11) -> #buff{disabled=[5], remove=[10]};
get(13) -> #buff{disabled=[3]};
get(14) -> #buff{};
get(17) -> #buff{};
get(Key) ->
    io:format("\n[~w:~w] Key(~w) undefined!\n", [?MODULE, ?LINE, Key]),
    undefined.