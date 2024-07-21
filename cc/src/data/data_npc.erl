-module(data_npc).
-export([get/1]).
-include("npc.hrl").
get(1) -> #npc{tid=1, hp=3, score=2, dmg_type=1};
get(2) -> #npc{tid=2, hp=6, score=3, dmg_type=1};
get(3) -> #npc{tid=3, hp=9, score=5, dmg_type=1};
get(4) -> #npc{tid=4, hp=3, score=2, dmg_type=1};
get(5) -> #npc{tid=5, hp=6, score=3, dmg_type=1};
get(6) -> #npc{tid=6, hp=9, score=5, dmg_type=1};
get(7) -> #npc{tid=7, hp=3, score=0, dmg_type=1};
get(8) -> #npc{tid=8, hp=1, score=2, dmg_type=1};
get(9) -> #npc{tid=9, hp=1, score=3, dmg_type=1};
get(10) -> #npc{tid=10, hp=1, score=5, dmg_type=2};
get(11) -> #npc{tid=11, hp=1, score=0, dmg_type=1};
get(12) -> #npc{tid=12, hp=1, score=0, dmg_type=1};
get(13) -> #npc{tid=13, hp=1, score=2, dmg_type=1};
get(14) -> #npc{tid=14, hp=1, score=3, dmg_type=1};
get(15) -> #npc{tid=15, hp=1, score=5, dmg_type=2};
get(16) -> #npc{tid=16, hp=1, score=0, dmg_type=1};
get(17) -> #npc{tid=17, hp=11, score=0, dmg_type=1};
get(18) -> #npc{tid=18, hp=1, score=2, dmg_type=1};
get(19) -> #npc{tid=19, hp=1, score=3, dmg_type=1};
get(20) -> #npc{tid=20, hp=1, score=5, dmg_type=2};
get(21) -> #npc{tid=21, hp=1, score=0, dmg_type=1};
get(22) -> #npc{tid=22, hp=1, score=0, dmg_type=1};
get(23) -> #npc{tid=23, hp=1, score=0, dmg_type=2};
get(24) -> #npc{tid=24, hp=1, score=0, dmg_type=2};
get(25) -> #npc{tid=25, hp=100, score=0, dmg_type=2};
get(26) -> #npc{tid=26, hp=3, score=2, dmg_type=1};
get(27) -> #npc{tid=27, hp=6, score=3, dmg_type=1};
get(28) -> #npc{tid=28, hp=3, score=2, dmg_type=1};
get(29) -> #npc{tid=29, hp=6, score=3, dmg_type=1};
get(30) -> #npc{tid=30, hp=3, score=2, dmg_type=1};
get(31) -> #npc{tid=31, hp=6, score=3, dmg_type=1};
get(32) -> #npc{tid=32, hp=3, score=2, dmg_type=1};
get(33) -> #npc{tid=33, hp=6, score=3, dmg_type=1};
get(34) -> #npc{tid=34, hp=3, score=2, dmg_type=1};
get(35) -> #npc{tid=35, hp=6, score=3, dmg_type=1};
get(36) -> #npc{tid=36, hp=3, score=2, dmg_type=1};
get(37) -> #npc{tid=37, hp=6, score=3, dmg_type=1};
get(38) -> #npc{tid=38, hp=9, score=10, dmg_type=1};
get(Key) ->
    io:format("\n[~w:~w] Key(~w) undefined!\n", [?MODULE, ?LINE, Key]),
    undefined.