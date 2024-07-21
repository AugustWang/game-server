-module(data_lev).
-export([get/1]).
-include("lev.hrl").
get(1) -> #lev{exp_max=3000, exp_sum=0, dmg=0, hp_max=0};
get(2) -> #lev{exp_max=5000, exp_sum=3000, dmg=0, hp_max=4};
get(3) -> #lev{exp_max=8000, exp_sum=8000, dmg=0, hp_max=8};
get(4) -> #lev{exp_max=10000, exp_sum=16000, dmg=0, hp_max=12};
get(5) -> #lev{exp_max=20000, exp_sum=26000, dmg=0, hp_max=17};
get(6) -> #lev{exp_max=40000, exp_sum=46000, dmg=0, hp_max=22};
get(7) -> #lev{exp_max=60000, exp_sum=86000, dmg=0, hp_max=28};
get(8) -> #lev{exp_max=95000, exp_sum=146000, dmg=0, hp_max=34};
get(9) -> #lev{exp_max=110000, exp_sum=241000, dmg=0, hp_max=41};
get(10) -> #lev{exp_max=125000, exp_sum=351000, dmg=0, hp_max=48};
get(11) -> #lev{exp_max=144000, exp_sum=476000, dmg=0, hp_max=55};
get(12) -> #lev{exp_max=156000, exp_sum=620000, dmg=0, hp_max=62};
get(13) -> #lev{exp_max=168000, exp_sum=776000, dmg=0, hp_max=69};
get(14) -> #lev{exp_max=192000, exp_sum=944000, dmg=0, hp_max=76};
get(15) -> #lev{exp_max=216000, exp_sum=1136000, dmg=0, hp_max=84};
get(16) -> #lev{exp_max=240000, exp_sum=1352000, dmg=0, hp_max=92};
get(17) -> #lev{exp_max=264000, exp_sum=1592000, dmg=0, hp_max=100};
get(18) -> #lev{exp_max=288000, exp_sum=1856000, dmg=0, hp_max=108};
get(19) -> #lev{exp_max=312000, exp_sum=2144000, dmg=0, hp_max=117};
get(20) -> #lev{exp_max=348000, exp_sum=2456000, dmg=0, hp_max=126};
get(21) -> #lev{exp_max=384000, exp_sum=2804000, dmg=0, hp_max=136};
get(22) -> #lev{exp_max=420000, exp_sum=3188000, dmg=0, hp_max=146};
get(23) -> #lev{exp_max=456000, exp_sum=3608000, dmg=0, hp_max=157};
get(24) -> #lev{exp_max=492000, exp_sum=4064000, dmg=0, hp_max=169};
get(25) -> #lev{exp_max=528000, exp_sum=4556000, dmg=0, hp_max=181};
get(26) -> #lev{exp_max=576000, exp_sum=5084000, dmg=0, hp_max=193};
get(27) -> #lev{exp_max=624000, exp_sum=5660000, dmg=0, hp_max=206};
get(28) -> #lev{exp_max=672000, exp_sum=6284000, dmg=0, hp_max=219};
get(29) -> #lev{exp_max=720000, exp_sum=6956000, dmg=0, hp_max=233};
get(30) -> #lev{exp_max=768000, exp_sum=7676000, dmg=0, hp_max=248};
get(31) -> #lev{exp_max=850000, exp_sum=8444000, dmg=0, hp_max=263};
get(32) -> #lev{exp_max=940000, exp_sum=9294000, dmg=0, hp_max=279};
get(33) -> #lev{exp_max=1050000, exp_sum=10234000, dmg=0, hp_max=296};
get(34) -> #lev{exp_max=1200000, exp_sum=11284000, dmg=0, hp_max=314};
get(35) -> #lev{exp_max=1380000, exp_sum=12484000, dmg=0, hp_max=332};
get(36) -> #lev{exp_max=1600000, exp_sum=13864000, dmg=0, hp_max=351};
get(37) -> #lev{exp_max=1900000, exp_sum=15464000, dmg=0, hp_max=372};
get(38) -> #lev{exp_max=2250000, exp_sum=17364000, dmg=0, hp_max=394};
get(39) -> #lev{exp_max=2600000, exp_sum=19614000, dmg=0, hp_max=416};
get(40) -> #lev{exp_max=0, exp_sum=22214000, dmg=0, hp_max=440};
get(max) -> 40;get(Key) ->
    io:format("\n[~w:~w] Key(~w) undefined!\n", [?MODULE, ?LINE, Key]),
    undefined.