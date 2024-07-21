-module(data_skill).
-export([get/1]).
-include("skill.hrl").
get(1101) -> #skill{lev_min=7, time=6000, time_cd=24000, next_id=1102, exp_max=400, time_ef=4000, sort=1, ctl1=700, ctl2=0.15, ctl3=12};
get(1102) -> #skill{time=7000, time_cd=22000, next_id=1103, exp_max=1000, time_ef=4000, sort=1, ctl1=625, ctl2=0.19, ctl3=20};
get(1103) -> #skill{time=8000, time_cd=20000, next_id=1104, exp_max=2000, time_ef=5000, sort=1, ctl1=550, ctl2=0.18, ctl3=24};
get(1104) -> #skill{time=9000, time_cd=18000, next_id=1105, exp_max=3000, time_ef=5000, sort=1, ctl1=475, ctl2=0.21, ctl3=33};
get(1105) -> #skill{time=10000, time_cd=16000, next_id=0, exp_max=0, time_ef=6000, sort=1, ctl1=400, ctl2=0.2, ctl3=35};
get(1201) -> #skill{lev_min=5, time=6000, time_cd=32000, next_id=1202, exp_max=400, time_ef=2000, sort=2, ctl2=0.2, ctl3=16};
get(1202) -> #skill{time=7000, time_cd=28000, next_id=1203, exp_max=1000, time_ef=2000, sort=2, ctl2=0.25, ctl3=26};
get(1203) -> #skill{time=8000, time_cd=28000, next_id=1204, exp_max=2000, time_ef=3000, sort=2, ctl2=0.2, ctl3=30};
get(1204) -> #skill{time=9000, time_cd=24000, next_id=1205, exp_max=3000, time_ef=3000, sort=2, ctl2=0.23, ctl3=36};
get(1205) -> #skill{time=10000, time_cd=24000, next_id=0, exp_max=0, time_ef=4000, sort=2, ctl2=0.2, ctl3=38};
get(1301) -> #skill{lev_min=1, time=6000, time_cd=24000, next_id=1302, exp_max=400, time_ef=4000, sort=3, ctl2=0.8, ctl3=64};
get(1302) -> #skill{time=7000, time_cd=22000, next_id=1303, exp_max=1000, time_ef=4000, sort=3, ctl2=1, ctl3=125};
get(1303) -> #skill{time=8000, time_cd=20000, next_id=1304, exp_max=2000, time_ef=4000, sort=3, ctl2=1.2, ctl3=160};
get(1304) -> #skill{time=9000, time_cd=18000, next_id=1305, exp_max=3000, time_ef=4000, sort=3, ctl2=1.4, ctl3=225};
get(1305) -> #skill{time=10000, time_cd=16000, next_id=0, exp_max=0, time_ef=4000, sort=3, ctl2=1.6, ctl3=300};
get(1401) -> #skill{lev_min=3, time=2, time_cd=25000, next_id=1402, exp_max=1000, sort=4, ctl1=2};
get(1402) -> #skill{time=3, time_cd=21000, next_id=1403, exp_max=2000, sort=4, ctl1=3};
get(1403) -> #skill{time=4, time_cd=18000, next_id=0, exp_max=0, sort=4, ctl1=4};
get(1501) -> #skill{lev_min=9, time=6000, time_cd=35000, next_id=1502, exp_max=400, sort=5, ctl1=[4,6,8,10,11,17], ctl2=0.6, ctl3=48};
get(1502) -> #skill{time=7000, time_cd=32000, next_id=1503, exp_max=1000, sort=5, ctl1=[4,6,8,10,11,17], ctl2=0.7, ctl3=80};
get(1503) -> #skill{time=8000, time_cd=30000, next_id=1504, exp_max=2000, sort=5, ctl1=[4,6,8,10,11,17], ctl2=0.8, ctl3=120};
get(1504) -> #skill{time=9000, time_cd=28000, next_id=1505, exp_max=3000, sort=5, ctl1=[4,6,8,10,11,17], ctl2=0.9, ctl3=156};
get(1505) -> #skill{time=10000, time_cd=25000, next_id=0, exp_max=0, sort=5, ctl1=[4,6,8,10,11,17], ctl2=1, ctl3=222};
get(1601) -> #skill{lev_min=10, time=0, time_cd=32000, next_id=1602, exp_max=400, sort=6, ctl2=0.6, ctl3=48};
get(1602) -> #skill{time=0, time_cd=30000, next_id=1603, exp_max=1000, sort=6, ctl2=0.75, ctl3=80};
get(1603) -> #skill{time=0, time_cd=28000, next_id=1604, exp_max=2000, sort=6, ctl2=0.9, ctl3=120};
get(1604) -> #skill{time=0, time_cd=26000, next_id=1605, exp_max=3000, sort=6, ctl2=1.05, ctl3=156};
get(1605) -> #skill{time=0, time_cd=24000, next_id=0, exp_max=0, sort=6, ctl2=1.2, ctl3=222};
get(ids) -> [1101, 1102, 1103, 1104, 1105, 1201, 1202, 1203, 1204, 1205, 1301, 1302, 1303, 1304, 1305, 1401, 1402, 1403, 1501, 1502, 1503, 1504, 1505, 1601, 1602, 1603, 1604, 1605];
get(Key) ->
    io:format("\n[~w:~w] Key(~w) undefined!\n", [?MODULE, ?LINE, Key]),
    undefined.