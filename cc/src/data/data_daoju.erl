-module(data_daoju).
-export([get/1]).
-include("daoju.hrl").
get(120001) -> #daoju{ctl2=80, ctl_time=999, use_type=1};
get(120002) -> #daoju{ctl1=2, ctl2=100, ctl_time=8, use_type=1, buff_id=1};
get(120003) -> #daoju{ctl1=3, ctl2=1, ctl_time=6, use_type=1, buff_id=2, gold=150};
get(120004) -> #daoju{ctl1=4, ctl2=1, ctl_time=999, use_type=1};
get(120005) -> #daoju{ctl1=5, ctl2=1, ctl_time=999, use_type=1};
get(120006) -> #daoju{ctl2=80, ctl_time=999, use_type=1};
get(120007) -> #daoju{ctl1=7, ctl2=1, ctl_time=5, use_type=1, buff_id=3};
get(120008) -> #daoju{ctl1=8, ctl2=300, use_type=2};
get(120009) -> #daoju{ctl1=12, ctl2=120013, use_type=2};
get(120010) -> #daoju{ctl1=9, ctl2=1, ctl_time=4, use_type=1, buff_id=4};
get(120011) -> #daoju{ctl1=10, ctl2=1, ctl_time=4, use_type=2, buff_id=5};
get(120012) -> #daoju{ctl1=11, ctl2=1, use_type=2};
get(120013) -> #daoju{ctl1=15, ctl2=400, ctl_time=5, use_type=1, buff_id=6};
get(120014) -> #daoju{ctl1=15, ctl2=1600, ctl_time=8, use_type=1, buff_id=15, gold=300};
get(120015) -> #daoju{ctl1=12, ctl2=120016, use_type=2};
get(120016) -> #daoju{ctl1=13, ctl2=1, ctl_time=999, use_type=1};
get(120017) -> #daoju{ctl1=14, ctl2=3, ctl_time=999, use_type=2, buff_id=7};
get(120018) -> #daoju{ctl1=16, ctl2=1, use_type=2, gold=200};
get(120019) -> #daoju{ctl1=17, ctl2=[4,6,8,10,11], use_type=2};
get(120020) -> #daoju{ctl1=12, ctl2=120021, use_type=2};
get(120021) -> #daoju{ctl1=18, ctl2=1, ctl_time=8, use_type=1, buff_id=8};
get(120022) -> #daoju{ctl1=19, ctl2=1, ctl_time=6, use_type=2, buff_id=9};
get(120023) -> #daoju{ctl1=12, ctl2=120024, use_type=2};
get(120024) -> #daoju{ctl1=20, ctl2=100, ctl_time=10, use_type=1, buff_id=16};
get(120025) -> #daoju{ctl1=21, ctl2=1600, ctl_time=8, use_type=1, buff_id=18, gold=300};
get(130001) -> #daoju{};
get(130002) -> #daoju{};
get(_) -> undefined.