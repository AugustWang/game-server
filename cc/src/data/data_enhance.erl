-module(data_enhance).
-export([get/1]).
-include("enhance.hrl").
get({1, 1}) -> #enhance{success_rate=10, gold=500, a_name=[6,7], a_val=[4,10]};
get({1, 2}) -> #enhance{success_rate=20, gold=1000, a_name=[6,7], a_val=[6,10]};
get({1, 3}) -> #enhance{success_rate=40, gold=1500, a_name=[6,7], a_val=[8,10]};
get({1, 4}) -> #enhance{success_rate=80, gold=2000, a_name=[6,7], a_val=[9,10]};
get({1, 5}) -> #enhance{success_rate=160, gold=2500, a_name=[6,7], a_val=[10,10]};
get({1, 6}) -> #enhance{success_rate=300, gold=3000, a_name=[6,7], a_val=[11,15]};
get({1, 7}) -> #enhance{success_rate=500, gold=3500, a_name=[6,7], a_val=[12,15]};
get({1, 8}) -> #enhance{success_rate=750, gold=4000, a_name=[6,7], a_val=[13,15]};
get({1, 9}) -> #enhance{success_rate=1000, gold=4500, a_name=[6,7], a_val=[14,15]};
get({1, 10}) -> #enhance{success_rate=1300, gold=5000, a_name=[6,7], a_val=[15,15]};
get({1, 11}) -> #enhance{success_rate=1800, gold=5500, a_name=[6,7], a_val=[17,20]};
get({1, 12}) -> #enhance{success_rate=2600, gold=6000, a_name=[6,7], a_val=[19,20]};
get({1, 13}) -> #enhance{success_rate=3600, gold=6500, a_name=[6,7], a_val=[21,20]};
get({1, 14}) -> #enhance{success_rate=5000, gold=7000, a_name=[6,7], a_val=[23,20]};
get({1, 15}) -> #enhance{success_rate=7000, gold=7500, a_name=[6,7], a_val=[25,20]};
get({1, 16}) -> #enhance{success_rate=9000, gold=8000, a_name=[6,7], a_val=[27,30]};
get({1, 17}) -> #enhance{success_rate=11000, gold=8500, a_name=[6,7], a_val=[30,30]};
get({1, 18}) -> #enhance{success_rate=14000, gold=9000, a_name=[6,7], a_val=[34,30]};
get({1, 19}) -> #enhance{success_rate=17000, gold=9500, a_name=[6,7], a_val=[38,30]};
get({1, 20}) -> #enhance{success_rate=20000, gold=10000, a_name=[6,7], a_val=[43,30]};
get({11, 1}) -> #enhance{success_rate=10, gold=500, a_name=[2], a_val=[14]};
get({11, 2}) -> #enhance{success_rate=20, gold=1000, a_name=[2], a_val=[16]};
get({11, 3}) -> #enhance{success_rate=40, gold=1500, a_name=[2], a_val=[19]};
get({11, 4}) -> #enhance{success_rate=80, gold=2000, a_name=[2], a_val=[21]};
get({11, 5}) -> #enhance{success_rate=160, gold=2500, a_name=[2], a_val=[24]};
get({11, 6}) -> #enhance{success_rate=300, gold=3000, a_name=[2], a_val=[26]};
get({11, 7}) -> #enhance{success_rate=500, gold=3500, a_name=[2], a_val=[29]};
get({11, 8}) -> #enhance{success_rate=750, gold=4000, a_name=[2], a_val=[31]};
get({11, 9}) -> #enhance{success_rate=1000, gold=4500, a_name=[2], a_val=[34]};
get({11, 10}) -> #enhance{success_rate=1300, gold=5000, a_name=[2], a_val=[36]};
get({11, 11}) -> #enhance{success_rate=1800, gold=5500, a_name=[2], a_val=[39]};
get({11, 12}) -> #enhance{success_rate=2600, gold=6000, a_name=[2], a_val=[41]};
get({11, 13}) -> #enhance{success_rate=3600, gold=6500, a_name=[2], a_val=[44]};
get({11, 14}) -> #enhance{success_rate=5000, gold=7000, a_name=[2], a_val=[46]};
get({11, 15}) -> #enhance{success_rate=7000, gold=7500, a_name=[2], a_val=[49]};
get({11, 16}) -> #enhance{success_rate=9000, gold=8000, a_name=[2], a_val=[51]};
get({11, 17}) -> #enhance{success_rate=11000, gold=8500, a_name=[2], a_val=[54]};
get({11, 18}) -> #enhance{success_rate=14000, gold=9000, a_name=[2], a_val=[56]};
get({11, 19}) -> #enhance{success_rate=17000, gold=9500, a_name=[2], a_val=[59]};
get({11, 20}) -> #enhance{success_rate=20000, gold=10000, a_name=[2], a_val=[61]};
get(_) -> undefined.