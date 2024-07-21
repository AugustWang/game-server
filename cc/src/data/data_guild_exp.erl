-module(data_guild_exp).
-export([get/1]).
get(1) -> [{exp, 10000}, {exp_sum, 0}, {benefit_id, [1]}];
get(2) -> [{exp, 40000}, {exp_sum, 10000}, {benefit_id, [1,2]}];
get(3) -> [{exp, 100000}, {exp_sum, 50000}, {benefit_id, [1,2,3]}];
get(4) -> [{exp, 150000}, {exp_sum, 150000}, {benefit_id, [1,2,3,4]}];
get(5) -> [{exp, 0}, {exp_sum, 300000}, {benefit_id, [1,2,3,4,5]}];
get(_) -> undefined.