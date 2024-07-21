-module(data_guild_benefit).
-export([get/1]).
get(1) -> [{gold, 3}, {exp, 3}, {price, 50}];
get(2) -> [{gold, 10}, {price, 150}];
get(3) -> [{exp, 15}, {price, 150}];
get(4) -> [{gold, 20}, {price, 400}];
get(5) -> [{exp, 30}, {price, 400}];
get(_) -> undefined.