-module(data_npc_carbon).
-export([get/1]).
get({101, 1}) -> [{cid, 1}, {type, 1}, {hp, 200}, {range, 3}];
get({102, 1}) -> [{cid, 1}, {type, 2}, {hp, 200}];
get({103, 1}) -> [{cid, 1}, {type, 3}, {hp, 5000}];
get({104, 1}) -> [{cid, 1}, {type, 4}, {hp, 9999}];
get({105, 1}) -> [{cid, 1}, {type, 5}, {hp, 9999}];
get({106, 1}) -> [{cid, 1}, {type, 6}, {hp, 9999}];
get({101, 2}) -> [{cid, 2}, {type, 1}, {hp, 400}, {range, 3}];
get({102, 2}) -> [{cid, 2}, {type, 2}, {hp, 350}];
get({103, 2}) -> [{cid, 2}, {type, 3}, {hp, 15000}];
get({104, 2}) -> [{cid, 2}, {type, 4}, {hp, 9999}];
get({105, 2}) -> [{cid, 2}, {type, 5}, {hp, 9999}];
get({106, 2}) -> [{cid, 2}, {type, 6}, {hp, 9999}];
get({101, 3}) -> [{cid, 3}, {type, 1}, {hp, 650}, {range, 3}];
get({102, 3}) -> [{cid, 3}, {type, 2}, {hp, 550}];
get({103, 3}) -> [{cid, 3}, {type, 3}, {hp, 25000}];
get({104, 3}) -> [{cid, 3}, {type, 4}, {hp, 9999}];
get({105, 3}) -> [{cid, 3}, {type, 5}, {hp, 9999}];
get({106, 3}) -> [{cid, 3}, {type, 6}, {hp, 9999}];
get(_) -> undefined.