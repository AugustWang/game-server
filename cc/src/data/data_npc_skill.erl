-module(data_npc_skill).
-export([get/1]).
get({1, 1}) -> [{fb_attack, 30}, {cd, 2000}];
get({2, 1}) -> [{cd, 1500}];
get({3, 1}) -> [{fb_attack, 25}, {cd, 6000}];
get({4, 1}) -> [{fb_attack, 50}, {cd, 2000}];
get({5, 1}) -> [{fb_attack, 50}, {cd, 2000}];
get({6, 1}) -> [{fb_attack, 60}, {cd, 1300}, {time, 8}];
get({7, 1}) -> [{fb_attack, 1}];
get({8, 1}) -> [];
get({9, 1}) -> [{fb_attack, 60}, {cd, 1200}];
get({10, 1}) -> [{cd, 3000}];
get({11, 1}) -> [];
get({1, 2}) -> [{fb_attack, 50}, {cd, 1800}];
get({2, 2}) -> [{cd, 1500}];
get({3, 2}) -> [{fb_attack, 40}, {cd, 5000}];
get({4, 2}) -> [{fb_attack, 60}, {cd, 2000}];
get({5, 2}) -> [{fb_attack, 60}, {cd, 2000}];
get({6, 2}) -> [{fb_attack, 50}, {cd, 1000}, {time, 9}];
get({7, 2}) -> [{fb_attack, 1}];
get({8, 2}) -> [];
get({9, 2}) -> [{fb_attack, 80}, {cd, 1200}];
get({10, 2}) -> [{cd, 3000}];
get({11, 2}) -> [];
get({1, 3}) -> [{fb_attack, 70}, {cd, 1800}];
get({2, 3}) -> [{cd, 1500}];
get({3, 3}) -> [{fb_attack, 60}, {cd, 4000}];
get({4, 3}) -> [{fb_attack, 90}, {cd, 1800}];
get({5, 3}) -> [{fb_attack, 90}, {cd, 1800}];
get({6, 3}) -> [{fb_attack, 70}, {cd, 1000}, {time, 10}];
get({7, 3}) -> [{fb_attack, 1}];
get({8, 3}) -> [];
get({9, 3}) -> [{fb_attack, 100}, {cd, 1200}];
get({10, 3}) -> [{cd, 3000}];
get({11, 3}) -> [];
get(_) -> undefined.