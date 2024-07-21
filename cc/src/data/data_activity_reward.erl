-module(data_activity_reward).
-export([get/1]).
-include("activity_reward.hrl").
get({1, 1}) -> #activity_reward{condition=600, item=[320001], num=[1], gold=500, card=0};
get({1, 2}) -> #activity_reward{condition=1200, item=[], num=[], gold=1000, card=0};
get({1, 3}) -> #activity_reward{condition=1800, item=[], num=[], gold=2000, card=0};
get({1, 4}) -> #activity_reward{condition=3600, item=[], num=[], gold=4000, card=0};
get({2, 1}) -> #activity_reward{condition=1, item=[], num=[], gold=5000, card=0};
get({2, 2}) -> #activity_reward{condition=2, item=[220001], num=[3], gold=0, card=0};
get({2, 3}) -> #activity_reward{condition=3, item=[220001], num=[6], gold=0, card=0};
get({2, 4}) -> #activity_reward{condition=4, item=[170001], num=[3], gold=0, card=0};
get({2, 5}) -> #activity_reward{condition=5, item=[170001], num=[6], gold=0, card=0};
get({2, 6}) -> #activity_reward{condition=6, item=[180013], num=[2], gold=0, card=0};
get({2, 7}) -> #activity_reward{condition=7, item=[], num=[], gold=0, card=300};
get({3, 1}) -> #activity_reward{condition=1, item=[170001,320001], num=[2,1], gold=5000, card=0};
get({3, 2}) -> #activity_reward{condition=2, item=[170001], num=[4], gold=5000, card=0};
get({3, 3}) -> #activity_reward{condition=3, item=[170002,220001,320001], num=[2,2,1], gold=5000, card=0};
get({3, 4}) -> #activity_reward{condition=4, item=[170002,220001], num=[3,4], gold=5000, card=0};
get({3, 5}) -> #activity_reward{condition=5, item=[170002,220002,320001], num=[4,2,1], gold=5000, card=0};
get(_) ->
    undefined.