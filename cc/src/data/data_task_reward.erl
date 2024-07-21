-module(data_task_reward).
-export([get/1]).
-include("task_reward.hrl").
get({1, 1}) -> #task_reward{tid=230001, option=0, num=1, elev=0};
get({3, 1}) -> #task_reward{tid=150001, option=0, num=1, elev=0};
get({5, 1}) -> #task_reward{tid=220001, option=0, num=2, elev=0};
get({49, 1}) -> #task_reward{tid=220001, option=0, num=4, elev=0};
get({6, 1}) -> #task_reward{tid=140001, option=0, num=1, elev=0};
get({7, 1}) -> #task_reward{tid=170001, option=0, num=2, elev=0};
get({118, 1}) -> #task_reward{tid=160001, option=0, num=1, elev=0};
get({47, 1}) -> #task_reward{tid=220001, option=0, num=4, elev=0};
get({10, 1}) -> #task_reward{tid=170001, option=0, num=2, elev=0};
get({11, 1}) -> #task_reward{tid=240001, option=0, num=1, elev=0};
get({12, 1}) -> #task_reward{tid=110016, option=0, num=1, elev=0};
get({13, 1}) -> #task_reward{tid=230015, option=0, num=1, elev=0};
get({15, 1}) -> #task_reward{tid=270001, option=0, num=1, elev=0};
get({9, 1}) -> #task_reward{tid=220001, option=0, num=3, elev=0};
get({16, 1}) -> #task_reward{tid=240002, option=0, num=1, elev=0};
get({17, 1}) -> #task_reward{tid=140002, option=0, num=1, elev=0};
get({20, 1}) -> #task_reward{tid=150002, option=0, num=1, elev=0};
get({50, 1}) -> #task_reward{tid=160002, option=0, num=1, elev=0};
get({18, 1}) -> #task_reward{tid=230002, option=0, num=1, elev=0};
get({51, 1}) -> #task_reward{tid=110002, option=0, num=1, elev=0};
get({48, 1}) -> #task_reward{tid=220001, option=0, num=4, elev=0};
get({52, 1}) -> #task_reward{tid=140003, option=0, num=1, elev=0};
get({53, 1}) -> #task_reward{tid=150003, option=0, num=1, elev=0};
get({54, 1}) -> #task_reward{tid=160003, option=0, num=1, elev=0};
get({55, 1}) -> #task_reward{tid=110007, option=0, num=1, elev=0};
get({56, 1}) -> #task_reward{tid=230006, option=0, num=1, elev=0};
get({31, 1}) -> #task_reward{tid=180011, option=0, num=2, elev=0};
get({138, 1}) -> #task_reward{tid=180011, option=0, num=1, elev=0};
get({140, 1}) -> #task_reward{tid=180011, option=0, num=1, elev=0};
get({142, 1}) -> #task_reward{tid=180011, option=0, num=1, elev=0};
get({144, 1}) -> #task_reward{tid=180011, option=0, num=1, elev=0};
get({146, 1}) -> #task_reward{tid=180011, option=0, num=1, elev=0};
get({148, 1}) -> #task_reward{tid=180011, option=0, num=1, elev=0};
get({139, 1}) -> #task_reward{tid=320001, option=0, num=1, elev=0};
get({141, 1}) -> #task_reward{tid=320001, option=0, num=1, elev=0};
get({143, 1}) -> #task_reward{tid=320001, option=0, num=1, elev=0};
get({145, 1}) -> #task_reward{tid=320001, option=0, num=1, elev=0};
get({147, 1}) -> #task_reward{tid=320001, option=0, num=1, elev=0};
get({149, 1}) -> #task_reward{tid=320001, option=0, num=1, elev=0};
get(_) -> undefined.