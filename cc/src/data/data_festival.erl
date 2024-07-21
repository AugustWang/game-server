-module(data_festival).
-export([get/1]).
get(110043) -> [{tid, 280002}, {num, 99}, {is_show, 1}];
get(240016) -> [{tid, 280002}, {num, 79}, {is_show, 1}];
get(320001) -> [{tid, 280002}, {num, 29}, {is_show, 1}];
get(170001) -> [{tid, 280002}, {num, 10}, {is_show, 0}];
get(_) -> undefined.