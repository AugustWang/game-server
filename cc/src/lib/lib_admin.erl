-module(lib_admin).
-include("common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export([
    get_online_roleid/0
    ,get_online_roleid/1
]).

get_online_roleid()->
    MS = ets:fun2ms(
    fun(#online{id=Id})-> Id end
    ),
    L = ets:select(online, MS),
    Rt = [ integer_to_list(Rid) || Rid <- L ], % 这里不能直接返回，不然ei_rpc无法解包
    Rt.

get_online_roleid(list)->
    MS = ets:fun2ms(
    fun(#online{id=Id})-> Id end
    ),
    ets:select(online, MS).
