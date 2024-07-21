%%----------------------------------------------------
%% 地图相关
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_map).
-export([handle/3]).

-include("common.hrl").

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.
