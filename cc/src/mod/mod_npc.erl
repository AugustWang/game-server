%%----------------------------------------------------
%% 场景相关
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_npc).
-export([handle/3]).

-include("common.hrl").

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.
