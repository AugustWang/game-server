%%----------------------------------------------------
%% 地图相关
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_map).
-export([
        rand_pos/1
    ]
).

-include("common.hrl").

%% walk(Mid, {X, Y}) -> 
%%     ets:member(?ETS_MAP_POS, {Mid, X, Y}).

rand_pos(Mid) ->
    L = data_npc_pos:get(Mid),
    case util:rand_element(L) of
        undefined -> 
            ?WARNING("Map pos undefined! [MapId:~w, Data:~w]", [Mid, L]),
            {0, 0};
        Xy -> Xy
    end.

%% load_mask() ->
%%     ets:new(?ETS_MAP_POS, [named_table, public, set]),
%%     Data = data_map:get(),
%%     F = fun(L) ->
%%             {id, MapId} = lists:keyfind(id, 1, L),
%%             {mask, Mask} = lists:keyfind(mask, 1, L),
%%             load_mask(Mask, 0, 0, MapId)
%%     end,
%%     lists:foreach(F, Data).
%% 
%% load_mask([], _, _, _) ->
%%     ok;
%% load_mask([H | T], X, Y, MapId) ->
%%     case H of
%%         10 -> %% \n
%%             %% ?INFO("n -> X:~w, Y:~w", [X,Y]),
%%             load_mask(T, 0, Y+1, MapId);
%%         13 -> %% \r
%%             %% ?INFO("r -> X:~w, Y:~w", [X,Y]),
%%             load_mask(T, 0, Y, MapId);
%%         32 -> %% \space
%%             %% ?INFO("space -> X:~w, Y:~w", [X,Y]),
%%             load_mask(T, X, Y, MapId);
%%         48 -> %% 0
%%             %% ?INFO("0 -> X:~w, Y:~w", [X,Y]),
%%             load_mask(T, X+1, Y, MapId);
%%         49 -> %% 1
%%             %% ?INFO("1 -> X:~w, Y:~w", [X,Y]),
%%             ets:insert(?ETS_MAP_POS, {{MapId, X, Y}, MapId, 1}),
%%             load_mask(T, X+1, Y, MapId);
%%         50 -> %% 2
%%             %% ?INFO("2 -> X:~w, Y:~w", [X,Y]),
%%             ets:insert(?ETS_MAP_POS, {{MapId, X, Y}, MapId, 2}),
%%             load_mask(T, X+1, Y, MapId);
%%         51 -> %% 3
%%             %% ?INFO("3 -> X:~w, Y:~w", [X,Y]),
%%             ets:insert(?ETS_MAP_POS, {{MapId, X, Y}, MapId, 3}),
%%             load_mask(T, X+1, Y, MapId);
%%         52 -> %% 4
%%             %% ?INFO("4 -> X:~w, Y:~w", [X,Y]),
%%             ets:insert(?ETS_MAP_POS, {{MapId, X, Y}, MapId, 4}),
%%             load_mask(T, X+1, Y, MapId);
%%         124 -> %% |
%%             %% ?INFO("| -> X:~w, Y:~w", [X,Y]),
%%             load_mask(T, 0, Y+1, MapId);
%%         _Any ->
%%             ?ERR("Unknown Mask Elem: ~s", [_Any])
%%     end.
