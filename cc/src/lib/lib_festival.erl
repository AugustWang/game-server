%%----------------------------------------------------
%% festival
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_festival).
-export([
        at/1
        ,get/1
    ]
).

-include("common.hrl").
-define(FESTIVAL, christmas).

at(Key) ->
    case at_festival() of
        true ->
            case data_config_festival:get(Key) of
                [S, E] -> util:in_time(S, E);
                Else -> 
                    ?WARN("unexpected data: ~w", [Else]),
                    false
            end;
        false -> false
    end.

get(exp_mul) ->
    case at_festival() of
        true ->
            case at(exp_time) of
                true ->
                    Mul = data_config_festival:get(exp_mul),
                    case Mul > 1 of
                        true -> Mul;
                        false -> 1
                    end;
                false -> 1
            end;
        false -> 1
    end;
get(gold_mul) ->
    case at_festival() of
        true ->
            case at(gold_time) of
                true ->
                    Mul = data_config_festival:get(gold_mul),
                    case Mul > 1 of
                        true -> Mul;
                        false -> 1
                    end;
                false -> 1
            end;
        false -> 1
    end;
get(Key) ->
    data_config_festival:get(Key).

at_festival() ->
    case data_config_festival:get(festival) of
        ?FESTIVAL -> true;
        _ -> false
    end.

