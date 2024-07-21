-module(logger).

-export([info/1, info/2]).

info(Msg) ->
    case can_print() of
        true -> error_logger:info_msg(Msg);
        false -> ok
    end.

info(Formater, Args) ->
    case can_print() of
        true -> error_logger:info_msg(Formater, Args);
        false -> ok
    end.

can_print() ->
    case application:get_env(game_server, server_environment) of
        {ok, production} ->
            case application:get_env(game_server, enable_print_debug) of
                {ok, true} -> true;
                _ -> false
            end;
        _ -> true
    end.
