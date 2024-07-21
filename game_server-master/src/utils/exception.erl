-module(exception).
-export([notify/2, notify/3, notify/5, format/2]).

format(Type, Msg) ->
    io_lib:format("~p, ~p~n ~p", [Type, Msg, erlang:get_stacktrace()]).

notify(ErrorType, ErrorMsg) ->
    Stacktrace = format(ErrorType, ErrorMsg),
    do_notify(Stacktrace).

notify(ErrorType, Params, ErrorMsg) ->
    Stacktrace = io_lib:format("~p, ~p, ~p~n ~p", 
                               [Params, ErrorType, ErrorMsg, erlang:get_stacktrace()]),
    do_notify(Stacktrace).

notify(ErrorType, ErrorMsg, Controller, Action, Params) ->
    Stacktrace = io_lib:format("~p, ~p, ~p~n, ~p, ~p~n ~p", 
                               [Controller, Action, Params,
                                ErrorType, ErrorMsg, 
                                erlang:get_stacktrace()]),
    do_notify(Stacktrace).

do_notify(Stacktrace) ->
    error_logger:error_msg("Caught Exception: ~s~n", [Stacktrace]),
    case application:get_env(game_server, server_environment) of
        {ok, production} ->
            mail:exception_notify(Stacktrace);
        _ -> do_nothing
    end.
