-module(mail).

-export([send/4, exception_notify/1]).

exception_notify(ExceptionMsg) ->
    case application:get_env(game_server, exception_mail_receivers) of
        undefined -> ok;
        {ok, Receivers} ->
            send(os_utils:get_ip_string(), Receivers, "Server_Exception", ExceptionMsg)
    end.

send(From, Receivers, Subject, Body) ->
    To = lists:foldl(fun(Receiver, Acc) ->
                         if
                             Acc =:= "" ->
                                 Receiver;
                             true ->
                                 lists:concat([Acc, ",", Receiver])
                         end
                         
                     end, "", Receivers),
    Content = io_lib:format("Subject: ~s \r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", 
                            [Subject, From, To, Body]), 
    gen_smtp_client:send({From, Receivers, Content}, [{relay, "localhost"}, {port, 25}]).
