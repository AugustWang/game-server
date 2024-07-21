-module(unpack).
-export([p/2]).

p(_, <<>>) ->
    {ok, []};

p(10000, <<LenX1:16,X1:LenX1/binary>>) ->
    {ok, [X1]};

p(10003, <<LenX1:16,X1:LenX1/binary>>) ->
    {ok, [X1]};

p(10004, <<LenX1:16,X1:LenX1/binary,X2:8>>) ->
    {ok, [X1,X2]};

p(10005, <<LenX1:16,X1:LenX1/binary>>) ->
    {ok, [X1]};

p(11001, <<X1:8,X2:8,X3:8,X4:8,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(11003, <<X1:8,X2:8,X3:8,X4:8,X5:16,X6:16>>) ->
    {ok, [X1,X2,X3,X4,X5,X6]};

p(11006, <<X1:32>>) ->
    {ok, [X1]};

p(11009, <<X1:32,X2:8,X3:8>>) ->
    {ok, [X1,X2,X3]};

p(11022, <<X1:32>>) ->
    {ok, [X1]};

p(11034, <<X1:32,X2:32,X3:32>>) ->
    {ok, [X1,X2,X3]};

p(11035, <<X1:8>>) ->
    {ok, [X1]};

p(11036, <<X1:8>>) ->
    {ok, [X1]};

p(13001, <<X1:8>>) ->
    {ok, [X1]};

p(13003, <<X1:8,X2:32>>) ->
    {ok, [X1,X2]};

p(13004, <<X1:8,X2:32,LenX3:16,X3:LenX3/binary>>) ->
    {ok, [X1,X2,X3]};

p(13005, <<X1:8>>) ->
    {ok, [X1]};

p(13009, <<X1:32>>) ->
    {ok, [X1]};

p(14005, <<X1:8>>) ->
    {ok, [X1]};

p(14015, <<X1:32>>) ->
    {ok, [X1]};

p(14017, <<X1:8,LenX2:16,X2:LenX2/binary,LenX3:16,X3:LenX3/binary>>) ->
    {ok, [X1,X2,X3]};

p(14018, <<X1:8,X2:8>>) ->
    {ok, [X1,X2]};

p(14022, <<X1:8>>) ->
    {ok, [X1]};

p(14025, <<X1:8>>) ->
    {ok, [X1]};

p(14026, <<X1:8,X2:32>>) ->
    {ok, [X1,X2]};

p(14029, <<X1:32>>) ->
    {ok, [X1]};

p(14039, <<X1:8>>) ->
    {ok, [X1]};

p(14066, <<X1:8>>) ->
    {ok, [X1]};

p(17003, <<X1:32>>) ->
    {ok, [X1]};

p(17007, <<X1:32,X2:32>>) ->
    {ok, [X1,X2]};

p(17008, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(17009, <<X1:32>>) ->
    {ok, [X1]};

p(17010, <<X1:32>>) ->
    {ok, [X1]};

p(17021, <<X1:32,X2:32>>) ->
    {ok, [X1,X2]};

p(17103, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(17105, <<X1:8>>) ->
    {ok, [X1]};

p(17107, <<X1:32>>) ->
    {ok, [X1]};

p(17108, <<X1:8>>) ->
    {ok, [X1]};

p(17109, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,X12:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(17113, <<X1:32,X2:32,X3:32,X4:32,X5:32,X6:32>>) ->
    {ok, [X1,X2,X3,X4,X5,X6]};

p(17114, <<X1:32,X2:32>>) ->
    {ok, [X1,X2]};

p(17115, <<X1:32>>) ->
    {ok, [X1]};

p(17116, <<X1:32>>) ->
    {ok, [X1]};

p(17119, <<X1:32>>) ->
    {ok, [X1]};

p(17121, <<X1:32>>) ->
    {ok, [X1]};

p(17123, <<X1:32>>) ->
    {ok, [X1]};

p(17125, <<X1:32>>) ->
    {ok, [X1]};

p(18001, <<X1:8,LenX2:16,X2:LenX2/binary>>) ->
    {ok, [X1,X2]};

p(18003, <<X1:32,LenX2:16,X2:LenX2/binary>>) ->
    {ok, [X1,X2]};

p(18007, <<X1:32>>) ->
    {ok, [X1]};

p(18008, <<LenX1:16,X1:LenX1/binary>>) ->
    {ok, [X1]};

p(18009, <<X1:32>>) ->
    {ok, [X1]};

p(19003, <<X1:32,X2:32>>) ->
    {ok, [X1,X2]};

p(20001, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,X12:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(20003, <<X1:32>>) ->
    {ok, [X1]};

p(21001, <<X1:8,X2:8>>) ->
    {ok, [X1,X2]};

p(22006, <<X1:32>>) ->
    {ok, [X1]};

p(22007, <<LenX1:16,X1:LenX1/binary>>) ->
    {ok, [X1]};

p(22008, <<X1:32>>) ->
    {ok, [X1]};

p(22009, <<X1:32>>) ->
    {ok, [X1]};

p(22010, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(22011, <<X1:32>>) ->
    {ok, [X1]};

p(22015, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(23007, <<X1:32>>) ->
    {ok, [X1]};

p(23011, <<X1:8>>) ->
    {ok, [X1]};

p(Cmd, Data) -> 
    io:format("undefined_unpack_cmd:~w, data:~w", [Cmd, Data]),
    {error, undefined_unpack_cmd}.
