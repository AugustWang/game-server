-module(robot_pack).
-export([p/3]).

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(10000, [X1], Index) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 10000:16, Data/binary>>};

p(10003, [X1], Index) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 10003:16, Data/binary>>};

p(10004, [X1,X2], Index) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 10004:16, Data/binary>>};

p(10005, [X1], Index) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 10005:16, Data/binary>>};

p(11001, [X1,X2,X3,X4,X5], Index) ->
    Data = <<X1:8,X2:8,X3:8,X4:8,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 11001:16, Data/binary>>};

p(11003, [X1,X2,X3,X4,X5,X6], Index) ->
    Data = <<X1:8,X2:8,X3:8,X4:8,X5:16,X6:16>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 11003:16, Data/binary>>};

p(11006, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 11006:16, Data/binary>>};

p(11009, [X1,X2,X3], Index) ->
    Data = <<X1:32,X2:8,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 11009:16, Data/binary>>};

p(11022, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 11022:16, Data/binary>>};

p(11034, [X1,X2,X3], Index) ->
    Data = <<X1:32,X2:32,X3:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 11034:16, Data/binary>>};

p(11035, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 11035:16, Data/binary>>};

p(11036, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 11036:16, Data/binary>>};

p(13001, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 13001:16, Data/binary>>};

p(13003, [X1,X2], Index) ->
    Data = <<X1:8,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 13003:16, Data/binary>>};

p(13004, [X1,X2,X3], Index) ->
    LenX3 = byte_size(X3),
    Data = <<X1:8,X2:32,LenX3:16,X3/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 13004:16, Data/binary>>};

p(13005, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 13005:16, Data/binary>>};

p(13009, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 13009:16, Data/binary>>};

p(14005, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14005:16, Data/binary>>};

p(14015, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14015:16, Data/binary>>};

p(14017, [X1,X2,X3], Index) ->
    LenX2 = byte_size(X2),
    LenX3 = byte_size(X3),
    Data = <<X1:8,LenX2:16,X2/binary,LenX3:16,X3/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14017:16, Data/binary>>};

p(14018, [X1,X2], Index) ->
    Data = <<X1:8,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14018:16, Data/binary>>};

p(14022, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14022:16, Data/binary>>};

p(14025, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14025:16, Data/binary>>};

p(14026, [X1,X2], Index) ->
    Data = <<X1:8,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14026:16, Data/binary>>};

p(14029, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14029:16, Data/binary>>};

p(14039, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14039:16, Data/binary>>};

p(14066, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 14066:16, Data/binary>>};

p(17003, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17003:16, Data/binary>>};

p(17007, [X1,X2], Index) ->
    Data = <<X1:32,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17007:16, Data/binary>>};

p(17008, [X1,X2], Index) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17008:16, Data/binary>>};

p(17009, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17009:16, Data/binary>>};

p(17010, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17010:16, Data/binary>>};

p(17021, [X1,X2], Index) ->
    Data = <<X1:32,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17021:16, Data/binary>>};

p(17103, [X1,X2], Index) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17103:16, Data/binary>>};

p(17105, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17105:16, Data/binary>>};

p(17107, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17107:16, Data/binary>>};

p(17108, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17108:16, Data/binary>>};

p(17109, [X1], Index) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12]) ->  <<X11:32,X12:8>>;
        ({X11,X12}) ->  <<X11:32,X12:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17109:16, Data/binary>>};

p(17113, [X1,X2,X3,X4,X5,X6], Index) ->
    Data = <<X1:32,X2:32,X3:32,X4:32,X5:32,X6:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17113:16, Data/binary>>};

p(17114, [X1,X2], Index) ->
    Data = <<X1:32,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17114:16, Data/binary>>};

p(17115, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17115:16, Data/binary>>};

p(17116, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17116:16, Data/binary>>};

p(17119, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17119:16, Data/binary>>};

p(17121, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17121:16, Data/binary>>};

p(17123, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17123:16, Data/binary>>};

p(17125, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 17125:16, Data/binary>>};

p(18001, [X1,X2], Index) ->
    LenX2 = byte_size(X2),
    Data = <<X1:8,LenX2:16,X2/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 18001:16, Data/binary>>};

p(18003, [X1,X2], Index) ->
    LenX2 = byte_size(X2),
    Data = <<X1:32,LenX2:16,X2/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 18003:16, Data/binary>>};

p(18007, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 18007:16, Data/binary>>};

p(18008, [X1], Index) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 18008:16, Data/binary>>};

p(18009, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 18009:16, Data/binary>>};

p(19003, [X1,X2], Index) ->
    Data = <<X1:32,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 19003:16, Data/binary>>};

p(20001, [X1], Index) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12]) ->  <<X11:32,X12:8>>;
        ({X11,X12}) ->  <<X11:32,X12:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 20001:16, Data/binary>>};

p(20003, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 20003:16, Data/binary>>};

p(21001, [X1,X2], Index) ->
    Data = <<X1:8,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 21001:16, Data/binary>>};

p(22006, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 22006:16, Data/binary>>};

p(22007, [X1], Index) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 22007:16, Data/binary>>};

p(22008, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 22008:16, Data/binary>>};

p(22009, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 22009:16, Data/binary>>};

p(22010, [X1,X2], Index) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 22010:16, Data/binary>>};

p(22011, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 22011:16, Data/binary>>};

p(22015, [X1,X2], Index) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 22015:16, Data/binary>>};

p(23007, [X1], Index) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 23007:16, Data/binary>>};

p(23011, [X1], Index) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, 23011:16, Data/binary>>};

p(Cmd, Data, _) -> 
    io:format("undefined_test_pack_cmd:~w, data:~w", [Cmd, Data]),
    {error, undefined_test_pack_cmd}.
