-module(pack).
-export([p/2]).

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(10000, [X1]) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 10000:16, Data/binary>>};

p(10003, [X1,X2,X3,X4]) ->
    Data = <<X1:8,X2:8,X3:32,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 10003:16, Data/binary>>};

p(10004, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 10004:16, Data/binary>>};

p(10005, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 10005:16, Data/binary>>};

p(11001, [X1,X2,X3,X4,X5,X6]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8,X5:8,X6:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11001:16, Data/binary>>};

p(11003, [X1,X2,X3,X4,X5,X6,X7]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8,X5:8,X6:16,X7:16>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11003:16, Data/binary>>};

p(11005, [X1,X2,X3,X4]) ->
    Data = <<X1:32,X2:16,X3:8,X4:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11005:16, Data/binary>>};

p(11006, [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20]) ->
    LenX2 = byte_size(X2),
    LenX18 = byte_size(X18),
    LenArr20 = length(X20),
    F20 = fun
        ([X30,X31,X32]) ->  <<X30:32,X31:32,X32:8>>;
        ({X30,X31,X32}) ->  <<X30:32,X31:32,X32:8>> end,
    B20 = list_to_binary([F20(X) || X <- X20]),
    Data = <<X1:32,LenX2:16,X2/binary,X3:8,X4:8,X5:32,X6:32,X7:32,X8:32,X9:32,X10:32,X11:32,X12:32,X13:32,X14:32,X15:16,X16:16,X17:32,LenX18:16,X18/binary,X19:32,LenArr20:16, B20/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11006:16, Data/binary>>};

p(11008, [X1,X2,X3]) ->
    Data = <<X1:32,X2:8,X3:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11008:16, Data/binary>>};

p(11009, [X1,X2,X3,X4,X5]) ->
    Data = <<X1:8,X2:32,X3:32,X4:8,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11009:16, Data/binary>>};

p(11013, [X1,X2]) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11013:16, Data/binary>>};

p(11015, [X1,X2]) ->
    Data = <<X1:32,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11015:16, Data/binary>>};

p(11017, [X1,X2,X3,X4,X5,X6]) ->
    Data = <<X1:32,X2:32,X3:32,X4:32,X5:8,X6:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11017:16, Data/binary>>};

p(11018, [X1,X2,X3,X4]) ->
    Data = <<X1:32,X2:32,X3:32,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11018:16, Data/binary>>};

p(11019, [X1,X2,X3,X4]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11019:16, Data/binary>>};

p(11020, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11020:16, Data/binary>>};

p(11022, [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21]) ->
    LenX2 = byte_size(X2),
    LenX18 = byte_size(X18),
    LenArr20 = length(X20),
    F20 = fun
        ([X30,X31,X32]) ->  <<X30:32,X31:32,X32:8>>;
        ({X30,X31,X32}) ->  <<X30:32,X31:32,X32:8>> end,
    B20 = list_to_binary([F20(X) || X <- X20]),
    LenArr21 = length(X21),
    F21 = fun
        ([X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41]) ->  <<X31:32,X32:8,X33:16,X34:16,X35:16,X36:16,X37:16,X38:16,X39:16,X40:16,X41:32>>;
        ({X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41}) ->  <<X31:32,X32:8,X33:16,X34:16,X35:16,X36:16,X37:16,X38:16,X39:16,X40:16,X41:32>> end,
    B21 = list_to_binary([F21(X) || X <- X21]),
    Data = <<X1:32,LenX2:16,X2/binary,X3:8,X4:8,X5:32,X6:32,X7:32,X8:32,X9:32,X10:32,X11:32,X12:32,X13:32,X14:32,X15:16,X16:16,X17:32,LenX18:16,X18/binary,X19:32,LenArr20:16, B20/binary,LenArr21:16, B21/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11022:16, Data/binary>>};

p(11024, [X1,X2]) ->
    Data = <<X1:8,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11024:16, Data/binary>>};

p(11036, [X1,X2,X3]) ->
    Data = <<X1:32,X2:32,X3:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11036:16, Data/binary>>};

p(11030, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13]) ->  <<X11:8,X12:32,X13:8>>;
        ({X11,X12,X13}) ->  <<X11:8,X12:32,X13:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11030:16, Data/binary>>};

p(11037, [X1,X2]) ->
    Data = <<X1:8,X2:16>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11037:16, Data/binary>>};

p(11038, [X1]) ->
    Data = <<X1:16>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11038:16, Data/binary>>};

p(11040, [X1,X2]) ->
    Data = <<X1:8,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11040:16, Data/binary>>};

p(11041, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11041:16, Data/binary>>};

p(11044, [X1,X2,X3,X4]) ->
    Data = <<X1:8,X2:16,X3:8,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11044:16, Data/binary>>};

p(11045, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 11045:16, Data/binary>>};

p(12003, [X1,X2,X3,X4]) ->
    LenX2 = byte_size(X2),
    Data = <<X1:32,LenX2:16,X2/binary,X3:16,X4:16>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 12003:16, Data/binary>>};

p(12005, [X1,X2,X3,X4]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 12005:16, Data/binary>>};

p(12007, [X1,X2,X3,X4]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 12007:16, Data/binary>>};

p(12009, [X1,X2,X3,X4]) ->
    Data = <<X1:32,X2:32,X3:32,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 12009:16, Data/binary>>};

p(12011, [X1,X2,X3,X4,X5]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 12011:16, Data/binary>>};

p(13001, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15,X16,X17,X18]) -> 
    LenX13 = byte_size(X13), <<X11:32,X12:8,LenX13:16,X13/binary,X14:8,X15:8,X16:8,X17:8,X18:8>>;
        ({X11,X12,X13,X14,X15,X16,X17,X18}) -> 
    LenX13 = byte_size(X13), <<X11:32,X12:8,LenX13:16,X13/binary,X14:8,X15:8,X16:8,X17:8,X18:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 13001:16, Data/binary>>};

p(13003, [X1,X2,X3,X4,X5,X6,X7]) ->
    LenX4 = byte_size(X4),
    Data = <<X1:8,X2:32,X3:8,LenX4:16,X4/binary,X5:8,X6:8,X7:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 13003:16, Data/binary>>};

p(13004, [X1,X2,X3]) ->
    Data = <<X1:8,X2:32,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 13004:16, Data/binary>>};

p(13005, [X1,X2,X3]) ->
    LenX3 = byte_size(X3),
    Data = <<X1:32,X2:8,LenX3:16,X3/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 13005:16, Data/binary>>};

p(13007, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14]) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary,X13:8,X14:8>>;
        ({X11,X12,X13,X14}) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary,X13:8,X14:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 13007:16, Data/binary>>};

p(13009, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 13009:16, Data/binary>>};

p(14001, [X1,X2,X3]) ->
    LenArr2 = length(X2),
    F2 = fun
        ([X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25]) -> 
    LenX15 = byte_size(X15), <<X12:32,X13:8,X14:8,LenX15:16,X15/binary,X16:8,X17:8,X18:32,X19:32,X20:32,X21:32,X22:8,X23:8,X24:32,X25:32>>;
        ({X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25}) -> 
    LenX15 = byte_size(X15), <<X12:32,X13:8,X14:8,LenX15:16,X15/binary,X16:8,X17:8,X18:32,X19:32,X20:32,X21:32,X22:8,X23:8,X24:32,X25:32>> end,
    B2 = list_to_binary([F2(X) || X <- X2]),
    LenArr3 = length(X3),
    F3 = fun
        ([X13]) ->  <<X13:8>>;
        ({X13}) ->  <<X13:8>> end,
    B3 = list_to_binary([F3(X) || X <- X3]),
    Data = <<X1:32,LenArr2:16, B2/binary,LenArr3:16, B3/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14001:16, Data/binary>>};

p(14003, [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13]) ->
    LenX4 = byte_size(X4),
    Data = <<X1:32,X2:8,X3:8,LenX4:16,X4/binary,X5:8,X6:32,X7:32,X8:32,X9:32,X10:8,X11:8,X12:32,X13:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14003:16, Data/binary>>};

p(14004, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14004:16, Data/binary>>};

p(14005, [X1,X2]) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14005:16, Data/binary>>};

p(14008, [X1,X2]) ->
    Data = <<X1:8,X2:16>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14008:16, Data/binary>>};

p(14009, [X1,X2,X3,X4]) ->
    LenArr3 = length(X3),
    F3 = fun
        ([X13]) ->  <<X13:16>>;
        ({X13}) ->  <<X13:16>> end,
    B3 = list_to_binary([F3(X) || X <- X3]),
    LenArr4 = length(X4),
    F4 = fun
        ([X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30]) -> 
    LenX18 = byte_size(X18),
    LenArr30 = length(X30),
    F30 = fun
        ([X40]) ->  <<X40:32>>;
        ({X40}) ->  <<X40:32>> end,
    B30 = list_to_binary([F30(X) || X <- X30]), <<X14:32,X15:8,X16:16,X17:16,LenX18:16,X18/binary,X19:8,X20:32,X21:32,X22:32,X23:32,X24:32,X25:32,X26:8,X27:32,X28:32,X29:8,LenArr30:16, B30/binary>>;
        ({X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30}) -> 
    LenX18 = byte_size(X18),
    LenArr30 = length(X30),
    F30 = fun
        ([X40]) ->  <<X40:32>>;
        ({X40}) ->  <<X40:32>> end,
    B30 = list_to_binary([F30(X) || X <- X30]), <<X14:32,X15:8,X16:16,X17:16,LenX18:16,X18/binary,X19:8,X20:32,X21:32,X22:32,X23:32,X24:32,X25:32,X26:8,X27:32,X28:32,X29:8,LenArr30:16, B30/binary>> end,
    B4 = list_to_binary([F4(X) || X <- X4]),
    Data = <<X1:8,X2:8,LenArr3:16, B3/binary,LenArr4:16, B4/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14009:16, Data/binary>>};

p(14013, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14013:16, Data/binary>>};

p(14015, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14015:16, Data/binary>>};

p(14011, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14011:16, Data/binary>>};

p(14017, [X1,X2,X3,X4]) ->
    LenX3 = byte_size(X3),
    LenX4 = byte_size(X4),
    Data = <<X1:8,X2:8,LenX3:16,X3/binary,LenX4:16,X4/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14017:16, Data/binary>>};

p(14018, [X1,X2]) ->
    Data = <<X1:8,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14018:16, Data/binary>>};

p(14019, [X1]) ->
    Data = <<X1:16>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14019:16, Data/binary>>};

p(14021, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26]) ->  <<X11:32,X12:16,X13:16,X14:16,X15:16,X16:16,X17:16,X18:16,X19:16,X20:16,X21:16,X22:16,X23:16,X24:16,X25:16,X26:16>>;
        ({X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26}) ->  <<X11:32,X12:16,X13:16,X14:16,X15:16,X16:16,X17:16,X18:16,X19:16,X20:16,X21:16,X22:16,X23:16,X24:16,X25:16,X26:16>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14021:16, Data/binary>>};

p(14022, [X1,X2]) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14022:16, Data/binary>>};

p(14023, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14023:16, Data/binary>>};

p(14025, [X1,X2]) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14025:16, Data/binary>>};

p(14029, [X1,X2,X3,X4,X5]) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary,X2:8,X3:32,X4:8,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14029:16, Data/binary>>};

p(14031, [X1,X2,X3]) ->
    Data = <<X1:32,X2:8,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14031:16, Data/binary>>};

p(14037, [X1,X2,X3,X4,X5]) ->
    Data = <<X1:32,X2:16,X3:16,X4:16,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14037:16, Data/binary>>};

p(14012, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14012:16, Data/binary>>};

p(14033, [X1,X2,X3]) ->
    Data = <<X1:32,X2:8,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14033:16, Data/binary>>};

p(14039, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14039:16, Data/binary>>};

p(14050, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14050:16, Data/binary>>};

p(14053, [X1,X2]) ->
    Data = <<X1:32,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14053:16, Data/binary>>};

p(14060, [X1,X2,X3,X4]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14060:16, Data/binary>>};

p(14063, [X1,X2,X3]) ->
    LenArr3 = length(X3),
    F3 = fun
        ([X13,X14,X15]) ->  <<X13:32,X14:8,X15:8>>;
        ({X13,X14,X15}) ->  <<X13:32,X14:8,X15:8>> end,
    B3 = list_to_binary([F3(X) || X <- X3]),
    Data = <<X1:8,X2:8,LenArr3:16, B3/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14063:16, Data/binary>>};

p(14066, [X1,X2,X3,X4]) ->
    Data = <<X1:8,X2:32,X3:32,X4:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 14066:16, Data/binary>>};

p(15001, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 15001:16, Data/binary>>};

p(15002, [X1]) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 15002:16, Data/binary>>};

p(15005, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12]) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary>>;
        ({X11,X12}) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 15005:16, Data/binary>>};

p(15007, [X1,X2,X3,X4]) ->
    LenX2 = byte_size(X2),
    Data = <<X1:32,LenX2:16,X2/binary,X3:32,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 15007:16, Data/binary>>};

p(15008, [X1,X2,X3,X4]) ->
    LenX3 = byte_size(X3),
    Data = <<X1:8,X2:32,LenX3:16,X3/binary,X4:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 15008:16, Data/binary>>};

p(15011, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 15011:16, Data/binary>>};

p(15015, [X1]) ->
    LenX1 = byte_size(X1),
    Data = <<LenX1:16,X1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 15015:16, Data/binary>>};

p(16001, [X1,X2,X3,X4]) ->
    Data = <<X1:8,X2:8,X3:8,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 16001:16, Data/binary>>};

p(16003, [X1,X2,X3,X4,X5,X6]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8,X5:8,X6:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 16003:16, Data/binary>>};

p(16005, [X1,X2,X3,X4,X5]) ->
    Data = <<X1:32,X2:32,X3:32,X4:8,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 16005:16, Data/binary>>};

p(16007, [X1,X2,X3]) ->
    Data = <<X1:32,X2:8,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 16007:16, Data/binary>>};

p(16009, [X1,X2,X3]) ->
    Data = <<X1:32,X2:8,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 16009:16, Data/binary>>};

p(16010, [X1,X2,X3,X4,X5]) ->
    Data = <<X1:32,X2:8,X3:8,X4:8,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 16010:16, Data/binary>>};

p(16012, [X1,X2,X3,X4,X5]) ->
    Data = <<X1:32,X2:32,X3:8,X4:8,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 16012:16, Data/binary>>};

p(16015, [X1,X2,X3,X4,X5,X6,X7]) ->
    LenArr7 = length(X7),
    F7 = fun
        ([X17]) ->  <<X17:32>>;
        ({X17}) ->  <<X17:32>> end,
    B7 = list_to_binary([F7(X) || X <- X7]),
    Data = <<X1:8,X2:8,X3:8,X4:8,X5:8,X6:8,LenArr7:16, B7/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 16015:16, Data/binary>>};

p(17001, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12]) ->  <<X11:8,X12:32>>;
        ({X11,X12}) ->  <<X11:8,X12:32>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17001:16, Data/binary>>};

p(17002, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12]) ->  <<X11:8,X12:32>>;
        ({X11,X12}) ->  <<X11:8,X12:32>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17002:16, Data/binary>>};

p(17003, [X1,X2]) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17003:16, Data/binary>>};

p(17004, [X1,X2]) ->
    Data = <<X1:32,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17004:16, Data/binary>>};

p(17006, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13]) ->  <<X11:32,X12:32,X13:8>>;
        ({X11,X12,X13}) ->  <<X11:32,X12:32,X13:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17006:16, Data/binary>>};

p(17007, [X1,X2,X3,X4]) ->
    Data = <<X1:8,X2:32,X3:32,X4:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17007:16, Data/binary>>};

p(17008, [X1,X2]) ->
    Data = <<X1:8,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17008:16, Data/binary>>};

p(17009, [X1,X2,X3]) ->
    Data = <<X1:32,X2:8,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17009:16, Data/binary>>};

p(17021, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17021:16, Data/binary>>};

p(17101, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15,X16,X17,X18,X19]) -> 
    LenArr18 = length(X18),
    F18 = fun
        ([X28,X29,X30,X31,X32,X33,X34]) ->  <<X28:16,X29:16,X30:16,X31:16,X32:16,X33:16,X34:16>>;
        ({X28,X29,X30,X31,X32,X33,X34}) ->  <<X28:16,X29:16,X30:16,X31:16,X32:16,X33:16,X34:16>> end,
    B18 = list_to_binary([F18(X) || X <- X18]), <<X11:32,X12:32,X13:8,X14:8,X15:8,X16:16,X17:16,LenArr18:16, B18/binary,X19:32>>;
        ({X11,X12,X13,X14,X15,X16,X17,X18,X19}) -> 
    LenArr18 = length(X18),
    F18 = fun
        ([X28,X29,X30,X31,X32,X33,X34]) ->  <<X28:16,X29:16,X30:16,X31:16,X32:16,X33:16,X34:16>>;
        ({X28,X29,X30,X31,X32,X33,X34}) ->  <<X28:16,X29:16,X30:16,X31:16,X32:16,X33:16,X34:16>> end,
    B18 = list_to_binary([F18(X) || X <- X18]), <<X11:32,X12:32,X13:8,X14:8,X15:8,X16:16,X17:16,LenArr18:16, B18/binary,X19:32>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17101:16, Data/binary>>};

p(17107, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17107:16, Data/binary>>};

p(17108, [X1,X2]) ->
    Data = <<X1:8,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17108:16, Data/binary>>};

p(17109, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12]) ->  <<X11:32,X12:8>>;
        ({X11,X12}) ->  <<X11:32,X12:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17109:16, Data/binary>>};

p(17103, [X1,X2]) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17103:16, Data/binary>>};

p(17111, [X1,X2,X3,X4,X5,X6,X7,X8]) ->
    Data = <<X1:32,X2:32,X3:8,X4:8,X5:8,X6:16,X7:16,X8:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17111:16, Data/binary>>};

p(17113, [X1,X2,X3]) ->
    Data = <<X1:8,X2:32,X3:16>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17113:16, Data/binary>>};

p(17114, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17114:16, Data/binary>>};

p(17115, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17115:16, Data/binary>>};

p(17116, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17116:16, Data/binary>>};

p(17117, [X1,X2,X3,X4,X5,X6,X7,X8,X9]) ->
    LenArr8 = length(X8),
    F8 = fun
        ([X18,X19,X20,X21,X22,X23,X24]) ->  <<X18:16,X19:16,X20:16,X21:16,X22:16,X23:16,X24:16>>;
        ({X18,X19,X20,X21,X22,X23,X24}) ->  <<X18:16,X19:16,X20:16,X21:16,X22:16,X23:16,X24:16>> end,
    B8 = list_to_binary([F8(X) || X <- X8]),
    Data = <<X1:32,X2:32,X3:8,X4:8,X5:8,X6:16,X7:16,LenArr8:16, B8/binary,X9:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17117:16, Data/binary>>};

p(17119, [X1,X2,X3]) ->
    Data = <<X1:32,X2:32,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17119:16, Data/binary>>};

p(17121, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17121:16, Data/binary>>};

p(17125, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17125:16, Data/binary>>};

p(17130, [X1,X2,X3,X4,X5,X6]) ->
    Data = <<X1:8,X2:32,X3:32,X4:32,X5:32,X6:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17130:16, Data/binary>>};

p(17131, [X1,X2,X3,X4]) ->
    LenArr4 = length(X4),
    F4 = fun
        ([X14,X15,X16,X17]) -> 
    LenX15 = byte_size(X15), <<X14:32,LenX15:16,X15/binary,X16:32,X17:32>>;
        ({X14,X15,X16,X17}) -> 
    LenX15 = byte_size(X15), <<X14:32,LenX15:16,X15/binary,X16:32,X17:32>> end,
    B4 = list_to_binary([F4(X) || X <- X4]),
    Data = <<X1:32,X2:32,X3:32,LenArr4:16, B4/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17131:16, Data/binary>>};

p(17132, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14]) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary,X13:32,X14:32>>;
        ({X11,X12,X13,X14}) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary,X13:32,X14:32>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17132:16, Data/binary>>};

p(17140, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 17140:16, Data/binary>>};

p(18001, [X1,X2,X3,X4,X5]) ->
    LenX2 = byte_size(X2),
    LenX3 = byte_size(X3),
    Data = <<X1:32,LenX2:16,X2/binary,LenX3:16,X3/binary,X4:8,X5:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 18001:16, Data/binary>>};

p(18003, [X1,X2,X3,X4]) ->
    LenX2 = byte_size(X2),
    LenX3 = byte_size(X3),
    Data = <<X1:32,LenX2:16,X2/binary,LenX3:16,X3/binary,X4:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 18003:16, Data/binary>>};

p(18005, [X1,X2]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15]) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary,X13:16,X14:8,X15:8>>;
        ({X11,X12,X13,X14,X15}) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary,X13:16,X14:8,X15:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    LenArr2 = length(X2),
    F2 = fun
        ([X12,X13,X14,X15,X16]) -> 
    LenX13 = byte_size(X13), <<X12:32,LenX13:16,X13/binary,X14:16,X15:8,X16:8>>;
        ({X12,X13,X14,X15,X16}) -> 
    LenX13 = byte_size(X13), <<X12:32,LenX13:16,X13/binary,X14:16,X15:8,X16:8>> end,
    B2 = list_to_binary([F2(X) || X <- X2]),
    Data = <<LenArr1:16, B1/binary,LenArr2:16, B2/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 18005:16, Data/binary>>};

p(18007, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 18007:16, Data/binary>>};

p(18008, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 18008:16, Data/binary>>};

p(18009, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 18009:16, Data/binary>>};

p(19001, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15,X16,X17,X18,X19]) -> 
    LenArr17 = length(X17),
    F17 = fun
        ([X27,X28]) ->  <<X27:8,X28:32>>;
        ({X27,X28}) ->  <<X27:8,X28:32>> end,
    B17 = list_to_binary([F17(X) || X <- X17]),
    LenArr19 = length(X19),
    F19 = fun
        ([X29,X30,X31]) ->  <<X29:32,X30:8,X31:8>>;
        ({X29,X30,X31}) ->  <<X29:32,X30:8,X31:8>> end,
    B19 = list_to_binary([F19(X) || X <- X19]), <<X11:32,X12:32,X13:8,X14:16,X15:16,X16:16,LenArr17:16, B17/binary,X18:8,LenArr19:16, B19/binary>>;
        ({X11,X12,X13,X14,X15,X16,X17,X18,X19}) -> 
    LenArr17 = length(X17),
    F17 = fun
        ([X27,X28]) ->  <<X27:8,X28:32>>;
        ({X27,X28}) ->  <<X27:8,X28:32>> end,
    B17 = list_to_binary([F17(X) || X <- X17]),
    LenArr19 = length(X19),
    F19 = fun
        ([X29,X30,X31]) ->  <<X29:32,X30:8,X31:8>>;
        ({X29,X30,X31}) ->  <<X29:32,X30:8,X31:8>> end,
    B19 = list_to_binary([F19(X) || X <- X19]), <<X11:32,X12:32,X13:8,X14:16,X15:16,X16:16,LenArr17:16, B17/binary,X18:8,LenArr19:16, B19/binary>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 19001:16, Data/binary>>};

p(19003, [X1,X2]) ->
    Data = <<X1:32,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 19003:16, Data/binary>>};

p(20001, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 20001:16, Data/binary>>};

p(20003, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 20003:16, Data/binary>>};

p(20005, [X1,X2]) ->
    LenArr2 = length(X2),
    F2 = fun
        ([X12]) ->  <<X12:32>>;
        ({X12}) ->  <<X12:32>> end,
    B2 = list_to_binary([F2(X) || X <- X2]),
    Data = <<X1:32,LenArr2:16, B2/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 20005:16, Data/binary>>};

p(21001, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15,X16]) -> 
    LenX13 = byte_size(X13), <<X11:32,X12:32,LenX13:16,X13/binary,X14:8,X15:32,X16:8>>;
        ({X11,X12,X13,X14,X15,X16}) -> 
    LenX13 = byte_size(X13), <<X11:32,X12:32,LenX13:16,X13/binary,X14:8,X15:32,X16:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 21001:16, Data/binary>>};

p(21002, [X1,X2]) ->
    Data = <<X1:8,X2:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 21002:16, Data/binary>>};

p(21003, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 21003:16, Data/binary>>};

p(22005, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15,X16,X17]) -> 
    LenX12 = byte_size(X12),
    LenX14 = byte_size(X14), <<X11:32,LenX12:16,X12/binary,X13:32,LenX14:16,X14/binary,X15:8,X16:32,X17:8>>;
        ({X11,X12,X13,X14,X15,X16,X17}) -> 
    LenX12 = byte_size(X12),
    LenX14 = byte_size(X14), <<X11:32,LenX12:16,X12/binary,X13:32,LenX14:16,X14/binary,X15:8,X16:32,X17:8>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 22005:16, Data/binary>>};

p(22006, [X1,X2,X3]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15,X16,X17,X18]) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary,X13:8,X14:8,X15:32,X16:8,X17:8,X18:32>>;
        ({X11,X12,X13,X14,X15,X16,X17,X18}) -> 
    LenX12 = byte_size(X12), <<X11:32,LenX12:16,X12/binary,X13:8,X14:8,X15:32,X16:8,X17:8,X18:32>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary,X2:32,X3:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 22006:16, Data/binary>>};

p(22007, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 22007:16, Data/binary>>};

p(22008, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 22008:16, Data/binary>>};

p(22009, [X1,X2]) ->
    Data = <<X1:8,X2:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 22009:16, Data/binary>>};

p(22012, [X1,X2]) ->
    LenX2 = byte_size(X2),
    Data = <<X1:32,LenX2:16,X2/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 22012:16, Data/binary>>};

p(22014, [X1,X2]) ->
    LenArr2 = length(X2),
    F2 = fun
        ([X12,X13]) ->  <<X12:32,X13:32>>;
        ({X12,X13}) ->  <<X12:32,X13:32>> end,
    B2 = list_to_binary([F2(X) || X <- X2]),
    Data = <<X1:32,LenArr2:16, B2/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 22014:16, Data/binary>>};

p(22015, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 22015:16, Data/binary>>};

p(23005, [X1,X2]) ->
    LenArr2 = length(X2),
    F2 = fun
        ([X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24]) ->  <<X12:32,X13:32,(round(X14*100)):32,X15:32,(round(X16*100)):32,X17:32,(round(X18*100)):32,X19:32,(round(X20*100)):32,X21:32,(round(X22*100)):32,X23:32,(round(X24*100)):32>>;
        ({X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24}) ->  <<X12:32,X13:32,(round(X14*100)):32,X15:32,(round(X16*100)):32,X17:32,(round(X18*100)):32,X19:32,(round(X20*100)):32,X21:32,(round(X22*100)):32,X23:32,(round(X24*100)):32>> end,
    B2 = list_to_binary([F2(X) || X <- X2]),
    Data = <<X1:32,LenArr2:16, B2/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 23005:16, Data/binary>>};

p(23007, [X1]) ->
    Data = <<X1:8>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 23007:16, Data/binary>>};

p(23009, [X1]) ->
    LenArr1 = length(X1),
    F1 = fun
        ([X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26]) ->  <<X11:32,X12:32,(round(X13*100)):32,X14:32,(round(X15*100)):32,X16:32,(round(X17*100)):32,X18:32,(round(X19*100)):32,X20:32,(round(X21*100)):32,X22:32,(round(X23*100)):32,X24:8,X25:32,X26:32>>;
        ({X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26}) ->  <<X11:32,X12:32,(round(X13*100)):32,X14:32,(round(X15*100)):32,X16:32,(round(X17*100)):32,X18:32,(round(X19*100)):32,X20:32,(round(X21*100)):32,X22:32,(round(X23*100)):32,X24:8,X25:32,X26:32>> end,
    B1 = list_to_binary([F1(X) || X <- X1]),
    Data = <<LenArr1:16, B1/binary>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 23009:16, Data/binary>>};

p(23011, [X1]) ->
    Data = <<X1:32>>,
    Len = byte_size(Data),
    {ok, <<Len:16, 23011:16, Data/binary>>};

p(Cmd, Data) -> 
    io:format("undefined_pack_cmd:~w, data:~w", [Cmd, Data]),
    {error, undefined_pack_cmd}.
