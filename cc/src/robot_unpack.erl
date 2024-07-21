-module(robot_unpack).
-export([p/2]).

p(_, <<>>) ->
    {ok, []};

p(10000, <<LenX1:16,X1:LenX1/binary>>) ->
    {ok, [X1]};

p(10003, <<X1:8,X2:8,X3:32,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(10004, <<X1:8>>) ->
    {ok, [X1]};

p(10005, <<X1:8>>) ->
    {ok, [X1]};

p(11001, <<X1:32,X2:8,X3:8,X4:8,X5:8,X6:8>>) ->
    {ok, [X1,X2,X3,X4,X5,X6]};

p(11003, <<X1:32,X2:8,X3:8,X4:8,X5:8,X6:16,X7:16>>) ->
    {ok, [X1,X2,X3,X4,X5,X6,X7]};

p(11005, <<X1:32,X2:16,X3:8,X4:32>>) ->
    {ok, [X1,X2,X3,X4]};

p(11006, <<X1:32,LenX2:16,X2:LenX2/binary,X3:8,X4:8,X5:32,X6:32,X7:32,X8:32,X9:32,X10:32,X11:32,X12:32,X13:32,X14:32,X15:16,X16:16,X17:32,LenX18:16,X18:LenX18/binary,X19:32,LenArr20:16, X20/binary>>) ->
    F20 = fun(_, {B20, Result20}) ->
            <<X30:32,X31:32,X32:8,RemBin/binary>> = B20, 
            {ok, {RemBin, [[X30,X31,X32]|Result20]}}
    end,
    {ok, {_, L20}} = util:for(1, LenArr20, F20, {X20, []}),
    {ok, [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,L20]};

p(11008, <<X1:32,X2:8,X3:32>>) ->
    {ok, [X1,X2,X3]};

p(11009, <<X1:8,X2:32,X3:32,X4:8,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(11013, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(11015, <<X1:32,X2:32>>) ->
    {ok, [X1,X2]};

p(11017, <<X1:32,X2:32,X3:32,X4:32,X5:8,X6:8>>) ->
    {ok, [X1,X2,X3,X4,X5,X6]};

p(11018, <<X1:32,X2:32,X3:32,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(11019, <<X1:32,X2:8,X3:8,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(11020, <<X1:8>>) ->
    {ok, [X1]};

p(11022, Bin) ->
    protocol:unpack2([int32,string,int8,int8,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int16,int16,int32,string,int32,[int32,int32,int8],[int32,int8,int16,int16,int16,int16,int16,int16,int16,int16,int32]], 11022, Bin);

p(11024, <<X1:8,X2:32>>) ->
    {ok, [X1,X2]};

p(11036, <<X1:32,X2:32,X3:32>>) ->
    {ok, [X1,X2,X3]};

p(11030, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:8,X12:32,X13:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(11037, <<X1:8,X2:16>>) ->
    {ok, [X1,X2]};

p(11038, <<X1:16>>) ->
    {ok, [X1]};

p(11040, <<X1:8,X2:8>>) ->
    {ok, [X1,X2]};

p(11041, <<X1:8>>) ->
    {ok, [X1]};

p(11044, <<X1:8,X2:16,X3:8,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(11045, <<X1:8>>) ->
    {ok, [X1]};

p(12003, <<X1:32,LenX2:16,X2:LenX2/binary,X3:16,X4:16>>) ->
    {ok, [X1,X2,X3,X4]};

p(12005, <<X1:32,X2:8,X3:8,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(12007, <<X1:32,X2:8,X3:8,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(12009, <<X1:32,X2:32,X3:32,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(12011, <<X1:32,X2:8,X3:8,X4:8,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(13001, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,X12:8,LenX13:16,X13:LenX13/binary,X14:8,X15:8,X16:8,X17:8,X18:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13,X14,X15,X16,X17,X18]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(13003, <<X1:8,X2:32,X3:8,LenX4:16,X4:LenX4/binary,X5:8,X6:8,X7:8>>) ->
    {ok, [X1,X2,X3,X4,X5,X6,X7]};

p(13004, <<X1:8,X2:32,X3:8>>) ->
    {ok, [X1,X2,X3]};

p(13005, <<X1:32,X2:8,LenX3:16,X3:LenX3/binary>>) ->
    {ok, [X1,X2,X3]};

p(13007, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,LenX12:16,X12:LenX12/binary,X13:8,X14:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13,X14]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(13009, <<X1:8>>) ->
    {ok, [X1]};

p(14001, Bin) ->
    protocol:unpack2([int32,[int32,int8,int8,string,int8,int8,int32,int32,int32,int32,int8,int8,int32,int32],[int8]], 14001, Bin);

p(14003, <<X1:32,X2:8,X3:8,LenX4:16,X4:LenX4/binary,X5:8,X6:32,X7:32,X8:32,X9:32,X10:8,X11:8,X12:32,X13:32>>) ->
    {ok, [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13]};

p(14004, <<X1:32>>) ->
    {ok, [X1]};

p(14005, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(14008, <<X1:8,X2:16>>) ->
    {ok, [X1,X2]};

p(14009, Bin) ->
    protocol:unpack2([int8,int8,[int16],[int32,int8,int16,int16,string,int8,int32,int32,int32,int32,int32,int32,int8,int32,int32,int8,[int32]]], 14009, Bin);

p(14013, <<X1:32>>) ->
    {ok, [X1]};

p(14015, <<X1:32>>) ->
    {ok, [X1]};

p(14011, <<X1:8>>) ->
    {ok, [X1]};

p(14017, <<X1:8,X2:8,LenX3:16,X3:LenX3/binary,LenX4:16,X4:LenX4/binary>>) ->
    {ok, [X1,X2,X3,X4]};

p(14018, <<X1:8,X2:8>>) ->
    {ok, [X1,X2]};

p(14019, <<X1:16>>) ->
    {ok, [X1]};

p(14021, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,X12:16,X13:16,X14:16,X15:16,X16:16,X17:16,X18:16,X19:16,X20:16,X21:16,X22:16,X23:16,X24:16,X25:16,X26:16,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(14022, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(14023, <<X1:32>>) ->
    {ok, [X1]};

p(14025, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(14029, <<LenX1:16,X1:LenX1/binary,X2:8,X3:32,X4:8,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(14031, <<X1:32,X2:8,X3:8>>) ->
    {ok, [X1,X2,X3]};

p(14037, <<X1:32,X2:16,X3:16,X4:16,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(14012, <<X1:32>>) ->
    {ok, [X1]};

p(14033, <<X1:32,X2:8,X3:8>>) ->
    {ok, [X1,X2,X3]};

p(14039, <<X1:8>>) ->
    {ok, [X1]};

p(14050, <<X1:32>>) ->
    {ok, [X1]};

p(14053, <<X1:32,X2:32>>) ->
    {ok, [X1,X2]};

p(14060, <<X1:32,X2:8,X3:8,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(14063, <<X1:8,X2:8,LenArr3:16, X3/binary>>) ->
    F3 = fun(_, {B3, Result3}) ->
            <<X13:32,X14:8,X15:8,RemBin/binary>> = B3, 
            {ok, {RemBin, [[X13,X14,X15]|Result3]}}
    end,
    {ok, {_, L3}} = util:for(1, LenArr3, F3, {X3, []}),
    {ok, [X1,X2,L3]};

p(14066, <<X1:8,X2:32,X3:32,X4:32>>) ->
    {ok, [X1,X2,X3,X4]};

p(15001, <<X1:32>>) ->
    {ok, [X1]};

p(15002, <<LenX1:16,X1:LenX1/binary>>) ->
    {ok, [X1]};

p(15005, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,LenX12:16,X12:LenX12/binary,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(15007, <<X1:32,LenX2:16,X2:LenX2/binary,X3:32,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(15008, <<X1:8,X2:32,LenX3:16,X3:LenX3/binary,X4:32>>) ->
    {ok, [X1,X2,X3,X4]};

p(15011, <<X1:32>>) ->
    {ok, [X1]};

p(15015, <<LenX1:16,X1:LenX1/binary>>) ->
    {ok, [X1]};

p(16001, <<X1:8,X2:8,X3:8,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(16003, <<X1:32,X2:8,X3:8,X4:8,X5:8,X6:8>>) ->
    {ok, [X1,X2,X3,X4,X5,X6]};

p(16005, <<X1:32,X2:32,X3:32,X4:8,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(16007, <<X1:32,X2:8,X3:8>>) ->
    {ok, [X1,X2,X3]};

p(16009, <<X1:32,X2:8,X3:8>>) ->
    {ok, [X1,X2,X3]};

p(16010, <<X1:32,X2:8,X3:8,X4:8,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(16012, <<X1:32,X2:32,X3:8,X4:8,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(16015, <<X1:8,X2:8,X3:8,X4:8,X5:8,X6:8,LenArr7:16, X7/binary>>) ->
    F7 = fun(_, {B7, Result7}) ->
            <<X17:32,RemBin/binary>> = B7, 
            {ok, {RemBin, [[X17]|Result7]}}
    end,
    {ok, {_, L7}} = util:for(1, LenArr7, F7, {X7, []}),
    {ok, [X1,X2,X3,X4,X5,X6,L7]};

p(17001, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:8,X12:32,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(17002, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:8,X12:32,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(17003, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(17004, <<X1:32,X2:32>>) ->
    {ok, [X1,X2]};

p(17006, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,X12:32,X13:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(17007, <<X1:8,X2:32,X3:32,X4:32>>) ->
    {ok, [X1,X2,X3,X4]};

p(17008, <<X1:8,X2:32>>) ->
    {ok, [X1,X2]};

p(17009, <<X1:32,X2:8,X3:8>>) ->
    {ok, [X1,X2,X3]};

p(17021, <<X1:8>>) ->
    {ok, [X1]};

p(17101, Bin) ->
    protocol:unpack2([[int32,int32,int8,int8,int8,int16,int16,[int16,int16,int16,int16,int16,int16,int16],int32]], 17101, Bin);

p(17107, <<X1:8>>) ->
    {ok, [X1]};

p(17108, <<X1:8,X2:8>>) ->
    {ok, [X1,X2]};

p(17109, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,X12:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(17103, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(17111, <<X1:32,X2:32,X3:8,X4:8,X5:8,X6:16,X7:16,X8:32>>) ->
    {ok, [X1,X2,X3,X4,X5,X6,X7,X8]};

p(17113, <<X1:8,X2:32,X3:16>>) ->
    {ok, [X1,X2,X3]};

p(17114, <<X1:8>>) ->
    {ok, [X1]};

p(17115, <<X1:8>>) ->
    {ok, [X1]};

p(17116, <<X1:8>>) ->
    {ok, [X1]};

p(17117, Bin) ->
    protocol:unpack2([int32,int32,int8,int8,int8,int16,int16,[int16,int16,int16,int16,int16,int16,int16],int32], 17117, Bin);

p(17119, <<X1:32,X2:32,X3:8>>) ->
    {ok, [X1,X2,X3]};

p(17121, <<X1:8>>) ->
    {ok, [X1]};

p(17125, <<X1:8>>) ->
    {ok, [X1]};

p(17130, <<X1:8,X2:32,X3:32,X4:32,X5:32,X6:32>>) ->
    {ok, [X1,X2,X3,X4,X5,X6]};

p(17131, <<X1:32,X2:32,X3:32,LenArr4:16, X4/binary>>) ->
    F4 = fun(_, {B4, Result4}) ->
            <<X14:32,LenX15:16,X15:LenX15/binary,X16:32,X17:32,RemBin/binary>> = B4, 
            {ok, {RemBin, [[X14,X15,X16,X17]|Result4]}}
    end,
    {ok, {_, L4}} = util:for(1, LenArr4, F4, {X4, []}),
    {ok, [X1,X2,X3,L4]};

p(17132, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,LenX12:16,X12:LenX12/binary,X13:32,X14:32,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13,X14]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(17140, <<X1:8>>) ->
    {ok, [X1]};

p(18001, <<X1:32,LenX2:16,X2:LenX2/binary,LenX3:16,X3:LenX3/binary,X4:8,X5:8>>) ->
    {ok, [X1,X2,X3,X4,X5]};

p(18003, <<X1:32,LenX2:16,X2:LenX2/binary,LenX3:16,X3:LenX3/binary,X4:8>>) ->
    {ok, [X1,X2,X3,X4]};

p(18005, Bin) ->
    protocol:unpack2([[int32,string,int16,int8,int8],[int32,string,int16,int8,int8]], 18005, Bin);

p(18007, <<X1:8>>) ->
    {ok, [X1]};

p(18008, <<X1:8>>) ->
    {ok, [X1]};

p(18009, <<X1:8>>) ->
    {ok, [X1]};

p(19001, Bin) ->
    protocol:unpack2([[int32,int32,int8,int16,int16,int16,[int8,int32],int8,[int32,int8,int8]]], 19001, Bin);

p(19003, <<X1:32,X2:8>>) ->
    {ok, [X1,X2]};

p(20001, <<X1:8>>) ->
    {ok, [X1]};

p(20003, <<X1:8>>) ->
    {ok, [X1]};

p(20005, <<X1:32,LenArr2:16, X2/binary>>) ->
    F2 = fun(_, {B2, Result2}) ->
            <<X12:32,RemBin/binary>> = B2, 
            {ok, {RemBin, [[X12]|Result2]}}
    end,
    {ok, {_, L2}} = util:for(1, LenArr2, F2, {X2, []}),
    {ok, [X1,L2]};

p(21001, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,X12:32,LenX13:16,X13:LenX13/binary,X14:8,X15:32,X16:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13,X14,X15,X16]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(21002, <<X1:8,X2:8>>) ->
    {ok, [X1,X2]};

p(21003, <<X1:8>>) ->
    {ok, [X1]};

p(22005, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,LenX12:16,X12:LenX12/binary,X13:32,LenX14:16,X14:LenX14/binary,X15:8,X16:32,X17:8,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13,X14,X15,X16,X17]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(22006, Bin) ->
    protocol:unpack2([[int32,string,int8,int8,int32,int8,int8,int32],int32,int8], 22006, Bin);

p(22007, <<X1:8>>) ->
    {ok, [X1]};

p(22008, <<X1:8>>) ->
    {ok, [X1]};

p(22009, <<X1:8,X2:32>>) ->
    {ok, [X1,X2]};

p(22012, <<X1:32,LenX2:16,X2:LenX2/binary>>) ->
    {ok, [X1,X2]};

p(22014, <<X1:32,LenArr2:16, X2/binary>>) ->
    F2 = fun(_, {B2, Result2}) ->
            <<X12:32,X13:32,RemBin/binary>> = B2, 
            {ok, {RemBin, [[X12,X13]|Result2]}}
    end,
    {ok, {_, L2}} = util:for(1, LenArr2, F2, {X2, []}),
    {ok, [X1,L2]};

p(22015, <<X1:8>>) ->
    {ok, [X1]};

p(23005, <<X1:32,LenArr2:16, X2/binary>>) ->
    F2 = fun(_, {B2, Result2}) ->
            <<X12:32,X13:32,X14:32,X15:32,X16:32,X17:32,X18:32,X19:32,X20:32,X21:32,X22:32,X23:32,X24:32,RemBin/binary>> = B2, 
            {ok, {RemBin, [[X12,X13,X14/100,X15,X16/100,X17,X18/100,X19,X20/100,X21,X22/100,X23,X24/100]|Result2]}}
    end,
    {ok, {_, L2}} = util:for(1, LenArr2, F2, {X2, []}),
    {ok, [X1,L2]};

p(23007, <<X1:8>>) ->
    {ok, [X1]};

p(23009, <<LenArr1:16, X1/binary>>) ->
    F1 = fun(_, {B1, Result1}) ->
            <<X11:32,X12:32,X13:32,X14:32,X15:32,X16:32,X17:32,X18:32,X19:32,X20:32,X21:32,X22:32,X23:32,X24:8,X25:32,X26:32,RemBin/binary>> = B1, 
            {ok, {RemBin, [[X11,X12,X13/100,X14,X15/100,X16,X17/100,X18,X19/100,X20,X21/100,X22,X23/100,X24,X25,X26]|Result1]}}
    end,
    {ok, {_, L1}} = util:for(1, LenArr1, F1, {X1, []}),
    {ok, [L1]};

p(23011, <<X1:32>>) ->
    {ok, [X1]};

p(Cmd, Data) -> 
    io:format("undefined_robot_unpack_cmd:~w, data:~w", [Cmd, Data]),
    {error, undefined_unpack_cmd}.
