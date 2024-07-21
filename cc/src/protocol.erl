%%----------------------------------------------------
%% 服务端与客户端通信协议
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(protocol).
-export([pack/2, unpack/2, unpack2/3, test_pack/3, test_unpack/2]).
-include("common.hrl").

%%' 压包
pack(Cmd, []) -> {ok, <<0:16, Cmd:16>>};
pack(Cmd, List) ->
    T = erlang:now(),
    ?DEBUG("send [CMD:~w, DATA:~w]", [Cmd, List]),
    case lists:member(Cmd, [1]) of
        true -> ?INFO("CMD ~w:~w", [Cmd, List]);
        false -> skip
    end,
    Rule = protocol:pack(Cmd),
    Data = pack_data(Rule, List, []),
    Len = byte_size(Data),
    Bin = <<Len:16, Cmd:16, Data/binary>>,
    DT = timer:now_diff(erlang:now(), T) / 1000,
    case DT > 1 of
        true ->
            ?INFO("CMD:~w, PackTime:~w", [Cmd, DT]),
            ok;
        false -> skip
    end,
    {ok, Bin}.
%%. ==================================================

%%' 解包
unpack(Cmd, Bin) ->
    T = erlang:now(),
    Rule = protocol:unpack(Cmd),
    {Result, <<>>} = unpack(Rule, Bin, []),
    DT = timer:now_diff(erlang:now(), T) / 1000,
    case DT > 1 of
        true ->
            ?INFO("Cmd:~w, UnPackTime:~w", [Cmd, DT]),
            ok;
        false -> skip
    end,
    {ok, Result}.

unpack2(Rule, Cmd, Bin) ->
    T = erlang:now(),
    {Result, <<>>} = unpack(Rule, Bin, []),
    DT = timer:now_diff(erlang:now(), T) / 1000,
    case DT > 1 of
        true ->
            ?INFO("Cmd:~w, UnPackTime:~w", [Cmd, DT]),
            ok;
        false -> skip
    end,
    {ok, Result}.
%%. ==================================================

%%' pack_data(Format, Result) -> NewResult
pack_data([int8 | Format], [Val | Data], Result) ->
    NewResult = [<<Val:8>> | Result],
    pack_data(Format, Data, NewResult);

pack_data([int16 | Format], [Val | Data], Result) ->
    NewResult = [<<Val:16>> | Result],
    pack_data(Format, Data, NewResult);

pack_data([int32 | Format], [Val | Data], Result) ->
    NewResult = [<<Val:32>> | Result],
    pack_data(Format, Data, NewResult);

pack_data([string | Format], [Val | Data], Result) ->
    Size = byte_size(Val), 
    NewResult = [<<Size:16, Val/binary>> | Result],
    pack_data(Format, Data, NewResult);

pack_data([FormatH | Format], [DataH | Data], Result) ->
    {ArrayResult, Len} = pack_array(FormatH, DataH, [], 0),
    Result1 = [<<Len:16>> | Result],
    Result2 = [ArrayResult | Result1],
    pack_data(Format, Data, Result2);

pack_data([], [], Result) ->
    Result1 = lists:reverse(Result),
    list_to_binary(Result1).

pack_array(Format, [Val1 | Data], Result, Index) ->
    Val = case is_tuple(Val1) of
        true -> tuple_to_list(Val1);
        false -> Val1
    end,
    NewVal = pack_data(Format, Val, []),
    pack_array(Format, Data, [NewVal | Result], Index + 1);
pack_array(_, [], Result, Index) ->
    {Result, Index}.
%%.

%%' unpack(Format, Binary, Result) -> NewResult
unpack([int8 | Format], Bin, Result) ->
    <<Data:8, Bin1/binary>> = Bin,
    unpack(Format, Bin1, [Data | Result]);
unpack([int16 | Format], Bin, Result) ->
    <<Data:16, Bin1/binary>> = Bin,
    unpack(Format, Bin1, [Data | Result]);
unpack([int32 | Format], Bin, Result) ->
    <<Data:32, Bin1/binary>> = Bin,
    unpack(Format, Bin1, [Data | Result]);
unpack([string | Format], Bin, Result) ->
    <<Len:16, Data:Len/binary, Bin1/binary>> = Bin,
    unpack(Format, Bin1, [Data | Result]);
unpack([FormatH | Format], Bin, Result) ->
    <<Len:16, Bin1/binary>> = Bin,
    {Result1, Bin2} = unpack_array(FormatH, Bin1, Len, []),
    Result2 = lists:reverse(Result1),
    unpack(Format, Bin2, [Result2 | Result]);
unpack([], Bin, Result) -> 
    {lists:reverse(Result), Bin}.

unpack_array(_, Bin, 0, Result) -> {Result, Bin};
unpack_array(Format, Bin, Len, Result) ->
    {Result1, Bin1} = unpack(Format, Bin, []),
    unpack_array(Format, Bin1, Len - 1, [Result1 | Result]).
%%.

%%' TEST
test_pack(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};
test_pack(Cmd, List, Index) ->
    T = erlang:now(),
    Rule = protocol:unpack(Cmd),
    Data = pack_data(Rule, List, []),
    Len = byte_size(Data),
    Bin = <<Len:16, Index:8, Cmd:16, Data/binary>>,
    DT = timer:now_diff(erlang:now(), T) / 1000,
    case DT > 1 of
        true ->
            ?INFO("test_pack CMD:~w, PackTime:~w", [Cmd, DT]),
            ok;
        false -> skip
    end,
    {ok, Bin}.

test_unpack(0, Bin) ->
    ?INFO("test_unpack undefined data:~w, Cmd:~w~n", [Bin, 0]),
    {ok, []};
test_unpack(Cmd, Bin) ->
    T = erlang:now(),
    Rule = protocol:pack(Cmd),
    case unpack(Rule, Bin, []) of
        {Result, <<>>} ->
            DT = timer:now_diff(erlang:now(), T) / 1000,
            case DT > 1 of
                true ->
                    ?INFO("test_unpack Cmd:~w, UnPackTime:~w", 
                        [Cmd, DT]),
                    ok;
                false -> skip
            end,
            {ok, Result};
        Other ->
            ?INFO("test_unpack undefined data:~w, Cmd:~w", [Other, Cmd]),
            {ok, []}
    end.
%%.

%%% vim: set foldmethod=marker foldmarker=%%',%%.:
