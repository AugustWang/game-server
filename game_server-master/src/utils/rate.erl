-module(rate).
-export([happen/1, choose/1, range/1]).
-on_load(init/0).

init() ->
    mtwist:seed(time_utils:now()),
    ok.

happen(Rate) ->
    RandomValue = mtwist:uniform(10000),
    compare(RandomValue, Rate).

%% Rates = [{RateA, ValueA}, {RateB, ValueB}, {default, DefaultValue}],
choose([{_Rate, Value}]) -> Value;
choose(Rates) ->
    Value = lists:foldl(fun({Rate, _}, Result) ->
                            Result + Rate
                        end, 0, Rates),
    RandomValue = mtwist:uniform(Value),
    choose(Rates, RandomValue, 0).

choose([{Rate, Value}|Rates], RandomValue, Offset) ->
    case RandomValue =< Rate + Offset of
        true -> Value;
        false -> choose(Rates, RandomValue, Offset + Rate)
    end.

%% Range = [{RateA, ValueA}, {RateB, ValueB}, {default, DefaultValue}],
%% select the happened Rate 
range(Range) ->
    select(mtwist:uniform(10000), Range).

select(RandomValue, [{Rate, Value}|_Range]) when RandomValue =< Rate -> Value;
select(_RandomValue, [{default, Value}]) -> Value;
select(RandomValue, [{_Rate, _Value}|Range]) ->
    select(RandomValue, Range).

compare(A, B) when A < B -> true;
compare(_A, _B) -> false.
