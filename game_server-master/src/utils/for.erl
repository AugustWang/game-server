-module(for).

-export([times/2, times/3, each/2, upto/3, upto/4]).

times(0, _) -> ok;
times(N, Fun) ->
    Fun(),
    times(N - 1, Fun).

times(_Fun, Acc, 0) -> Acc;
times(Fun, Acc, N) ->
    times(Fun, Fun(Acc), N - 1).

each([], _) -> ok;
each([Item|List], Fun) ->
    Fun(Item),
    each(List, Fun).

upto(To, To, Fun) -> Fun(To);
upto(From, To, Fun) ->
    Fun(From),
    upto(From + 1, To, Fun).

upto(To, To, Fun, Acc) -> Fun(To, Acc);
upto(From, To, Fun, Acc) ->
    NewAcc = Fun(From, Acc),
    upto(From + 1, To, Fun, NewAcc).
