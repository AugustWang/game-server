-module(t).

-compile(export_all).

add1([]) -> [];
add1([H|T]) -> [H+1|add1(T)].

add2(R) -> add(R,[]).

add3(R) -> lists:reverse(add(R,[])).

add4([]) -> [];
add4(L) -> lists:map(fun(X) -> X+1 end,L).

add([],R) -> R;
add([H|T],R) -> add(T,[H+1|R]).

t(N) ->
   L = lists:seq(1,N),
   {T1,_}=timer:tc(a,add1,[L]),
   {T2,_}=timer:tc(a,add2,[L]),
   {T3,_}=timer:tc(a,add3,[L]),
   {T4,_}=timer:tc(a,add4,[L]),
   io:format("~p ~p ~p ~p ~n",[T1,T2,T3,T4]).
