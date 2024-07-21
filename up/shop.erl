-module(shop).
-export([cost/1]).

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.
%%Buy = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}].
%%cost([{What,N}|T] -> What*N*cost(T).