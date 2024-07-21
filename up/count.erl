-module(count).
-export([rang/1, sum/1, map/2, shi/1, huashi/1, factorial/1]).

rang([A|T]) -> A*rang(T);
rang([]) -> 1.

sum([B|S]) -> B+sum(S);
sum([])  -> 0.

map(_,[]) -> [];
map(F,[H|T]) -> [F(H)|map(F,T)].

%TempCouvert({c,C}) -> {f,32 + C*9/5};
%		   ({f,F}) -> {c,(F-32)*5/9}
%			end.

shi([C]) -> 32+C*9/5;  %%摄氏度转华氏度
shi([])  -> 0.

huashi([F]) -> (F-32)*5/9;  %%华氏度转摄氏度
huashi([])  -> 0.

%%Mult([Times]) -> Triple([X]) -> X*Times;
%%for(Max,Max,F) -> [F(Max)];
%%for(I,Max,F) -> [F(I)|for(I+1,Max,F)].

%factorial(0) -> 1;
%factorial(N) ->
%	N * factorial(N-1).
	
factorial(N) when N > 0 ->
	N * factorial(N - 1);
factorial(0) -> 1.