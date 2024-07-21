-module(fp_test).
-export([for/2,fib/1]).
-mode(compile).
-compile(export_all).

for([],[]) -> done;
for(M,N) when M=<N ->
	io:format("~p~n",[M]),
	for(M+1,N);
for(M,N) when M>N -> over.

fib(0) -> 1;
fib(1) -> 1;
fib(N) when N>=2 -> 
	fib(N-1) + fib(N-2).
	
main(_) ->
	statistics(wall_clock),
	spawn_processes(1),
	{_,T} = statistics(wall_clock),
	io:format("Total time: ~b ms\n",[T]).

spawn_processes(N) ->
	try spawn(fun() -> 
		receive
			_ -> ok
		end
		end) of 
		_ -> spawn_processes(N+1)
	catch
		_:_ ->io:format("Total processes:~b\n",[N])
	end.
	
reverse(List) ->
	reverse(List,[]).

reverse([H|T],Re_list) ->
	reverse(T,[H|Re_list]);

reverse([],Re_list) ->
	Re_list.
	
consult(File) ->
	case file:open(File,read) of
		{ok,S} ->
			Val = consult1(S),
			file:close(S),
			{ok,Val};
		{error,Why} ->
			{error,Why}
	end.

consult1(S) ->
	case io:read(S,'') of
		{ok,Term} -> [Term|consult1(S)];
		%%consult(S);
		eof -> [];
		Error -> Error
	end.