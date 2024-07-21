-module(try_test).
-compile(export_all).
%%-export([generate_exception/1,demol/0,catcher/1,demo2/0,sqrt/1,demo3/0]).

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT',a};
generate_exception(5) -> erlang:error(a).

demol() ->
	[catcher(I) || I <- [1,2,3,4,5]].

catcher(N) ->
	try generate_exception(N) of
		Val -> {N,normal,Val}
	catch
		throw:X -> {N,caught,thrown,X};
		exit:X -> {N,caught,exited,X};
		error:X -> {N,caught,error,X}
	end.

demo2() ->
	[{I,(catch generate_exception(I))} || I <- [1,2,3,4,5]].


sqrt(X) when X < 0 ->
	erlang:error({squareRootNegativeArgument,X});
sqrt(X) ->
	math:sqrt(X).

%%栈跟踪
demo3() -> 
	try [generate_exception(N) || N <- [1,2,3,4,5]]
	catch
		error:X ->
			{X,erlang:get_stacktrace()}
	end.