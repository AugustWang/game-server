-module(dicttest).
-export([test_put/1,test_get/1]).

test_put(N) ->
	Start = erlang:now(),
	dotimes(N,fun(I) -> put(I,hello) end),
	Stop = erlang:now(),
	N / time_diff(Start,Stop).
	
test_get(N) ->
	Start = erlang:now(),
	dotimes(N,fun(I) -> get(I) end),
	Stop = erlang:now(),
	N / time_diff(Start,Stop).
	
dotimes(0,_) -> done;
dotimes(N,F) ->
	F(N),
	dotimes(N-1,F).
	
time_diff({A1,A2,A3}, {B1,B2,B3}) ->
	(B1-A1)*1000000 + (B2-A2) + (B3-A3)/1000000.0.
	
%%27> c(dicttest).
%%{ok,dicttest}
%%28> dicttest:test_put(1000000).
%%4273504.273504273
%%29> dicttest:test_get(1000000).
%%21739130.43478261
%%30> length(element(2,process_info(self(),dictionary))).
%%1000000
