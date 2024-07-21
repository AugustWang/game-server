-module(stimer).
-export([start/2,cancel/1,stop/0,star/2]).

start(Time,Fun) -> spawn(fun() -> timer(Time,Fun) end).

cancel(Pid) -> Pid!cancel.

timer(_Time,Fun) ->
	receive
		cancel ->
			void
	after 0 ->
		Fun()
	end.
	
	
star(Time,Fun) -> 
	register(clock,spawn(fun() -> tick(Time,Fun) end)).

stop() -> clock ! stop.

tick(Time,Fun) ->
		receive
			stop ->
				void
		after Time ->
			Fun(),
			tick(Time,Fun)
		end.

%%stimer:star(5000,fun() -> o:format("Tick: ~p~n",[erlang:now()]) end).