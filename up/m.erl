-module(m).
-export([loop/0]).

loop() ->
	receive
		who ->
			io:format("i am ~p~n",[self()]),
			loop()
	end.