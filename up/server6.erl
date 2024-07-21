-module(server6).
-export([start/0, rpc/2]).

start() -> spawn(fun() -> wait() end).

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			Response
	end.
	
wait() ->
	receive
		{become, F} ->
			F()
	end.