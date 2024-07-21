-module(server3).
-export([start/2,rpc/2]).

start(Name,Mod) ->
	register(Name,spawn(fun() -> loop(Name,Mod,Mod:init()) end)).
	
rpc(Name,Request) ->
	From ! {self(),Request},
	receive
		{From, Response} -> Response
	end.

loop(Name,Mod,State) ->
	receive
		{From, Request} -> 
			{Response,NewState} -> Mod:headle(Request,State),
			From ! {Name,Response},
			loop(Name,Mod,NewState)
	end.