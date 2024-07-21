-module(server5).
-export([start/2, rpc/2, swap_code/2]).

start(Name, Mod) ->
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).
	
rpc(Name, Request) ->
	Name ! {self(), Request},
	receive
		{Name, Response} ->
			Response
	end.
	
swap_code(Name,Mod) -> rpc(Name, Mod).

loop(Name, Mod, State) ->
	receive
		{From, {swap_code, NewMod}} ->
			From ! {Name,ack},
			loop(Name,NewMod,State);   %%热替换
		{From,Request} ->
		    %% io:format("Mod:~w, Pid:~w, Req:~w", [Mod, From, Request]),
			try Mod:handle(Request, State) of
				{Response, NewState} ->
					From ! {Name, Response},
					loop(Name, Mod, NewState)
			catch
				_:Why ->
					log_error(Why),
					
					From ! {Name, crash},
					
					loop(Name, Mod, State)
			end
	end.

log_error(Msg) ->
	io:format("Msg:~p~n",[Msg]).