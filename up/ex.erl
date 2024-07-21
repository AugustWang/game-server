-module(ex).
-export([start/1,stop/0,rpc/2,on_exit/2]).

start(Fun) ->
	register(anName,spawn(fun() -> loop(Fun) end)).

rpc(Pid,Request) ->
	Pid ! {self(),Request},
	receive
		{Pid,Response} ->
			Response
	end.

stop() -> anName ! stop.
loop(Fun) ->
	receive
		stop ->
			void
	after 0 ->
		Fun(),
		loop(Fun)
	end.

%%ex:start(lang,fun() ->io:format("sss") end).

on_exit(Pid,Fun) ->
	spawn(fun() -> process_flag(trap_exit,true),
				   link(Pid),
				   receive
						{'EXIT',Pid,Why} ->
							Fun(Why)
					end
			end).