-module(area_server_final).
-export([start/0,area/2]).
-import(math,[pi/0]).

start() -> spawn(fun loop/0).

area(Pid,What) ->
	rpc(Pid,What).

rpc(Pid,Request) ->
	Pid ! {self(),Request},
	receive
		{Pid,Response} ->
			Response
	end.

loop() ->
	receive
		{FROM,{rect,W,H}} ->
			FROM ! {self(),W*H},
			loop();
		{FROM,{circle,R}} ->
			FROM ! {self(),math:pi()*R*R},
			loop();
		{FROM,Other} ->
			FROM ! {self(),{error,Other}},
			loop()
	end.