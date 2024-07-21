-module(a).
-compile(export_all).
-import(b,[x/0]).

start(Tag) ->
	spawn(fun() -> loop(Tag) end).
	
loop(Tag) ->
	timer:sleep(30000),
	Val = x(),
	%%io:format("Vsn1 (~p) b:x() =~p~n",[Tag,Val]),
	%%io:format("Vsn2 (~p) b:x() =~p~n",[Tag,Val]),
	io:format("Vsn3 (~p) b:x() =~p~n",[Tag,Val]),
	loop(Tag).
	