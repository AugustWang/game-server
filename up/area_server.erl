-module(area_server).
-export([loop/0,rpc/2]).

rpc(Pidd,Request) ->
	Pidd ! {self(),Request},
	receive
		{Pidd,Response} ->
			Response
	end.
	
loop() ->
	receive
		{FromM,{rect,W,H}} ->
			%%io:format("~p~n",[W*H]),
			FromM ! {self(),W*H},
			loop();
		{FromM,{circle,R}} ->
			%%io:format("~p~n",[3.14159*R*R]),
			FromM ! {self(),3.14159*R*R},
			loop();
		{FromM,Other} -> 
			FromM ! {self(),{error,Other}},
			%%io:format("~p",[Other]),
			loop()
end.