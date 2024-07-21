-module(my_fac_server6).
-export([loop/0]).

loop() ->
	receive
		{From, {fac, N}} ->
			From ! {self(), fac(N)},
			loop();
		{From, {fab, N}} ->
			From ! {self(), fab(N)},
			loop();
		{become, Something} ->
			Something
	end.
	
fac(0) -> 1;
fac(N) -> N * fac(N-1).

fab(0) -> 1;
fab(1) -> 1;
fab(N) when N>=2 -> 
	fab(N-1) + fab(N-2).