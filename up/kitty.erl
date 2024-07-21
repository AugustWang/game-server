-module(kitty).
-compile([export_all]).
-behaviour(gen_server).
-record(cat, {name, color=green, discription}).
-export([init/1, handle_call/3, handle_cast/3, handle_info/2,
		terminate/2, cord_change/3]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Discription) ->
	gen_server:call(Pid, {reply, Name, Color, Discription}).

return_cat(Pid, Cat = #cat{}) ->
	gen_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
	gen_server:call(Pid, terminate).

make_cat(Name, Col, Desc) ->
	#cat{name = Name, color = Col, discription = Desc}.

init([]) -> {ok, []}.

handle_call({reply, Name, Color, Discription}, _From, Cats) ->
	if
		Cats =:=[] ->
			{reply, make_cat(Name, Color, Discription), Cats};
		Cats =/=[] ->
			{reply, hd(Cats), tl(Cats)}
	end.

handle_call(terminate, _From, Cats) ->
	{stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
	{noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, Cats}.

terminate(normal, Cate) ->
	[io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.