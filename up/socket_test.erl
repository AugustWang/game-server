-module(socket_test).
-export([nano_get_url/0, nano_get_url/1, receive_data/2]).
-compile(export_all).

%% 从服务器上获取数据
nano_get_url() ->
	nano_get_url("www.baidu.com").
	
nano_get_url(Host) ->
	{ok, Socket} = gen_tcp:connect(Host,80,[binary, {packet,0}]),
	ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
	receive_data(Socket, []).
	
receive_data(Socket, Saf) ->
	receive
		{tcp, Socket, Bin} ->
			receive_data(Socket, [Bin|Saf]);
		{tcp_closed, Socket} ->
			list_to_binary(lists:reverse(Saf))
	end.

	
%% 一个简单的TCP服务器
%% 客户端
nano_client_eval(Str) ->
    {ok, Socket} = 
	gen_tcp:connect("localhost", 2345,
			[binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
	{tcp,Socket,Bin} ->
	    io:format("Client received binary = ~p~n",[Bin]),
	    Val = binary_to_term(Bin),
	    io:format("Client result = ~p~n",[Val]),
	    gen_tcp:close(Socket)
    end.

%% 服务端
start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},  %% (6)
					 {reuseaddr, true},
					 {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),  %% (7)
    gen_tcp:close(Listen),  %% (8)
    loop(Socket).
	
loop(Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    io:format("Server received binary = ~p~n",[Bin]),
	    Str = binary_to_term(Bin),  %% (9)
	    io:format("Server (unpacked)  ~p~n",[Str]),
	    Reply = lib_misc:string2value(Str),  %% (10)
	    io:format("Server replying = ~p~n",[Reply]),
	    gen_tcp:send(Socket, term_to_binary(Reply)),  %% (11)
	    loop(Socket);
	{tcp_closed, Socket} ->
	    io:format("Server socket closed~n")
    end.
	
%% 顺序型服务器:一次只接收一个连接
start_nano_ord_server() ->
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
									{reuseaddr, true},
									{active, true}]),
	seq_loop(Listen).
	
seq_loop(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	loop(Socket),
	seq_loop(Listen).
	
%% 并行服务器：同时可以接收多个并行连接
%% 每次gen_tcp:accept函数接收到一个新的连接时就去启动一个新进程
start_parallel_server() ->
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
									{reuseaddr, true},
									{active, true}]),
	spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
		{ok, Socket} = gen_tcp:accept(Listen),
		spawn(fun() -> par_connect(Listen) end),
		loop(Socket).

%% 3种模式套接字：active, active once, passive.		