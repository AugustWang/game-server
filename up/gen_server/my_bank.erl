-module(my_bank).
-compile(export_all).

%% start() :开启银行
%% stop()  :关闭银行
%% new_account(Who) :创建账户
%% deposit(Who, Amount) : 存款 
%% withdraw(Who, Amount): 取款

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).  %% 启动一个受监控的服务器进程
stop()  -> gen_server:call(?MODULE, stop).  %% 向服务器进程发送同步请求,对服务器的远程调用

new_account(Who)  -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who,Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).

init([]) -> {ok, ets:new(?MODULE,[])}.

handle_call({new, Who}, _From, Tab) ->  %% Module:handle_call处理同步调用的回调函数
	Reply = case ets:lookup(Tab,Who) of
				[] -> ets:insert(Tab, {Who, 0}),
					  {welcome, Who};
				[_] -> {Who, you_already_are_a_customer}
			end,
	%%State1 = 
	{reply, Reply, Tab};
	
handle_call({add, Who, X}, _From, Tab) ->
	Reply = case ets:lookup(Tab, Who) of
				[] -> not_a_customer;
				[{Who, Balance}] ->
					NewBalance = Balance + X,
					ets:insert(Tab, {Who, NewBalance}),
					{thanks, Who, your_balance_is, NewBalance}
			end,
	{reply, Reply, Tab};

handle_call({remove, Who, X}, _From, Tab) ->
	Reply = case ets:lookup(Tab, Who) of
				[] -> not_a_customer;
				[{Who, Balance}] when X =< Balance ->
					NewBalance = Balance - X,
					ets:insert(Tab, {Who, NewBalance}),
					{thanks, Who, you_only_have, Balance, NewBalance};
				[{Who, Balance}] -> 
					{sorry, Who, you_only_have, Balance, in_the_bank}
			end,
	{reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
	{stop, normal, stopped, Tab}.
	
handle_cast(_Msg, State) -> {noreply, State}.		%% 实现通知，处理异步调用的回调函数
handle_info(_Info, State) -> {noreply, State}.		%% 发给服务器原生消息
terminate(_Reason, _State) -> ok.                   %% 终止服务器调用
code_change(_OldVsn, State, _Extra) -> {ok, State}.  %% 热代码替换