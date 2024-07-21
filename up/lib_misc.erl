-module(lib_misc).
-compile(export_all).
-export([
		for/3,
		factorial/1,
		factorial_1/2,
		factorial_loop/1,
		qsort/1,
		sort/1,
		sort/2,
		exchange/1,
		exchange/2,
		pythag/1, 
		perms/1,
		filter/2,
		odds_and_evens/1,
		odds_and_evens_acc/1,
		odds_and_even/1,
		floor/1,
		ceil/1,
		unixtime/0,
		unixtime/1,
		sleep/1,
		print/1,
		rand/1,
		rand/2,
		rand_float1/2,
		rand_float2/2,
		rand_element/1,
		rand_element/2,
		find_repeat_element/1,
		find_repeat_element/2,
		get_time_list/1,
		get_day_list/1,
		t/1,
		t1/1,
		test/1,
		md5/1
		]).
	
-export([new/0, 
		add_element/2, 
		del_element/2,
		is_element/2, 
		is_empty/1, 
		union/2, 
		intersection/2]).
		
		
%% map实现
map(_,[]) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

%% 并行化顺序map实现
pmap(F, L) ->
	S = self(),
	Ref = erlang:make_ref(),
	Pids = map(fun(I) ->
		spawn(fun() -> do_f(S, Ref, F, I) end)
		end, L),
	gather(Pids, Ref).
	
do_f(Parent, Ref, F, I) ->
	Parent ! {self(), Ref, (catch F(I))}.
	
gather([Pid|T], Ref) ->
	receive
		{Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
	end;
gather([], _) -> [].


%% for循环
for(Min,Max,_F) when Min>Max -> error;
for(Max,Max,F) -> [F(Max)];
for(I,Max,F) -> [F(I)|for(I+1,Max,F)].

%% C实现
%% sum = 0;
%% for(i = 0; i<max; i++){ sum += f(i) }

%%for(I, Max, F, Sum) when I < Max ->
%%	for(I+1, Max, F, Sum + F(I));
%%for(I, Max, F, Sum) ->
%%	Sum.
	
%% 调用
%% Sum0 = 0.
%% Sum = for(0,Max,F,Sum0).

%% 求阶乘（非尾递归）
factorial(0) -> 1;
factorial(N) -> N*factorial(N-1).

%% 求阶乘（尾递归）
factorial_loop(N) -> factorial_1(N, 1).

factorial_1(0, X) -> X;
factorial_1(N, X) -> factorial_1(N-1, N*X).

%% 快速排序
qsort([]) -> [];
qsort([Pivot|T]) ->
		qsort([X || X <- T,X < Pivot])
		++ [Pivot] ++
		qsort([X || X <- T,X >= Pivot]).

%% 冒泡
sort([])          -> [];
sort(L)           -> sort(L,[]).
sort([],Result)   -> Result;
sort(List,Result) -> 
	[Biggest|Rest] = exchange(List),
	io:format("sort:~p~n",[lists:reverse([Biggest|Rest])]),
	sort(Rest,[Biggest|Result]).
	
exchange(List)     -> exchange(List,[]).
exchange([H],List) -> [H|List];
exchange([H,H2|T],List)    when H>H2 ->
    exchange([H|T],[H2|List]);
exchange([H,H2|T],List) when H=<H2 ->
    exchange([H2|T],[H|List]).

%% 快速排序
sort2([]) -> [];
sort2([Pivot|Rest]) ->
	{Smaller, Bigger} = split(Pivot, Rest),
	lists:append(sort2(Smaller), [Pivot|sort2(Bigger)]).
	
split(Pivot, L) ->
	split(Pivot, L, [], []).
split(_Pivot, [], Smaller, Bigger) ->
	{Smaller,Bigger};
split(Pivot, [H|T], Smaller, Bigger) when H < Pivot ->
	split(Pivot, T, [H|Smaller], Bigger);
split(Pivot, [H|T], Smaller, Bigger) when H >= Pivot ->
	split(Pivot, T, Smaller, [H|Bigger]).
	
%% bubble_once用于处理依次冒泡排序，将最小元素移到最后，
%% 然后再对N-1个元素使用bubble_once进行冒泡。
bubble_sort(L) -> bubble_sort(L, len(L)).
bubble_sort(L,0) -> L;
bubble_sort([H|T], N) ->
	Result = bubble_once(H,T),
	io:format("Result:~p~n", [Result]),
	bubble_sort(Result,N-1).

bubble_once(H,[]) -> [H];
bubble_once(X, [H|T]) ->
	if
		X > H -> [H|bubble_once(X,T)];
		true  -> [X|bubble_once(H,T)]
	end.

len([]) -> 0;
len([_H|T]) -> 1 + len(T).
	
%% 算法：插入排序，insert_sort(L1,L2)将L2依次插入L1中,
%% L1默认为空。normal函数实现一遍插入排序。
insert_sort(L) -> insert_sort([],L).
insert_sort(L,[]) -> L;
insert_sort(L, [H|T]) ->
	%% io:format("L:~p~n",[L]),
	insert_sort(normal(H,L), T).

normal(X,[]) -> [X];  
normal(X,[H|T]) ->  
    if X > H ->  
            [H|normal(X,T)];  
        true ->  
            [X|[H|T]]  
    end. 
	
%%毕达哥拉斯三元组
pythag([]) -> [];
pythag(N) ->
	[{A,B,C} ||
		A <- lists:seq(1,N),
		B <- lists:seq(1,N),
		C <- lists:seq(1,N),
		A+B+C =< N,
		A*A+B*B =:= C*C
		].

%%排列组合
perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L,T <- perms(L--[H])].

%%Even = fun(X) -> (X rem 2) =:= 0 end.
filter(P,[H|T]) ->
	case P(H) of
		true -> [H|filter(P,T)];
		false -> filter(P,T)
	end;
filter(_P,[]) ->
	[].

%%列表奇数or偶数分离，列表遍历两次
odds_and_evens(L) ->
	Odds = [X || X <- L, (X rem 2) =:=1],
	Evens = [X || X <- L, (X rem 2) =:=0],
	{Odds,Evens}.

%%列表奇数or偶数分离，列表遍历一次	
odds_and_evens_acc(L) ->
	odds_and_evens_acc(L,[],[]).

odds_and_evens_acc([H|T],Odds,Evens) ->
	case (H rem 2) of
		1 -> odds_and_evens_acc(T,[H|Odds],Evens);
		0 -> odds_and_evens_acc(T,Odds,[H|Evens])
	end;
	
odds_and_evens_acc([],Odds,Evens) ->
	%%{lists:reverse(Odds),lists:reverse(Evens)}.
	{list_to_tuple(qsort(Odds)),qsort(Evens)}.

%%if/end
odds_and_even(L) ->
	odds_and_even(L,[],[]).
	
odds_and_even([H|T],Odds,Even) ->
	if
		(H rem 2) =:= 0 -> odds_and_even(T,[H|Odds],Even);
		(H rem 2) =:= 1 -> odds_and_even(T,Odds,[H|Even])
	end;

odds_and_even([],Odds,Even) ->
	{list_to_tuple(Odds),list_to_tuple(Even)}.

%% 取小于X的最大整数
floor(X) ->
	T = erlang:trunc(X),
	case (X < T) of
		true -> T -1;
		_ -> T
	end.
	
%% 取大于X的最小整数
ceil(X) ->
	T = erlang:trunc(X),
	case (X > T) of
		true -> T + 1;
		_ -> T
	end.

%% 取当前时间戳
unixtime() ->
	{M, S, _} = erlang:now(),
	M * 1000000 + S.

%% 取当前时间戳，精确到毫秒
unixtime(micro) ->
		{MegaSecs, Secs, MicroSecs} = erlang:now(),
		MegaSecs * 1000000 + Secs + MicroSecs/1000000;
		
%% 取当天0时0分0秒的时间戳
unixtime(today) ->
    {M, S, MS} = now(),
    {_, Time} = calendar:now_to_local_time({M, S, MS}),%将now转换为日期和时间
    M * 1000000 + S - calendar:time_to_seconds(Time);%计算从午夜起到Time时间的秒数

%% 取明天0时0分0秒的时间戳
unixtime(tomorrow) ->
	unixtime(today) + 86400;
	
%% 取昨天0时0分0秒的时间戳
unixtime(yesterday) ->
	unixtime(today) - 86400;

%% 取今天12时0分0秒的时间戳
unixtime(noon) ->
	unixtime(today) + 43200.
	
%% 延迟M毫秒执行
sleep(M) ->
	receive
	after M ->
		true
	end.
	
%% print打印
print(Data) ->
	io:format("~ts~n",[xmerl_ucs:from_utf8(Data)]).

%% 随机整数
rand({Min, Max}) when Min > Max ->
	rand(Max, Min);
	
rand({Min, Max}) ->
	rand(Min, Max);
	
rand([Min, Max]) when Min > Max ->
	rand(Max, Min);
	
rand([Min, Max]) ->
	rand(Min, Max).

rand([], []) -> 0;

rand(0, 1) ->
	random:uniform();

rand(Same, Same) -> Same;

rand(Min, Max) ->
	%% R = Max - Min,
	M = random:uniform(Max),
	case M >= Min of
		true -> M;
		false -> rand(Min, Max)
	end.
	
%% 随机浮点数
rand_float1(Min, Max) when is_float(Min); is_float(Max) ->
	rand(round(Min*10), round(Max*10))/10;
	
rand_float1(Min, Max) ->
	rand(Min, Max).
	
rand_float2(Min, Max) when is_float(Min); is_float(Max) ->
	rand(round(Min*100), round(Max*100))/100;
	
rand_float2(Min, Max) ->
	rand(Min, Max).
	
%% 列表中随机一个元素
rand_element([]) -> undefined;

rand_element([L]) -> L;

rand_element(List) ->
	Len = length(List),
	Num = rand(1, Len),
	lists:nth(Num, List).

%% 	随机抽取列表List中Num个元素
rand_element(Num, List) ->
    rand_element(Num, List, []).

rand_element(0, _List, Reply) -> Reply;
rand_element(Num, List, Reply) ->
    case rand_element(List) of
        undefined -> Reply;
        E ->
            Reply1 = [E | Reply],
            List1 = lists:delete(E, List),
            rand_element(Num - 1, List1, Reply1)
    end.

%% 查找并返回全部重复元素
find_repeat_element([]) -> [];

find_repeat_element(List) -> 
	find_repeat_element(List, []).

find_repeat_element([H|T], Reply) ->
	case lists:member(H, T) of
		true -> find_repeat_element(T, [H|Reply]);
		false -> find_repeat_element(T, Reply)
	end;
	
find_repeat_element([], Reply) -> [lists:nth(1,Reply)|Reply].

%% 日期
get_day_list(now) ->
    {{Y,Mo,D}, _} = erlang:localtime(),
    [t(Y), t(Mo), t(D)].

get_time_list(now) ->
    {{Y,Mo,D}, {H,Mi,S}} = erlang:localtime(),
    [t(Y), t(Mo), t(D), t(H), t(Mi), t(S)].
	
t(X) when is_integer(X) -> 
	t1(integer_to_list(X));
t(_) -> "".

t1([X]) -> [$0,X];
t1(X) -> X.

test(X) when X>9;X<15 -> X. % 分号orelse 逗号andalso

%% 转换成hex格式的MD5
md5(S) ->
	list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
	

%% 递归and尾递归
tg1() ->
    Pid = spawn(fun() -> do_t1() end),
    send_msg(Pid, 100000).

tg2() ->
    Pid = spawn(fun() -> do_t2() end),
    send_msg(Pid, 100000).

send_msg(_Pid, 0) ->
    ok;
send_msg(Pid, N) ->
    Pid ! <<2:(N)>>,
    timer:sleep(200),
    send_msg(Pid, N-1).

do_t1() ->
    erlang:garbage_collect(self()),
    Result = erlang:process_info(self(), [memory, garbage_collection]),
    io:format("~w ~n", [Result]),
    io:format("backtrace:~w~n~n", [erlang:process_display(self(), backtrace)]),
    try
      receive
          _ ->
              do_t1()
      end
    catch
      _:_ ->
          do_t1()
    end.

do_t2() ->
    erlang:garbage_collect(self()),
    Result = erlang:process_info(self(), [memory, garbage_collection]),
    io:format("~w ~n", [Result]),
    io:format("backtrace:~w~n~n", [erlang:process_display(self(), backtrace)]),
    receive
      _ ->
          do_t2()
    end.
	

%% 递归排序
qsort1([],Depth)-> 
    io:format("Recursion Depth: ~.B~n",[Depth]), 
    []; 
qsort1([Pivot|[]],Depth)-> 
 
    io:format("Recursion Depth: ~.B~n",[Depth]), 
 
    [Pivot]; 
 
qsort1([Pivot|T],Depth)-> 
    qsort1([ X || X <- T,X =< Pivot],Depth+1) ++ [Pivot] ++ qsort1([X||X<-T,X>Pivot],Depth+1). 
 
qsort2([],Depth)-> 
    io:format("Recursion Depth~.B~n",[Depth]), 
    []; 
qsort2([Pivot|[]],Depth)-> 
    io:format("Recursion Depth~.B~n",[Depth]), 
    [Pivot]; 
 
qsort2([Pivot|T],Depth)-> 
    L = [X || X <- T, X =< Pivot], 
    R = [X || X <- T, X > Pivot], 
    %io:format("Pivot:~.B~n",[Pivot]), 
    %io:format("Left:~w~n",[L]),  
    %io:format("Right:~w~n",[R]),    
    case length(L) =< length(R) of 
       true-> 
        if 
            length(R) > 0 -> 
                RL = [X || X <- tl(R),X =< hd(R)], 
                RR = [X || X <- tl(R),X > hd(R)], 
                qsort2(L,Depth+1) ++ [Pivot] ++ qsort2(RL,Depth+1) ++ [hd(R)] ++ qsort2(RR,Depth+1); 
            length(R) == 0 -> 
                qsort2(L,Depth+1) ++ [Pivot] ++ qsort2([],Depth+1) 
        end; 
       false-> 
        if 
            length(L) > 0 -> 
                LL = [X || X <- tl(L),X =< hd(L)], 
                LR = [X || X <- tl(L),X > hd(L)], 
    %io:format("xPivot:~.B~n",[hd(L)]), 
    %io:format("xLeft:~w~n",[LL]),  
    %io:format("xRight:~w~n",[LR]),  
                qsort2(LL,Depth+1) ++ [hd(L)] ++ qsort2(LR,Depth+1) ++ [Pivot] ++ qsort2(R,Depth+1);  
            length(L) == 0 -> 
                qsort2([],Depth+1) ++ [Pivot] ++ qsort2(R,Depth+1) 
        end 
    end.

%% 复制一个给定的参数N次
duplicate_forest(0, _) -> [];
duplicate_forest(N, Term) when N > 0 ->
	[Term | duplicate_forest(N-1, Term)].
	
%% 递归实现
%% 尾递归的实现中,添加了第三个辅助变量"List",
%% 形参List一直被用来传递上一次迭代调用的结果,作为下一次迭代调用的形参
tail_duplicate_forest(N, Term) ->
	tail_duplicate_forest(N, Term, []).
	
tail_duplicate_forest(0, _ ,List) -> List;
tail_duplicate_forest(N, Term, List) when N > 0 -> 
	tail_duplicate_forest(N-1, Term, [Term|List]).
	
%% list倒转
reverse_list([]) -> [];
reverse_list([H|T]) ->
	reverse_list(T) ++ [H].
	
%% 递归实现
recursion_reverse_list(List) ->
	recursion_reverse_list(List,[]).

recursion_reverse_list([],Acc) -> Acc;
recursion_reverse_list([H|T],Acc) ->
	recursion_reverse_list(T, [H|Acc]).
	
%% 返回list前N个element
sublist_forest(_,0) -> [];
sublist_forest([],_) ->[];
sublist_forest([H|T], N) when N > 0 ->
	[H|sublist_forest(T,N-1)].
	
%% 递归实现
recursion_sublist_forest(List, N) ->
	recursion_sublist_forest(List, N, []).
	
recursion_sublist_forest(_, 0, Acc) -> Acc;
recursion_sublist_forest([], _, Acc) -> Acc;
recursion_sublist_forest([H|T], N, Acc) when N > 0 ->
	recursion_sublist_forest(T, N-1, [H|Acc]).
	
%% 命令行实现递归
%% Fact = fun(X) -> G=fun(0,F) -> 1; (N,F) -> N*F(N-1,F) end, G(X,G) end.
%% GG = fun(X) -> Gro=fun([],F)-> [] ; ([H1,H2,H3|H4],F) -> [{H1,H2,H3}|F(H4,F)] end, Gro(X,Gro) end.

%% Fact(5). %% 阶乘
%% GG([50,10,30,5,90,20,40,2,25,10,8,0]). %% 返回[{50,10,30},{5,90,20},{40,2,25},{10,8,0}]

msToDate(Milliseconds) ->
   BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   Seconds       = BaseDate + (Milliseconds div 1000),
   { Date,_Time} = calendar:gregorian_seconds_to_datetime(Seconds),
   Date.
   
%% timer:tc(Module, Function, Arguments) -> {Time, Value}.测试函数运行的时间
testFun(N)->
    {T1,ok}=timer:tc(fun()-> [get_seconds1()|| _T <- lists:seq(1,N)], ok end ),
    {T2,ok}=timer:tc(fun()-> [get_seconds2()|| _T <- lists:seq(1,N)], ok end ),
    io:format("~p,~p~n",[T1,T2]).

get_seconds1()->
    {G,S,_M}=now(),
    G*1000000+S.

-define(SECONDS_1970,62167219200).
get_seconds2()->
    Dt=calendar:universal_time(),
    GSeconds=calendar:datetime_to_gregorian_seconds(Dt),
    GSeconds-?SECONDS_1970.

%% get_val()取value值,[{coin, 1000}, {diamond, 50}, {tid_num, [{11338,5}]}]
get_val(Key, L) ->
	get_val(Key, L, undefined).
	
get_val(Key, L, Default) when is_list(L) ->
	case lists:keyfind(Key, 1, L) of
		false -> Default;
		{_, V} -> V
	end;
	
get_val(_Key, _L, Default) -> Default.

%% 获取本地时间的时间戳
mktime() ->
	DT = erlang:universaltime(), %% 获取当前UTC时间
	% DT = calendar:universal_time(), %% 获取当前UTC时间
	% DT = erlang:localtime_to_universaltime(erlang:localtime()), % 转换为UTC时间
	calendar:datetime_to_gregorian_seconds(DT) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

%% 生成一个指定日期的unix时间戳(无时区问题)
%% Date = date() = {Y, M, D}
%% Time = time() = {H, I, S}
%% 参数必须大于1970年1月1日
mktime({Date, Time}) ->
	DT = erlang:localtime_to_universaltime({Date, Time}), % 转换为UTC时间
	calendar:datetime_to_gregorian_seconds(DT) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
	
string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.	
	
%% 集合
new() -> []. 

add_element(X, Set) ->
	case is_element(X, Set) of
		true  -> Set;
		false -> [X|Set]
	end.
	
del_element(X, [X|T]) -> T;
del_element(X, [Y|T]) -> [Y|del_element(X, T)];
del_element(_, [])    -> [].

is_element(X, [X|_])   -> true;
is_element(X, [_|Set]) -> is_element(X, Set);
is_element(_, [])      -> false.

is_empty([]) -> true;
is_empty(_)  -> false.

union([H|T], Set) -> union(T, add_element(H, Set));
union([], Set)    -> Set.

intersection(S1, S2)       -> intersection(S1, S2, []).
intersection([], _, S)     -> S;
intersection([H|T], S1, S) ->
	case is_element(H,S1) of
		true  -> intersection(T, S1, [H|S]);
		false -> intersection(T, S1, S)
	end.
	
%% 素数
range(N, N) ->
	[N];
range(Min, Max) ->
	[Min | range(Min+1, Max)].
remove_multiples(N, [H|T]) when H rem N == 0 ->
	remove_multiples(N, T);
remove_multiples(N, [H|T]) ->
	[H | remove_multiples(N, T)];
remove_multiples(_, []) ->
	[].
sieve([H|T]) ->
	[H | sieve(remove_multiples(H, T))];
sieve([]) ->
	[].
primes(Max) ->
	sieve(range(2, Max)).
	
%% 密码加密
encode(Pin, Password) ->
	Code = {nil,nil,nil,nil,nil,nil,nil,nil,nil,
			nil,nil,nil,nil,nil,nil,nil,nil,nil,
			nil,nil,nil,nil,nil,nil,nil,nil},
	encode(Pin, Password, Code).
encode([], _, Code) ->
	Code;
encode(_Pin, [], _Code) ->
	io:format("Out of Letters~n",[]);
encode([H|T], [Letter|T1], Code) ->
	Arg = index(Letter) + 1,
	io:format("Arg:~p,Letter:~p~n",[Arg,Letter]),
	case element(Arg, Code) of
		nil ->
			encode(T, T1, setelement(Arg, Code, index(H)));
		_ ->
			encode([H|T], T1, Code)
	end.
index(X) when X >= $0, X =< $9 ->
	X - $0;
index(X) when X >= $A, X =< $Z ->
	X - $A.
	
%% 随机数替换没有被填充的nil
print_code([], Seed) ->
	Seed;
print_code([nil|T], Seed) ->
	NewSeed = ran(Seed),
	Digit = NewSeed rem 10,
	io:format("~w ",[Digit]),
	print_code(T, NewSeed);
print_code([H|T],Seed) ->
	io:format("~w ",[H]),
	print_code(T, Seed).
ran(Seed) ->
	(125 * Seed + 1) rem 4096.

%% 连接
testpass() ->
	title(),
	Password = "DECLARATIVE",
	entries([{"3451",Password,lisa},
			{"1234",Password,carwash},
			{"4321",Password,bigbank},
			{"7568",Password,doorcode1},
			{"8832",Password,doorcode2},
			{"4278",Password,cashcard},
			{"4278",Password,chequecard}]).
title() ->
	io:format("a b c d e f g h i j k l m n o p q r s t u v w x y z~n",[]).
entries(List) ->
	{_,_,Seed} = time(),
	entries(List, Seed).
entries([], _) -> true;
entries([{Pin,Password,Title}|T], Seed) ->
	Code = encode(Pin, Password),
	NewSeed = print_code(tuple_to_list(Code), Seed),
	io:format(" ~w~n",[Title]),
	entries(T, NewSeed).