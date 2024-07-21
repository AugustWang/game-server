%%----------------------------------------------------
%% 环境变量
%% 
%% 加载到application env 环境变量中，此处加载变量会覆盖xge.app中变量
%% 注意不要与xge.app配置文件中的变量冲突
%% 
%% @author QingXuan
%% @end
%%----------------------------------------------------
-module(cc_env).
-export([init/0, get/1, set/2]
).
-include("common.hrl").
-compile(export_all).

%% @hidden
init() ->
    ?INFO("init env ... "),
    ets:new(cc_env, [{keypos, 1}, named_table, public, set]),
    compile([]),
    ok.

get(Key) ->
    case application:get_env(cc, Key) of
        {ok, V} -> V;
        Else -> Else
    end.

set(K, V) ->
    ets:insert(cc_env, {K, V}),
    Kvs = ets:tab2list(cc_env),
    compile(Kvs).

del(K) ->
    ets:delete(cc_env, K),
    Kvs = ets:tab2list(cc_env),
    compile(Kvs).

compile(Kvs) ->
    try
        Src = get_src(Kvs),
        {Mod, Code} = dynamic_compile:from_string(Src),
        code:load_binary(Mod, "env.erl", Code)
    catch
        T : X -> ?WARNING("Error when compiling env! [~p:~p]", [T, X])
    end.

get_src(Kvs) ->
    Kvs1 = lists:map(fun({K, V})->
                io_lib:format("get(~p) -> ~p;\n", [K, V])
        end, Kvs),
    Kvs2 = lists:flatten(Kvs1),
    {H, T} = get_init_src(),
    H ++ Kvs2 ++ T.

get_init_src() ->
    L = application:get_all_env(cc),
    Fun = lists:map(fun({K, V})->
                io_lib:format("get(~w) -> ~w;\n", [K, V])
        end, L),
    SrcHead = "
        -module(env).    
        -compile(export_all).
        " ++ lists:flatten(Fun),
    SrcTail = "get(_) -> undefined.\n",
    {SrcHead, SrcTail}.
