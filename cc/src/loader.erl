%%----------------------------------------------------
%% 加载数据
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(loader).
-compile(export_all).

-include("common.hrl").

data() ->
    Str = "-module(data).
-compile(export_all).

tt() ->
    test_tt.
    ",
    {Mod,Code} = dynamic_compile:from_string(Str),
    code:load_binary(Mod, "data.erl", Code),
    ok.
