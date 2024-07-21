%%---------------------------------------------------
%% Erlang模块热更新到所有线路（包括server的回调函数，如果对state有影响时慎用）
%%
%% 检查：u:c()                %% 列出前5分钟内编译过的文件
%%       u:c(N)               %% 列出前N分钟内编译过的文件
%%
%% 更新：u:u()                %% 更新前5分钟内编译过的文件               
%%       u:u(N)               %% 更新前N分钟内编译过的文件   
%%       u:u([mod_xx, ...])   %% 指定模块（不带后缀名）
%%       u:u(m)               %% 编译并加载文件
%%       u:m()                %% 编译并加载文件
%%
%% Tips: u - update, c - check
%% 
%% @author rolong@vip.qq.com
%%---------------------------------------------------
%% 自动执行命令
%%
%% 命令：u:start()  %% 成功启动后会生成文件夹update和update/history
%%       u:stop()
%%
%% 说明：定时检查文件(update/command.txt)
%%       如果存在则执行并重新命名到update/history
%%       执行成功则生成 update/ok.txt
%%       执行失败则生成 update/error.txt
%%       每次执行前会先删除老的 ok.txt
%%       command.txt文件内容为erlang命令，
%%       可以多行，以,或.结尾(最后一行必段以.结尾)
%% @author rolong@vip.qq.com
%%----------------------------------------------------

-module(u).
-behaviour(gen_fsm).  
-export(
    [
        init/1
        ,handle_event/3
        ,handle_sync_event/4 
        ,handle_info/3
        ,terminate/3
        ,code_change/4
    ]
).
-compile(export_all).
-include_lib("kernel/include/file.hrl").
-include("common.hrl").

%% 调试开关
%% Debug = on | off
debug(on) -> 
    gen_event:notify(cc_logger, {debug, on}),
    debug_on;
debug(off) -> 
    gen_event:notify(cc_logger, {debug, off}),
    debug_off.

print_info() ->
    P = self(),
    Data = {P, erlang:process_info(P, memory), erlang:process_info(P, message_queue_len)},
    io:format("~n~w", [Data]).

%% @spec cc_info() -> ok
%% @doc 输出系统信息, 具体内容见log/info_xxxx.log文件
info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    Info = io_lib:format( "abormal termination:
        ~n   Scheduler id:                         ~p
        ~n   Num scheduler:                        ~p
        ~n   Process count:                        ~p
        ~n   Process limit:                        ~p
        ~n   Memory used by erlang processes:      ~p
        ~n   Memory allocated by erlang processes: ~p
        ~n   The total amount of memory allocated: ~p
        ~n",
        [SchedId, SchedNum, ProcCount, ProcLimit,
            ProcMemUsed, ProcMemAlloc, MemTot]),
    {{Y, M, D}, {H, M2, S}} = erlang:localtime(),
    F = fun(Int) ->
        case Int < 10 of
            true -> "0" ++integer_to_list(Int);
            false -> integer_to_list(Int)
        end
    end,
    DateStr = lists:concat([[F(X) || X <- [Y, M, D]], "_", [F(X) || X <- [H, M2, S]]]),
    File1 = "log/info_" ++ DateStr ++ ".log",
    A = lists:foldl( fun(P, Acc0) -> [{P, erlang:process_info(P, registered_name), erlang:process_info(P, memory), erlang:process_info(P, message_queue_len), erlang:process_info(P, current_function), erlang:process_info(P, initial_call)} | Acc0] end, [], erlang:processes()),
    B = io_lib:format("~s~n~p", [Info, A]),
    file:write_file(File1, B),
    io:format("~s", [Info]),
    ok.

%% 查看在线信息
online() ->
    L = ets:tab2list(online),
    do_online(L, 0),
    io:format("\n\nOnline role num: ~w\n\n", [length(L)]),
    ok.

do_online([O | T], Index) when Index < 30 ->
    io:format("\n#~-5w - ~ts", [O#online.id, O#online.name]),
    do_online(T, Index + 1);
do_online(_, _) -> ok.

%% install release
upgrade(ReleasePackage) ->
    {ok, Vsn} = release_handler:unpack_release(ReleasePackage),
    ?INFO("Unpacked Release ~p", [Vsn]),
    {ok, OtherVsn, Desc} = release_handler:check_install_release(Vsn),
    {ok, OtherVsn, Desc} = release_handler:install_release(Vsn), 
    ?INFO("Installed Release ~p", [Vsn]),
    ok = release_handler:make_permanent(Vsn),
    ?INFO("Made Release ~p Permanent", [Vsn]).

make() ->
    os:cmd("./rebar compile").

make_flash() ->
    os:cmd("./bin/make_flash").

make_flash_debug() ->
    os:cmd("./bin/make_flash_debug").

c() ->
    c(5).
c(command_file) ->
    case file:open("update/command.txt",read) of
        {ok, _S} -> ok;
        _ -> file_not_founded
    end;
c(S) when is_integer(S) ->
    case file:list_dir(get_path()) of
        {ok, FileList} -> 
            Files = get_new_file(FileList, S * 60),
            info("---------check modules---------~n~w~n=========check modules=========", [Files]);
        Any -> info("Error Dir: ~w", [Any])
    end;
c(_) -> info("ERROR===> Badarg", []).

admin()->
    spawn(fun()->u(m) end),
    ok.

m() ->
    u:u(m).

u() ->
    u(5).
u(m) ->
    u:p(),
    StartTime = util:unixtime(),
    info("----------makes----------", []),
    make:all(),
    EndTime = util:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    u(Time / 60);
u(S) when is_number(S) ->
    Path = get_path(),
    case file:list_dir(Path) of
        {ok, FileList} -> 
            Files = get_new_file(FileList, util:ceil(S * 60) + 3),
            AllZone = srv_net:node_list(all),
            info("---------modules---------~n~w~n----------nodes----------", [Files]),
            loads(AllZone, Files),
            util:term_to_string(Files);
        Any -> info("Error Dir: ~w", [Any])
    end;
u(Files) when is_list(Files) ->
    AllZone = srv_net:node_list(all),
    info("---------modules---------~n~w~n----------nodes----------", [Files]),
    loads(AllZone, Files);
u(_) -> info("ERROR===> Badarg", []).

info(V) ->
    info(V, []).
info(V, P) ->
    io:format("~n" ++ V, P).

%% 更新到所有线路
loads([], _Files) -> 
    info("----------- ok ----------"),
    ok;
loads([H | T], Files) ->
    info("#~w -> ~s", [H#node.id, H#node.name]),
    %% rpc:cast(H#node.name, u, load, [Files]),
    u:load(Files),
    loads(T, Files).

get_new_file(Files, S) -> 
    get_new_file(get_path(), Files, S, []).
get_new_file(_Path, [], _S, Result) -> Result;
get_new_file(Path, [H | T], S, Result) ->
    NewResult = case string:tokens(H, ".") of
        [Left, Right] when Right =:= "beam" ->
            case file:read_file_info(Path ++ H) of
                {ok, FileInfo} -> 
                    Now = calendar:local_time(),
                    case calendar:time_difference(FileInfo#file_info.mtime, Now) of
                        {Days, Times} -> 
                            Seconds = calendar:time_to_seconds(Times), 
                            case Days =:= 0 andalso Seconds < S of
                                true ->
                                    FileName = list_to_atom(Left),
                                    [FileName | Result];
                                false -> Result
                            end;
                        _ -> Result
                    end;
                _ -> Result
            end;
        _ -> Result
    end,
    get_new_file(Path, T, S, NewResult).

load([]) -> ok;
load([FileName1 | T]) ->
    FileName = case is_list(FileName1) of
        true -> list_to_atom(FileName1);
        false -> FileName1
    end,
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    TimeString = io_lib:format("[~w-~w-~w ~w:~w:~w]", [Y, M, D, H, I, S]),
    case code:soft_purge(FileName) of
        true -> 
            case code:load_file(FileName) of
                {module, _} -> 
                    info("loaded: ~s", [FileName]);
                {error, What} -> 
                    case filelib:is_dir("update") of
                        false -> file:make_dir("update");
                        true -> skip
                    end,
                    LoadErrorInfo = io_lib:format("~s ERROR===> loading: ~w (~w)\n", [TimeString, FileName, What]),
                    info("~s", [LoadErrorInfo]),
                    file:write_file("update/error.txt", LoadErrorInfo, [append])
            end;
        false -> 
            case filelib:is_dir("update") of
                false -> file:make_dir("update");
                true -> skip
            end,
            PurgeErrorInfo = io_lib:format("~s ERROR===> Processes lingering : ~w \n", [TimeString, FileName]),
            info("~s", [PurgeErrorInfo]),
            file:write_file("update/error.txt", PurgeErrorInfo, [append]),
            ok
    end,
    load(T).

%%----------------------------------------------------
%% 自动执行命令
%% 
%% @author rolong@vip.qq.com
%%----------------------------------------------------

start() -> 
    start_link(15).

start_link(Time) ->  
    ?INFO("start (~w)~w...", [Time, ?MODULE]),
    gen_fsm:start_link({local, u}, u, Time, []).

stop() ->
    gen_fsm:send_all_state_event(u, stop).
  
init(Time) ->  
    %% 文件夹不存在则创建
    case filelib:is_dir("update") of
        false -> file:make_dir("update");
        true -> skip
    end,
    {ok, watting, {Time}, Time * 1000}.  
  
handle_event(stop, _StateName, State) ->
    info("stopped!"),
    {stop, normal, State}.

handle_sync_event(_Any, _From, StateName, State) ->
    {reply, {error, unhandled}, StateName, State}.

handle_info(_Any, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Any, _StateName, _Opts) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

watting(timeout, {Time}) ->
    update_file(),
    {next_state, watting, {Time}, Time * 1000}.

update_file() ->
    FileDir = "./update/",
    case file:list_dir(FileDir) of
        {ok, FileList} -> 
            case get_beam_file(FileDir, FileList, []) of
                [] -> ok;
                Files -> 
                    update_file(Files),
                    load(Files)
            end;
        Any -> info("Error Dir: ~w", [Any])
    end.

update_file([H | T]) ->
    backup_file(H),
    move_file(H),
    update_file(T);
update_file([]) -> ok.

get_path() ->
    [Path] = case filelib:is_dir("./ebin") of
        true -> 
            %% 开发版ebin目录
            ["./ebin"];
        false ->
            %% 发布版ebin目录
            Paths = code:get_path(),
            F = fun(P) ->
                    string:str(P, "/cc-") > 0
            end,
            lists:filter(F, Paths)
    end,
    Path ++ "/".

get_beam_file(_Path, [], Result) -> Result;
get_beam_file(Path, [H | T], Result) ->
    NewResult = case string:tokens(H, ".") of
        [Left, "beam"] ->
            [Left | Result];
        [V, "tar", "gz"] ->
            %% Process upgrade package
            process_upgrade(V, H),
            Result;
        _ -> Result
    end,
    get_beam_file(Path, T, NewResult).

process_upgrade(V, File) ->
    case file:rename("./update/" ++ File, "./releases/" ++ File) of
        ok -> 
            upgrade(V),
            ok;
        {error, RenameErr} -> 
            info("Rename Error when process_upgrade:~s", [RenameErr]),
            ok
    end.

move_file(Mod) -> 
    Path = get_path(),
    case file:rename("./update/"++Mod++".beam", Path++Mod++".beam") of
        ok -> ok;
        {error, RenameErr} -> 
            info("Rename Error when move_file:~s", [RenameErr]),
            ok
    end.

backup_file(Mod) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    TimeString = io_lib:format("~w-~w-~w_~w-~w-~w", [Y, M, D, H, I, S]),
    Path = get_path(),
    FileName = Path++Mod++".beam",
    case filelib:is_file(FileName) of
        true ->
            case file:rename(Path++Mod++".beam", Path++Mod++".beam."++TimeString++".bak") of
                ok -> ok;
                {error, RenameErr} -> 
                    info("Rename Error when backup_file:~s", [RenameErr]),
                    ok
            end;
        false -> skip
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 生成协议
p() ->
    PackTxt = "src/pack.txt",
    PackErl = "src/pack.erl",
    PackMod = "pack",
    UnPackTxt = "src/unpack.txt",
    UnPackErl = "src/unpack.erl",
    UnPackMod = "unpack",
    case filelib:is_file(PackErl) andalso filelib:is_file(UnPackErl) of
        true ->
            {ok, PackTxtInfo} = file:read_file_info(PackTxt),
            {ok, PackErlInfo} = file:read_file_info(PackErl),
            {Days1, _Times1} =  calendar:time_difference(PackTxtInfo#file_info.mtime, PackErlInfo#file_info.mtime),
            {ok, UnPackTxtInfo} = file:read_file_info(UnPackTxt),
            {ok, UnPackErlInfo} = file:read_file_info(UnPackErl),
            {Days2, _Times2} =  calendar:time_difference(UnPackTxtInfo#file_info.mtime, UnPackErlInfo#file_info.mtime),
            case Days1 < 0 of
                true -> gen_pack_code({PackTxt, PackErl, PackMod});
                false -> ok
            end,
            case Days2 < 0 of
                true -> gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod});
                false -> ok
            end,
            ok;
        false ->
            gen_pack_code({PackTxt, PackErl, PackMod}),
            gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod}),
            ok
    end,
    p_robot(),
    ok.

p_robot() ->
    PackTxt = "src/unpack.txt",
    PackErl = "src/robot_pack.erl",
    PackMod = "robot_pack",
    UnPackTxt = "src/pack.txt",
    UnPackErl = "src/robot_unpack.erl",
    UnPackMod = "robot_unpack",
    case filelib:is_file(PackErl) andalso filelib:is_file(UnPackErl) of
        true ->
            {ok, PackTxtInfo} = file:read_file_info(PackTxt),
            {ok, PackErlInfo} = file:read_file_info(PackErl),
            {Days1, _Times1} =  calendar:time_difference(PackTxtInfo#file_info.mtime, PackErlInfo#file_info.mtime),
            {ok, UnPackTxtInfo} = file:read_file_info(UnPackTxt),
            {ok, UnPackErlInfo} = file:read_file_info(UnPackErl),
            {Days2, _Times2} =  calendar:time_difference(UnPackTxtInfo#file_info.mtime, UnPackErlInfo#file_info.mtime),
            case Days1 < 0 of
                true -> gen_robot_pack_code({PackTxt, PackErl, PackMod});
                false -> ok
            end,
            case Days2 < 0 of
                true -> gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod});
                false -> ok
            end,
            ok;
        false ->
            gen_robot_pack_code({PackTxt, PackErl, PackMod}),
            gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod}),
            ok
    end,
    ok.

%%' 生成打包协议
gen_pack_code({RulePath, TargetPath, Mod}) ->
    {ok, CmdList} = file:consult(RulePath),
    PackCode = gen_pack_code(CmdList, ""),
    Code = "-module("++Mod++").
-export([p/2]).

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

" ++ PackCode ++ "p(Cmd, Data) -> 
    io:format(\"undefined_pack_cmd:~w, data:~w\", [Cmd, Data]),
    {error, undefined_pack_cmd}.
",
    %% file:write_file("src/pack.erl", Code, [append]).
    ?INFO("Generate ~s ok!~n", [TargetPath]),
    file:write_file(TargetPath, Code, []).

gen_pack_code([{Cmd, Rule} | T], Result) ->
    [L, B, C] = pack_data(1, Rule, "", "", ""),
    CmdL = integer_to_list(Cmd),
    Code = "p("++CmdL++", ["++L++"]) ->"++C++"
    Data = <<"++B++">>,
    Len = byte_size(Data),
    {ok, <<Len:16, "++CmdL++":16, Data/binary>>};

",
    gen_pack_code(T, Result ++ Code);
gen_pack_code([], Result) -> Result.

%%' 生成打包协议
gen_robot_pack_code({RulePath, TargetPath, Mod}) ->
    {ok, CmdList} = file:consult(RulePath),
    PackCode = gen_robot_pack_code(CmdList, ""),
    Code = "-module("++Mod++").
-export([p/3]).

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

" ++ PackCode ++ "p(Cmd, Data, _) -> 
    io:format(\"undefined_robot_pack_cmd:~w, data:~w\", [Cmd, Data]),
    {error, undefined_robot_pack_cmd}.
",
    %% file:write_file("src/pack.erl", Code, [append]).
    ?INFO("Generate ~s ok!~n", [TargetPath]),
    file:write_file(TargetPath, Code, []).

gen_robot_pack_code([{Cmd, Rule} | T], Result) ->
    [L, B, C] = pack_data(1, Rule, "", "", ""),
    CmdL = integer_to_list(Cmd),
    Code = "p("++CmdL++", ["++L++"], Index) ->"++C++"
    Data = <<"++B++">>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, "++CmdL++":16, Data/binary>>};

",
    gen_robot_pack_code(T, Result ++ Code);
gen_robot_pack_code([], Result) -> Result.
    

pack_data(Index, [int8 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":8";
        _ -> ResultB ++ "," ++ Name ++ ":8"
    end,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

pack_data(Index, [int16 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":16";
        _ -> ResultB ++ "," ++ Name ++ ":16"
    end,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

pack_data(Index, [int32 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":32";
        _ -> ResultB ++ "," ++ Name ++ ":32"
    end,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

pack_data(Index, [float2 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    Name1 = "(round(" ++ Name ++ "*100))",
    ResultB1 = case ResultB of
        "" -> Name1 ++ ":32";
        _ -> ResultB ++ "," ++ Name1 ++ ":32"
    end,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

pack_data(Index, [string | Format], ResultL, ResultB, ResultC) ->
    I = integer_to_list(Index),
    Name = "X" ++ I,
    LenC = "
    LenX" ++ I ++ " = byte_size(X" ++ I ++ "),",
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB0 = case ResultB of "" -> ResultB; _ -> ResultB ++ "," end,
    ResultB1 = ResultB0 ++ "LenX" ++ I ++ ":16," ++ Name ++ "/binary",
    ResultC1 = ResultC ++ LenC,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC1);

%% Array
pack_data(Index, [FormatH | Format], ResultL, ResultB, C) ->
    I = integer_to_list(Index),
    Name = "X" ++ I,
    L = case ResultL of "" -> ResultL; _ -> ResultL ++ "," end,
    B = case ResultB of "" -> ResultB; _ -> ResultB ++ "," end,
    [L1, B1, C1] = pack_data(Index+10, FormatH, "", "", ""),
    C0 = "
    LenArr"++I++" = length("++Name++"),
    F"++I++" = fun
        (["++L1++"]) -> "++C1++" <<"++B1++">>;
        ({"++L1++"}) -> "++C1++" <<"++B1++">> end,
    B"++ I ++" = list_to_binary([F"++I++"(X) || X <- "++Name++"]),",
    L2 = L ++ Name,
    B2 = B ++ "LenArr"++I++":16, B"++ I ++"/binary",
    C2 = C ++ C0,
    pack_data(Index + 1, Format, L2, B2, C2);

pack_data(_, [], ResultL, ResultB, ResultC) ->
    [ResultL, ResultB, ResultC].
%%.

%%' 生成解包协议
gen_unpack_code({RulePath, TargetPath, Mod}) ->
    {ok, CmdList} = file:consult(RulePath),
    PackCode = gen_unpack_code(CmdList, ""),
    Code = "-module("++Mod++").
-export([p/2]).

p(_, <<>>) ->
    {ok, []};

" ++ PackCode ++ "p(Cmd, Data) -> 
    io:format(\"undefined_"++Mod++"_cmd:~w, data:~w\", [Cmd, Data]),
    {error, undefined_unpack_cmd}.
",
    %% file:write_file("src/pack.erl", Code, [append]).
    ?INFO("Generate ~s ok!~n", [TargetPath]),
    file:write_file(TargetPath, Code, []).

gen_unpack_code([{Cmd, Rule} | T], Result) ->
    RuleL = binary_to_list(util:term_to_string(Rule)),
    CmdL = integer_to_list(Cmd),
    Code = case string:str(RuleL, "],") > 0 orelse string:str(RuleL, "]]]") > 0 of
        true ->
            "p("++CmdL++", Bin) ->
    protocol:unpack2("++RuleL++", "++CmdL++", Bin);

";
        false ->
            [L, B, C] = unpack_data(1, Rule, "", "", ""),
            "p("++CmdL++", <<"++B++">>) ->"++C++"
    {ok, [" ++ L ++ "]};

"
    end,
    gen_unpack_code(T, Result ++ Code);
gen_unpack_code([], Result) -> Result.
    

unpack_data(Index, [int8 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":8";
        _ -> ResultB ++ "," ++ Name ++ ":8"
    end,
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

unpack_data(Index, [int16 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":16";
        _ -> ResultB ++ "," ++ Name ++ ":16"
    end,
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

unpack_data(Index, [int32 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":32";
        _ -> ResultB ++ "," ++ Name ++ ":32"
    end,
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

unpack_data(Index, [float2 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name ++ "/100";
        _ -> ResultL ++ "," ++ Name ++ "/100"
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":32";
        _ -> ResultB ++ "," ++ Name ++ ":32"
    end,
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

unpack_data(Index, [string | Format], ResultL, ResultB, ResultC) ->
    I = integer_to_list(Index),
    Name = "X" ++ I,
    ResultL0 = case ResultL of "" -> ResultL; _ -> ResultL ++ "," end,
    ResultB0 = case ResultB of "" -> ResultB; _ -> ResultB ++ "," end,
    ResultL1 = ResultL0 ++ Name,
    ResultB1 = ResultB0 ++ "LenX" ++ I ++ ":16," ++ Name ++ ":LenX" ++ I ++ "/binary",
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

%% Array
unpack_data(Index, [FormatH | Format], ResultL, ResultB, C) ->
    I = integer_to_list(Index),
    Name = "X" ++ I,
    L = case ResultL of "" -> ResultL; _ -> ResultL ++ "," end,
    B = case ResultB of "" -> ResultB; _ -> ResultB ++ "," end,
    [L1, B1, C1] = unpack_data(Index+10, FormatH, "", "", ""),
    C0 = "
    F"++I++" = fun(_, {B"++I++", Result"++I++"}) ->
            <<"++B1++",RemBin/binary>> = B"++I++", "++C1++"
            {ok, {RemBin, [["++L1++"]|Result"++I++"]}}
    end,
    {ok, {_, L"++I++"}} = util:for(1, LenArr"++I++", F"++I++", {"++Name++", []}),",
    L2 = L ++ "L" ++ I,
    B2 = B ++ "LenArr"++I++":16, "++ Name ++ "/binary",
    C2 = C ++ C0,
    unpack_data(Index + 1, Format, L2, B2, C2);

unpack_data(_, [], ResultL, ResultB, ResultC) ->
    [ResultL, ResultB, ResultC].
%%.

ready_stop() ->
    ready_stop(60).

ready_stop(Time) ->
    Pid = spawn_link(?MODULE, ready, [stop]),
    Pid ! {stop, Time}.

ready_restart() ->
    ready_restart(60).

ready_restart(Time) ->
    Pid = spawn_link(?MODULE, ready, [restart]),
    Pid ! {restart, Time}.

ready(stop) ->
    receive
        {stop, Second} when Second < -2 ->
            ?INFO("Server shutdown");
        {stop, Second} ->
            if
                Second == -2 -> cc:stop();
                Second == 1 ->
                    ?INFO("Shutdown after 1 second"),
                    lib_conn:send_notice(<<"Shutdown after 1 second">>);
                Second == 2 ->
                    ?INFO("Shutdown after 2 second"),
                    lib_conn:send_notice(<<"Shutdown after 2 second">>);
                Second == 3 ->
                    ?INFO("Shutdown after 3 second"),
                    lib_conn:send_notice(<<"Shutdown after 3 second">>);
                Second == 5 ->
                    ?INFO("Shutdown after 5 second"),
                    lib_conn:send_notice(<<"Shutdown after 5 second">>);
                Second == 10 ->
                    ?INFO("Shutdown after 10 second"),
                    lib_conn:send_notice(<<"Shutdown after 10 second">>);
                Second == 20 ->
                    ?INFO("Shutdown after 20 second"),
                    lib_conn:send_notice(<<"Shutdown after 20 second">>);
                Second == 30 ->
                    ?INFO("Shutdown after 30 second"),
                    lib_conn:send_notice(<<"Shutdown after 30 second">>);
                Second == 58 ->
                    ?INFO("Shutdown after 58 second"),
                    lib_conn:send_notice(<<"Shutdown after 58 second">>);
                Second == 59 ->
                    ?INFO("Shutdown after 59 second"),
                    lib_conn:send_notice(<<"Shutdown after 59 second">>);
                Second == 60 ->
                    ?INFO("Shutdown after 60 second"),
                    lib_conn:send_notice(<<"Shutdown after 60 second">>);
                true -> 
                    ok
            end,
            Second1 = Second - 1,
            erlang:send_after(1000, self(), {stop, Second1}),
            ready(stop);
        Other ->
            ?INFO("undefined msg: ~w", [Other])
    end;

ready(restart) ->
    receive
        {restart, Second} when Second < -2 ->
            ?INFO("Server Shutdown");
        {restart, Second} ->
            if
                Second == -2 -> cc:restart();
                Second == 1 ->
                    ?INFO("Shutdown after 1 second"),
                    lib_conn:send_notice(<<"Shutdown after 1 second">>);
                Second == 2 ->
                    ?INFO("Shutdown after 2 second"),
                    lib_conn:send_notice(<<"Shutdown after 2 second">>);
                Second == 3 ->
                    ?INFO("Shutdown after 3 second"),
                    lib_conn:send_notice(<<"Shutdown after 3 second">>);
                Second == 5 ->
                    ?INFO("Shutdown after 5 second"),
                    lib_conn:send_notice(<<"Shutdown after 5 second">>);
                Second == 10 ->
                    ?INFO("Shutdown after 10 second"),
                    lib_conn:send_notice(<<"Shutdown after 10 second">>);
                Second == 20 ->
                    ?INFO("Shutdown after 20 second"),
                    lib_conn:send_notice(<<"Shutdown after 20 second">>);
                Second == 30 ->
                    ?INFO("Shutdown after 30 second"),
                    lib_conn:send_notice(<<"Shutdown after 30 second">>);
                Second == 58 ->
                    ?INFO("Shutdown after 58 second"),
                    lib_conn:send_notice(<<"Shutdown after 58 second">>);
                Second == 59 ->
                    ?INFO("Shutdown after 59 second"),
                    lib_conn:send_notice(<<"Shutdown after 59 second">>);
                Second == 60 ->
                    ?INFO("Shutdown after 60 second"),
                    lib_conn:send_notice(<<"Shutdown after 60 second">>);
                true -> 
                    ok
            end,
            Second1 = Second - 1,
            erlang:send_after(1000, self(), {restart, Second1}),
            ready(restart);
        Other ->
            ?INFO("undefined msg: ~w", [Other])
    end.
