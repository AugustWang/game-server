%%----------------------------------------------------
%% 数据库API封装
%% 
%% @author yeahoo2000@gmail.com
%%----------------------------------------------------
-module(db).
-export(
    [
        execute/1
        ,execute/2
        ,execute2/2
        ,spec_execute/2
        ,spec_execute/3
        ,select_limit/3
        ,select_limit/4
        ,get_one/1
        ,get_one/2
        ,spec_get_one/2
        ,get_row/1
        ,get_row/2
        ,spec_get_row/3
        ,get_all/1
        ,get_all/2
        ,spec_get_all/2
        ,get_insert_sql/2
        ,get_insert_sql/3
        ,get_update_sql/3
        ,format_sql/2
        ,last_insert_id/0
        ,insert/3
        ,replace/3
        ,tx/1               %% 事务
    ]
).
-include("common.hrl").

%% 执行一个SQL查询,返回影响的行数
execute(Sql) ->
    case mysql:fetch(cc:get_poll_name(), Sql) of
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.

execute(Sql, Args) when is_atom(Sql) ->
    case mysql:execute(cc:get_poll_name(), Sql, Args) of
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end;
execute(Sql, Args) ->
    Query = format_sql(Sql, Args),
    case mysql:fetch(cc:get_poll_name(), Query) of
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Query, Reason]);
        {error, Reason} -> mysql_halt([Query, Reason])
    end.

%% 执行一个SQl，不返回任何信息
execute2(Sql, Args) ->
    Query = format_sql(Sql, Args),
    try mysql:fetch(cc:get_poll_name(), Query) of
        _Any -> ok
    catch
        _:_ -> error
    end.

spec_execute(Db, Sql) ->
    case mysql:fetch(Db, Sql) of
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.
spec_execute(Db, Sql, Args) ->
    Query = format_sql(Sql, Args),
    case mysql:fetch(Db, Query) of
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Query, Reason]);
        {error, Reason} -> mysql_halt([Query, Reason])
    end.


%% 执行分页查询返回结果中的所有行
select_limit(Sql, Offset, Num) ->
    S = list_to_binary([Sql, <<" limit ">>, integer_to_list(Offset), <<", ">>, integer_to_list(Num)]),
    case mysql:fetch(cc:get_poll_name(), S) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.
select_limit(Sql, Args, Offset, Num) ->
    S = list_to_binary([Sql, <<" limit ">>, list_to_binary(integer_to_list(Offset)), <<", ">>, list_to_binary(integer_to_list(Num))]),
    mysql:prepare(s, S),
    case mysql:execute(cc:get_poll_name(), s, Args) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.

%% 取出查询结果中的第一行第一列
%% 未找到时返回null
get_one(Sql) ->
    case mysql:fetch(cc:get_poll_name(), Sql) of
        {data, {_, _, [], _, _}} -> null;
        {data, {_, _, [[R]], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.
get_one(Sql, Args) when is_atom(Sql) ->
    case mysql:execute(cc:get_poll_name(), Sql, Args) of
        {data, {_, _, [], _, _}} -> null;
        {data, {_, _, [[R]], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end;
get_one(Sql, Args) ->
    Query = format_sql(Sql, Args),
    case mysql:fetch(cc:get_poll_name(), Query) of
        {data, {_, _, [], _, _}} -> null;
        {data, {_, _, [[R]], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Query, Reason]);
        {error, Reason} -> mysql_halt([Query, Reason])
    end.

spec_get_one(Db, Sql) ->
    case mysql:fetch(Db, Sql) of
        {data, {_, _, [], _, _}} -> null;
        {data, {_, _, [[R]], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.

%% 取出查询结果中的第一行
get_row(Sql) ->
    case mysql:fetch(cc:get_poll_name(), Sql) of
        {data, {_, _, [], _, _}} -> [];
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.
get_row(Sql, Args) when is_atom(Sql) ->
    case mysql:execute(cc:get_poll_name(), Sql, Args) of
        {data, {_, _, [], _, _}} -> [];
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end;
get_row(Sql, Args) ->
    Query = format_sql(Sql, Args),
    case mysql:fetch(cc:get_poll_name(), Query) of
        {data, {_, _, [], _, _}} -> [];
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Query, Reason]);
        {error, Reason} -> mysql_halt([Query, Reason])
    end.

spec_get_row(Db, Sql, Args) ->
    Query = format_sql(Sql, Args),
    case mysql:fetch(Db, Query) of
        {data, {_, _, [], _, _}} -> [];
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Query, Reason]);
        {error, Reason} -> mysql_halt([Query, Reason])
    end.

%% 取出查询结果中的所有行
get_all(Sql) ->
    case mysql:fetch(cc:get_poll_name(), Sql) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.
get_all(Sql, Args) when is_atom(Sql) ->
    case mysql:execute(cc:get_poll_name(), Sql, Args) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end;
get_all(Sql, Args) ->
    Query = format_sql(Sql, Args),
    case mysql:fetch(cc:get_poll_name(), Query) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Query, Reason]);
        {error, Reason} -> mysql_halt([Query, Reason])
    end.

spec_get_all(Db, Sql) ->
    case mysql:fetch(Db, Sql) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
        {error, Reason} -> mysql_halt([Sql, Reason])
    end.


%% @doc 显示[人可以看得懂]的错误信息
mysql_halt([Sql, Reason]) ->
    case is_binary(Reason) of
        true ->
            ?WARNING("~n[Database Error]~nQuery:~s~nError:~s", [Sql, Reason]);
        false ->
            ?WARNING("~n[Database Error]~nQuery:~s~nError:~w", [Sql, Reason])
    end,
    {error, Reason}.

%% @spec insert(Table, Keys, Data) -> int()
%% Table = atom()
%% Keys = [atom()]
%% Data = [term()]
%% @doc  插入操作
%% 
insert(_Table, _Keys, []) -> 0;
insert(Table, Keys, Data) ->
    execute(get_insert_sql(Table, Keys, Data)).

%% @spec replace(Table, Keys, Data) -> int()
%% Table = atom()
%% Keys = [atom()]
%% Data = [term()]
%% @doc  替换操作
%% 
%% @equiv replace(Table, Keys, Data, none)
replace(_Table, _Keys, []) -> 0;
replace(Table, Keys, Data) ->
    execute(get_replace_sql(Table, Keys, Data)).

%% 解析生成insert的sql语句
%% @param Table:atom()
%% @param PkeyList:[atom()]
%% @param Data:[{atom(), binary() | int()}]
%% @return binary()
get_insert_sql(Table, Data) ->
    parse_insert_sql(Data, Table, [], []).

%% 解析生成insert的sql语句
%% @param Table:atom()
%% @param Keys:[atom()]
%% @param Data:[[term()]]
%% @return binary()
get_insert_sql(Table, Keys, Data) ->
    <<_:1/binary, KeysBin/binary>> = list_to_binary([[",`", atom_to_list(Key),"`"]|| Key <- Keys]),
    SqlHead = list_to_binary(["INSERT INTO `", atom_to_list(Table), "`(",KeysBin, ") VALUES"]),
    SqlBody = get_insert_sql_f1(Data, []),
    <<SqlHead/binary, SqlBody/binary>>.
    
get_insert_sql_f1([], Res) -> list_to_binary(lists:reverse(Res));
get_insert_sql_f1([Row|T], Res) ->
    EndChar = 
    case T == [] of
    false -> ",";
    true -> ";"
    end,
    SqlBody = list_to_binary(["(~s", lists:duplicate(length(Row)-1, ",~s") ,")", EndChar]),
    get_insert_sql_f1(T, [format_sql(SqlBody, Row)|Res]).

%% 解析生成replace的sql语句
%% @param Table:atom()
%% @param Keys:[atom()]
%% @param Data:[[term()]]
%% @return binary()
get_replace_sql(Table, Keys, Data) ->
    <<_:1/binary, KeysBin/binary>> = list_to_binary([[",`", atom_to_list(Key),"`"]|| Key <- Keys]),
    SqlHead = list_to_binary(["REPLACE INTO `", atom_to_list(Table), "`(",KeysBin, ") VALUES"]),
    SqlBody = get_insert_sql_f1(Data, []),
    <<SqlHead/binary, SqlBody/binary>>.

%% 解析生成update的sql语句
%% @param Table:atom()
%% @param PkeyList:[atom()]
%% @param Data:[{atom(), binary() | int()}]
%% @return binary()
get_update_sql(Table, PKeyList, Data) ->
    parse_update_sql(Data, Table, PKeyList, [], []).


%% 解析生成insert的sql语句
%% @return binary()
parse_insert_sql([], Table, KeyList, VarList) ->
    {_, Keys} = split_binary(list_to_binary(KeyList), 1),
    {_, Vars} = split_binary(list_to_binary(VarList), 1),
    list_to_binary([<<"insert into `">>, erlang:atom_to_binary(Table, utf8), <<"` (">>, Keys, <<") values(">>, Vars, <<");">>]);
%%
parse_insert_sql([{Key, Var}|TData], Table, KeyList, VarList) ->
    K = [<<",`">>, erlang:atom_to_binary(Key, utf8), <<"`">>],
    V = case is_number(Var) of
        true -> [<<",">>, integer_to_list(Var)];
        _ -> [<<",'">>, Var, <<"'">>]
    end,
    parse_insert_sql(TData, Table, [K|KeyList], [V|VarList]).

%% 解析生成update的sql语句
%% @return binary()
parse_update_sql([], Table, _PKeyList, PKVList, KVList) ->
    list_to_binary([<<"update `">>, erlang:atom_to_binary(Table, utf8), <<"` set ">>, KVList, <<" where ">>, PKVList, <<";">>]);
%%
parse_update_sql([{Key, Var}|TData], Table, PKeyList, PKVList, KVList) ->
    V = case is_number(Var) of
        true -> integer_to_list(Var);
        _ -> Var
    end,
    case lists:member(Key, PKeyList) of
        true  ->
            KV = case PKVList of
                [] -> [<<"`">>, erlang:atom_to_binary(Key, utf8), <<"`='">>, V, <<"'">>];
                _  -> [<<"`">>, erlang:atom_to_binary(Key, utf8), <<"`='">>, V, <<"' and ">>]
            end,
            parse_update_sql(TData, Table, PKeyList, [KV|PKVList], KVList);
        false ->
            KV = case KVList of
                [] -> [<<"`">>, erlang:atom_to_binary(Key, utf8), <<"`='">>, V, <<"'">>];
                _  -> [<<"`">>, erlang:atom_to_binary(Key, utf8), <<"`='">>, V, <<"',">>]
            end,
            parse_update_sql(TData, Table, PKeyList, PKVList, [KV|KVList])
    end.

%% 格式化sql语句
%% @param Sql    string() | bitstring()     SQL语句，变量用~s表示
%% @param Args   list()                     变量参数列表
%% @return bitstring()
format_sql(Sql, Args) when is_list(Sql) ->
    S = re:replace(Sql, "\\?", "~s", [global, {return, list}]),
    L = [ mysql:encode(A) || A <- Args],
    list_to_bitstring(io_lib:format(S, L));
format_sql(Sql, Args) when is_bitstring(Sql) ->
    format_sql(bitstring_to_list(Sql), Args).

%% 最后插入的ID（需要结合事务使用）
last_insert_id() ->
    db:get_one("SELECT LAST_INSERT_ID()").

%% 事务
%% @return {ok, Result} | {error, Reason}
tx(Fun) ->
    tx(Fun, undefined).

tx(Fun, Timeout) ->
    case mysql:transaction(cc:get_poll_name(), Fun, Timeout) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, {Reason, {rollback_result, _Result}}} ->
            {error, Reason}
    end.

%% ----------------------------------------------------
