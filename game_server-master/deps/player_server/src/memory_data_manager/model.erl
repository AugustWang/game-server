%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2024
%% Savin Max <mafei.198@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(model).
-export([update/2,
         update/1,
         find/1,
         find/2,
         find_or_create/1,
         all/1,
         all/2,
         id_status_list/1,
         count_all/1,
         where/1,
         delete/1,
         create/1,
         create/2,
         count/1,
         sql/2,
         get_persist_all_sql/0,
         persist_all/0]).

-define(MODEL_ORIGIN, 1).
-define(MODEL_UPDATE, 2).
-define(MODEL_DELETE, 3).
-define(MODEL_CREATE, 4).

find(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case record_mapper:get_field(Selector, uuid) of
        undefined -> selectOne(Table, Values);
        Id -> get({Table, Id})
    end.

find(Table, Id) ->
    ensure_load_data(Table),
    get({Table, Id}).

find_or_create(Selector) ->
    case find(Selector) of
        undefined -> create(Selector);
        Rec -> Rec
    end.

count_all(Table) ->
    ensure_load_data(Table),
    length(id_status_list(Table)).

all(Table) ->
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            lists:foldl(fun
                ({Id, _}, Result) ->
                    case get({Table, Id}) of
                        undefined -> Result;
                        Rec -> [Rec|Result]
                    end
            end, [], IdList)
    end.

all(Table, {Offset, Limit}) ->
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            SubIdList = lists:sublist(IdList, Offset, Limit),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    case get({Table, Id}) of
                        undefined -> Result;
                        Rec -> [Rec|Result]
                    end
            end, [], SubIdList)
    end.

where(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            Fields = record_mapper:get_mapping(Table),
            FieldsAndValues = makepat(Fields, Values),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  -> [Rec|Result];
                        false -> Result
                    end
            end, [], IdList)
    end.

count(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    Fields = record_mapper:get_mapping(Table),
    case makepat(Fields, Values) of
        [] -> length(id_status_list(Table));
        FieldsAndValues ->
            case id_status_list(Table) of
                [] -> 0;
                IdList ->
                    lists:foldl(fun
                        ({Id, _}, Result) ->
                            Rec = get({Table, Id}),
                            case match(Rec, FieldsAndValues) of
                                true  -> Result + 1;
                                false -> Result
                            end
                    end, 0, IdList)
            end
    end.

update(Record) ->
    [Table|_] = tuple_to_list(Record),
    Id = record_mapper:get_field(Record, uuid),
    case get({Table, Id}) of
        undefined -> ok;
        _ ->
            update_status(Table, Id, ?MODEL_UPDATE),
            put({Table, Id}, Record),
            Record
    end.

update(Selector, Modifier) ->
    [Table|_] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case record_mapper:get_field(Selector, uuid) of
        undefined -> match_update(Table, Selector, Modifier);
        Id -> update_by_key(Table, Id, Modifier)
    end.

update_by_key(Table, Id, Modifier) ->
    case get({Table, Id}) of
        undefined -> ok;
        Rec ->
            update_status(Table, Id, ?MODEL_UPDATE),
            NewRec = update_record(Rec, Modifier),
            put({Table, Id}, NewRec),
            NewRec
    end.

match_update(Table, Selector, Modifier) ->
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            Fields = record_mapper:get_mapping(Table),
            [_|Values] = tuple_to_list(Selector),
            FieldsAndValues = makepat(Fields, Values),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  ->
                            update_status(Table, Id, ?MODEL_UPDATE),
                            NewRec = update_record(Rec, Modifier),
                            put({Table, Id}, NewRec),
                            [NewRec|Result];
                        false -> 
                            Result
                    end
            end, [], IdList)
    end.

delete(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case record_mapper:get_field(Selector, uuid) of
        undefined ->
            Fields = record_mapper:get_mapping(Table),
            FieldsAndValues = makepat(Fields, Values),
            match_delete(Table, FieldsAndValues);
        Id ->
            update_status(Table, Id, ?MODEL_DELETE),
            erase({Table, Id})
    end.

match_delete(Table, FieldsAndValues) ->
    case id_status_list(Table) of
        [] -> ok;
        IdList ->
            lists:foreach(fun
                ({Id, _}) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  ->
                            update_status(Table, Id, ?MODEL_DELETE),
                            erase({Table, Id});
                        false -> undefined
                    end
            end, IdList),
            ok
    end.

%% create new record.
create(Records) when is_list(Records) ->
    lists:foreach(fun create/1, Records);
create(Record) ->
    {RecWithId, Id} = case record_mapper:get_field(Record, uuid) of
        undefined ->
            NewId = uuid_factory:gen(),
            {record_mapper:set_field(Record, uuid, NewId), NewId};
        Uuid -> {Record, Uuid}
    end,
    [Table|_Values] = tuple_to_list(RecWithId),
    ensure_load_data(Table),
    update_status(Table, Id, ?MODEL_CREATE),
    put({Table, Id}, RecWithId),
    RecWithId.

%% Load data from databse.
create(Record, load) ->
    [Table|_] = tuple_to_list(Record),
    Id = record_mapper:get_field(Record, uuid),
    update_status(Table, Id, ?MODEL_ORIGIN),
    put({Table, Id}, Record).

generate_persist_sql(Table) ->
    StatusIdList = id_status_list(Table),
    DeleteIdList = delete_status_list(Table),
    if
        StatusIdList =:= [] andalso DeleteIdList =:= [] -> <<>>;
        true ->
            case sqls(Table, StatusIdList ++ DeleteIdList) of
                [] -> <<>>;
                Sqls -> binary_string:join(Sqls, <<";">>)
            end
    end.

persist_all() ->
    try do_persist_all() of
        Result -> 
            Tables = all_loaded_tables(),
            reset_tables_status(Tables),
            Result
    catch
        Type:Msg ->
            exception:notify(Type, Msg)
    end.

do_persist_all() ->
    case get_persist_all_sql() of
        <<>> -> do_nothing;
        JoinedSql -> 
            execute_with_procedure(JoinedSql),
            JoinedSql
    end.

get_persist_all_sql() ->
    Tables = all_loaded_tables(),
    PlayerID = get(player_id),
    logger:info("PERSIST FOR: ~p~n", [PlayerID]),
    Sqls = lists:foldl(fun(Table, Result) ->
        case generate_persist_sql(Table) of
            <<>> -> Result;
            Sql -> 
                logger:info("[~p] Table: ~p SQL: ~p~n", [PlayerID, Table, Sql]),
                [Sql|Result]
        end
    end, [], Tables),
    binary_string:join(Sqls, <<";">>).

reset_tables_status(Tables) ->
    lists:foreach(fun reset_status/1, Tables).

reset_status(Table) ->
    put({Table, deleteIdList}, []),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            NewIdList = lists:foldl(fun
                            ({Id, _}, Result) ->
                                [{Id, ?MODEL_ORIGIN}|Result]
                        end, [], IdList),
            put({Table, idList}, NewIdList)
    end.

execute_with_procedure(Sql) ->
    ProcedureName = db:procedure_name(<<"player">>, get(player_id)),
    % logger:info("PERSIST FOR [~p] Sql: ~p~n", [get(player_id), Sql]),
    db:execute_with_procedure(ProcedureName, Sql).

%% Private Methods
selectOne(Table, Values) ->
    Fields = record_mapper:get_mapping(Table),
    case id_status_list(Table) of
        [] -> undefined;
        IdList ->
            FieldsAndValues = makepat(Fields, Values),
            selectOne(Table, IdList, FieldsAndValues)
    end.

selectOne(_, [], _) ->
    undefined;
selectOne(Table, [{Id, _}|IdList], FieldsAndValues) ->
    Rec = get({Table, Id}),
    case match(Rec, FieldsAndValues) of
        true -> Rec;
        false -> selectOne(Table, IdList, FieldsAndValues)
    end.

update_status(Table, Id, Status) ->
    IdList = id_status_list(Table),
    case lists:keyfind(Id, 1, IdList) of
        false ->
            if 
                Status =:= ?MODEL_DELETE -> ok;
                Status =:= ?MODEL_ORIGIN orelse Status =:= ?MODEL_CREATE ->
                    put({Table, idList}, [{Id, Status}|IdList])
            end;
        {Id, ?MODEL_CREATE} ->
            if 
                Status =:= ?MODEL_UPDATE -> ok;
                Status =:= ?MODEL_DELETE ->
                    NewIdList = lists:delete({Id, ?MODEL_CREATE}, IdList),
                    put({Table, idList}, NewIdList)
            end;
        {Id, ?MODEL_UPDATE} ->
            if 
                Status =:= ?MODEL_UPDATE -> ok;
                Status =:= ?MODEL_DELETE ->
                    NewIdList = lists:delete({Id, ?MODEL_UPDATE}, IdList),
                    put({Table, idList}, NewIdList),
                    add_to_delete_list(Table, Id)
            end;
        {Id, ?MODEL_ORIGIN} ->
            if 
                Status =:= ?MODEL_UPDATE ->
                    NewIdList = lists:delete({Id, ?MODEL_ORIGIN}, IdList),
                    put({Table, idList}, [{Id, Status}|NewIdList]);
                Status =:= ?MODEL_DELETE ->
                    NewIdList = lists:delete({Id, ?MODEL_ORIGIN}, IdList),
                    put({Table, idList}, NewIdList),
                    add_to_delete_list(Table, Id)
            end
    end.

makepat(Fields, Values) ->
    makepat(Fields, Values, []).

makepat([], [], Result) ->
    lists:reverse(Result);
makepat([_Field|Fields], [Value|Values], Result) when Value =:= undefined ->
    makepat(Fields, Values, Result);
makepat([Field|Fields], [Value|Values], Result) ->
    makepat(Fields, Values, [{Field, Value}|Result]).

match(Record, FieldsAndValues) ->
    [RecordName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(RecordName),
    match(Fields, Values, FieldsAndValues).

match(_, _, []) ->
    true;
match([], [], _) ->
    false;
match([Field|Fields], [Value|Values], [{Field, Value}|FieldsAndValues]) ->
    match(Fields, Values, FieldsAndValues);
match([_Field|Fields], [_Value|Values], FieldsAndValues) ->
    match(Fields, Values, FieldsAndValues).

ensure_load_data(Table) ->
    case is_table_loaded(Table) of
        true  -> true;
        false ->
            PlayerID = get(player_id),
            Module = list_to_atom(atom_to_list(Table) ++ "_model"),
            case Module:load_data(PlayerID) of
                {ok, []} -> undefined;
                {ok, Recs} -> insert_recs(Recs, Module)
            end,
            record_loaded_table(Table),
            case erlang:function_exported(Module, after_load_data, 1) of
                true -> Module:after_load_data(PlayerID);
                false -> ok
            end
    end.

is_table_loaded(Table) ->
    case get({loaded, Table}) of
        true -> true;
        _ -> false
    end.

record_loaded_table(Table) ->
    put({loaded, Table}, true),
    case get({loaded, tables}) of
        undefined ->
            put({loaded, tables}, [Table]);
        Tables ->
            case lists:member(Table, Tables) of
                true -> ok;
                false -> put({loaded, tables}, [Table|Tables])
            end
    end.

all_loaded_tables() ->
    case get({loaded, tables}) of
        undefined -> [];
        Tables when is_list(Tables) -> Tables
    end.

id_status_list(Table) ->
    case get({Table, idList}) of
        undefined -> [];
        IdList -> IdList
    end.

delete_status_list(Table) ->
    case get({Table, deleteIdList}) of
        undefined -> [];
        IdList -> IdList
    end.

sqls(Table, IdList) ->
    lists:foldl(fun
        ({Id, Status}, Result) ->
            if
                Status =:= ?MODEL_ORIGIN ->
                    Result;
                Status =:= ?MODEL_CREATE orelse Status =:= ?MODEL_UPDATE ->
                    Rec = get({Table, Id}),
                    [sql(Rec, Status)|Result];
                Status =:= ?MODEL_DELETE ->
                    [sql({Table, Id}, Status)|Result]
            end
    end, [], IdList).

sql(Rec, ?MODEL_CREATE) ->
    {Table, Fields, Values} = rec_info(Rec),
    db_fmt:format("INSERT INTO `~s` (~s) VALUES (~s)", 
                  [Table, join_fields(Fields), join_values(Values)]);
sql(Rec, ?MODEL_UPDATE) ->
    {Table, Fields, Values} = rec_info(Rec),
    Uuid = record_mapper:get_field(Rec, uuid),
    db_fmt:format("UPDATE `~s` SET ~s WHERE `uuid` = ~s", 
                  [Table, db_fmt:map(Fields, Values), db_fmt:encode(Uuid)]);
sql({Table, Uuid}, ?MODEL_DELETE) ->
    db_fmt:format("DELETE FROM `~s` WHERE `uuid` = ~s", 
                  [Table, db_fmt:encode(Uuid)]).

rec_info(Rec) ->
    [Table|Values] = tuple_to_list(Rec),
    Fields = record_mapper:get_mapping(Table),
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    Rule = proplists:get_value(serialize, Module:module_info(attributes)),
    SerializedValues = serialize(Values, Fields, Rule),
    {Table, Fields, SerializedValues}.

add_to_delete_list(Table, Id) ->
    List = get({Table, deleteIdList}),
    if
        List =:= undefined orelse List =:= [] ->
            put({Table, deleteIdList}, [{Id, ?MODEL_DELETE}]);
        true ->
            put({Table, deleteIdList}, [{Id, ?MODEL_DELETE}|List])
    end.

update_record(Record, Modifier) ->
    [Table|RecordValues] = tuple_to_list(Record),
    [_|ModifierValues] = tuple_to_list(Modifier),
    Values = update_record(RecordValues, ModifierValues, []),
    list_to_tuple([Table|Values]).

update_record([], [], Result) ->
    lists:reverse(Result);
update_record([RecordValue|RecordValues], [ModifierValue|ModifierValues], Result) when ModifierValue =:= undefined ->
    update_record(RecordValues, ModifierValues, [RecordValue|Result]);
update_record([_RecordValue|RecordValues], [ModifierValue|ModifierValues], Result)->
    update_record(RecordValues, ModifierValues, [ModifierValue|Result]).

insert_recs(Recs, Module) ->
    case lists:keyfind(serialize, 1, Module:module_info(attributes)) of
        false ->
            lists:foreach(fun(Rec) -> create(Rec, load) end, Recs);
        {serialize, Rule} ->
            Fields = record_mapper:get_mapping(hd(tuple_to_list(hd(Recs)))),
            lists:foreach(fun(Rec) -> 
                NewRec = deserialize(Rec, Fields, Rule),
                create(NewRec, load) 
            end, Recs)
    end.

deserialize(Rec, Fields, Rule) ->
    [RecName|Values] = tuple_to_list(Rec),
    TermValues = deserialize(Values, Fields, Rule, []),
    list_to_tuple([RecName|TermValues]).

deserialize([], [], _Rule, Result) -> 
    lists:reverse(Result);
deserialize([Value|Values], [Field|Fields], Rule, Result) ->
    case lists:member(Field, Rule) of
        true when Value =/= undefined -> 
            TermValue = case base64:decode(Value) of
                <<>> -> undefined;
                Data -> binary_to_term(Data)
            end,
            deserialize(Values, Fields, Rule, [TermValue|Result]);
        _ ->
            deserialize(Values, Fields, Rule, [Value|Result])
    end.


serialize(Values, _Fields, undefined) -> Values;
serialize(Values, Fields, Rule) ->
    serialize(Values, Fields, Rule, []).

serialize([], [], _Rule, Result) ->
    lists:reverse(Result);
serialize([Value|Values], [Field|Fields], Rule, Result) ->
    case proplists:get_bool(Field, Rule) of
        true when Value =/= undefined -> 
            Data = term_to_binary(Value),
            SerializedValue = base64:encode(Data),
            serialize(Values, Fields, Rule, [SerializedValue|Result]);
        _ ->
            serialize(Values, Fields, Rule, [Value|Result])
    end.

join_fields(Fields) ->
    join_fields(Fields, []).
join_fields([], Result) ->
    binary_string:join(lists:reverse(Result), <<", ">>);
join_fields([Field|Fields], Result) ->
    NewField = list_to_binary([<<"`">>, atom_to_binary(Field, utf8), <<"`">>]),
    join_fields(Fields, [NewField|Result]).

join_values(Values) ->
    join_values(Values, []).
join_values([], Result) ->
    binary_string:join(lists:reverse(Result), <<", ">>);
join_values([Value|Values], Result) -> 
    join_values(Values, [db_fmt:encode(Value)|Result]).
