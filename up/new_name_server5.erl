-module(new_name_server5).
-export([add/2, whereis/1, handle/2, init/0, delete/1, all_names/0]).
-import(server5, [rpc/2]).

init() -> dict:new().

add(Key, Value) -> rpc(name_server,{add,Key,Value}).
whereis(Key)    -> rpc(name_server,{whereis, Key}).
delete(Key)     -> rpc(name_server,{delete, Key}).
all_names()     -> rpc(name_server,{all_names()}).

handle({add, Key, Value}, Dict) -> {ok, dict:store(Key, Value, Dict)};
handle({whereis, Key}, Dict)    -> {dict:find(Key,Dict), Dict};
handle({delete, Key}, Dict)     -> {dict:erase(Key,Dict), Dict};
handle(all_names,Dict)          -> {dict:fetch_keys(Dict), Dict}.