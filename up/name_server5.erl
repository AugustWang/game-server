-module(name_server5).
-export([init/0, add/2, whereis/1, handle/2]).
-import(server5,[rpc/2]).

init() -> dict:new().

add(Key, Value) -> rpc(name_server, {add, Key, Value}).
whereis(Key)    -> rpc(name_server, {whereis,Key}).

handle({add,Key,Value}, Dict) -> {ok, dict:store(Key,Value,Dict)};
handle({whereis, Key}, Dict)  -> {dict:find(Key,Dict),Dict}.