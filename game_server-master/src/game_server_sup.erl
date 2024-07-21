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


-module(game_server_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    GamePort = case application:get_env(game_server, game_port) of
                   undefined -> 5555;
                   {ok, GPort} -> GPort
               end,
    APIPort = case application:get_env(game_server, api_port) of
                  undefined -> 6666;
                  {ok, APort} -> APort
              end,
    error_logger:info_msg("GamePort: ~p, APIPort: ~p~n", [GamePort, APIPort]),
    RanchSupSpec = ?CHILD(ranch_sup, ranch_sup, supervisor, []),
    ListenerSpec = ranch:child_spec(ranch_tcp_listener, 8,
        ranch_tcp, [{port, GamePort}, 
                    {max_connections, infinity},
                    {keepalive, true},
                    {send_timeout, 3000}
                   ],
        game_connection, []
    ),
    ApiServerSpec = ranch:child_spec(api_tcp_listener, 1,
        ranch_tcp, [{port, APIPort}], api_connection, []
    ),
    GameServerSpec = ?CHILD(game_server, game_server, worker, []),
    RedisPoolSupSpec = ?CHILD(redis_pool_sup, redis_pool_sup, supervisor, []),
    NameServerSupSpec = ?CHILD(name_server_sup, name_server_sup, supervisor, []),
    IapServerSupSpec = ?CHILD(iap_server_sup, iap_server_sup, supervisor, []),
    HttpWorkerSpec = ?CHILD(http, http, worker, []),
    Specs = [GameServerSpec, RanchSupSpec, ListenerSpec, 
             ApiServerSpec, RedisPoolSupSpec, NameServerSupSpec,
             IapServerSupSpec, HttpWorkerSpec],
    {ok, {{one_for_one, 10, 10}, Specs}}.
