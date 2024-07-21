%%----------------------------------------------------
%% 网络节点服务
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(srv_net).
-behaviour(gen_server).
-export([
        start_link/3
        ,node_list/0
        ,node_list/1
        ,node_info/1
        ,node_cast/4
        ,node_call/4
        ,node_func/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").

-record(state, {
        id
        ,host
        ,port
        ,hidden
    }
).

%% --- 对外接口 ---

%% 新建连接
start_link(Id, Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Id, Host, Port], []).

node_list(all) ->
    ets:tab2list(node).

%% 获取所有战区的列表(不包括当前战区和已经隐藏的战区)
%% 返回:[#zone{} | ...]
node_list() ->
    lists:keydelete(node(), 3, ets:tab2list(node)).

%% 获取指定战区的信息
node_info(NodeId) ->
    case ets:lookup(node, NodeId) of
        [N] -> N;
        _ -> null
    end.

%% 各节点调用
node_cast(Nid, M, F, P) when is_integer(Nid) ->
    node_cast([node_info(Nid)], M, F, P);
node_cast(N, M, F, P) when is_record(N, node) ->
    node_cast([N], M, F, P);
node_cast(N, M, F, P) when is_atom(N) ->
    rpc:cast(N, M, F, P);
node_cast([], _M, _F, _P) -> ok;
node_cast([N|T], undefined, F, P) when is_function(F) ->
    rpc:cast(N#node.name, ?MODULE, node_func, [F, P]),
    node_cast(T, undefined, F, P);
node_cast([N|T], M, F, P) ->
    rpc:cast(N#node.name, M, F, P),
    node_cast(T, M, F, P);
node_cast(Type, M, F, P) ->
    node_cast(node_list(Type), M, F, P).

%% 各节点同步调用
node_call(Nid, M, F, P) when is_integer(Nid) ->
    [R|_] = node_call([node_info(Nid)], M, F, P),
    R;
node_call(N, M, F, P) when is_atom(N) ->
    rpc:call(N, M, F, P);
node_call(NList, undefined, F, P) when is_function(F) ->
    [rpc:call(N#node.name, ?MODULE, node_func, [F, P]) || N <- NList];
node_call(NList, M, F, P) when is_list(NList) ->
    [rpc:call(N#node.name, M, F, P) || N <- NList];
node_call(Type, M, F, P) ->
    node_call(node_list(Type), M, F, P).

node_func(Fun, Args) ->
    apply(Fun, Args).

%% --- 服务器内部实现 ---

init([Id, Host, Port]) ->
    ?INFO("start ~w (~w) ...", [?MODULE, Port]),
    net_kernel:monitor_nodes(true),
    ets:new(node, [{keypos, 2}, named_table, protected, set]),
    ets:insert(node, #node{id = Id, name = node(), host = Host, port = Port}),
    State = #state{id = Id, host = Host, port = Port, hidden = 0},
    {ok, State}.

%% 获取节点ID
handle_call(get_node_id, _From, State) ->
    {reply, State#state.id, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 新节点加入
handle_info({add_node, NodeId, Name, Host, Port}, State) ->
    ?INFO("nodeup(~w):~s", [NodeId, Name]),
    Node = #node{id = NodeId, name = Name, host = Host, port = Port},
    ets:insert(node, Node),
    case State#state.id =:= 0 of
        true -> srv_hall:update_nodes(nodeup, Node);
        false -> skip
    end,
    {noreply, State};

%% 处理新节点加入事件
handle_info({nodeup, Node}, State) ->
    erlang:send({?MODULE, Node}, {add_node, State#state.id, node(), State#state.host, State#state.port}),
    {noreply, State};

%% 处理节点关闭事件
handle_info({nodedown, Node}, State) ->
    case ets:match_object(node, #node{name = Node, _ = '_'}) of
        [N] ->
            ?INFO("nodedown(~w):~s", [N#node.id, N#node.name]),
            ets:match_delete(node, #node{name = Node, _ = '_'}),
            case State#state.id =:= 0 of
                true -> srv_hall:update_nodes(nodedown, N);
                false -> skip
            end;
        _ -> ignore
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---
