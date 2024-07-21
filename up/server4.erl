%% 1.声明接口:模块的导出函数/导入函数
-module(server4).
-export([start/2,rpc/2,swap_code/2]).
%% 2.注册服务:启动起一个服务进程并通知运行时环境
start(Name, Mod)->
    register(Name, spawn(fun()->loop(Name, Mod, Mod:init() end)).
%% 3.适配器: 把过程调用转换成消息请求
swap_code(Name, Request)->
    rpc(Name, {swap_code, Request}).%% 4.服务请求: 消息发送-接收
rpc(Name, Request)->
    Name ! {self(), Request},
    receive
        {Name, crash} -> exit(rpc);
        {Name, ok, Response} -> Response
    end.
%% 5.服务响应:消息接收-发送 
loop(Name, Mod, OldState)->
    receive
        {From, {swap_code, NewCallbackMod} ->
            From ! {Name, ok, ack},
            loop(Name, NewCallbackMod, OldState); %% 调换代码
        {From, Request} ->
            try Mod:handle(Request, OldState) of
                {Response, NewState} ->
                    From ! {Name, ok, Response)
            catch
                _: Why ->
                    log_the_error(Name, Request, Why)
            end
    end.
	
log_the_error(Name, Request, Why) ->
	io:format("server ~p~n Request ~p~n Exception ~p~n",[Name,Request,Why]).