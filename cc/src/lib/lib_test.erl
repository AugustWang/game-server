%%----------------------------------------------------
%% 测试 
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_test).
-compile(export_all).

-include("common.hrl").

gm(<<>>, _Rs) -> {ok};
gm(Bin, Rs) when is_binary(Bin) ->
    String = erlang:bitstring_to_list(Bin),
    gm(String, Rs);
gm(String, Rs) ->
    ?INFO("Recv command:~s", [String]),
    Tokens = string:tokens(String, " "),
    command(Tokens, Rs).

command(["买装备", Num1], Rs) ->
    Num = list_to_integer(Num1),
    F = fun(_) ->
            Tids = [110001, 140001, 150001, 160001],
            Tid = util:rand_element(Tids),
            lib_item:add_item(Tid, Rs#role.pid_sender)
    end,
    util:for(1, Num, F),
    {ok};

command(["买道具", Num1], Rs) ->
    Num = list_to_integer(Num1),
    F = fun(_) ->
            Tids = [120001, 120002, 120003, 120004, 120005, 120006, 120007, 120008, 120009, 120010, 120011, 120012, 130001, 130002, 170001],
            Tid = util:rand_element(Tids),
            lib_item:add_item(Tid, Rs#role.pid_sender)
    end,
    util:for(1, Num, F),
    {ok};

command(["买物品", Tid1, Num1], Rs) ->
    Tid = list_to_integer(Tid1),
    Num = list_to_integer(Num1),
    lib_item:test_add_item(Tid, Num, Rs#role.pid_sender),
    {ok};

command(["加金币", Num1], Rs) ->
    Num = list_to_integer(Num1),
    catch Rs#role.pid ! {add_attr, gold, Num},
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
    {ok};
command(["减金币", Num1], Rs) ->
    Num2 = list_to_integer(Num1),
    Num = case Num2 > Rs#role.gold of
        true -> Rs#role.gold;
        false -> Num2
    end,
    case lib_role:spend(gold, Num, Rs) of
        {ok, Rs1} -> 
            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
            {ok, Rs1};
        _ -> {ok}
    end;

command(["加点券", Num1], Rs) ->
    Num = list_to_integer(Num1),
    catch Rs#role.pid ! {add_attr, card, Num},
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
    {ok};
command(["减点券", Num1], Rs) ->
    Num2 = list_to_integer(Num1),
    Num = case Num2 > Rs#role.card of
        true -> Rs#role.card;
        false -> Num2
    end,
    case lib_role:spend(card, Num, Rs) of
        {ok, Rs1} -> 
            lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
            {ok, Rs1};
        _ -> {ok}
    end;

command(["加经验", Num1], Rs) ->
    Num = list_to_integer(Num1),
    Rs1 = lib_role:add_exp(Rs, Num),
    lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
    {ok, Rs1};

command(["加贡献", Num1], Rs) ->
    Num = list_to_integer(Num1),
    gen_server:cast(srv_guild, {add_exp, Rs#role.guild_id, Rs#role.id, Num}),
    Rs1 = Rs#role{guild_v = Rs#role.guild_v + Num},
    {ok, Rs1};

command(["掉物品", Num1], Rs) ->
    Num = list_to_integer(Num1),
    catch Rs#role.pid_room ! {fall_item, Num, Rs#role.id},
    {ok};

command(["清怪"], Rs) ->
    catch Rs#role.pid_room ! {test_hit_npc, Rs#role.id},
    {ok};

command(["清空背包"], _Rs) ->
    lib_item:clear_myitems(),
    {ok};

command(["接任务", C1], _Rs) ->
    C = list_to_integer(C1),
    self() ! {handle_event, 6090, [C]},
    {ok};

command(["task", C1], _Rs) ->
    C = list_to_integer(C1),
    self() ! {task, C},
    {ok};

command(["task", C1, A1], _Rs) ->
    C = list_to_integer(C1),
    A = list_to_integer(A1),
    self() ! {task, C, {add, A}},
    {ok};

command(["task_to", C1, A1], _Rs) ->
    C = list_to_integer(C1),
    A = list_to_integer(A1),
    self() ! {task, C, {to, A}},
    {ok};

command(["task_set_to", C1, A1], _Rs) ->
    C = list_to_integer(C1),
    A = list_to_integer(A1),
    self() ! {task, C, {set_to, A}},
    {ok};

command(["开启公会战", C1], _Rs) ->
    C = list_to_integer(C1),
    srv_guild:start_act(C),
    {ok};

command(["role_state"], Rs) ->
    ?INFO("Role State:\n
           id............. ~p
           account_id..... ~p
           room_id........ ~p
           room_type...... ~p
           lev............ ~p
           exp............ ~p
           exp_max........ ~p
           gold........... ~p
           card........... ~p
           status......... ~p
           score.......... ~p
           ticket......... ~p
           hp............. ~p
           hp_max......... ~p
           dmg............ ~p
           crit........... ~p
           dmg_speed...... ~p
           move_speed..... ~p
           port........... ~p
           sex............ ~p
           pid............ ~p
           pid_conn....... ~p
           pid_sender..... ~p
           pid_room....... ~p
           socket......... ~p
           ip............. ~p
           name........... ~p
           game_count..... ~p
           task_tail...... ~p
           enable_dtask... ~p
           dtask_time..... ~p
           save........... ~p
           online_reward.. ~p
           sign_reward.... ~p
        ",
        [
            Rs#role.id           
            ,Rs#role.account_id  
            ,Rs#role.room_id     
            ,Rs#role.room_type   
            ,Rs#role.lev         
            ,Rs#role.exp         
            ,Rs#role.exp_max     
            ,Rs#role.gold        
            ,Rs#role.card        
            ,Rs#role.status      
            ,Rs#role.score       
            ,Rs#role.ticket      
            ,Rs#role.hp          
            ,Rs#role.hp_max      
            ,Rs#role.dmg         
            ,Rs#role.crit
            ,Rs#role.dmg_speed   
            ,Rs#role.move_speed  
            ,Rs#role.port        
            ,Rs#role.sex         
            ,Rs#role.pid         
            ,Rs#role.pid_conn    
            ,Rs#role.pid_sender  
            ,Rs#role.pid_room    
            ,Rs#role.socket      
            ,Rs#role.ip          
            ,Rs#role.name        
            ,Rs#role.game_count  
            ,Rs#role.task_tail   
            ,Rs#role.enable_dtask
            ,Rs#role.dtask_time  
            ,Rs#role.save        
            ,Rs#role.online_reward        
            ,Rs#role.sign_reward        
        ]),
    {ok};

command(["test_role"], Rs) ->
    %% Rs#role.pid_room ! test,
    Rs#role.pid ! test,
    {ok};

command(["test_sender"], Rs) ->
    %% Rs#role.pid_room ! test,
    Rs#role.pid_sender ! test,
    {ok};

command(["test_room"], Rs) ->
    catch Rs#role.pid_room ! test,
    {ok};

command(["mytasks"], _Rs) ->
    ?INFO("mytasks:\n~p\n", [lib_task:get_mytasks()]),
    {ok};

command(["myitems"], _Rs) ->
    ?INFO("myitems:\n~p\n", [lib_item:get_myitems()]),
    {ok};

command(["cmd", CmdList], Rs) ->
    command(["cmd", CmdList, ""], Rs);

command(["cmd", CmdList, ArgList], Rs) ->
    Cmd = list_to_integer(CmdList),
    Data = case util:string_to_term(ArgList) of
        undefined -> [];
        A -> A
    end,
    %% ArgBin = list_to_binary(ArgList),
    %% {ok, Data} = unpack:p(Cmd, ArgBin),
    Rt = srv_role:routing(Cmd, Data, Rs),
    case Rt of
        {noreply, Rs1} when is_record(Rs1, role) -> 
            ?INFO("noreply"),
            {ok, Rs1};
        {Reply, Rs1} when is_record(Rs1, role) -> 
            ?INFO("Routing Reply:~p", [Reply]),
            {ok, Rs1};
        {ok, Reply, Rs} ->
            lib_conn:pack_send(Rs#role.pid_sender, Cmd, Reply),
            ?INFO("Routing Reply:~p", [Reply]),
            {ok};
        {ok, Reply} when is_list(Reply) -> 
            lib_conn:pack_send(Rs#role.pid_sender, Cmd, Reply),
            ?INFO("Routing Reply:~p", [Reply]),
            {ok};
        {ok, NewRs} ->
            ?INFO("Routing NewRs:~p", [NewRs]),
            {ok, NewRs};
        {ok, Reply, NewRs} ->
            lib_conn:pack_send(Rs#role.pid_sender, Cmd, Reply),
            ?INFO("Routing Reply:~p, NewRs:~p", [Reply, NewRs]),
            {ok, NewRs};
        _O -> 
            ?INFO("Routing Reply _O:~p", [_O]),
            {ok}
    end;

command(Tokens, _Rs) ->
    ?INFO("未知命令:~w", [Tokens]),
    {ok}.

cmd(Cmd, Data) ->
    [#online{pid = Pid} | _] = ets:tab2list(online),
    gen_server:cast(Pid, {handle_event, Cmd, Data}),
    ok.

gm_list() ->
    "
$买装备 N (N为数量，下同)
$买道具 N
$清空背包
$买物品 ID Num
$加金币 N
$加点券 N
$加经验 N
$task Tid
$掉物品 ID
$开启公会战 N (N为活动时间，分钟)
$接任务 ID
$清怪   (清除副本中的怪)

--------------------------
        ID     | Name     
--------------------------
$掉物品 120001 | 加速     
$掉物品 120002 | 分数翻倍 
$掉物品 120003 | 隐身衣   
$掉物品 120004 | 横向力量 
$掉物品 120005 | 纵向力量 
$掉物品 120006 | 疯狂面具 
$掉物品 120007 | 大力丸   
$掉物品 120008 | 血瓶     
$掉物品 120009 | 蜘蛛     
$掉物品 120010 | 恶魔     
$掉物品 120011 | 无敌     
$掉物品 120012 | 瞬移     
$掉物品 120013 | 蜘蛛网   
$掉物品 120014 | 高级加速 
$掉物品 120015 | 地雷A    
$掉物品 120016 | 地雷B    
$掉物品 120017 | 全体攻击 
$掉物品 120018 | 弹簧 
$掉物品 120019 | 淨化水
$掉物品 120020 | 章魚
$掉物品 120021 | 黑章魚
$掉物品 120022 | 護盾
--------------------------
     Tid | Name    
--------------------
$task        [任务类型]       | 任务进度增加1
$task        [任务类型] [N]   | 任务进度增加N
$task_to     [任务类型] [值]  | 如升级到5级：$task_to 1 5
$task_set_to [任务类型] [值]  | 设置进度

$接任务 ID

$task 1  | 升到x级
$task 2  | 积分战获胜x场
$task 3  | 参加x场积分战
$task 4  | 杀死怪x只
$task 5  | 强化x到y级
$task 6  | 合成x个y物品
$task 7  | 購買x物品y個
$task 8  | 参加竞技战x场
$task 9  | 竞技战获胜x场
$task 10 | 使用xx道具y次
$task 11 | 參觀xx建築y次
$task 12 | 使用xx技能y次
$task 13 | 穿上xx裝備y次
$task 14 | 升级一个技能到x级
$task 15 | 积分战斗中击杀x人
$task 16 | 对战模式中击杀x人
$task 17 | 強化任意裝備到x級
$task 18 | 加入或創建一個公會
$task 19 | 添加x個好友
$task 20 | 与异性玩家一起战斗x场
$task 21 | 杀死x个敌人
$task 22 | 连胜x场
$task 23 | 弹跳的瞬间使用弹簧
$task 24 | 充值x点券
$task 25 | 分享心情
$task 26 | 与公会成员一起战斗x场
$task 27 | 与人组队一起战斗x场
-------------------
ok".

%%% vim: set foldmethod=marker foldmarker=%%',%%.:
