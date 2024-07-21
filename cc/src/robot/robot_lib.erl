%%----------------------------------------------------
%% Test 
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(robot_lib).
-compile(export_all).

-include("common.hrl").
-include("rs.hrl").

set_stop() ->
    set_stop(util:rand(100, 180)).
set_stop(Time) ->
    cancel_stop(),
    StopRef = erlang:send_after(Time * 1000, self(), stop),
    put(stop_ref, StopRef).

cancel_stop() ->
    case get(stop_ref) of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end.

%% 进程回调，返回新的Rs
walk([Act, Dir, Type, X, Y], Rs) ->
    %% ?INFO("walk:[Act:~w, Dir:~w, Type:~w, X:~w, Y:~w]", [Act, Dir, Type, X, Y]),
    S = Rs#rs.pid_sender,
    X1 = X * 50 + 25,
    Y1 = Y * 36,
    case Type of
        98 -> 
            S ! {cmd, 11003, [Act, Dir, Type, 0, X1, Y1]},
            ok;
        3  -> 
            S ! {cmd, 11003, [Act, Dir, 3, 0, X1, Y1]},
            S ! {cmd, 11003, [Act, Dir, 98, 0, X1, Y1]},
            ok;
        2  -> 
            S ! {cmd, 11003, [Act, Dir, 98, 0, X1, Y1]},
            S ! {cmd, 11003, [Act, Dir, 2, 0, X1, Y1]},
            ok;
        _ -> 
            ?INFO("unexpected type: ~w", [Type])
    end,
    Rs.

add_daoju(ItemId, Tid) ->
    Daoju = case get(daoju) of
        undefined -> [];
        D -> D
    end,
    put(daoju, [{ItemId, Tid} | Daoju]).

use_daoju(Tid, Rs) ->
    Daoju = case get(daoju) of
        undefined -> [];
        D -> D
    end,
    case lists:keyfind(Tid, 2, Daoju) of
        false -> ok;
        {ItemId, _} ->
            Rs#rs.pid_sender ! {cmd, 17003, [ItemId]},
            put(daoju, lists:keydelete(ItemId, 1, Daoju))
    end.

%% count_robots() ->
%%     {ok, [Start, _End]} = application:get_env(cc, robot_accounts),
%%     End = length(get_names()) + Start - 1,
%%     db:get_one("select count(*) from role where account_id >= ~s and account_id <= ~s", [Start, End]).

%%' robot ids
%% get_ids() ->
%%     {ok, [Start, _End]} = application:get_env(cc, robot_accounts),
%%     End = length(get_names()) + Start - 1,
%%     lists:seq(Start, End).
%%.
%%' role name

get_name(AccId) ->
    Nth = case get(name_index) of
        undefined -> 
            put(name_index, 1),
            1;
        N -> 
            put(name_index, N + 1),
            N + 1
    end,
    Sex = case util:rand(1, 3) of
        1 -> 1;
        _ -> 2
    end,
    Name = if
        Nth =< 3 ->
            util:rand_element(name_list());
        Nth =< 5 ->
            util:rand_element(name_list()) ++ integer_to_list(util:rand(1, 9));
        Nth =< 7 ->
            util:rand_element(name_list()) ++ integer_to_list(util:rand(10, 99));
        Nth =< 8 ->
            "Ac" ++ integer_to_list(AccId);
        true -> integer_to_list(util:unixtime())
    end,
    B = list_to_binary(Name),
    {Sex, B}.

%% get_name(test, AccId) ->
%%     L = "AC" ++ integer_to_list(AccId) ++ "-" ++ integer_to_list(util:rand(100, 999)),
%%     B = list_to_binary(L),
%%     {util:rand(1, 2), B}.

%% 游戏名字（181个）
%% get_names() -> 
%%     [
%%         {1, "做迩旳天"}
%%         ,{2, "自生自滅"}
%%         ,{2, "謊話一個"}
%%         ,{2, "紟苼約錠"}
%%         ,{2, "淡寫莪滴噯"}
%%         ,{2, "妞給爺啵一個"}
%%         ,{2, "夜還昰照樣黑"}
%%         ,{2, "酒醒看清一切"}
%%         ,{2, "等你在網路"}
%%         ,{2, "呐份愛已卟在"}
%%         ,{2, "獨角戲"}
%%         ,{2, "雖痛猶唱"}
%%         ,{2, "傳統男人"}
%%         ,{2, "人在囧途"}
%%         ,{2, "颩雨過後"}
%%         ,{2, "潶咖啡苫澀"}
%%         ,{2, "空思落地"}
%%         ,{2, "蔙葎"}
%%         ,{2, "無苛"}
%%         ,{2, "流著淚的筆尖"}
%%         ,{2, "眷戀"}
%%         ,{2, "謀侽孓"}
%%         ,{2, "未聞花名"}
%%         ,{2, "滴血的心"}
%%         ,{2, "慢慢淡忘"}
%%         ,{2, "迷失在愛途"}
%%         ,{2, "四國之家"}
%%         ,{2, "Over"}
%%         ,{2, "受傷的男人"}
%%         ,{2, "信念執著"}
%%         ,{2, "此人心已死"}
%%         ,{2, "愛情睡醒了"}
%%         ,{2, "陌生"}
%%         ,{2, "為你傾盡一世"}
%%         ,{2, "如果愛忘了"}
%%         ,{2, "誰的錯"}
%%         ,{2, "無言以對"}
%%         ,{2, "你旳笑很巴黎"}
%%         ,{2, "我們就此完結"}
%%         ,{2, "何必再用情"}
%%         ,{2, "複習那回憶"}
%%         ,{2, "你是我的信仰"}
%%         ,{2, "昂貴"}
%%         ,{2, "聰明的笨蛋"}
%%         ,{2, "鳳之夢幻"}
%%         ,{2, "笑著哭最痛"}
%%         ,{2, "年輪回歸線"}
%%         ,{2, "回首相濡以沫"}
%%         ,{2, "缺陷美"}
%%         ,{2, "最後一張王牌"}
%%         ,{2, "太現實"}
%%         ,{2, "花該謝了"}
%%         ,{2, "你該消失了"}
%%         ,{2, "掛名男友"}
%%         ,{2, "寂寞微涼"}
%%         ,{2, "回到最後抉擇"}
%%         ,{2, "向日葵的微笑"}
%%         ,{2, "只屬於太陽"}
%%         ,{2, "醉紅顔"}
%%         ,{2, "已久的陰霾"}
%%         ,{2, "銘記那一段"}
%%         ,{2, "卑微旳曾經"}
%%         ,{2, "冰冷旳身軀."}
%%         ,{2, "湔迣妗泩哋緣"}
%%         ,{2, "用時間抹去"}
%%         ,{2, "你是我心中"}
%%         ,{2, "不滅的光"}
%%         ,{2, "痛了也好"}
%%         ,{2, "風的吹逐只"}
%%         ,{2, "是一位相求"}
%%         ,{2, "小肥羊愛戀"}
%%         ,{2, "淚芯傷痕"}
%%         ,{2, "莫傷太高貴"}
%%         ,{2, "莈洧洳淉"}
%%         ,{2, "難以追求幸福"}
%%         ,{2, "寂寞離別日"}
%%         ,{2, "我的青春ぢ"}
%%         ,{2, "你不懂"}
%%         ,{2, "逍遙飄搖"}
%%         ,{1, "麥田守望者"}
%%         ,{2, "瘋掉的記憶"}
%%         ,{2, "戀上寂寞"}
%%         ,{2, "哼著小曲"}
%%         ,{2, "咫尺幸福"}
%%         ,{2, "愛笑的豬"}
%%         ,{2, "思戀已泛黃"}
%%         ,{2, "人皮面具"}
%%         ,{2, "青春的年華"}
%%         ,{2, "如夢似花"}
%%         ,{2, "我的昵稱沒有"}
%%         ,{2, "必要那麼長"}
%%         ,{2, "邂逅相遇"}
%%         ,{2, "遇後相知"}
%%         ,{2, "吶傷"}
%%         ,{2, "生死戀"}
%%         ,{2, "時辰被我拋棄"}
%%         ,{2, "在回顧裡"}
%%         ,{2, "幸福是what"}
%%         ,{2, "透過呼吸"}
%%         ,{2, "天才魔兒"}
%%     ].

name_list() ->
    [
        "Abigail"
        ,"Abby"
        ,"Ada"
        ,"Adelaide"
        ,"Adeline"
        ,"Alexandra"
        ,"Ailsa"
        ,"Aimee"
        ,"Alice"
        ,"Alina"
        ,"Allison"
        ,"Amanda"
        ,"Amy"
        ,"Amber"
        ,"Anastasia"
        ,"Andrea"
        ,"Angela"
        ,"Angelia"
        ,"Angelina"
        ,"Ann"
        ,"Anne"
        ,"Annie"
        ,"Anita"
        ,"Ariel"
        ,"April"
        ,"Ashley"
        ,"Aviva"
        ,"Barbara"
        ,"Beata"
        ,"Beatrice"
        ,"Becky"
        ,"Betty"
        ,"Blanche"
        ,"Bonnie"
        ,"Brenda"
        ,"Camille"
        ,"Candice"
        ,"Carina"
        ,"Carmen"
        ,"Caro"
        ,"Caroline"
        ,"Carry"
        ,"Carrie"
        ,"Cassandra"
        ,"Cassie"
        ,"Catherine"
        ,"Cathy"
        ,"Chelsea"
        ,"Charlene"
        ,"Charlotte"
        ,"Cherry"
        ,"Cheryl"
        ,"Chris"
        ,"Christina"
        ,"Christine"
        ,"Christy"
        ,"Cindy"
        ,"Claudia"
        ,"Clement"
        ,"Cloris"
        ,"Connie"
        ,"Constance"
        ,"Cora"
        ,"Corrine"
        ,"Crystal"
        ,"Daisy"
        ,"Daphne"
        ,"Darcy"
        ,"Debbie"
        ,"Deborah"
        ,"Debra"
        ,"Demi"
        ,"Diana"
        ,"Dolores"
        ,"Donna"
        ,"Doris"
        ,"Edith"
        ,"Editha"
        ,"Elaine"
        ,"Eleanor"
        ,"Elizabeth"
        ,"Ella"
        ,"Ellen"
        ,"Ellie"
        ,"Emerald"
        ,"Emily"
        ,"Emma"
        ,"Enid"
        ,"Elsa"
        ,"Erica"
        ,"Estelle"
        ,"Esther"
        ,"Eudora"
        ,"Eva"
        ,"Eve"
        ,"Fannie"
        ,"Fiona"
        ,"Frances"
        ,"Frederica"
        ,"Frieda"
        ,"Gina"
        ,"Gillian"
        ,"Gladys"
        ,"Gloria"
        ,"Grace"
        ,"Greta"
        ,"Gwendolyn"
        ,"Hannah"
        ,"Helena"
        ,"Hellen"
        ,"Hebe"
        ,"Heidi"
        ,"Ingrid"
        ,"Ishara"
        ,"Irene"
        ,"Iris"
        ,"Ivy"
        ,"Jacqueline"
        ,"Jamie"
        ,"Jane"
        ,"Janet"
        ,"Jean"
        ,"Jessica"
        ,"Jessie"
        ,"Jennifer"
        ,"Jenny"
        ,"Jill"
        ,"Joan"
        ,"Joanna"
        ,"Jocelyn"
        ,"Josephine"
        ,"Josie"
        ,"Joy"
        ,"Joyce"
        ,"Judith"
        ,"Judy"
        ,"Julia"
        ,"Juliana"
        ,"Julie"
        ,"June"
        ,"Karen"
        ,"Karida"
        ,"Katherine"
        ,"Kate"
        ,"Kathy"
        ,"Katrina"
        ,"Kay"
        ,"Kelly"
        ,"Kitty"
        ,"Lareina"
        ,"Laura"
        ,"Lena"
        ,"Lydia"
        ,"Lillian"
        ,"Linda"
        ,"Lisa"
        ,"Liz"
        ,"Lorraine"
        ,"Louisa"
        ,"Louise"
        ,"Lucia"
        ,"Lucy"
        ,"Lucine"
        ,"Lulu"
        ,"Lynn"
        ,"Maggie"
        ,"Mamie"
        ,"Manda"
        ,"Mandy"
        ,"Margaret"
        ,"Mariah"
        ,"Martha"
        ,"Mary"
        ,"Matilda"
        ,"Maureen"
        ,"Mavis"
        ,"Maxine"
        ,"May"
        ,"Mayme"
        ,"Megan"
        ,"Melinda"
        ,"Melissa"
        ,"Melody"
        ,"Mercedes"
        ,"Meredith"
        ,"Michelle"
        ,"Milly"
        ,"Miranda"
        ,"Miriam"
        ,"Miya"
        ,"Molly"
        ,"Monica"
        ,"Nancy"
        ,"Natalie"
        ,"Natasha"
        ,"Nicole"
        ,"Nikita"
        ,"Nina"
        ,"Olina"
        ,"Oprah"
        ,"Pamela"
        ,"Paula"
        ,"Pauline"
        ,"Pearl"
        ,"Peggy"
        ,"Philomena"
        ,"Phoebe"
        ,"Phyllis"
        ,"Polly"
        ,"Priscilla"
        ,"Quentina"
        ,"Rachel"
        ,"Rebecca"
        ,"Regina"
        ,"Rita"
        ,"Rose"
        ,"Roxanne"
        ,"Ruth"
        ,"Sabrina"
        ,"Sandra"
        ,"Samantha"
        ,"Sandy"
        ,"Sarah"
        ,"Selma"
        ,"Selina"
        ,"Serena"
        ,"Sharon"
        ,"Sheila"
        ,"Shelley"
        ,"Sherry"
        ,"Shirley"
        ,"Silvia"
        ,"Sonia"
        ,"Stacy"
        ,"Stella"
        ,"Stephanie"
        ,"Sue"
        ,"Sunny"
        ,"Susan"
        ,"Tamara"
        ,"Tammy"
        ,"Tess"
        ,"Teresa"
        ,"Tiffany"
        ,"Tina"
        ,"Tracy"
        ,"Vanessa"
        ,"Vicky"
        ,"Victoria"
        ,"Vivian"
        ,"Wanda"
        ,"Wendy"
        ,"Winnie"
        ,"Yolanda"
        ,"Yvette"
        ,"Yvonne"
        ,"Zoey"
    ].
%%.

%% 1000000, 1009996
%% clean_accounts() ->
%%     {ok, [Start, _End]} = application:get_env(cc, robot_accounts),
%%     End = length(get_names()) + Start - 1,
%%     clean_accounts(Start, End).
%% clean_accounts(Start, End) ->
%%     robot ! prep_stop,
%%     L = db:get_all("select id, account_id, name from role where account_id >= ~s and account_id <= ~s", [Start, End]),
%%     logout(L),
%%     util:sleep(1000),
%%     clean_account(L),
%%     clean_log(),
%%     ok.

%% clean_account([[Id, AccId, Name] | T]) ->
%%     db:execute("delete from log_upgrade where role_id = ~s", [Id]),
%%     db:execute("delete from rank where id = ~s", [Id]),
%%     db:execute("delete from role where id = ~s", [Id]),
%%     ?INFO("~w:~s", [AccId, Name]),
%%     clean_account(T);
%% clean_account([]) -> ok.
%% 
%% clean_log() ->
%%     R1 = db:execute("delete from log_reg where ip = '127.0.0.1'"),
%%     ?INFO("clean log_reg:~w", [R1]),
%%     R2 = db:execute("delete from log_login where ip = '127.0.0.1'"),
%%     ?INFO("clean log_login:~w", [R2]),
%%     ok.

logout([[Id, _AccId, _Name] | T]) ->
    lib_role:kick(Id, <<>>),
    logout(T);
logout([]) -> ok.

process_item(Id, _Tid, 8, _Pos, _Lev, _Data1, S) ->
    erlang:send_after(util:rand(10, 1000), S, {cmd, 17107, [Id]});
process_item(Id, Tid, Sort, Pos, Lev, Data1, S) ->
    ToPos = case Sort of
        1 -> 97;
        11 -> 98;
        5 -> 99;
        6 -> 100;
        4 -> 101;
        12 -> 102;
        _ -> 0
    end,
    case ToPos of
        0 -> ok;
        Pos -> ok;
        _ ->
            case lists:keyfind(ToPos, 5, Data1) of
                false -> 
                    erlang:send_after(util:rand(10, 1000), S, {cmd, 17103, [Id, ToPos]});
                {_Id1, Tid1, _Tab1, _Sort1, _Pos1, _Num1, _Lev1, _, _} when Tid > Tid1 ->
                    erlang:send_after(util:rand(10, 1000), S, {cmd, 17103, [Id, ToPos]});
                {_Id1, Tid1, _Tab1, _Sort1, _Pos1, _Num1, _Lev1, _, _} when Tid < Tid1 ->
                    erlang:send_after(util:rand(10, 1000), S, {cmd, 17107, [Id]});
                {_Id1, _Tid1, _Tab1, _Sort1, _Pos1, _Num1, Lev1, _, _} when Lev > Lev1 ->
                    erlang:send_after(util:rand(10, 1000), S, {cmd, 17103, [Id, ToPos]});
                _ -> ok
            end
    end.
%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
