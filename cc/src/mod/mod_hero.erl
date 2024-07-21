%%----------------------------------------------------
%% hero
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(mod_hero).
-export([handle/3]).

-include("common.hrl").
-include("hero_base.hrl").
-include("hero.hrl").

%% 定时刷新的英雄列表
handle(23005, [], _Rs) ->
    Now = util:unixtime(),
    Interval = 3600,
    {Heroes, RestTime} = case get(heroes_shop) of
        undefined ->
            Heroes1 = rand_hero(3),
            put(heroes_shop, {Heroes1, Now}),
            {Heroes1, Interval};
        {Heroes0, LastTime} -> 
            RestTime0 = LastTime + Interval - Now,
            case RestTime0 > 0 of
                true -> {Heroes0, RestTime0};
                false ->
                    Heroes1 = rand_hero(3),
                    put(heroes_shop, {Heroes1, Now}),
                    {Heroes1, Interval}
            end
    end,
    HeroesData = hero_base_to_pdata(Heroes),
    ?INFO("~p", [[RestTime, HeroesData]]),
    {ok, [RestTime, HeroesData]};

%% 购买英雄
handle(23007, [Id], Rs) ->
    case get(heroes_shop) of
        undefined -> 
            ?WARN("heroes_shop undefined", []),
            {ok};
        {Heroes, RefTime} ->
            ?INFO("Heroes:~w", [Heroes]),
            case lists:keyfind(Id, #hero_base.id, Heroes) of
                false -> 
                    ?WARN("No hero(~w) at heroes_shop", [Id]),
                    {ok};
                Hero ->
                    case lib_role:spend(gold, Hero#hero_base.gold, Rs) of
                        {ok, Rs1} -> 
                            Heroes1 = lists:keydelete(Id, #hero_base.id, Heroes),
                            put(heroes_shop, {Heroes1, RefTime}),
                            lib_hero:add_myheroes(init_hero(Hero)),
                            {ok, [0], Rs1};
                        {error, _} -> 
                            {ok, [1]}
                    end
            end
    end;

%% 我的英雄列表
handle(23009, [], _Rs) ->
    Heroes = lib_hero:get_myheroes(),
    HeroesData = hero_to_pdata(Heroes),
    ?INFO("HeroesData:~p", [HeroesData]),
    {ok, [HeroesData]};

%% 选择英雄
handle(23011, [Id], _Rs) ->
    case lib_hero:select_myhero(Id) of
        true -> {ok, [0]};
        false -> {ok, [1]}
    end;

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%% 私有函数

rand_hero(Num) ->
    Data = data_hero_rand:get(range),
    rand_hero(Num, Data, []).

rand_hero(0, _, Reply) -> Reply;
rand_hero(Num, Data, Reply) ->
    Rand = util:rand(1, 100000),
    HeroId = rand_hero1(Rand, Data),
    case lists:member(HeroId, Reply) of
        true -> rand_hero(Num, Data, Reply);
        false -> 
            Num1 = Num - 1,
            Reply1 = [init_base_hero(HeroId) | Reply],
            rand_hero(Num1, Data, Reply1)
    end.

rand_hero1(Rand, [{Id, Min, Max} | T]) ->
    case Rand >= Min andalso Rand =< Max of
        true -> Id;
        false -> rand_hero1(Rand, T)
    end;
rand_hero1(_, []) -> 0.

init_base_hero(Id) ->
    Hero = data_hero_base:get(Id),
    #hero_base{
        grade                = Grade      
        ,gold                = Gold
        ,hp_max              = HpMax             
        ,hp_max_rate         = HpMaxRate      
        ,fb_attack           = FbAttack        
        ,fb_attack_rate      = FbAttackRate   
        ,attack              = Attack           
        ,attack_rate         = AttackRate      
        ,move_speed          = MoveSpeed       
        ,move_speed_rate     = MoveSpeedRate  
        ,attack_speed        = AttackSpeed     
        ,attack_speed_rate   = AttackSpeedRate
        ,crit                = Crit             
        ,crit_rate           = CritRate        
    } = Hero,
    #hero_base{
        id                   = Id
        ,gold                = Gold
        ,grade               = Grade
        ,hp_max              = rand_val(HpMax          )  
        ,hp_max_rate         = rand_val(HpMaxRate      )
        ,fb_attack           = rand_val(FbAttack       )
        ,fb_attack_rate      = rand_val(FbAttackRate   )
        ,attack              = rand_val(Attack         ) 
        ,attack_rate         = rand_val(AttackRate     )
        ,move_speed          = rand_val(MoveSpeed      )
        ,move_speed_rate     = rand_val(MoveSpeedRate  )
        ,attack_speed        = rand_val(AttackSpeed    )
        ,attack_speed_rate   = rand_val(AttackSpeedRate)
        ,crit                = rand_val(Crit           ) 
        ,crit_rate           = rand_val(CritRate       )
    }.

init_hero(HeroBase) ->
    #hero_base{
        id                   = Id
        ,grade               = Grade      
        ,hp_max              = HpMax             
        ,hp_max_rate         = HpMaxRate      
        ,fb_attack           = FbAttack        
        ,fb_attack_rate      = FbAttackRate   
        ,attack              = Attack           
        ,attack_rate         = AttackRate      
        ,move_speed          = MoveSpeed       
        ,move_speed_rate     = MoveSpeedRate  
        ,attack_speed        = AttackSpeed     
        ,attack_speed_rate   = AttackSpeedRate
        ,crit                = Crit             
        ,crit_rate           = CritRate        
    } = HeroBase,
    #hero{
        id                   = Id
        ,grade               = Grade
        ,hp_max              = HpMax             
        ,hp_max_rate         = HpMaxRate      
        ,fb_attack           = FbAttack        
        ,fb_attack_rate      = FbAttackRate   
        ,attack              = Attack           
        ,attack_rate         = AttackRate      
        ,move_speed          = MoveSpeed       
        ,move_speed_rate     = MoveSpeedRate  
        ,attack_speed        = AttackSpeed     
        ,attack_speed_rate   = AttackSpeedRate
        ,crit                = Crit             
        ,crit_rate           = CritRate        
        % -----------------------------------
        ,lev                 = 1
        ,exp                 = 0
        ,exp_max             = util:get_val(exp_max, data_hero_exp:get(1))
    }.

rand_val({Min, Max}) ->
    util:rand_float1(Min, Max).

hero_base_to_pdata(Heroes) ->
    hero_base_to_pdata(Heroes, []).

hero_base_to_pdata([Hero | T], Reply) ->
    #hero_base{
        id                   = Id             
        ,hp_max              = HpMax             
        ,hp_max_rate         = HpMaxRate      
        ,fb_attack           = FbAttack        
        ,fb_attack_rate      = FbAttackRate   
        ,attack              = Attack           
        ,attack_rate         = AttackRate      
        ,move_speed          = MoveSpeed       
        ,move_speed_rate     = MoveSpeedRate  
        ,attack_speed        = AttackSpeed     
        ,attack_speed_rate   = AttackSpeedRate
        ,crit                = Crit             
        ,crit_rate           = CritRate        
    } = Hero,
    Hero1 = [
        Id
        ,HpMax          
        ,HpMaxRate      
        ,FbAttack       
        ,FbAttackRate   
        ,Attack         
        ,AttackRate     
        ,MoveSpeed      
        ,MoveSpeedRate  
        ,AttackSpeed    
        ,AttackSpeedRate
        ,Crit           
        ,CritRate       
    ],
    hero_base_to_pdata(T, [Hero1 | Reply]);
hero_base_to_pdata([], Reply) -> 
    Reply.

hero_to_pdata(Heroes) ->
    hero_to_pdata(Heroes, []).

hero_to_pdata([Hero | T], Reply) ->
    #hero{
        id                   = Id             
        ,hp_max              = HpMax             
        ,hp_max_rate         = HpMaxRate      
        ,fb_attack           = FbAttack        
        ,fb_attack_rate      = FbAttackRate   
        ,attack              = Attack           
        ,attack_rate         = AttackRate      
        ,move_speed          = MoveSpeed       
        ,move_speed_rate     = MoveSpeedRate  
        ,attack_speed        = AttackSpeed     
        ,attack_speed_rate   = AttackSpeedRate
        ,crit                = Crit             
        ,crit_rate           = CritRate        
        ,lev                 = Lev
        ,exp                 = Exp
        ,exp_max             = ExpMax
    } = Hero,
    Hero1 = [
        Id
        ,HpMax          
        ,HpMaxRate      
        ,FbAttack       
        ,FbAttackRate   
        ,Attack         
        ,AttackRate     
        ,MoveSpeed      
        ,MoveSpeedRate  
        ,AttackSpeed    
        ,AttackSpeedRate
        ,Crit
        ,CritRate       
        ,Lev
        ,Exp
        ,ExpMax
    ],
    hero_to_pdata(T, [Hero1 | Reply]);
hero_to_pdata([], Reply) -> 
    Reply.

%% vim: set foldmethod=marker foldmarker=%%',%%.:
