%%----------------------------------------------------
%% hero
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-record(hero, {
        id
        ,grade
        ,hp_max
        ,hp_max_rate
        ,fb_attack
        ,fb_attack_rate
        ,attack
        ,attack_rate
        ,move_speed
        ,move_speed_rate
        ,attack_speed
        ,attack_speed_rate
        ,crit
        ,crit_rate
        ,lev
        ,exp
        ,exp_max
        ,status = 0 %% 0=正常，1=当前选择
    }
).
