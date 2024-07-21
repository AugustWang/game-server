%%----------------------------------------------------
%% record
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-record(task, {
        type
        ,condition
        ,ctl1
        ,ctl2
        ,lev_min
        ,lev_max
        ,before
        ,next
        ,loop
        ,rfactor
        ,rcard
        ,rgold
        ,rexp
    }).

-record(mytask, {
        id        %% 任务唯一ID
        ,tid      %% 任务ID
        ,type
        ,ctype    %% 条件类型
        ,ctl1
        ,ctl2
        ,process = 0
        ,loop = 0
        ,loop_max
        ,rfactor
        ,reward
        ,select_item = 0
        ,reward_item = []
    }).
