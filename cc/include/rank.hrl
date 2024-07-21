%%----------------------------------------------------
%% 排行榜数据结构定义
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

%% 排行榜数据
-record(myrank_data,
    {
        rank_pos        = 0 %%名次
        ,id             = 0     %% 角色ID
        ,lev            = 0     %% 等级
        ,name           = <<>>  %% 角色名字
        ,trand          = 2 %% 1升2平3降
        ,level_day      = 0
        ,level_week     = 0
        ,level_all      = 0
        ,win_day        = 0
        ,win_week       = 0
        ,win_all        = 0
        ,gold_day       = 0
        ,gold_week      = 0
        ,gold_all       = 0
    }
).

