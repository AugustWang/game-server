%%----------------------------------------------------
%% 角色相关
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_skill).
-export([
        init_skill/1
        ,set_pos/3
        ,add_exp/3
        ,check/2
        ,get_skilled/1
    ]
).

-include("common.hrl").
-include("skill.hrl").

init_skill(Rs) ->
    F = fun(Id) ->
            case data_skill:get(Id) of
                undefined -> false;
                #skill{lev_min = LevMin} ->
                    LevMin > 0 andalso Rs#role.lev >= LevMin
            end
    end,
    Ids = lists:filter(F, data_skill:get(ids)),
    Skill1 = case Rs#role.skill of
        [] -> [{X, 0, 0} || X <- Ids];
        S -> 
            F1 = fun(Id, SS) ->
                    Ids2 = [X div 100 || {X, _, _} <- SS],
                    Id2 = Id div 100,
                    case lists:keyfind(Id, 1, SS) == false andalso
                        lists:member(Id2, Ids2) == false of
                        true ->
                            NewSkill = {Id, 0, 0},
                            lists:keystore(Id, 1, SS, NewSkill);
                        false -> SS
                    end
            end,
            lists:foldl(F1, S, Ids)
    end,
    Skill2 = do_skill(Skill1),
    case Skill1 =/= Skill2 of
        true ->
            ?INFO("ID:~w, skill:~w", [Rs#role.id, Rs#role.skill]),
            ?INFO("ID:~w, skill:~w", [Rs#role.id, Skill2]),
            ok;
        false -> ok
    end,
    Rs#role{skill = Skill2}.

do_skill(S) ->
    Ids2 = [{X1 div 100, X1} || {X1, _X2, _X3} <- S],
    RepeatS = util:find_repeat_key_element(1, Ids2),
    case find_repeat_ids(RepeatS, []) of
        [] -> S;
        [RepeatId | _T] ->
            ?INFO("Del Skill:~w", [[RepeatId | _T]]),
            lists:keydelete(RepeatId, 1, S)
    end.

find_repeat_ids([{Sort, Id1} | T], Reply) ->
    case lists:keyfind(Sort, 1, T) of
        false -> find_repeat_ids(T, Reply);
        {_, Id2} -> 
            Id = lists:min([Id1, Id2]),
            find_repeat_ids(T, [Id | Reply])
    end;
find_repeat_ids([], Reply) -> Reply.

set_pos(Rs, SkillId, 0) ->
    Skill = Rs#role.skill,
    case lists:keyfind(SkillId, 1, Skill) of
        false -> {error, error_id};
        {_, Exp, _} -> 
            Skill2 = lists:keyreplace(SkillId, 1, Skill, {SkillId, Exp, 0}),
            {ok, Rs#role{skill = Skill2}}
    end;
set_pos(Rs, SkillId, Pos) ->
    Skill = Rs#role.skill,
    Skill1 = case lists:keyfind(Pos, 3, Skill) of
        false -> Skill;
        {X1, X2, _} -> 
            lists:keyreplace(X1, 1, Skill, {X1, X2, 0})
    end,
    case lists:keyfind(SkillId, 1, Skill1) of
        false -> {error, error_id};
        {_, Exp, _} -> 
            Skill2 = lists:keyreplace(SkillId, 1, Skill1, {SkillId, Exp, Pos}),
            {ok, Rs#role{skill = Skill2}}
    end.

add_exp(Rs, SkillId, AddExp) ->
    Skills = Rs#role.skill,
    case lists:keyfind(SkillId, 1, Skills) of
        false -> {error, error_id};
        {_, Exp, Pos} -> 
            Exp1 = Exp + AddExp,
            #skill{exp_max = ExpMax, next_id = NextId} = data_skill:get(SkillId),
            if
                NextId == 0 ->
                    {error, top_lev};
                Exp1 >= ExpMax ->
                    %% 升级
                    Exp2 = Exp1 - ExpMax,
                    Skill1 = lists:keyreplace(SkillId, 1, Skills, {NextId, Exp2, Pos}),
                    Rs1 = Rs#role{skill = Skill1},
                    {ok, Rs1, NextId, Exp2};
                true ->
                    Skill1 = lists:keyreplace(SkillId, 1, Skills, {SkillId, Exp1, Pos}),
                    Rs1 = Rs#role{skill = Skill1},
                    {ok, Rs1, SkillId, Exp1}
            end
    end.

check(Rs, SkillId) ->
    Skill = Rs#role.skill,
    case lists:keyfind(SkillId, 1, Skill) of
        false -> false;
        {_, _, Pos} -> Pos > 0
    end.

get_skilled(Rs) ->
    [Id || {Id, _, Pos} <- Rs#role.skill, Pos > 0].
