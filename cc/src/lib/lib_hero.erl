%%----------------------------------------------------
%% Hero
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_hero).
-export([
        get_selected_hero/0
        ,select_myhero/1
        ,get_myheroes/0
        ,set_myheroes/1
        ,add_myheroes/1
    ]
).

-include("common.hrl").
-include("hero_base.hrl").
-include("hero.hrl").

get_selected_hero() ->
    Heroes = get_myheroes(),
    lists:keyfind(1, #hero.status, Heroes).

get_myheroes() ->
    case get(myheroes) of
        undefined -> [];
        D -> D
    end.

select_myhero(Id) ->
    %% mapfoldl(Fun, Acc0, List1) -> {List2, Acc1}
    Heroes = lib_hero:get_myheroes(),
    {Heroes1, Return} = lists:mapfoldl(fun(Hero, Rt) ->
                case Hero#hero.id of
                    Id -> {Hero#hero{status = 1}, true};
                    _ -> {Hero#hero{status = 0}, Rt}
                end
        end, false, Heroes),
    case Return of
        true -> set_myheroes(Heroes1);
        false -> ok
    end,
    ?INFO("select_myhero(~w):~w", [Id, Return]),
    Return.

set_myheroes(Heroes) ->
    put(myheroes, Heroes).

add_myheroes(Hero) ->
    Heroes = lib_hero:get_myheroes(),
    set_myheroes([Hero | Heroes]).


