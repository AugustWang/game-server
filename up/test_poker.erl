-module(test_poker).

%% -compile(export_all).

-export([
        make_cards/1
        ,face/1
        ,suit/1
        ,make_rep/1
    ]).

%% S = "4D JH 5D 8C QD TD 7H"
make_cards(S)
  when is_list(S) ->
    lists:map(fun make_card/1,
        string:tokens(S, " ")).

%% Make a single card tuple

make_card([H, T]) ->
    Rank = case H of
        $2 -> two;
        $3 -> three;
        $4 -> four;
        $5 -> five;
        $6 -> six;
        $7 -> seven;
        $8 -> eight;
        $9 -> nine;
        $T -> ten;
        $J -> jack;
        $Q -> queen;
        $K -> king;
        $A -> ace
    end,
    Suit = case T of
        $C -> clubs;      %% 梅花
        $D -> diamonds;   %% 方块
        $H -> hearts;     %% 红桃
        $S -> spades      %% 黑桃
    end,
    {Rank, Suit}.

face(Face) when is_atom(Face)->
    1 bsl case Face of
        ace -> 13;
        king -> 12;
        queen -> 11;
        jack -> 10;
        ten -> 9;
        nine -> 8;
        eight -> 7;
        seven -> 6;
        six -> 5;
        five -> 4;
        four -> 3;
        three -> 2;
        two -> 1
    end;

face(X) when is_number(X) ->
    face(X, [ace, king, queen, jack, ten, nine,
            eight, seven, six, five, four, three, two]).

face(_X, []) ->
    none;

face(X, [Face|Rest]) ->
    Match = (X band face(Face)) > 0,
    io:format("~w:~w~n", [Face,Match]),
    if
        Match ->
            Face;
        true ->
            face(X, Rest)
    end.

suit(Suit) when is_atom(Suit) ->
    case Suit of
        clubs -> 1;
        diamonds -> 2;
        hearts -> 3;
        spades -> 4
    end;

suit(Suit) when is_number(Suit) ->
    case Suit of
        1 -> clubs;
        2 -> diamonds;
        3 -> hearts;
        4 -> spades
    end.

%% Rep = test_poker:make_cards("4D JH 5D 8C QD TD 7H").
%% make_rep(Rep).
make_rep(Cards) when is_list(Cards) ->
    make_rep(Cards, {0, 0, 0, 0}).

make_rep([{Face, Suit}|T], Rep) ->
    Suit1 = suit(Suit),
    Old = element(Suit1, Rep),
    Face1 = face(Face),
    make_rep(T, setelement(Suit1, Rep, Old bor Face1));

make_rep([], Rep) ->
    tuple_to_list(Rep).
