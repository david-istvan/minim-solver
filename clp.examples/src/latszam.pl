:- use_module(library(clpfd)).
:- use_module(library(lists)).

latszam(L, N):-
        makeLatszam(L, N, N1),
        (
           without_last(L, WL),
           length(WL, LWL),
           LWL > 0
        ->
           latszam(WL, N1)
        ;
           N1 #= 0
        ).

makeLatszam(L, N0, N1):-
        lastIsMax(L) #<=> S,
        S #=1,
        N1 is N0-S.

lastIsMax([_]).
lastIsMax(L):-
        without_last(L, WL),
        last(L, LE),
        elementIsMax(WL, LE).

elementIsMax([], _).
elementIsMax(L, E):-
        last(L, LE),
        E > LE,
        without_last(L, WL),
        elementIsMax(WL, E).


without_last([_], []).
without_last([X|Xs], [X|WithoutLast]):-
        without_last(Xs, WithoutLast).
