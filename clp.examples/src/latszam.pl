:- module(latszam, [latszam/2]).
:- use_module(library(clpfd)).

latszam([X|Ls], N):-
        K is N-1,
        latszam(X, Ls, K).
        
latszam(CM, [Y|Ls], K):-
        makeLatszam(CM, Y, K, K1, NM),
        (
           Ls \== []
        ->
           latszam(NM, Ls, K1)
        ;
           K1 #= 0
        ).

makeLatszam(CM, Y, K0, K1, NM):-
        Y #> CM #<=> S,
        K1 #= K0-S,
        max(CM, Y, NM).

max(X, X, X).
max(X, Y, X):-
        X #> Y.
max(X, Y, Y):-
        Y #> X.
