:- use_module(library(clpfd)).
:- use_module(library(lists)).

latszam([X|Ls], N):-
        latszam2(X, Ls, N).
        
latszam2(CM, [Y|Ls], K):-
        makeLatszam(CM, Y, K, K1, NM),
        (
           length(Ls, Lt), Lt > 0
        ->
           latszam2(NM, Ls, K1)
        ;
           K1 #= 0
        )
        .

latszam2(_, [], _).        

makeLatszam(CM, Y, K0, K1, NM):-
        Y #> CM #<=> S,
        (
           S #= 1, K1 is K0-1, NM #= Y
        ;
           S #= 0, K1 is K0, NM #= CM
        )
        .
