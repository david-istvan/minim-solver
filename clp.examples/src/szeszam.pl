:- use_module(library(clpfd)).
:- use_module(library(lists)).

szeszam([X,Y,Z|Tail], K):-
        makeSzeszam(X, Y, Z, K, K1),
        (
           length(Tail,Lt), Lt>0
        ->
           szeszam([Y,Z|Tail], K1)
        ;
           K1 #= 0
        )
        .
 
szeszam([], _).

makeSzeszam(X, Y, Z, K0, K1):-
        (((X#<Y) #/\ (Y#>Z)) #\/ ((X#>Y) #/\ (Y#<Z))) #<=> S,
        S #=1,
        K1 is K0-S
        .