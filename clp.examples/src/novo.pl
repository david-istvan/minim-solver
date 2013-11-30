:- use_module(library(clpfd)).
:- use_module(library(lists)).

novo([X|Ls], N):-
        novo(X, Ls, N).

novo(X, [Y|Ls], N):-
        Y-X#>=N,
        novo(Y, Ls, N)      
        .

novo(_, [], _).

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
        write('testing '), write(X), write(Y), write(Z), nl,
        (((X#<Y) #/\ (Y#>Z)) #\/ ((X#>Y) #/\ (Y#<Z))) #<=> S,
        S #=1,
        K1 is K0-S
        .