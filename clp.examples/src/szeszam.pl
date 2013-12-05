:- use_module(library(clpfd)).

szeszam([X|L], K):-
        L = [Y,Z|_],!,
        makeSzeszam(X, Y, Z, K, K1),
        szeszam(L, K1).
szeszam(_, 0).

makeSzeszam(X, Y, Z, K0, K1):-
       (((X#<Y) #/\ (Y#>Z)) #\/ ((X#>Y) #/\ (Y#<Z))) #<=> S,
       K1#=K0-S.