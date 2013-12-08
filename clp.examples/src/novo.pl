:- use_module(library(clpfd)).

novo([X|Ls], N):-
        novo(Ls, X, N).

novo([Y|Ls], X, N):-
        Y-X#>=N,
        novo(Ls, Y, N).
novo([], _, _).