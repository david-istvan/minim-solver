:- use_module(library(clpfd)).
:- use_module(library(lists)).

novo([X|Ls], N):-
        novo(Ls, X, N).

novo([Y|Ls], X, N):-
        Y-X#>=N,
        novo(Ls, Y, N).
novo([], _, _).