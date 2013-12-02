:- use_module(library(clpfd)).
:- use_module(library(lists)).

novo([X|Ls], N):-
        novo(X, Ls, N).

novo(X, [Y|Ls], N):-
        Y-X#>=N,
        novo(Y, Ls, N)      
        .

novo(_, [], _).