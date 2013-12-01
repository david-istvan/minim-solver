:- use_module(library(clpfd)).
:- use_module(library(lists)).

sudoku_simple(Rows, N):-
        append(Rows, Vs),
        domain(Vs, 1, N),
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns).
