:- module(sudoku_simple, [sudoku_simple/2, expandElementsInList/2]).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

sudoku_simple(Rows, N):-
        length(Rows, N),
        expandElementsInList(Rows, N),
        append(Rows, Vs),
        domain(Vs, 1, N),
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns).

expandElementsInList([X|Tail], N):-
        length(X, N),
        expandElementsInList(Tail, N).
expandElementsInList([], _).