:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module(latszam).
:- use_module(sudoku_simple).

panorama(N, Latvanyok, Rows):-
        length(Rows, N),
        expandElementsInList(Rows, N),
        append(Rows, Vs),
        domain(Vs, 1, N),
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        (
           foreach(X, Latvanyok),
           param(Rows, Columns)
           do
           processLatvany(X, Rows, Columns)
        ),
        labeling([], Vs)
        .

processLatvany(bal(I, K), Rows, _):-
        Index is I-1,
        nth0(Index, Rows, Row),
        latszam(Row, K).

processLatvany(jobb(I, K), Rows, _):-
        Index is I-1,
        nth0(Index, Rows, Row),
        reverse(Row, ReversedRow),
        latszam(ReversedRow, K).

processLatvany(felul(J, K), _, Columns):-
        Index is J-1,
        nth0(Index, Columns, Column),
        latszam(Column, K).

processLatvany(alul(J, K), _, Columns):-
        Index is J-1,
        nth0(Index, Columns, Column),
        reverse(Column, ReversedColumn),
        latszam(ReversedColumn, K).
