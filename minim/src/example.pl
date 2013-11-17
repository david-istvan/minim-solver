/* -*- Mode:Prolog; coding:iso-8859-1; -*- */
:- use_module(library(clpfd)).                  %loads the CLP(FD) module
:- use_module(library(fdbg)).                   %loads the debug module

main :-
        write('hello world').


tester :-
        X in 1..3,
        Y in 2..3,
        X#>=Y,
        labeling([min], [X,Y]).

list_sum([Item], Item).

list_sum([Item1,Item2 | Tail], Total) :-
    SumItem is Item1+Item2,
    list_sum([SumItem|Tail], Total).

deleteSome(L, _, L).
deleteSome(L, D, R) :-
  select(E, L, L1),
  select(E, D, D1),
  !, deleteSome(L1, D1, R).

select(X, [X|Tail], Tail).
select(Elem, [Head|Tail], [Head|Rest]) :-
select(Elem, Tail, Rest).