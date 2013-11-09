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