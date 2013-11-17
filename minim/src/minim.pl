/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

/*ugraphs, wgraphs*/

:- use_module(library(chr)).
:- use_module(library(lists)).

handler minimize.
constraints minimize/1.

%minimize @ minimize(WL) <=> mergeweight(_, _, WL, NWL) | NWL.
minimize @ minimize(WL) <=> mergeWeightList(WL) | WL.

hasMergeableElements(WL):-
        select([_X,W], WL, R1),
        select([_Y,W], R1, _).

notMinimized(WL):-
        length(WL, L),
        L>1.

mergeWeightList(WL):-
        writeln(WL),
        notMinimized(WL),
        hasMergeableElements(WL),
        select([X,W], WL, R1),
        select([_Y,W], R1, R2),
        W2 is W+1,
        X2 is X+10,
        append(R2, [[X2, W2]], NWL),
        mergeWeightList(NWL).

writeln(Str) :-
        write(Str), nl.

/*
connected(X,Y) :-
        edge(X,Y);
        edge(Y,X).

sameWeight(X,Y) :-
       weight(X,W),
       weight(Y,W).

mergeable(X,Y) :-
        connected(X,Y),
        sameWeight(X,Y).
*/
factorial(0,1).
factorial(X,Y) :-
        X1 is X - 1,
        factorial(X1,Z),
        Y is Z*X,!.