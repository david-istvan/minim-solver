/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

/*ugraphs, wgraphs*/

:- use_module(library(chr)).
:- use_module(library(lists)).

handler leq.
handler merge.
constraints leq/2.
constraints merge/4.
constraints minimize/1.
% X leq Y means variable X is less-or-equal to variable Y
:- op(500, xfx, leq).

reflexivity @ X leq Y <=> X = Y | true.
antisymmetry @ X leq Y , Y leq X <=> X=Y.
idempotence @ X leq Y \ X leq Y <=> true.
transitivity @ X leq Y , Y leq Z ==> X leq Z.

merge @ merge(X, W1, Y, W2) <=> connected(X,Y), W1 is W2 | true.

%minimize @ minimize(WL) <=> mergeweight(_, _, WL, NWL) | NWL.
minimize @ minimize(WL) <=> removeelement(_, _, WL, NWL) | NWL.

mergeweight(X, Y, WL, NWL):-
        select([X,W], WL, R1),
        select([Y,W], R1, R2),
        W2 is W+1,
        append(R2, [[X, W2]], NWL);
        mergeweight(_, _, NWL, NWL).

removeelement(X, W, WL, NWL):-
        select([X,W], WL, R),
        W2 is W+1,
        append(R, [[X, W2]], NWL),
        removeelement(_, _, NWL, NWL).

connected(X,Y) :-
        edge(X,Y);
        edge(Y,X).

sameWeight(X,Y) :-
       weight(X,W),
       weight(Y,W).

mergeable(X,Y) :-
        connected(X,Y),
        sameWeight(X,Y).