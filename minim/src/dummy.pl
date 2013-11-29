/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

size2(WL, S):-
        length(WL, S).

dummy(X+Y, S):-
        dummy2(X, R),
        S = R.
        %size2(WL, S).

dummy2(X-Y, S):-
        S = X.

testGraph(W+L+Z, S):-
        S=W.