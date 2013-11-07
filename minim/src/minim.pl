/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

edge(1,2).
edge(1,3).
edge(2,3).
edge(2,4).
edge(3,4).

weight(1,1).
weight(2,1).
weight(3,2).
weight(4,3).

connected(X,Y) :-
        edge(X,Y);
        edge(Y,X).

sameWeight(X,Y) :-
       weight(X,W1),
       weight(Y,W2),
       W1 = W2;
       false.

mergeable(X,Y) :-
        connected(X,Y),
        sameWeight(X,Y);
        false.