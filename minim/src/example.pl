/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

main :-
        write('hello world').

fibacc(N,N,F1,F2,F) :-
        F is F1+F2.

fibacc(N,I,F1,F2,F) :-
        I<N,
        Ipls1 is I +1, F1New is F1+F2, F2New is F1,
        fibacc(N,Ipls1,F1New,F2New,F).

fib(N,F) :-
        fibacc(N,2,1,0,F).

sum(A,X1,X2) :-
        A is X1+X2.