/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

/*ugraphs, wgraphs*/

:- use_module(library(chr)).
:- use_module(library(lists)).

handler minimize.
constraints minimize/2.

minimize @ minimize(WL, EL) <=> mergeWeightList(WL, EL) | true.

notMinimized(WL):-
        length(WL, L),
        L>1.

isConnected(X, Y, EL):-
        select([X,Y], EL, _).

mergeNodes(X, Y, EL, ID, NEL):-
        writeln('current edge list: ' + EL),
        (
           select([X,Z1], EL, R1), Z1\=Y ->
                append(R1, [[ID, Z1]], R1),
                select([X, Y], R1, NEL),
                writeln('new edge list: '+NEL);
           select([Z2,X], EL, R2), Z2\=Y ->
                append(R2, [[Z2, ID]], R1),
                select([X, Y], R1, NEL),
                writeln('new edge list: '+NEL);
           select([Y,Z3], EL, R3), Z3\=X ->
                append(R3, [[ID, Z3]], R1),
                select([X, Y], R1, NEL),
                writeln('new edge list: '+NEL);
           select([Z4,Y], EL, R4), Z4\=X ->                
                append(R4, [[Z4, ID]], R1),
                select([X, Y], R1, NEL),
                writeln('new edge list: '+NEL);              
           select([X, Y], EL, NEL) ->
                writeln('new edge list: '+NEL);
           select([Y, X], EL, NEL) ->
                writeln('new edge list: '+NEL)).

mergeWeightList(WL, EL):-
        writeln('current weight list: ' + WL),
        select([X,W], WL, R1),
        select([Y,W], R1, R2),
        %isConnected(X, Y, EL),
        X<Y,
        W2 is W+1,
        ID is X+10,
        mergeNodes(X, Y, EL, ID, NEL),
        append(R2, [[ID, W2]], NWL),
        writeln('new weight list: ' + NWL),
        mergeWeightList(NWL, NEL).

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