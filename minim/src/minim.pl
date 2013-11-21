/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- use_module(library(lists)).

minimize(G0, G):-
        (
           mergeable(G0, E)
        ->
           merge(E, G0, G1),
           minimize(G1, G)
        ;
           G= G0
        ).

mergeable(WL+EL, E):-
        mergeable(WL, EL, E).

mergeable(WL, EL, E):-
        member((X-W), WL),
        member((Y-W), WL),
        X\=Y,
        E = (X-Y),
        member(E, EL).

merge(X-Y, WL+EL, G1):-
        NID is X+10,
        merge_weights(X, Y, NID, WL, WL1),
        merge_edges(X, Y, NID, EL, EL1),
        G1 = WL1+EL1.

merge_weights(X, Y, NID, WL0, WL1):-
        select((X-W), WL0, R1),
        select((Y-W), R1, R2),
        X<Y,
        NW is W+1,
        append(R2, (NID-NW), WL1).

merge_edges(X, Y, NID, EL0, EL1):-
        (
           (member((X-Z), EL0);member((Z-X), EL0)), Z\=Y
        ->
           replace_edge(X, NID, Z, EL0, R1),
           select((X-Y), R1, EL1)
        ;
           (member((Y-Z), EL0);member((Z-Y), EL0)), Z\=X
        ->
           replace_edge(Y, NID, Z, EL0, R1),
           select((X-Y), R1, EL1)
        )
        .

replace_edge(ID0, ID1, Z, EL0, EL1):-
        select((ID0-Z), EL0, R1),
        append((ID1-Z), R1, EL1).