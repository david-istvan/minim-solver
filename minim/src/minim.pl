/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- use_module(library(lists)).
:- use_module(utils).

minimize(G0, G):-
        (
         %  write('input graph: '), write(G0), nl,
           not(mergeable(G0, _E))
        ->
          % write('not mergeable: '), write(G0), nl,
           G=G0
        ;
           mergeable(G0, E),
           merge(E, G0, G1),
          % write('new graph : '), write(G1), nl,
           minimize(G1, G)
        ).

not(P) :- (call(P) -> fail ; true).

mergeable(WL+EL, E):-
        %write('mergeability check...'), write('weights:'),write(WL),write('edges:'),write(EL), nl,
        member((X-W), WL),
        member((Y-W), WL),
        E=(X-Y),
        member(E, EL).

merge(X-Y, WL+EL, G1):-
        %write('merging: '),write(X), write(' with '),write(Y),write(' in '),write('WL: '),write(WL),write('EL: '),write(EL),nl,
        next_id(WL, NextId),
        merge_weights(X, Y, NextId, WL, WL1),
        merge_edges(X, Y, NextId, EL, EL1),
        G1 = WL1+EL1.

merge_weights(X, Y, NID, WL0, WL1):-
        memberchk((X-W), WL0),
        memberchk((Y-W), WL0),
        NW is W+1,
        select((X-W), WL0, R1),
        select((Y-W), R1, R2),
        append(R2, [(NID-NW)], WL1).

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
        ;
           (member((X-Y), EL0);member((Y-X), EL0))
        ->
           select((X-Y), EL0, EL1)
        )
        .

replace_edge(ID0, ID1, Z, EL0, EL1):-
        select((ID0-Z), EL0, R1),
        append(R1, [(ID1-Z)], EL1).