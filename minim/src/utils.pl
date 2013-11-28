:-  module(utils, [next_id/3, del/3, size/2, findMinimalGraph/2]).

:- use_module(library(lists)).

next_id(WL, LID, NextId):-
        (
           LID is 0
        ->
           ids(WL, IDList),
           max_member(MaxId, IDList),
           NextId is MaxId+1
        ;
           NextId is LID+1
        ).

ids([], []).
ids([A-_B|Pairs], [A|Keys]) :-
    ids(Pairs, Keys).

del(X,[X|Tail],Tail).
del(X,[Y|Tail],[Y|Tail1]):-
        del(X,Tail,Tail1).

findMinimalGraph([G|Gs], Min) :-
    findMinimalGraph(Gs, G, Min).

size(WL+_+_, S):-
        length(WL, S).

findMinimalGraph([], Min, Min).
findMinimalGraph([G|Gs], G0, Min) :-
    size(G0, S0),
    size(G, S),
    (
       S0<S
    ->
       G1 = G0
    ;
       G1 = G
    ),
    findMinimalGraph(Gs, G1, Min).