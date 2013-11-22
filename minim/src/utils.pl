:-  module(utils, [next_id/2]).

next_id(WL, NextId):-
        ids(WL, IDList),
        maxlist(IDList, MaxId),
        NextId is MaxId+1.

ids([], []).
ids([A-_B|Pairs], [A|Keys]) :-
    ids(Pairs, Keys).

max(X,Y,Max) :-
        X > Y,!,
        Max = X.
max(_X,Y,Max) :-
        Max = Y.

maxlist([],0).
maxlist([X],X).
maxlist([X,Y|List],Max) :-
        max(X,Y,Max1),
        maxlist([Max1|List],Max).