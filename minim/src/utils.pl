:-  module(utils, [next_id/2]).

:- use_module(library(lists)).

next_id(WL, NextId):-
        ids(WL, IDList),
        max_member(MaxId, IDList),
        NextId is MaxId+1.

ids([], []).
ids([A-_B|Pairs], [A|Keys]) :-
    ids(Pairs, Keys).