:-  module(utils, [next_id/2, del/3]).

:- use_module(library(lists)).

next_id(WL, NextId):-
        ids(WL, IDList),
        max_member(MaxId, IDList),
        NextId is MaxId+1.

ids([], []).
ids([A-_B|Pairs], [A|Keys]) :-
    ids(Pairs, Keys).

del(X,[X|Tail],Tail).
del(X,[Y|Tail],[Y|Tail1]):-
        del(X,Tail,Tail1).