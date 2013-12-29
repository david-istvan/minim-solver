:- module(szp_pl, []).


:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(samsort)).

% findMinSol(Graph, Actions): Actions is a list of merge actions that
% reduce graph G to a state where no further merge can take place. Only
% solutions that result in a minimal size residual graph are enumerated.
% (That is, findMinSol/2 will return multiple solutions if there are
% multiple action lists resulting in different graphs of the same, minimal
% size)
% An element of the list Actions takes the form:

%     action(Node1,Node2,NewNode,NewWeight)

% meaning: Node1 is merged with Node2 resulting in a new node with id
% NewNode and weight NewWeight.

% Sample run:

% | ?- szp_pl:findMinSol([1-1,2-1,3-1,4-1]+[1-2,2-3,3-4,1-4], As).
% As = [action(1,2,5,2),action(3,4,6,2),action(5,6,7,3)] ? ;
% As = [action(1,4,5,2),action(2,3,6,2),action(5,6,7,3)] ? ;
% As = [action(2,3,5,2),action(1,4,6,2),action(5,6,7,3)] ? ;
% As = [action(3,4,5,2),action(1,2,6,2),action(5,6,7,3)] ? ;
% no
findMinSol(WL+EL, Actions) :-
	length(WL, N),
	length(Etalon, N),
	findall(JList, (   between(1, N, I),
			   findall(J, (   member(I-J, EL) 
				      ;   member(J-I, EL)
				      ), JList)
		       ),
		JLists
	       ),
	(   for(M, 1, N), foreach(M-W, WL), foreach(E, Etalon),
	    foreach(List, JLists),
	    foreach(nd(M,E,W,Links), Nodes0), param(Etalon)
	do  (   foreach(K, List), foreach(Link, Links), param(Etalon)
	    do  nth1(K, Etalon, Link)
	    )
	),
	NextId is N+1,
	append(Nodes0, NodesTail, Nodes),
	State0=sta(Nodes,NodesTail,NextId,_Opts),
	findall(SizeRem-Actions, findSol(State0, Actions, SizeRem),
		SizeActions0),
	keysort(SizeActions0, SizeActions),
	SizeActions = [Min-_|_],
	member(Min-Actions, SizeActions).


% :- type nodes == list(node).
% :- type node ---> nd(id, var, weight, links).
% :- type links == list(var)
	

findSol(State0, Actions, Size) :-
	log(initial_state:State0),
	merge_nodes(Actions, State0, State),
	sol_size(State, Size).

merge_nodes(As) -->
	(   mergeable_nodes -> {As = [Action|Actions]},
	    merge_node(Action), merge_nodes(Actions)
	;   {As = []}
	).

sol_size(St0, N) :-
	St0 = sta(Nodes,[],_,_), length(Nodes, N).

merge_node(Action, sta(Ns0,NsT0,N0,Opts), State) :-
	select_oe(nd(P1,V1,W,L1), Ns0, Ns1),
	select_oe(nd(P2,V2,W,L2), Ns1, Ns), P1 < P2,
	memberchk_id(V2, L1),
	memberchk_id(V1, L2),
	V1 = V2,
	append(L1, L2, L12), sort(L12, LN),
	WN is W+1, 
	NsT0 = [nd(N0,V1,WN,LN)|NsT],
	N is N0+1,
	Action = action(P1,P2,N0,WN),
	State = sta(Ns,NsT,N,Opts),
	log((action:Action, new_state:State)).


log(Term) :-
	(   current_prolog_flag(debugging, zip)
            % call `zip.' to switch on, `nozip.' to switch off
	    -> print(Term), nl
        ;   true
	).

mergeable_nodes(St, St) :-
	St = sta(Ns0,_NsT0,_N0,_Opts),
	select_oe(nd(_P1,_V1,W,L1), Ns0, Ns1),
	select_oe(nd(_P2,V2,W,_L2), Ns1, _Ns), 
	memberchk_id(V2, L1).

select_oe(_, L, _) :- var(L), !, fail.
select_oe(E, [E|L], L).
select_oe(E, [X|L], [X|R]) :-
	select_oe(E, L, R).

memberchk_id(E, [X|_L]) :- X == E, !.
memberchk_id(E, [_X|L]) :-
	memberchk_id(E, L).

:- multifile portray/1.


user:portray(sta(Ns0,NST0,_N0,_)) :-
	\+ \+ (
		NST0 = [] ->
		(   foreach(nd(P1,P1,_,_), Ns0)
		do  true
		),
		(   foreach(nd(P,_,W,L0), Ns0)
		do  samsort(L0, L), nl, print(P/W:L)
		)
	      ; print(sta(obsolete))
	      ).
user:portray(action(Node1,Node2,NewNode,NewWeight)) :-
	Weight is NewWeight-1,
	format('~w + ~w (~w) -> ~w (~w)',
	       [Node1,Node2,Weight,NewNode,NewWeight]).


end_of_file.


| ?- szp_pl:findMinSol([1-1,2-1,3-1,4-1]+[1-2,2-3,3-4,1-4], As).
As = [1 + 2 (1) -> 5 (2),3 + 4 (1) -> 6 (2),5 + 6 (2) -> 7 (3)] ? ;
As = [1 + 4 (1) -> 5 (2),2 + 3 (1) -> 6 (2),5 + 6 (2) -> 7 (3)] ? ;
As = [2 + 3 (1) -> 5 (2),1 + 4 (1) -> 6 (2),5 + 6 (2) -> 7 (3)] ? ;
As = [3 + 4 (1) -> 5 (2),1 + 2 (1) -> 6 (2),5 + 6 (2) -> 7 (3)] ? ;
no
% The debugger will first zip -- showing spypoints (zip)
yes
% zip
| ?- szp_pl:findMinSol([1-1,2-1,3-2,4-3,5-4]+[1-2,2-3,3-4,1-4,1-5,3-5,5-4], As).
initial_state:
1/1:[2,4,5]
2/1:[1,3]
3/2:[2,4,5]
4/3:[1,3,5]
5/4:[1,3,4]
action:1 + 2 (1) -> 6 (2),new_state:
3/2:[4,5,6]
4/3:[3,5,6]
5/4:[3,4,6]
6/2:[3,4,5,6]
action:3 + 6 (2) -> 7 (3),new_state:
4/3:[5,7,7]
5/4:[4,7,7]
7/3:[4,5,7]
action:4 + 7 (3) -> 8 (4),new_state:
5/4:[8,8,8]
8/4:[5,8]
action:5 + 8 (4) -> 9 (5),new_state:
9/5:[9]
As = [1 + 2 (1) -> 6 (2),3 + 6 (2) -> 7 (3),4 + 7 (3) -> 8 (4),5 + 8 (4) -> 9 (5)] ? ;
no
% zip
| ?- nozip.
% The debugger is switched off
yes
| ?- 
