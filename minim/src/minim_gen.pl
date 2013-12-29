:- use_module(library(random)).
:- use_module(library(samsort)).

% random_minim(Nodes, EdgesPercent, WeightsPercent, Graph)
random_minim(N, EPct, WPct, WL+SEL) :-
	CompleteGraphSize is N*(N-1)//2,
	Edges is CompleteGraphSize*EPct//100,
	MaxWeight1 is N*WPct//100+1,
	(   for(I, 1, N), foreach(I-W, WL), param(MaxWeight1)
	do  random(1, MaxWeight1, W)
	),
	(   for(_J, 1, Edges), fromto([], EL0, [E|EL0], EL), param(N)
	do  random_edge(N, EL0, E)
	),
	sort(EL, SEL).

random_edge(N, EL0, A-B) :-
	N1 is N+1,
	repeat,
	   random(1, N1, A),
	   random(1, N1, B),
	   A < B,
	   nonmember(A-B, EL0),
	!.

compare(szp_pl, di_ka).
%compare(szp_pl, szp_pl).

test :-
	test(16, 20, 35).

test(N, EPct, WPct) :-
	compare(Mod1, Mod2),
	repeat,
	getrand(RandomSt),
	random_minim(N, EPct, WPct, G),
	time(findall(As, Mod1:findMinSol(G, As), Ass1), T1),
	time(findall(As, Mod2:findMinSol(G, As), Ass2), T2),
	format('~w~t~15|', RandomSt), nl, flush_output(user),
	format('~|~w:~t~10+~t~2f~10+, ', [Mod1,T1]), flush_output(user),
	format('~|~w:~t~10+~t~2f~10+\n', [Mod2,T2]),
	\+ same_sol_sets(Ass1, Ass2),
	write(RandomSt), nl,
	write(-------------), nl, write(G), nl,
	(   member(As, Ass1), print(As), nl, fail
	;   true
	),
	write(=============), nl,
	(   member(As, Ass2), print(As), nl, fail
	;   true
	).

same_sol_sets(S1, S2) :-
	samsort(S1, SS1),
	samsort(S2, SS2),
	SS1 == SS2.

time(Goal, TSec) :-
	statistics(runtime, [T0,_]), % T0 is the pure CPU execution time in
	                             % msecs since SICStus has been started.
				     % This excludes stack shifting
	                             % and garbage collection time.
	(Goal, fail; true),
	statistics(runtime, [T1,_]),
	TSec is (T1-T0)/1000.

end_of_file.





| ?- szp_pl:findMinSol([1-1,2-1]+[1-2], As).
As = [action(1,2,3,2)] ? ;
no
| ?- szp_pl:findMinSol([1-1,2-1,3-2,4-3]+[1-2,2-3,1-4], As).
As = [action(1,2,5,2),action(3,5,6,3),action(4,6,7,4)] ? ;
no
| ?- szp_pl:findMinSol([1-1,2-1,3-1,4-1]+[1-2,2-3,1-4], As).
As = [action(1,4,5,2),action(2,3,6,2),action(5,6,7,3)] ? ;
As = [action(2,3,5,2),action(1,4,6,2),action(5,6,7,3)] ? ;
no
| ?- test.
random(1689,7975,11526,425005073)
szp_pl:         0.01, szp_pl:         0.01
  C-c C-cProlog interruption (h for help)? a
% Execution aborted
| ?- test.
random(1016,28804,17650,425005073)
szp_pl:         0.00, szp_pl:         0.00
random(21113,8736,25225,425005073)
szp_pl:         0.23, szp_pl:         0.26
random(22713,9350,28305,425005073)
szp_pl:         0.21, szp_pl:         0.19
random(11918,20981,26197,425005073)
szp_pl:         1.13, szp_pl:         1.46
random(13683,29087,22698,425005073)
szp_pl:         0.00, szp_pl:         0.00
random(25152,3640,24225,425005073)
szp_pl:         0.00, szp_pl:         0.00
random(17375,5172,6712,425005073)
szp_pl:         0.01, szp_pl:         0.01
random(8180,23322,28982,425005073)
szp_pl:         0.00, szp_pl:         0.00
random(15704,27698,22245,425005073)
szp_pl:         0.00, szp_pl:         0.00
random(2139,5142,22375,425005073)
szp_pl:         0.40, szp_pl:         0.28
random(13908,19839,22462,425005073)
szp_pl:         0.92, szp_pl:         2.62
random(10089,18365,14552,425005073)
szp_pl:         0.02, szp_pl:         0.02
random(14192,24203,14513,425005073)
szp_pl:         0.01, szp_pl:         0.00
random(24425,29320,23250,425005073)
szp_pl:         0.00, szp_pl:         0.00
random(5559,8875,6206,425005073)
szp_pl:         0.01, szp_pl:         0.00
random(20403,25150,23528,425005073)
szp_pl:         0.00, szp_pl:         0.00
random(26971,20226,9723,425005073)
szp_pl:         0.01, szp_pl:         0.00
random(13095,2533,9909,425005073)
szp_pl:         0.01, szp_pl:         0.01
random(404,10683,7392,425005073)
szp_pl:         0.01, szp_pl:         0.00
random(18553,23956,1747,425005073)
szp_pl:         0.01, szp_pl:         0.02
random(6805,29640,15037,425005073)
szp_pl:         0.00, szp_pl:         0.00
random(15980,11494,30145,425005073)
szp_pl:         0.07, szp_pl:         0.09
