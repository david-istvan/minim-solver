:- module(di_ka, []).

:- use_module(library(lists)).
:- use_module(utils).

/*
 * The input point of the program. This predicate
 * collects all the solution candidates found by
 * #minimize/2 and selects the minimal one as the
 * solution using #findMinimalSize/2.
 */
findMinSol(G, S):-
        findall(X, minimize(G+0, X), C),
        findMinimalSize(C, S).

/*
 * Given a list of graphs (GL), the predicate
 * returns the size of the smallest one.
 */
findMinimalSize(GL, S):-
        findMinimalGraph(GL, M),
        size(M, S).

/*
 * Executes the minimization steps until it is feasible
 * and returns the minimal graph.
 */
minimize(G0, G):-
        (
           log('>>>MINIMIZE/2... input graph: ~w\n', G0),
           not(mergeable(G0, _E))
        ->
           log('>>>MINIMIZE/2... not mergeable: ~w\n', G0),
           G=G0
        ;
           mergeable(G0, E),
           merge(E, G0, G1),
           log('>>>MINIMIZE/2... new graph: ~w\n', G1),
           minimize(G1, G)
        ).
not(P) :- (call(P) -> fail ; true).

/*
 * Decides wether there are any mergeable edges in
 * the given graph structure and returns a possible
 * edge for merging.
 */
mergeable(WL+EL+_, E):-
        log('>>>MERGEABLE/2... mergeability check: WL: ~w, EL: ~w\n', [WL,EL]),
        member((X-W), WL),
        member((Y-W), WL),
        E=(X-Y),
        member(E, EL).

/*
 * Executes the merging step consisting of:
 * -calculating the next ID for the merged node
 * -merging the weight list
 * -merging the edge list
 */
merge(X-Y, WL+EL+LID, G1):-
        log('>>>MERGE/3... merging ~w with ~w in ~w + ~w\n', [X, Y, WL, EL]),
        next_id(WL, LID, NextId),
        mergeWeights(X, Y, NextId, WL, WL1),
        mergeEdges(X, Y, NextId, EL, EL1),
        remove_dups(EL1, EL2),
        remove_dups(WL1, WL2),
        G1 = WL2+EL2+NextId.

/*
 * Merges the weight list by selecting two nodes
 * of the same weight (W) and creating a new one with
 * the weight of W+1.
 */
mergeWeights(X, Y, NID, WL0, WL1):-
        memberchk((X-W), WL0),
        memberchk((Y-W), WL0),
        NW is W+1,
        select((X-W), WL0, R1),
        select((Y-W), R1, R2),
        WL1 = [(NID-NW)|R2].

/*
 * Merges the edges. First, it deletes the edge between
 * the two nodes to be merged, then it redirects every edge
 * connecting any of the two nodes to be merged with any other
 * node. Finally, the possible loops are removed.
 */
mergeEdges(X, Y, NID, EL0, EL1):-
        del((X-Y), EL0, R1),
        redirectEdges(X, Y, NID, R1, R2),
        removeLoops(R2, EL1).

/*
 * Handles the redirections of every edge
 * affected by the merger.
 */
redirectEdges(X, Y, NID, EL0, EL1):-
        (
           not(redirectableEdge(X, Y, EL0, _))
        ->
           EL1 = EL0
        ;
           redirectableEdge(X, Y, EL0, E),
           redirectEdge(E, NID, EL0, EL2),
           redirectEdges(X, Y, NID, EL2, EL1)
        ).

/*
 * Returns a redirectable edge. Note that the node not
 * being one of the merged ones (Z) is always returned
 * in the second position in the edge. This convention
 * simplifies the #redirectEdge/4 predicate.
 */
redirectableEdge(X, Y, EL, E):-
        (
           member((X-Z), EL)
        ->
           E = (X-Z)
        ;
           member((Y-Z), EL)
        ->
           E = (Y-Z)
        ;
           member((Z-X), EL)
        ->
           E = (X-Z)
        ;
           member((Z-Y), EL)
        ->
           E = (Y-Z)
        ).

/*
 * Executes the redirection of an edge. Y denotes
 * the node not being one of the ones to be merged.
 */
redirectEdge(X-Y, NID, EL0, EL1):-
        (
           member((X-Y), EL0)
        ->
           del((X-Y), EL0, R1),
           EL1 = [(NID-Y)|R1]
        ;
           member((Y-X), EL0)
        ->
           del((Y-X), EL0, R1),
           EL1 = [(NID-Y)|R1]
        ).

/*
 * Loops are not allowed in the graph structure of Minim,
 * but might occur when merging nodes. Its' removal is handled
 * by this predicate.
 */
removeLoops(EL0, EL1):-
        (
           not(member((X-X), EL0))
        ->
           EL1=EL0
        ;
           del((X-X), EL0, EL1)
        ).