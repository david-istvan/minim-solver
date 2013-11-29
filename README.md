minim-solver
============

This project delivers a Prolog-based program for finding the size of the optimal solution (the smallest reachable graph) from a given state of the [Minim game](http://www.kongregate.com/games/atomiccicada/minim). The state is represented by the list of the node weights and the list of edges.
 * project [minim](https://github.com/david-istvan/minim-solver/tree/master/minim) contains the Prolog code. ([SICStus Prolog](http://sicstus.sics.se/) was used.)
 * project [minim.java](https://github.com/david-istvan/minim-solver/tree/master/minim.java) provides an API for reaching the Prolog code from Java. (Based on the [Jasper](http://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_12.html) Prolog-Java interface).

These pages usually cover the obstacles to overcome when getting started:
 * [Fundamentals on mixing Java and Prolog via the Jasper library](http://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_12.html)
 * ["java.lang.UnsatisfiedLinkError: no spnative in java.library.path"](http://sicstus.sics.se/sicstus/docs/4.0.8/html/relnotes.html/Running-SICStus-from-Java.html)


Examples
--------

##### Trivial cases (minimization is straightforward)

![ScreenShot](https://dl.dropboxusercontent.com/u/44011277/bme/study/aflp/minim/g1.PNG)
```prolog
| ?- minimize([(10-1), (20-1), (30-2), (40-3)] + [(10-20), (20-30), (30-40)] + 0, G).
G = [43-4]+[]+43 ? 
yes
```

![ScreenShot](https://dl.dropboxusercontent.com/u/44011277/bme/study/aflp/minim/g2.PNG)
```prolog
| ?- minimize([(10-1), (20-1), (30-2), (40-3)] + [(10-20), (20-30), (30-40), (40-20)] + 0, G).
G = [43-4]+[]+43 ?
yes
```

![ScreenShot](https://dl.dropboxusercontent.com/u/44011277/bme/study/aflp/minim/g3.PNG)
```prolog
| ?- minimize([(10-1), (20-1), (30-2), (40-3)] + [(10-20), (20-30), (30-40), (30-10), (40-20)] + 0, G).
G = [43-4]+[]+43 ?
yes
```

##### Non-trivial cases (minimization requires backtracking)

![ScreenShot](https://dl.dropboxusercontent.com/u/44011277/bme/study/aflp/minim/g4.PNG)
This case should be solved as (20+30+40) + (10+50+60), resulting a minimal graph with the size of one.
Backtracking is the key for finding the globally best (minimal) graph, which is controlled in the findMinimalSolution/2 predicate.
```prolog
| ?- findMinimalSolution([(20-1), (30-1), (40-2), (10-1), (50-1), (60-2)] + [(10-20), (20-30), (30-40), (40-50), (50-60), (10-50)] + 0, S).
S = 1 ? 
yes
```
