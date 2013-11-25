minim-solver
============

Aims at finding the optimal solution for the given snapshot of the [Minim game](http://www.kongregate.com/games/atomiccicada/minim).


The solver uses [SICStus Prolog](http://sicstus.sics.se/) and Java.
These pages usually cover the obstacles to overcome when getting started:
 * [Fundamentals on mixing Java and Prolog via the Jasper library](http://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_12.html)
 * ["java.lang.UnsatisfiedLinkError: no spnative in java.library.path"](http://sicstus.sics.se/sicstus/docs/4.0.8/html/relnotes.html/Running-SICStus-from-Java.html)


##### Examples for the trivial cases (minimization is straightforward)

![ScreenShot](http://david-istvan.github.io/images/minim/g1.PNG)
```prolog
| ?- minimize([(10-1), (20-1), (30-2), (40-3)] + [(10-20), (20-30), (30-40)], G).
G = [43-4]+[] ?
yes
```

![ScreenShot](http://david-istvan.github.io/images/minim/g2.PNG)
```prolog
| ?- minimize([(10-1), (20-1), (30-2), (40-3)] + [(10-20), (20-30), (30-40), (40-20)], G).
G = [43-4]+[] ?
yes
```

![ScreenShot](http://david-istvan.github.io/images/minim/g3.PNG)
```prolog
| ?- minimize([(10-1), (20-1), (30-2), (40-3)] + [(10-20), (20-30), (30-40), (30-10), (40-20)], G).
G = [43-4]+[] ?
yes
```

##### Examples for the non-trivial cases (minimization requires backtracking)

![ScreenShot](http://david-istvan.github.io/images/minim/g4.PNG)
This case should be solved as (20+30+40) + (10+50+60), resulting a minimal graph with the size of one. The algorithm, however, 
merges nodes #10 and #20 in the first step and terminates without finding the minimal solution.
```prolog
| ?- minimize([(10-1), (20-1), (30-1), (40-2), (50-1), (60-2)] + [(10-20), (20-30), (30-40), (40-50), (50-60), (10-50)], G).
G = [30-1,40-2,50-1,60-2,61-2]+[30-40,40-50,50-60,61-50,61-30] ?
no
```
