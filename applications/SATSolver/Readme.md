Important note: The solver does not work, probably due to issue #295. This can be fixed by adding this line``| Ty.isTypeVar ty    = AsExpr $ Deref (AsExpr $ Var "_this") `Dot` (typeVarRefName ty)`` just before the `otherwise` in `runtimeType ty` in the file `encore/src/back/CodeGen/Type.hs`.


INTRODUCTION
------------
This file contains compiling and running instructions for the Encore SAT.
To be able to compile and run the program the Encore version installed
needs to be installed from the features/plenary branch at github, otherwise
the program will not build and run.

BUILD INSTRUCTIONS
------------------
From  the root of the directory run:

$ encorec Main.enc

(Make sure that encore is properly installed and added to your path)

RUNNING EXAMPLES
----------------
At least a file containing a CNF as argument is required to run:
(file format described here: http://www.satcompetition.org/2004/format-solvers2004.html)

$ ./Main report_suite/bart11.shuffled.cnf


INPUT PARAMETER CONFIGURATIONS
------------------------------
The following arguments can be added before to control the solver:
[-s nsolvers [-heu val1 var1 val2 var2 ...] | -i niter | -ccl len | -ccn num | -ccm num | -r restart ]
-s n : use n solvers
Optionally after -s, -heu val1 var1 ... valn varn : use heuristic combinations (val1, var1), ..., (valn varn)
See the source code of Heurisic.enc for integer to heuristic mappings
-i n : spin n iterations in main solve loop before recieving clauses
-ccl n : send only clause of length <= n
-ccn n : send only n clauses per solver in total
-ccm n : send only every n:th clauses
-r n : restart after n iterations (using restarts may cause some heuristics to misbehave)

INPUT PARAMETER EXAMPLES
------------------------
Example of using three VSIDS solvers, while only sending every second conflict clause:
$ ./Main -s 3 -heu 2 6 3 6 4 6 -ccm 2 report_suite/dp04s04.shuffled.cnf

Example of using 5 solvers sharing no conflict clauses:
$ ./Main -s 5 -ccl 0 report_suite/8cnf20_2400000_2400000_1.shuffled.cnf

REPORTED BUGS
-------------
Currently the -r flag, has some problems and does for some input-cnfs produce
corrupt output. Therefore it is defaulted to be turned off.