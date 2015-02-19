Mandelbrot benchmark
===
This is a trivial benchmark for encore.
The two encore programs `mbr.enc` and `mbr-tree.enc` both computes a plot of the Mandelbrot set although they do it in a
different fashion. 


Both programs take the `size` of the plot as the first argument. `mbr-tree` also takes a second optional argument which is the `tree_depth`.
If only one argument is given to `mbr-tree` the depth of the tree defaults to `8`.


**NOTE:** `mbr-tree` may segfault sporadically if the `size` of the plot is lower than `2^tree_depth`.


Running the benchmarks
===
You can run the tests by first running `make`. This will build all of the implementations(for which you have the correct compiler installed) of the Mandelbrot benchmark.
The building will not be interrupted if you lack support for one of the languages. It will continue to build the languages for which you have support.

Then you can run the benchmarks by running `make bench`. This will run all of the programs which was successfully compiled. 
You can control the size of the plot by setting the flag `PLOT_SIZE`(which defaults to 16000) to the desired value.