## Broken tests

The following list contain broken benchmarks.
This could be because the benchmark was broken from the beginning,
or because if I enter the wrong input the test breaks.

- Parallel QuickSort

  I try to run it with `./Main 20 2 30 4`. It crashes.

- Big

  This benchmark crashes non-deterministically. the original number of
  messages is 16 * 1024. we had to reduce the size of numMessages because
  it uses too much memory, and the process was killed by OS before exiting.
  More information can be found at https://github.com/parapluu/encore/issues/743
