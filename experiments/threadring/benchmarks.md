# Benchmarks for thread-ring

All benchmarks have been done in a Mac OS X 10.8, dual-core with 8GB-RAM

Proceeding:
    
    # forces completion of pending disk writes and forces disk
    # cache to be purge

    sync && sudo purge 

The execution time of the programs below do not make the execution time
of [thread-ring](http://benchmarksgame.alioth.debian.org/u32/performance.php?test=threadring).

In some ocassions, the difference between the expected value from the website and
our experience is abysmal (e.g. C++ performance).

Please find below different setups ordered by their real time (wall clock):

## [Go #5](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=threadring&lang=go&id=5)

    ./threadring.go 50000000  6.57s user 0.01s system 99% cpu 6.619 total



## [Clojure #2](http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=clojure&id=2)

    lein uberjar
	time java -server -XX:+TieredCompilation -XX:+AggressiveOpts -jar target/threadring-0.1.0-SNAPSHOT-standalone.jar 50000000

    java -server -XX:+TieredCompilation -XX:+AggressiveOpts -jar  50000000
	90.44s user 42.70s system 180% cpu 1:13.90 total

## [Racket](http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=racket&id=1)

    racket threadring.racket 50000000
	
    racket threadring.racket 50000000  68.69s user 17.64s system 99% cpu 1:26.65 total

## Asynchronous setup: tr.enc (**Encore**) Killing everything related to CD

    encorec -clang tr.enc
    time ./tr
    
    ./tr  167.76s user 54.15s system 186% cpu 1:58.70 total

## Asynchronous setup: threadring.enc (**Encore**) Killing everything related to CD

    ./threadring  215.64s user 51.46s system 199% cpu 2:13.92 total
	

## Asynchronous setup: tr.enc and exit on last message (**Encore**)

    ./tr  124.30s user 155.06s system 199% cpu 2:20.07 total

## Synchronous setup and exit on last message (**Encore**)

    ./threadring  180.40s user 173.07s system 199% cpu 2:57.25 total

## [Java #7](http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=java&id=7)

    javac threadring.java
    java  -server -XX:+TieredCompilation -XX:+AggressiveOpts threadring 50000000

    java -server -XX:+TieredCompilation -XX:+AggressiveOpts threadring 50000000
    68.73s user 139.27s system 101% cpu 3:25.73 total

## Asynchronous setup: tr.enc (**Encore**)

    ./tr  213.57s user 242.83s system 199% cpu 3:48.79 total
	
## [Pony]
    
    /bin/debug/ring --size 503 --count 1 --pass 50000000  214.26s user 247.69s system 199% cpu 3:51.63 total

## [Ocaml #2](http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=ocaml&id=2)

    ./threadring.ocaml-2.ocaml_run 50000000  51.05s user 244.33s system 126% cpu 3:54.22 total
    
## Synchronous setup and heavy us of futures: threadring.enc

    encorec -clang threadring.enc
    time ./threadring.enc

    ./threadring  237.62s user 231.54s system 199% cpu 3:55.23 total

## Tr.enc (single core)

    time ./tr --ponythreads 1
    ./tr --ponythreads 1  166.46s user 144.37s system 99% cpu 5:11.02 total

## threadring.enc (single core)

    time ./threadring --ponythreads 1
    ./threadring --ponythreads 1  168.65s user 147.10s system 99% cpu 5:16.30 total

## [Ocaml #3](http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=ocaml&id=3)

    ./threadring.ocaml-3.ocaml_run 50000000
	./threadring.ocaml-3.ocaml_run 50000000  54.87s user 760.82s system 150% cpu 9:02.60 total


## [C gcc](http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=gcc&id=1)

    /usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -pthread threadring.c -o threadring.gcc_run
	./threadring.gcc_run 50000000

	/threadring.gcc_run 50000000  36.64s user 724.29s system 122% cpu 10:22.35 total


## [C++, G++ #5](http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=gpp&id=5)
    /usr/bin/g++ -c -pipe -O3 -fomit-frame-pointer -march=native   threadring.gpp-5.c++ -o threadring.gpp-5.c++.o &&  \
	        /usr/bin/g++ threadring.gpp-5.c++.o -o threadring.gpp-5.gpp_run -lboost_system -lpthread
			
    ./threadring.gpp-5.gpp_run 50000000  106.72s user 1863.33s system 301% cpu 10:53.42 total

## [Ruby #2](http://benchmarksgame.alioth.debian.org/u32q/program.php?test=threadring&lang=yarv&id=2)

    ruby threadring.yarv-2.yarv 50000000
	ruby threadring.yarv-2.yarv 50000000  352.12s user 1023.07s system 141% cpu 16:13.53 total

## tr.enc (64 core)

    /usr/bin/time --verbose ./tr --ponythreads 32
	
    Command being timed: "./tr --ponythreads 32"
    User time (seconds): 2554.50
    System time (seconds): 630.74
    Percent of CPU this job got: 276%
    Elapsed (wall clock) time (h:mm:ss or m:ss): 19:12.89
    Average shared text size (kbytes): 0
    Average unshared data size (kbytes): 0
    Average stack size (kbytes): 0
    Average total size (kbytes): 0
    Maximum resident set size (kbytes): 5338212
    Average resident set size (kbytes): 0
    Major (requiring I/O) page faults: 0
    Minor (reclaiming a frame) page faults: 1334727
    Voluntary context switches: 495204614
    Involuntary context switches: 15847042
	Swaps: 0
    File system inputs: 0
    File system outputs: 0
    Socket messages sent: 0
    Socket messages received: 0
    Signals delivered: 0
     Page size (bytes): 4096
     Exit status: 0

    2554.50s user 630.74s system 276% cpu 19:12.89 total

## threadring.enc (64 core)

     /usr/bin/time --verbose ./threadring --ponythreads 32
     Finish: 292
	 Command being timed: "./threadring --ponythreads 32"
	 User time (seconds): 2840.07
	 System time (seconds): 721.65
     Percent of CPU this job got: 257%
     Elapsed (wall clock) time (h:mm:ss or m:ss): 23:03.46
     Average shared text size (kbytes): 0
     Average unshared data size (kbytes): 0
     Average stack size (kbytes): 0
     Average total size (kbytes): 0
     Maximum resident set size (kbytes): 5283532
     Average resident set size (kbytes): 0
     Major (requiring I/O) page faults: 0
     Minor (reclaiming a frame) page faults: 1321050
     Voluntary context switches: 612796255
     Involuntary context switches: 21410138
     Swaps: 0
     File system inputs: 0
     File system outputs: 0
     Socket messages sent: 0
     Socket messages received: 0
     Signals delivered: 0
     Page size (bytes): 4096
     Exit status: 0

    2840.07s user 721.65 system 257% cpu 23.03.46 total
    
