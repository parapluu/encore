# Benchmarks for thread-ring

All benchmarks have been done in a Mac OS X 10.8, dual-core with 8GB-RAM

Proceeding:
    
    # forces completion of pending disk writes and forces disk
    # cache to be purge

    sync && sudo purge 
    

## Synchronous setup and heavy us of futures: threadring.enc

    encorec -clang threadring.enc
    time ./threadring.enc

    ./threadring  215.64s user 51.46s system 199% cpu 2:13.92 total

## Asynchronous setup: tr.enc

    encorec -clang tr.enc
    time ./tr
    
    ./tr  167.76s user 54.15s system 186% cpu 1:58.70 total

## Racket 5, pos: #25

    racket threadring.racket 50000000
	
    racket threadring.racket 50000000  68.69s user 17.64s system 99% cpu 1:26.65 total

## C++, G++5, pos: 
    /usr/bin/g++ -c -pipe -O3 -fomit-frame-pointer -march=native   threadring.gpp-5.c++ -o threadring.gpp-5.c++.o &&  \
	        /usr/bin/g++ threadring.gpp-5.c++.o -o threadring.gpp-5.gpp_run -lboost_system -lpthread
			
    ./threadring.gpp-5.gpp_run 50000000  106.72s user 1863.33s system 301% cpu 10:53.42 total

## Clojure #2, pos: #13

    lein uberjar
	time java -server -XX:+TieredCompilation -XX:+AggressiveOpts -jar target/threadring-0.1.0-SNAPSHOT-standalone.jar 50000000

    java -server -XX:+TieredCompilation -XX:+AggressiveOpts -jar  50000000
	90.44s user 42.70s system 180% cpu 1:13.90 total


## C gcc, pos: #21

    /usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -pthread threadring.c -o threadring.gcc_run
	./threadring.gcc_run 50000000

	/threadring.gcc_run 50000000  36.64s user 724.29s system 122% cpu 10:22.35 total

## Java #7: pos: #35

    javac threadring.java
    java  -server -XX:+TieredCompilation -XX:+AggressiveOpts threadring 50000000

    java -server -XX:+TieredCompilation -XX:+AggressiveOpts threadring 50000000
    68.73s user 139.27s system 101% cpu 3:25.73 total
