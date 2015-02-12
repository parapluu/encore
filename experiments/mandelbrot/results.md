
usr/bin/time -f '%C\nwall-time: %E\nuser-time:  %U\nload: %P'



Encore rows
===
```
./mbr --ponythreads 8 16000
wall-time: 2:36.83
user-time: 382.99 
System Time: 20.94
load: 257%

./mbr --ponythreads 8 16000
wall-time: 2:37.68
user-time: 386.82 
System Time: 20.48
load: 258%

./mbr --ponythreads 8 16000
wall-time: 2:37.48
user-time: 386.03 
System Time: 20.25
load: 257%
```

Encore Tree
===
```
./mbr-tree --ponythreads 8 16000 10
wall-time: 1:36.76
user-time: 395.92 
System Time: 8.11
load: 417%

./mbr-tree --ponythreads 8 16000 10
wall-time: 1:44.53
user-time: 349.72 
System Time: 9.88
load: 344%

./mbr-tree --ponythreads 8 16000 10
wall-time: 1:24.93
user-time: 384.59 
System Time: 7.65
load: 461%
``` 

GCC C
===
```
./gcc9 16000
wall-time: 0:05.80
user-time: 45.48 
System Time: 0.04
load: 783%

./gcc9 16000
wall-time: 0:05.97
user-time: 45.33 
System Time: 0.04
load: 758%

./gcc9 16000
wall-time: 0:05.87
user-time: 45.39 
System Time: 0.03
load: 772%
```

Erlang Hipe
===
```
erl -smp enable -noshell -run mandelbrot main 16000
wall-time: 3:56.21
user-time: 1719.05 
System Time: 7.13
load: 730%

erl -smp enable -noshell -run mandelbrot main 16000
wall-time: 4:00.59
user-time: 1748.61 
System Time: 7.53
load: 729%

erl -smp enable -noshell -run mandelbrot main 16000
wall-time: 3:59.08
user-time: 1745.91 
System Time: 7.06
load: 733%
```

Java
===
```
java -server -XX:+TieredCompilation -XX:+AggressiveOpts mandelbrot 16000
wall-time: 0:07.58
user-time: 57.95 
System Time: 0.28
load: 768%

java -server -XX:+TieredCompilation -XX:+AggressiveOpts mandelbrot 16000
wall-time: 0:07.61
user-time: 58.17 
System Time: 0.37
load: 768%

java -server -XX:+TieredCompilation -XX:+AggressiveOpts mandelbrot 16000
wall-time: 0:07.58
user-time: 57.97 
System Time: 0.27
load: 767%
```

OCaml
===
```
./mandelbrot.ocaml_run 16000
wall-time: 0:10.88
user-time: 84.92 
System Time: 0.20
load: 781%

./mandelbrot.ocaml_run 16000
wall-time: 0:11.08
user-time: 86.43 
System Time: 0.19
load: 781%

./mandelbrot.ocaml_run 16000
wall-time: 0:10.88
user-time: 84.96 
System Time: 0.21
load: 782%
```

Racket
===
```
racket mandelbrot.racket-4.racket 16000
wall-time: 0:17.48
user-time: 135.45 
System Time: 0.29
load: 776%

racket mandelbrot.racket-4.racket 16000
wall-time: 0:19.41
user-time: 150.06 
System Time: 0.29
load: 774%

racket mandelbrot.racket-4.racket 16000
wall-time: 0:18.42
user-time: 142.05 
System Time: 0.32
load: 772%
```















































