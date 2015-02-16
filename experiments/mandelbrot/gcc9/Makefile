PLOT_SIZE=1600

all:
	gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 -D_GNU_SOURCE -mfpmath=sse -msse2 -fopenmp gcc9.c -o gcc9 

run:
	./gcc9 $(PLOT_SIZE)

