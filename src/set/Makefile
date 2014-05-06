GCC=clang
FLAGS=-std=c11 -ggdb -Wall

TARGETS=set.c

all: set

set: set.c
	$(GCC) $(FLAGS) -c $(TARGETS)

test: set
	$(GCC) $(FLAGS) test.c -o test set.o
	./test

clean: 
	rm -f set.o test
	rm -f *~
	rm -rf *.dSYM

_PHONY: set test clean
