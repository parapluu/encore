CC=clang
FLAGS=-std=c11 -ggdb -Wall

OBJECTS=set.o

all: $(OBJECTS)

$(OBJECTS) : %.o : %.c
	$(CC) $(FLAGS) -c $<

test: set.o
	$(CC) $(FLAGS) test.c -o test set.o
	./test

clean: 
	rm -f set.o test
	rm -f *~
	rm -rf *.dSYM

.PHONY: test clean