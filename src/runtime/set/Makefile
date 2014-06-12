CC=clang
FLAGS=-std=c11 -ggdb -Wall

OBJECTS=set.o

all: $(OBJECTS)

$(OBJECTS) : %.o : %.c %.h
	$(CC) $(FLAGS) -c $<

test: set.o
	$(CC) $(FLAGS) test.c -o test set.o
	./test

doc: Doxyfile
	doxygen $<

clean: 
	rm -f set.o test
	rm -f *\~
	rm -rf *.dSYM
	rm -rf html
	rm -rf latex

.PHONY: test doc clean