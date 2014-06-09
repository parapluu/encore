CC=clang
FLAGS=-std=c11 -ggdb -Wall

OBJECTS=closure.o

all: $(OBJECTS)

$(OBJECTS) : %.o : %.c %.h
	$(CC) $(FLAGS) -c $<

test: closure.o
	$(CC) $(FLAGS) test.c -o test closure.o
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