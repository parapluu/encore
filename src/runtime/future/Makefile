CC=clang
FLAGS=-std=c11 -ggdb -Wall

all: context.o

context.o: context.c
	$(CC) $(FLAGS) context.c -c

clean:
	rm -f context.o

.PHONY: all clean
