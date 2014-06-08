CC=clang
FLAGS=-std=c11 -ggdb -Wall

all: ccontext.o future.o future_actor.o context.o

future_actor.o: future_actor.c future_actor.h
	$(CC) $(FLAGS) -I../pony/inc/ -I../pony/src -I../set future_actor.c -c

future.o: future.c future.h
	$(CC) $(FLAGS) -I../pony/inc/ -I../pony/src -I../set  future.c -c

context.o: context.c
	$(CC) $(FLAGS) context.c -c

ccontext.o: ccontext.c
	$(CC) $(FLAGS) ccontext.c -c

clean:
	rm -f context.o

.PHONY: all clean
