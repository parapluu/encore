CC=clang
FLAGS=-std=gnu11 -ggdb -Wall -pthread -Wno-deprecated-declarations

all: ccontext.o future.o future_actor.o context.o doc

future_actor.o: future_actor.c future_actor.h
	$(CC) $(FLAGS) -I../pony/inc/ -I../pony/src -I../set future_actor.c -c

future.o: future.c future.h
	$(CC) $(FLAGS) -I../pony/inc/ -I../pony/src -I../set  future.c -c

context.o: context.c
	$(CC) $(FLAGS) context.c -c

ccontext.o: ccontext.c
	$(CC) $(FLAGS) ccontext.c -c

doc:
	doxygen

clean:
	rm -f *.o html latex

.PHONY: all clean doc
