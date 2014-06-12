CC=clang
FLAGS=-std=gnu11 -ggdb -g -Wall -pthread -Wno-deprecated-declarations

all: ccontext.o future.o future_actor.o context.o context_future.o

future_actor.o: future_actor.c future_actor.h
	$(CC) $(FLAGS) -I../closure -I../pony/inc/ -I../pony/src -I../set future_actor.c -c

future.o: future.c future.h
	$(CC) $(FLAGS) -I../closure -I../pony/inc/ -I../pony/src -I../set future.c -c

context_future.o: context_future.c future.h context.o future_actor.o
	$(CC) $(FLAGS) -Werror -I../closure -I../pony/inc -I../pony/src -I../set context_future.c -c

context.o: context.c
	$(CC) $(FLAGS) -Werror context.c -c

ccontext.o: ccontext.c
	$(CC) $(FLAGS) -I../pony/inc/ -I../pony/src ccontext.c -c

doc:
	doxygen

clean:
	rm -rf *.o html latex

.PHONY: all _ clean doc
