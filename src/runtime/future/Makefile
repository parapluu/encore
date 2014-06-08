CC=clang
FLAGS=-std=gnu11 -ggdb -Wall -pthread

all: context.o future.o future_actor.o

future_actor.o: future_actor.c future_actor.h
	$(CC) $(FLAGS) -I../pony/inc/ -I../pony/src -I../set future_actor.c -c

future.o: future.c future.h
	$(CC) $(FLAGS) -I../pony/inc/ -I../pony/src -I../set  future.c -c

context.o: context.c
	$(CC) $(FLAGS) context.c -c

clean:
	rm -f *.o

.PHONY: all clean
