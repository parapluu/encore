CC=clang
FLAGS=-std=gnu11 -ggdb -g -Wall -pthread -Wno-deprecated-declarations

all: future.o future_actor.o tit_lazy.o tit_eager.o 

future_actor.o: future_actor.c future_actor.h
	$(CC) $(FLAGS) -I../closure -I../pony/inc/ -I../pony/src -I../set future_actor.c -c

future.o: future.c future.h
	$(CC) $(FLAGS) -I../closure -I../pony/inc/ -I../pony/src -I../set future.c -c

ccontext.o: ccontext.c
	$(CC) $(FLAGS) -I../pony/inc/ -I../pony/src ccontext.c -c

tit_lazy.o: tit_lazy.c
	$(CC) $(FLAGS) -Werror -I../pony/inc -I../pony/src tit_lazy.c -c

tit_eager.o: tit_eager.c
	$(CC) $(FLAGS) -Werror -I../pony/inc -I../pony/src tit_eager.c -c

doc:
	doxygen

clean:
	rm -rf *.o html latex

.PHONY: all _ clean doc
