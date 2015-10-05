CC=clang
TARGET=party
INC=/Users/kikofernandezreyes/Code/encore/release/inc/
LIB=/Users/kikofernandezreyes/Code/encore/release/lib/*.a
FLAGS=-std=gnu11 -ggdb -Wall -fms-extensions -Wno-microsoft -Wno-parentheses-equality -Wno-unused-variable -Wno-unused-value -lpthread -Wno-attributes
BENCH_FLAGS=-std=gnu11 -O3 -Wall -fms-extensions -Wno-microsoft -Wno-parentheses-equality -Wno-unused-variable -Wno-unused-value -lpthread -Wno-attributes
DEPS=party.c
TESTS=party_test.c

all: clean pony test

pony:
	cd ../../../ ; make ; cd $(OLDPWD)

$(TARGET): clean $(DEPS)
	$(CC) $(FLAGS) -DNDEBUG -I $(INC) -I .. $(DEPS) $(LIB)

bench: $(DEPS)
	$(CC) $(BENCH_FLAGS) -I $(INC) -I .. $(DEPS) $(LIB)

party_test: $(TESTS)
	export CK_FORK=no && \
	$(CC) $(FLAGS) -I $(INC) -I .. -I . $(TESTS) $(LIB) lib/libcheck.a -o $@;
	./party_test

test: party_test

clean:
	rm -rf $(TARGET) $(TARGET).dSYM party_test singlylist.o

.PHONY: all bench clean singlylist.o test
