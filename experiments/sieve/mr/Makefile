all:	run

ENCOREC=../../../release/encorec
INC_DIR=-I../../../release/inc
LIB_DIR=-L../../../release/lib
LIBS=-lpony -lclosure -lfuture -lset -lc
EXPECTED=../primes/26M.txt
BITSET_C=
PS=mr
PROG=./mr
DEFAULT_INPUT_SIZE=1048576

generate:
	$(ENCOREC) $(PS).enc

compile:
	clang -ggdb $(INC_DIR) $(LIB_DIR) $(LIBS) $(PS).pony.c $(BITSET_C) -o $(PROG)

debug: generate
	clang -ggdb $(INC_DIR) $(LIB_DIR) $(LIBS) $(PS).pony.c $(BITSET_C) -o $(PROG)

release: generate
	clang -O3 $(INC_DIR) $(LIB_DIR) $(LIBS) $(PS).pony.c $(BITSET_C) -o $(PROG)

run:	release
	$(PROG) $(DEFAULT_INPUT_SIZE) 2

test:	generate compile
	$(PROG) $(DEFAULT_INPUT_SIZE) 2
	echo "No test scaffolding yet for this type of printout"

measure: release
	$(PROG) $(DEFAULT_INPUT_SIZE) 2
	echo "No test scaffolding yet for this type of printout"

profile: release
	iprofiler -T 30 -timeprofiler -activitymonitor $(PROG) > /dev/null

clean:
	rm -f $(PS).pony.c
	rm -f *.o
	rm -f $(PROG)

