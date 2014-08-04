all:	run

ENCOREC=../../../release/encorec
INC_DIR=-I../bitset -I../../../release/inc
LIB_DIR=-L../bitset -L../../../release/lib
LIBS=-lpony -lclosure -lfuture -lc
EXPECTED=../primes/26M.txt
BITSET_C=../bitset/bitset.c ../../../src/runtime/set/set.c
PS=mr

generate:
	$(ENCOREC) $(PS).enc

compile:
	clang -ggdb $(INC_DIR) $(LIB_DIR) $(LIBS) $(PS).pony.c $(BITSET_C) -o ps

debug: generate
	clang -ggdb $(INC_DIR) $(LIB_DIR) $(LIBS) $(PS).pony.c $(BITSET_C) -o ps
	./ps

release: generate
	clang -O3 $(INC_DIR) $(LIB_DIR) $(LIBS) $(PS).pony.c $(BITSET_C) -o ps

run:	release
	./ps > /dev/null

test:	generate compile
	./ps | sort -n > result.txt
	head -n `wc -l result.txt | awk '{print $$1}'` $(EXPECTED) > /tmp/expected.txt
	@(cmp --silent result.txt /tmp/expected.txt && echo "TEST PASSED") || echo "Output incorrect"
	@#../tools/scmp.rb result.txt /tmp/expected.txt

measure: release
	time ./ps > /tmp/result.txt
	cat /tmp/result.txt | sort -n > result.txt
	head -n `wc -l result.txt | awk '{print $$1}'` $(EXPECTED) > /tmp/expected.txt
	@(cmp --silent result.txt /tmp/expected.txt && echo "AND CORRECT") || echo "BUT BAD RESULT"
	@wc -l result.txt | awk '{print $$1}'

profile: release
	iprofiler -T 30 -timeprofiler -activitymonitor ./ps > /dev/null

clean:
	rm -f test.c
	rm -f ps
	rm -f ../bitset/bitset.o

