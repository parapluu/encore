all:	run

ENCOREC=../../../release/encorec
INC_DIR=-I../bitset -I../../../release/inc
LIB_DIR=-L../bitset -L../../../release/lib
LIBS=-lpony -lclosure -lfuture -lset
EXPECTED=../primes/5M.txt
BITSET_C=../bitset/bitset.c
PS=sieve

generate:
	$(ENCOREC) $(PS).enc
	# Ugly trick until https://github.com/parapluu/mylittlepony/issues/10 is resolved
	@echo "#include \"bitset.h\"" > temp.c 
	@cat $(PS).pony.c >> temp.c 

debug: generate
	@clang -ggdb $(INC_DIR) $(LIB_DIR) $(LIBS) temp.c $(BITSET_C) -o ps
	./ps

release: generate
	@clang -O3 $(INC_DIR) $(LIB_DIR) $(LIBS) temp.c $(BITSET_C) -o ps

run:	release
	./ps > /dev/null

test:	release
	./ps | sort -n > result.txt
	head -n `wc -l result.txt | awk '{print $$1}'` $(EXPECTED) > /tmp/expected.txt
	diff result.txt /tmp/expected.txt 

measure: release
	time ./ps > /dev/null

profile: release
	iprofiler -T 30 -timeprofiler -activitymonitor ./ps > /dev/null

clean:
	rm -f test.c
	rm -f ps
	rm -f ../bitset/bitset.o

