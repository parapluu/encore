ROOT_PATH=../..

PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

$(ENCORE_TARGETS): ../src/front/encorec
	../src/front/encorec $@

test: encorec
	make -C encore test
	make -C compiler test
	make -C pony test

benchmark:
	make -C benchmarks benchmark


encorec:
	make -C $(ROOT_PATH)
	ln -fs $(ROOT_PATH)/release/encorec encorec

# use case: you've generated output, it doesn't work. You manually
# fixed the pony.c file. With this target, you can compile it:
compile_fixed_ponyc: $(TARGETS)
	@echo $(TARGETS)
	@echo "done"

$(TARGETS):
	clang $@.pony.c ../src/runtime/bin/debug/libpony.a -o $@ -I../src/runtime/inc/

clean:
	echo clean
	rm -rf *.dSYM
	rm -f encore.*.pony.c
	rm -f *\~
	rm -f encorec
	rm -f encore.*

.PHONY: test all encorec compilefixedC $(TARGETS)
