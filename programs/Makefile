ROOT_PATH=..
ENCOREC=$(ROOT_PATH)/release/encorec
ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)
ENCOREC_FLAGS=-clang -c
PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

encorec:
	make -C $(ROOT_PATH)

all: encorec $(ENCORE_TARGETS)

$(ENCORE_SOURCES):
	@echo "compiling" $@
	$(ENCOREC) $(ENCOREC_FLAGS) $@

$(TARGETS): $(PONYCS)
	clang -Wall -ggdb $@.pony.c ../src/runtime/bin/debug/libpony.a -o $@ -I../src/runtime/inc/

# use case: you've generated output, it doesn't work. You manually
# fixed the pony.c file. With this target, you can compile it:
compile_fixed_ponyc: $(TARGETS)
	@echo $(TARGETS)
	@echo "done"


clean:
	echo clean
	rm -rf *.dSYM
	rm -f *.pony.c
	rm -f *\~
	rm -f encorec
	rm -rf a.out*
	rm -rf $(ENCORE_TARGETS)

.PHONY: all encorec $(ENCORE_SOURCES)
