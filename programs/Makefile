ROOT_PATH=..
ENCOREC=$(ROOT_PATH)/release/encorec
ENCORE_TARGETS=$(shell ls *.enc)
ENCOREC_FLAGS=-clang -c
PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

encorec:
	make -C $(ROOT_PATH)

all: encorec $(ENCORE_TARGETS)

$(ENCORE_TARGETS):
	@echo "compiling" $@
	@$(ENCOREC) $(ENCOREC_FLAGS) $@

$(TARGETS):
	clang $@.pony.c ../src/runtime/bin/debug/libpony.a -o $@ -I../src/runtime/inc/

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
	rm -f encore.*

.PHONY: all encorec $(ENCORE_TARGETS)
