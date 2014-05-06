ROOT_PATH=..
ENCOREC=$(ROOT_PATH)/release/encorec
ENCORE_TARGETS=$(shell ls *.enc)
ENCOREC_FLAGS=-clang -c

encorec:
	make -C $(ROOT_PATH)

all: encorec $(ENCORE_TARGETS)

$(ENCORE_TARGETS):
	@echo "compiling" $@
	@$(ENCOREC) $(ENCOREC_FLAGS) $@

clean:
	echo clean
	rm -rf *.dSYM
	rm -f *.pony.c
	rm -f *\~
	rm -f encorec
	rm -f encore.*

.PHONY: all encorec $(ENCORE_TARGETS)
