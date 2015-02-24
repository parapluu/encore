ROOT_PATH=..
ENCOREC=$(ROOT_PATH)/release/encorec
ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)
ENCOREC_FLAGS=-clang

all: encorec $(ENCORE_TARGETS)

encorec:
	make -C $(ROOT_PATH)

$(ENCORE_TARGETS): %: %.enc
	$(ENCOREC) $(ENCOREC_FLAGS) $<

clean:
	rm -rf *.dSYM
	rm -f *.pony.c
	rm -f *.encore.c
	rm -f *\~
	rm -f encorec
	rm -f $(ENCORE_TARGETS)

.PHONY: all encorec compile_fixed_ponyc
