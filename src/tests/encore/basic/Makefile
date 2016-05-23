ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

embed_tl.o: embed_tl.c embed_tl.h
	clang -c embed_tl.c

test: embed_tl.o
	@./test.sh $(ENCORE_SOURCES)

clean:
	@rm -rf *.dSYM *_src $(ENCORE_TARGETS)
	@rm -f embed_tl.o

.PHONY: test clean
