ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

test: 
	@./test.sh $(ENCORE_SOURCES)

clean:
	rm -rf *.dSYM *_src $(ENCORE_TARGETS)
	rm -f embed_tl.o

.PHONY: test clean
