ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

build:
	@echo "..." # the src/tests/test script will call this. It should not do anything

test:
	@./test.sh $(ENCORE_SOURCES)

clean:
	@rm -rf *.dSYM *_src $(ENCORE_TARGETS)
	@rm -f embed_tl.o

.PHONY: test clean
