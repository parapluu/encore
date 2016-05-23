ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

all:
	@echo ""

test:
	@./test.sh $(ENCORE_SOURCES)

clean:
	rm -rf *.dSYM *_src $(ENCORE_TARGETS)

.PHONY: test clean
