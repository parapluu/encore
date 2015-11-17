ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

all: test

clean:
	@rm -fr *.dSYM *_src $(ENCORE_TARGETS)

test:
	@$(foreach PROG,$(ENCORE_TARGETS), make -BRrk $(PROG);)
	@echo
	@echo Type Synonyms:
	@./test.sh $(ENCORE_TARGETS)

%: %.enc %.out
	@rm -fr $@
	@$(ENCOREC) $< -c

.PHONY: all test clean
