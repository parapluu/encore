ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

ENCORE_SOURCES+=each.enc
ENCORE_SOURCES+=extract.enc
ENCORE_SOURCES+=general.enc
ENCORE_SOURCES+=liftf.enc
ENCORE_SOURCES+=liftv.enc
ENCORE_SOURCES+=sequence.enc
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

build:
	@$(foreach PROG,$(ENCORE_TARGETS), make -BRrk $(PROG);)

clean:
	@rm -fr *.dSYM *_src $(ENCORE_TARGETS)

test: build
	@$(foreach PROG,$(ENCORE_TARGETS), make -BRrk $(PROG);)
	@echo
	@echo Parallel Combinators:
	@./test.sh $(ENCORE_TARGETS)

%: %.enc %.out
	@rm -fr $@
	@$(ENCOREC) $< -c

.PHONY: all test clean
