ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

OUTFILES=$(shell ls *.out)

ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

test: 
	@$(foreach PROG,$(ENCORE_TARGETS), make -Bqk $(PROG) || make -Bk $(PROG);)
	@echo 
	@echo Basic tests:
	@./test.sh $(ENCORE_TARGETS)

%: %.enc
#ls -l
	@echo "compiling '$@'"
	@$(ENCOREC) $@.enc -clang -c

clean:
	rm -rf *.pony.c *.dSYM $(ENCORE_TARGETS)

.PHONY: test clean