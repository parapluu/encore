ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

OUTFILES=$(shell ls *.out)

ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

test: compiler compare_outs

$(ENCORE_TARGETS): %: %.enc $(ENCOREC)
#ls -l
	@echo "compiling '$@'"
	@$(ENCOREC) $@.enc -clang -c

compare_outs: clean
	make -k $(ENCORE_TARGETS) || true # try to build as many as possible but don't abort
	@echo 
	@echo Basic tests:
	@./test.sh $(ENCORE_TARGETS)

compiler:
	make -C $(ROOT_PATH) encorec

clean:
	rm -rf *.pony.c *.dSYM $(ENCORE_TARGETS)

.PHONY: test clean compiler compare_outs
