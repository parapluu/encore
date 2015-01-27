ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

OUTFILES=$(shell ls *.out)

ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

test: embed_tl.o
	@$(foreach PROG,$(ENCORE_TARGETS), make -Bqk $(PROG) || make -Bk $(PROG) || true;) 
	@echo 
	@echo Basic tests:
	@./test.sh $(ENCORE_TARGETS)

embed_tl.o: embed_tl.c embed_tl.h
	clang -c embed_tl.c

%: %.enc
#ls -l
	@rm -f $@
	@echo "compiling '$@'"
	@$(ENCOREC) $@.enc -clang -c

clean:
	rm -rf *.dSYM *_src $(ENCORE_TARGETS)
	rm -f embed_tl.o

.PHONY: test clean
