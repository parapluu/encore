ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

#INGORE=Lib.enc

#OUTFILES=$(shell ls *.out)
OUTFILES=Importer.out DeepImporter.out FarImporter.out

#ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_SOURCES=Importer.enc DeepImporter.enc FarImporter.enc

ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

#test: 	embed_tl.o
test:
	@$(foreach PROG,$(ENCORE_TARGETS), make -Bqk $(PROG) || make -Bk $(PROG) || true;) 
	@echo 
	@echo Module tests:
	@./test.sh $(ENCORE_TARGETS)

#embed_tl.o: embed_tl.c embed_tl.h
#	clang -c embed_tl.c

%: %.enc
#ls -l
	@rm -f $@
	@echo "compiling '$@'"
	@$(ENCOREC) $@.enc -clang -c -I lib

clean:
	rm -rf *.pony.c *.dSYM *_src $(ENCORE_TARGETS)
	rm -f embed_tl.o

.PHONY: test clean
