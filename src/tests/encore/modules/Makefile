ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

#INGORE=Lib.enc

#ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_SOURCES=Importer.enc DeepImporter.enc FarImporter.enc DupImporter.enc Qualified.enc \
  VectorTest.enc CallImported.enc StdLib.enc PassiveImport.enc

ENCORE_SOURCES += trait_import.enc

ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

build:
	@$(foreach PROG,$(ENCORE_TARGETS), make -Bqk $(PROG) || make -Bk $(PROG) || true;)


#test: 	embed_tl.o
test: build
	@echo
	@echo Module tests:
	@./test.sh $(ENCORE_TARGETS)

#embed_tl.o: embed_tl.c embed_tl.h
#	clang -c embed_tl.c

%: %.enc
#ls -l
	@rm -f $@
	@echo "compiling '$@'"
	@$(ENCOREC) $@.enc -clang -c -I lib:otherlib

clean:
	@rm -rf *.pony.c *.dSYM *_src $(ENCORE_TARGETS)
	@rm -f embed_tl.o

.PHONY: test clean
