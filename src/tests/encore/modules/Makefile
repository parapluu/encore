ENCOREC=../../../../release/encorec

build: DeepImporter FarImporter Qualified DupImporter

%: %.enc
	$(ENCOREC) $@.enc -I lib:otherlib

clean:
	@rm -rf *.pony.c *.dSYM *_src $(ENCORE_TARGETS)

.PHONY: build clean
