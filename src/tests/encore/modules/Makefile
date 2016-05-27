ENCOREC=../../../../release/encorec

build: DeepImporter FarImporter DupImporter Qualified

%: %.enc
	$(ENCOREC) $@.enc -I lib:otherlib

.PHONY: build
