SRC_DIR=src
ENCOREC=dist/build/encorec/encorec

RUNTIME_DIR=$(SRC_DIR)/runtime

CLOSURE_INC=$(RUNTIME_DIR)/closure/closure.h
CLOSURE_LIB=$(RUNTIME_DIR)/pony/bin/debug/libclosure.a

RELEASE_DIR=release
INC_DIR=$(RELEASE_DIR)/inc
LIB_DIR=$(RELEASE_DIR)/lib

all: encorec

encorec: dirs pony
	cabal configure
	cabal build
	cp -r $(ENCOREC) $(RELEASE_DIR)

fetch-hs-deps:
	cabal install --dependencies-only

test: encorec
	make -C $(SRC_DIR) test

SET_DIR=$(RUNTIME_DIR)/set
FUTURE_DIR=$(RUNTIME_DIR)/future
doc:
	haddock -o doc/html -h $$(find . -name "*.hs" | grep -v "\.#")
	make -C $(SET_DIR) doc
	make -C $(FUTURE_DIR) doc

dirs: $(INC_DIR) $(LIB_DIR)

$(INC_DIR):
	mkdir -p $(INC_DIR)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

PONY_INC=$(RUNTIME_DIR)/pony/inc/pony
PONY_LIB=$(RUNTIME_DIR)/pony/bin/debug/libpony.a
FUTURE_INC=$(FUTURE_DIR)/future.h
FUTURE_LIB=$(RUNTIME_DIR)/pony/bin/debug/libfuture.a
SET_INC=$(SET_DIR)/set.h
SET_LIB=$(RUNTIME_DIR)/pony/bin/debug/libset.a
pony: dirs $(PONY_INC)
	make -C $(SRC_DIR) pony
	cp -r $(PONY_INC) $(INC_DIR)
	cp -r $(SET_INC) $(INC_DIR)
	cp -r $(FUTURE_INC) $(INC_DIR)
	cp -r $(CLOSURE_INC) $(INC_DIR)
	cp -r $(PONY_LIB) $(LIB_DIR)
	cp -r $(FUTURE_LIB) $(LIB_DIR)
	cp -r $(CLOSURE_LIB) $(LIB_DIR)
	cp -r $(SET_LIB) $(LIB_DIR)

clean:
	cabal clean
	rm -rf dist
	make -C $(SRC_DIR) clean
	make -C programs clean
	rm -rf $(RELEASE_DIR)
	rm -rf $(INC_DIR)
	rm -rf $(LIB_DIR)
	rm -rf doc/html

# @ supresses output from shell
# - supresses ignore errors
vagrant:
	-@vagrant up


.PHONY: all encorec fetch-hs-deps test dirs pony clean vagrant
