SRC_DIR=src

RUNTIME_DIR=$(SRC_DIR)/runtime

CONFIG=debug
CONFIG=release

RELEASE_DIR=release
INC_DIR=$(RELEASE_DIR)/inc
LIB_DIR=$(RELEASE_DIR)/lib

all: encorec

typecheck:
	cabal build --ghc-option=-fno-code

encorec: dirs pony install-deps
	export ENCORE_MODULES="$(CURDIR)/modules/" && \
	stack --install-ghc --system-ghc install --local-bin-path $(RELEASE_DIR)

install-deps:
	stack --install-ghc --system-ghc install --dependencies-only

test: encorec
	make -C $(SRC_DIR) test

coverage: dirs pony
	rm -rf coverage dist/hpc;
	find src -name "*.tix" -print0 | xargs -0 rm -rf;
	cabal clean;
	cabal configure --enable-tests --enable-coverage;
	ENCORE_BUNDLES="$(CURDIR)/bundles/" cabal build;
	cp dist/build/encorec/encorec $(RELEASE_DIR);
	-make -C $(SRC_DIR) test;
	mkdir -p coverage;
	find src -name "*.tix" -print0 | xargs -0 hpc sum > coverage/coverage.tix;
	hpc markup coverage/coverage.tix --hpcdir=dist/hpc/vanilla/mix/encorec/ --destdir=coverage;
	rm -rf coverage/coverage.tix dist/hpc;
	find src -name "*.tix" -print0 | xargs -0 rm -rf;
	echo "Open 'coverage/hpc_index.html' to see coverage results.";

SET_DIR=$(RUNTIME_DIR)/set
FUTURE_DIR=$(RUNTIME_DIR)/future
ENCORE_DIR=$(RUNTIME_DIR)/encore
doc: install-deps
	export ENCORE_BUNDLES="$(CURDIR)/bundles/" && \
	make -C doc/encore/ && \
	stack haddock

dirs: $(INC_DIR) $(LIB_DIR)

$(INC_DIR):
	mkdir -p $(INC_DIR)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

COMMON_INC=$(RUNTIME_DIR)/common/*
POOL_INC=$(RUNTIME_DIR)/pony/libponyrt/mem/pool.h
PONY_INC=$(RUNTIME_DIR)/pony/libponyrt/*.h
PONY_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libponyrt.a
FUTURE_INC=$(FUTURE_DIR)/future.h
FUTURE_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libfuture.a
OPTION_INC=$(RUNTIME_DIR)/adt/option.h
OPTION_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/liboptiontype.a
ENCORE_INC=$(ENCORE_DIR)/encore.h
ENCORE_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libencore.a
CLOSURE_INC=$(RUNTIME_DIR)/closure/closure.h
CLOSURE_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libclosure.a
TASK_INC=$(RUNTIME_DIR)/task/task.h
TASK_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libtask.a
PARTY_INC=$(RUNTIME_DIR)/party/party.h
PARTY_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libparty.a
STREAM_INC=$(RUNTIME_DIR)/stream/stream.h
STREAM_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libstream.a
ARRAY_INC=$(RUNTIME_DIR)/array/array.h
ARRAY_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libarray.a
TUPLE_INC=$(RUNTIME_DIR)/tuple/tuple.h
TUPLE_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/libtuple.a
RANGE_INC=$(RUNTIME_DIR)/range/range.h
RANGE_LIB=$(RUNTIME_DIR)/pony/bin/$(CONFIG)/librange.a

pony: dirs $(PONY_INC)
	make -C $(SRC_DIR) pony
	cp -r $(COMMON_INC) $(INC_DIR)
	cp -r $(POOL_INC) $(INC_DIR)
	cp -r $(PONY_INC) $(INC_DIR)
	cp -r $(FUTURE_INC) $(INC_DIR)
	cp -r $(OPTION_INC) $(INC_DIR)
	cp -r $(CLOSURE_INC) $(INC_DIR)
	cp -r $(TASK_INC) $(INC_DIR)
	cp -r $(PARTY_INC) $(INC_DIR)
	cp -r $(STREAM_INC) $(INC_DIR)
	cp -r $(ENCORE_INC) $(INC_DIR)
	cp -r $(ARRAY_INC) $(INC_DIR)
	cp -r $(TUPLE_INC) $(INC_DIR)
	cp -r $(RANGE_INC) $(INC_DIR)
	cp -r $(PONY_LIB) $(LIB_DIR)
	cp -r $(FUTURE_LIB) $(LIB_DIR)
	cp -r $(CLOSURE_LIB) $(LIB_DIR)
	cp -r $(TASK_LIB) $(LIB_DIR)
	cp -r $(OPTION_LIB) $(LIB_DIR)
	cp -r $(PARTY_LIB) $(LIB_DIR)
	cp -r $(ENCORE_LIB) $(LIB_DIR)
	cp -r $(STREAM_LIB) $(LIB_DIR)
	cp -r $(ARRAY_LIB) $(LIB_DIR)
	cp -r $(TUPLE_LIB) $(LIB_DIR)
	cp -r $(RANGE_LIB) $(LIB_DIR)

clean:
	stack clean
	rm -rf dist
	make -C doc/encore clean
	make -C $(SRC_DIR) clean
	rm -rf $(RELEASE_DIR)
	rm -rf $(INC_DIR)
	rm -rf $(LIB_DIR)
	rm -rf doc/html
	rm -rf coverage

vagrant:
	-@vagrant up

.PHONY: all encorec typecheck fetch-hs-deps test dirs pony clean doc vagrant coverage
