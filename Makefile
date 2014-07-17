SRC_DIR=src
ENCOREC=$(SRC_DIR)/front/encorec

RUNTIME_DIR=$(SRC_DIR)/runtime

SET_DIR=$(RUNTIME_DIR)/set
SET_INC=$(SET_DIR)/set.h
SET_LIB=$(PONY_DIR)/bin/debug/libset.a

CLOSURE_DIR=$(RUNTIME_DIR)/closure
CLOSURE_INC=$(CLOSURE_DIR)/closure.h
CLOSURE_LIB=$(PONY_DIR)/bin/debug/libclosure.a

PONY_DIR=$(RUNTIME_DIR)/pony
PONY_INC=$(PONY_DIR)/inc/pony
PONY_LIB=$(PONY_DIR)/bin/debug/libpony.a

FUTURE_DIR=$(RUNTIME_DIR)/future
FUTURE_INC=$(FUTURE_DIR)/future.h
FUTURE_LIB=$(PONY_DIR)/bin/debug/libfuture.a

RELEASE_DIR=release
INC_DIR=$(RELEASE_DIR)/inc
LIB_DIR=$(RELEASE_DIR)/lib

PONY_OBJECTS=$(PONY_LIB) $(PONY_INC)
SET_OBJECTS=$(SET_LIB) $(SET_INC)
CLOSURE_OBJECTS=$(CLOSURE_LIB) $(CLOSURE_INC)
FUTURE_OBJECTS=$(FUTURE_INC)

all: encorec

encorec: release
	make -C $(SRC_DIR) compiler
	cp -r $(ENCOREC) $(RELEASE_DIR)

test: encorec
	make -C $(SRC_DIR) test

doc:
	haddock -o doc/html -h $$(find . -name "*.hs" | grep -v "\.#")
	make -C $(SET_DIR) doc
	make -C $(FUTURE_DIR) doc

release: dirs pony

dirs: $(INC_DIR) $(LIB_DIR)

$(INC_DIR):
	mkdir -p $(INC_DIR)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

pony: dirs $(PONY_OBJECTS)
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
	make -C $(SRC_DIR) clean
	make -C programs clean
	rm -rf $(RELEASE_DIR)
	rm -rf $(INC_DIR)
	rm -rf $(LIB_DIR)
	rm -rf doc/html

.PHONY: all doc release encorec clean $(PONY_OBJECTS) $(SET_OBJECTS) $(CLOSURE_OBJECTS) $(FUTURE_OBJECTS)
