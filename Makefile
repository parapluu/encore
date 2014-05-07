SRC_DIR=src
ENCOREC=$(SRC_DIR)/front/encorec

SET_DIR=$(SRC_DIR)/set
SET_INC=$(SET_DIR)/set.h
SET_LIB=$(SET_DIR)/set.o

PONY_DIR=$(SRC_DIR)/runtime
PONY_INC=$(PONY_DIR)/inc/pony
PONY_LIB=$(PONY_DIR)/bin/debug/libpony.a

RELEASE_DIR=release
INC_DIR=$(RELEASE_DIR)/inc
LIB_DIR=$(RELEASE_DIR)/lib

PONY_TARGETS=$(PONY_LIB) $(PONY_INC)
SET_TARGETS=$(SET_LIB) $(SET_INC)

encorec: runtime $(ENCOREC)
	cp -r $(ENCOREC) $(RELEASE_DIR)

$(ENCOREC):
	make -C $(SRC_DIR) compiler

runtime: $(PONY_TARGETS) $(SET_TARGETS)
	mkdir -p $(INC_DIR)
	mkdir -p $(LIB_DIR)
	cp -r $(SET_INC) $(INC_DIR)
	cp -r $(SET_LIB) $(LIB_DIR)
	cp -r $(PONY_INC) $(INC_DIR)
	cp -r $(PONY_LIB) $(LIB_DIR)

$(PONY_TARGETS):
	make -C $(SRC_DIR) pony

$(SET_TARGETS):
	make -C $(SRC_DIR) set
clean:
	make -C $(SRC_DIR) clean
	rm -rf $(RELEASE_DIR)
	rm -rf $(INC_DIR)
	rm -rf $(LIB_DIR)

.PHONY: encorec clean