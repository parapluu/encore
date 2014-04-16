LIBPONY_A_PATH=../src/runtime/bin/debug/libpony.a
RUNTIME_PATH=../src/runtime/
RUNTIME_INC_PATH=$(RUNTIME_PATH)/inc/
SRC_PATH=../src

PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

all: $(TARGETS)

% : %.pony.c $(LIBPONY_A_PATH)
	@echo "building $@"
	@echo "depends on $@.pony.c"
	gcc $@.pony.c -o $@ $(LIBPONY_A_PATH) -I$(RUNTIME_INC_PATH)

$(LIBPONY_A_PATH):
	make -C $(SRC_PATH)

clean:
	echo clean
	rm -f $(TARGETS)

.PHONY: test all
