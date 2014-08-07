LIBPONY_A_PATH=../src/runtime/pony/bin/debug/libpony.a
RELEASE_PATH=../release
RELEASE_INC_PATH=$(RELEASE_PATH)/inc/
RELEASE_LIB_PATH=$(RELEASE_PATH)/lib/

PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

all: $(TARGETS)

% : %.pony.c $(LIBPONY_A_PATH)
	clang $@.pony.c -ggdb -o $@ $(RELEASE_LIB_PATH)/* -I$(RELEASE_INC_PATH)

$(LIBPONY_A_PATH):
	@echo Making libpony.a
	cd $(SRC_PATH); make clean_pony
	make -C $(SRC_PATH); make

counter: counter.c
	  clang -ggdb -Wall -Werror $(RELEASE_LIB_PATH)/* -I $(RELEASE_INC_PATH) counter.c -o counter

clean:
	rm -f $(TARGETS)
	rm -rf *.dSYM
	rm -f *\~

.PHONY: clean all Futures
