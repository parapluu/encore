LIBPONY_A_PATH=../src/runtime/pony/bin/debug/libpony.a
RUNTIME_PATH=../src/runtime/
RUNTIME_INC_PATH=$(RUNTIME_PATH)/inc/

PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

all: $(TARGETS)

% : %.pony.c $(LIBPONY_A_PATH)
	clang $@.pony.c -ggdb -o $@ $(LIBPONY_A_PATH) -I$(RUNTIME_INC_PATH)

$(LIBPONY_A_PATH):
	@echo Making libpony.a
	cd $(SRC_PATH); make clean_pony
	make -C $(SRC_PATH); make

clean:
	rm -f $(TARGETS)
	rm -rf *.dSYM
	rm -f *\~

.PHONY: clean all
