LIBPONY_A_PATH=../src/runtime/bin/debug/libpony.a
RUNTIME_PATH=../src/runtime/
RUNTIME_INC_PATH=$(RUNTIME_PATH)/inc/
SRC_PATH=../src

PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

all: $(TARGETS) Main

% : %.pony.c $(LIBPONY_A_PATH)
	clang $@.pony.c -o $@ $(LIBPONY_A_PATH) -I$(RUNTIME_INC_PATH)

Main.pony.c: ../src/front/encorec
	../src/front/encorec > $@

$(LIBPONY_A_PATH):
	@echo Making libpony.a
	cd $(SRC_PATH); make clean_pony
	make -C $(SRC_PATH); make

clean:
	echo clean
	rm -f $(TARGETS)

.PHONY: test all
