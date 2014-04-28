LIBPONY_A_PATH=../src/runtime/bin/debug/libpony.a
RUNTIME_PATH=../src/runtime/
RUNTIME_INC_PATH=$(RUNTIME_PATH)/inc/


ENCORE_PONYCS=HelloPonyworld.pony.c Countdown.pony.c TheOthers.pony.c PrimitiveSend.pony.c StringSend.pony.c ActorSend.pony.c
ENCORE_TARGETS=$(ENCORE_TARGETS:.pony.c)

SRC_PATH=../src

PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

$(ENCORE_TARGETS): ../src/front/encorec
	../src/front/encorec $@

all: $(TARGETS)

% : %.pony.c $(LIBPONY_A_PATH)
	clang $@.pony.c -ggdb -o $@ $(LIBPONY_A_PATH) -I$(RUNTIME_INC_PATH)

encorec:
	make -C ../src
	ln -fs ../src/front/encorec encorec

#encore.hello.pony.c: encorec
#	../src/front/encorec hello

#encore.theOthers.pony.c: encorec
#	../src/front/encorec theOthers

#encore.countdown.pony.c: encorec
#	../src/front/encorec countdown

$(LIBPONY_A_PATH):
	@echo Making libpony.a
	cd $(SRC_PATH); make clean_pony
	make -C $(SRC_PATH); make

clean:
	echo clean
	rm -f $(TARGETS) Hello.pony.c
	rm -rf *.dSYM
	rm -f encore.*.pony.c
	rm -f *\~
	rm -f encorec
	rm -f encore.*

.PHONY: test all encorec
