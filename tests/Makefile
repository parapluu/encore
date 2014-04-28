SRC_PATH=../src

PONYCS=$(shell ls *.pony.c)
TARGETS=$(PONYCS:.pony.c=)

$(ENCORE_TARGETS): ../src/front/encorec
	../src/front/encorec $@

encorec:
	make -C $(SRC_PATH)
	ln -fs $(SRC_PATH)/front/encorec encorec

clean:
	echo clean
	rm -rf *.dSYM
	rm -f encore.*.pony.c
	rm -f *\~
	rm -f encorec
	rm -f encore.*

.PHONY: test all encorec
