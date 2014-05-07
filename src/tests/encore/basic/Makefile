SRC_PATH=../../../../src

ENCORE_TARGETS=$(shell ls *.enc)

encorec:
	make -C $(SRC_PATH)
	ln -fs $(SRC_PATH)/front/encorec encorec

all: encorec $(ENCORE_TARGETS)

test: all
	@echo "Testing"

$(ENCORE_TARGETS):
	@echo "compiling" $@
	@./encorec $@

clean:
	echo clean
	rm -rf *.dSYM
	rm -f *.pony.c
	rm -f *\~
	rm -f encorec
	rm -f encore.*

.PHONY: all encorec $(ENCORE_TARGETS)
