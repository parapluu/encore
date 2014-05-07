SRC_PATH=../../../src

$(ENCORE_TARGETS): $(SRC_PATH)/front/encorec
	$(SRC_PATH)/front/encorec $@

encorec:
	make -C $(SRC_PATH)
	ln -fs $(SRC_PATH)/front/encorec encorec

test: encorec
	make -C basic test

clean:
	echo clean
	rm -rf *.dSYM
	rm -f encore.*.pony.c
	rm -f *\~
	rm -f encorec
	rm -f encore.*

.PHONY: test all encorec compilefixedC $(TARGETS)
