ROOT_PATH=../../..
ENCOREC=$(ROOT_PATH)/release/encorec

test: $(ENCOREC)
	make -C basic test

$(ENCOREC):
	make -C $(ROOT_PATH)

clean:
	make -C basic clean

.PHONY: test all encorec $(TARGETS)
