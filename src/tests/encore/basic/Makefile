ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

ENCORE_TARGETS=$(shell ls *.enc)

test: $(ENCOREC) $(ENCORE_TARGETS)

$(ENCORE_TARGETS):
	@echo "compiling" $@
	@$(ENCOREC) $@

$(ENCOREC):
	make -C $(ROOT_PATH)

clean:
	rm -f *.pony.c

.PHONY: test clean $(ENCORE_TARGETS)
