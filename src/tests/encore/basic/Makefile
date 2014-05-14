ROOT_PATH=../../../..
ENCOREC=$(ROOT_PATH)/release/encorec

OUTFILES=$(shell ls *.out)

ENCORE_SOURCES=$(shell ls *.enc)
ENCORE_TARGETS=$(ENCORE_SOURCES:.enc=)

test: $(ENCOREC) $(ENCORE_TARGETS) compare_outs

$(ENCORE_TARGETS):
	@echo "compiling" $@
	@$(ENCOREC) $@.enc -clang

compare_outs:
	for o in $(OUTFILES); do \
	  echo $$o: ; \
	  ./$$(echo $$o | sed "s/\(.*\)\.out/\1/") | cmp $$o && echo OK ; \
	done

$(ENCOREC):
	make -C $(ROOT_PATH)

clean:
	rm -rf *.pony.c *.dSYM $(ENCORE_TARGETS)

.PHONY: test clean $(ENCORE_TARGETS)
