PONY_DIR=runtime/pony

all: pony

test:
	make -C tests

stress:
	make -C tests stress

pony:
	cd $(PONY_DIR); premake4 gmake $(use)
	make -C $(PONY_DIR) config=debug
	make -C $(PONY_DIR) config=release

clean: clean_pony clean_dtrace
	@echo "cleaning test files..."
	make -C tests clean
	@echo "done"

clean_pony:
	@echo "cleaning pony files..."
	[ -f $(PONY_DIR)/Makefile ] && make -C $(PONY_DIR) clean || true
	rm -rf $(PONY_DIR)/*.make $(PONY_DIR)/Makefile $(PONY_DIR)/bin $(PONY_DIR)/obj

clean_dtrace:
	rm -f $(PONY_DIR)/../common/dtrace_probes.h
	rm -f $(PONY_DIR)/../common/encore_probes.h
	rm -f $(PONY_DIR)/../common/dtrace_probes.o
	rm -f $(PONY_DIR)/../common/encore_probes.o

.PHONY: all test stress pony clean clean_pony
