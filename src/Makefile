PONY_DIR=runtime/pony

all: pony

test: 
	make -C tests

pony: 
	cd $(PONY_DIR); premake4 gmake
	make -C $(PONY_DIR) config=debug
	make -C $(PONY_DIR) config=release

clean: clean_pony
	@echo "cleaning test files..."
	make -C tests clean
	@echo "done"

clean_pony:
	@echo "cleaning pony files..."
	[ -f $(PONY_DIR)/Makefile ] && make -C $(PONY_DIR) clean || true
	rm -rf $(PONY_DIR)/*.make $(PONY_DIR)/Makefile $(PONY_DIR)/bin $(PONY_DIR)/obj

.PHONY: all test pony clean clean_pony
