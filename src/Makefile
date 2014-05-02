PONY_DIR=./runtime/

all: compiler $(PONY_DIR)/bin/debug/libpony.a $(PONY_DIR)/bin/release/libpony.a

compiler: 
	make -C front

$(PONY_DIR)/bin/debug/libpony.a: $(PONY_DIR)/Makefile
	make -C $(PONY_DIR) config=debug

$(PONY_DIR)/bin/release/libpony.a: $(PONY_DIR)/Makefile
	make -C $(PONY_DIR) config=release

$(PONY_DIR)/Makefile:
	cd $(PONY_DIR); premake4 gmake

clean: clean_pony
	make -C front clean
	rm -rf encore.hello*
	@echo "done"

clean_pony:
	make -C $(PONY_DIR) clean || true
	rm -rf $(PONY_DIR)/*.make $(PONY_DIR)/Makefile $(PONY_DIR)/bin $(PONY_DIR)/obj

_PHONY: clean clean_pony compiler run
