PONY_DIR=./runtime/

$(PONY_DIR)/bin: $(PONY_DIR)/Makefile
	make -C $(PONY_DIR) config=debug

$(PONY_DIR)/Makefile:
	cd $(PONY_DIR); premake4 gmake

clean: clean_pony
	@echo "done"

clean_pony:
	make -C $(PONY_DIR) clean || true
	rm -rf $(PONY_DIR)/*.make $(PONY_DIR)/Makefile $(PONY_DIR)/bin $(PONY_DIR)/obj

_PHONY: clean clean_pony
