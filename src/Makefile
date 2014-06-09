PONY_DIR=runtime/pony
SET_DIR=runtime/set
CLOSURE_DIR=runtime/closure

all: compiler

compiler: parser ir types back set pony
	make -C front

parser: 
	make -C parser

ir:
	make -C ir

types: 
	make -C types

back:
	make -C back

set: 
	make -C $(SET_DIR)

closure: 
	make -C $(CLOSURE_DIR)

test: 
	make -C tests

pony: $(PONY_DIR)/bin/debug/libpony.a $(PONY_DIR)/bin/release/libpony.a 

$(PONY_DIR)/bin/debug/libpony.a: $(PONY_DIR)/Makefile
	make -C $(PONY_DIR) config=debug

$(PONY_DIR)/bin/release/libpony.a: $(PONY_DIR)/Makefile
	make -C $(PONY_DIR) config=release

$(PONY_DIR)/Makefile:
	cd $(PONY_DIR); premake4 gmake

clean: clean_pony
	@echo "cleaning haskell files..."
	make -C front clean
	make -C ir clean
	make -C parser clean
	make -C back clean
	@echo "cleaning library files..."
	make -C $(SET_DIR) clean
	@echo "cleaning test files..."
	make -C tests clean
	@echo "done"

clean_pony:
	@echo "cleaning pony files..."
	[ -f $(PONY_DIR)/Makefile ] && make -C $(PONY_DIR) clean || true
	rm -rf $(PONY_DIR)/*.make $(PONY_DIR)/Makefile $(PONY_DIR)/bin $(PONY_DIR)/obj

.PHONY: all compiler parser ir types back clean clean_pony set closure
