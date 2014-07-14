RUNTIME_DIR=runtime
PONY_DIR=runtime/pony
SET_DIR=runtime/set
CLOSURE_DIR=runtime/closure
FUTURE_DIR=runtime/future

all: compiler pony

compiler: 
	make -C front

parser: 
	make -C parser

ir:
	make -C ir

types: 
	make -C types

back:
	make -C back

test: 
	make -C tests

runtime:
	cd $(RUNTIME_DIR); premake4 gmake
	make -C $(RUNTIME_DIR) config=debug
	make -C $(RUNTIME_DIR) config=release

pony: runtime
	cd $(PONY_DIR); premake4 gmake
	make -C $(PONY_DIR) config=debug
	make -C $(PONY_DIR) config=release

clean: clean_pony
	@echo "cleaning haskell files..."
	make -C front clean
	make -C ir clean
	make -C parser clean
	make -C back clean
	@echo "cleaning library files..."
	make -C $(RUNTIME_DIR) clean
	@echo "cleaning test files..."
	make -C tests clean
	@echo "done"

clean_pony:
	@echo "cleaning pony files..."
	[ -f $(PONY_DIR)/Makefile ] && make -C $(PONY_DIR) clean || true
	rm -rf $(PONY_DIR)/*.make $(PONY_DIR)/Makefile $(PONY_DIR)/bin $(PONY_DIR)/obj

.PHONY: all compiler runtime parser ir types back clean clean_pony
