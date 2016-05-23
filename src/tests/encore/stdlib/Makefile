ROOT_PATH=../../../..
ENCOREC=${ROOT_PATH}/release/encorec

TESTS = string

string: string.enc
	$(ENCOREC) $<

test: $(TESTS)
	./string

clean:
	@rm -f $(TESTS)
	@rm -rf *.dSYM

.PHONY: clean test
