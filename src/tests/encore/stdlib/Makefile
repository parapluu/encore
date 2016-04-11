ROOT_PATH=../../../..
ENCOREC=${ROOT_PATH}/release/encorec

TESTS = string

test: $(TESTS)

%: %.enc
	$(ENCOREC) $<
	./$@

clean:
	rm -f $(TESTS)
	rm -rf *.dSYM

.PHONY: clean test