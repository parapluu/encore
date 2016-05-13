test:
	make -C basic clean test
	make -C concurrency clean test
	make -C par clean test
	make -C modules clean test
	make -C stdlib clean test
	make -C typesyn clean test

clean:
	make -C basic clean
	make -C concurrency clean
	make -C par clean
	make -C modules clean
	make -C stdlib clean
	make -C typesyn clean

.PHONY: test clean $(TEST_DIRECTORY)
