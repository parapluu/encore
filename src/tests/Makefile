all: test benchmark

test: 
	make -C encore test
	make -C compiler test
	make -C pony test

benchmark: 
	make -C benchmarks benchmark

clean:
	make -C encore clean
	make -C compiler clean
	make -C benchmarks clean
	make -C lib clean
	make -C pony clean

.PHONY: all test benchmark clean
