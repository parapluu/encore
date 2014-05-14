ROOT_PATH=../..
ENCOREC=$(ROOT_PATH)/release/encorec

all: test benchmark encorec

test: $(ENCOREC)
	make -C encore test
	make -C compiler test
	make -C pony test

benchmark: encorec
	make -C benchmarks benchmark

$(ENCOREC):
	make -C $(ROOT_PATH)

clean:
	make -C encore clean
	make -C compiler clean
	make -C benchmarks clean
	make -C lib clean
	make -C pony clean

.PHONY: all test benchmark clean
