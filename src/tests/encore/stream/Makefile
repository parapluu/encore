ENCOREC=../../../../release/encorec

STREAM_LIB=../../../../bundles/standard/StreamLib/StreamChain

all:
	$(ENCOREC) -I $(STREAM_LIB) streamChainInt.enc
	$(ENCOREC) -I $(STREAM_LIB) streamChainReal.enc

clean:
	rm -rf streamChainInt
	rm -rf streamChainReal

.PHONY: all clean
