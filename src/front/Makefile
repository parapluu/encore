compiler: encorec 

encorec: TopLevel.hs
	ghc --make TopLevel.hs -o encorec -i../parser:../ir:../back 

clean:
	rm -f encorec *.hi *.o

.PHONY: clean
