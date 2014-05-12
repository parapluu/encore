HC=ghc
FLAGS=--make -i$(INC_PATH)
INC_PATH=../parser:../ir:../types:../back

all: encorec

encorec: TopLevel.hs
	$(HC) $< -o encorec -i$(INC_PATH)

clean:
	rm -f encorec *.hi *.o *\~

.PHONY: all encorec clean
