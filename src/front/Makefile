HC=ghc
FLAGS=--make -i$(INC_PATH)
INC_PATH=../parser:../ir:../types:../opt:../back

all: encorec

encorec: TopLevel.hs
	$(HC) $< -o encorec $(FLAGS)

clean:
	rm -f encorec *.hi *.o *\~

.PHONY: all encorec clean
