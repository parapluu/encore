HC=ghc
FLAGS=--make -c -i$(INC_PATH)
SRC_PATH=../..
INC_PATH=..:$(SRC_PATH)/ir:..:$(SRC_PATH)/types
OBJECTS=Parser.o Parser.hi

all: parser

parser: $(OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all parser clean 