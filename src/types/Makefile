HC=ghc
FLAGS=--make -c -i$(INC_PATH)
SRC_PATH=../
INC_PATH=$(SRC_PATH)/ir
TYPES_OBJECTS=Types.hi Types.o

all: types typechecker

typechecker:
	make -C Typechecker

types: $(TYPES_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~
	make -C Typechecker clean

.PHONY: all clean typechecker types