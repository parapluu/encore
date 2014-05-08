HC=ghc
FLAGS=--make -c -i$(INC_PATH)
INC_PATH=../:../../ir
MAIN_OBJECTS=Main.hi Main.o
PRETTY_OBJECTS=PrettyCCode.hi PrettyCCode.o

all: $(MAIN_OBJECTS) $(PRETTY_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all clean
