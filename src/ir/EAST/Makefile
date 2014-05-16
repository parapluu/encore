HC=ghc
INC_PATH=..
FLAGS=--make -c -i$(INC_PATH)
EAST_OBJECTS=EAST.o EAST.hi
PRETTYPRINTER_OBJECTS=PrettyPrinter.o PrettyPrinter.hi

all: east prettyprinter

east: $(EAST_OBJECTS)

prettyprinter: $(PRETTYPRINTER_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all east prettyprinter clean