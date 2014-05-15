HC=ghc
FLAGS=--make -c
EXAMPLES_OBJECTS=Examples.o Examples.hi
IDENTIFIERS_OBJECTS=Identifiers.o Identifiers.hi

all: ast examples identifiers

ast:
	make -C AST

identifiers: $(IDENTIFIERS_OBJECTS)

examples: ast $(EXAMPLES_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	make -C AST clean
	rm -f *.hi *.o *\~

.PHONY: all clean