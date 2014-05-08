HC=ghc
FLAGS=--make -c
AST_OBJECTS=AST.o AST.hi
EXAMPLES_OBJECTS=Examples.o Examples.hi
PRETTYPRINTER_OBJECTS=PrettyPrinter.o PrettyPrinter.hi

all: ast prettyprinter examples

ast: $(AST_OBJECTS)

examples: ast $(EXAMPLES_OBJECTS)

prettyprinter: ast $(PRETTYPRINTER_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all ast examples prettyprinter clean