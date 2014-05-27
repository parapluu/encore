HC=ghc
INC_PATH=..:../../types
FLAGS=--make -c -i$(INC_PATH)
AST_OBJECTS=AST.o AST.hi
PRETTYPRINTER_OBJECTS=PrettyPrinter.o PrettyPrinter.hi
META_OBJECTS=Meta.o Meta.hi

all: ast prettyprinter examples

ast: $(AST_OBJECTS)

prettyprinter: ast $(PRETTYPRINTER_OBJECTS)

meta: $(META_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all ast examples meta prettyprinter clean