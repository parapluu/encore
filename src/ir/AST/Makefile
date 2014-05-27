HC=ghc
INC_PATH=..:../../types
FLAGS=--make -c -i$(INC_PATH)
AST_OBJECTS=AST.o AST.hi
PRETTYPRINTER_OBJECTS=PrettyPrinter.o PrettyPrinter.hi
META_OBJECTS=Meta.o Meta.hi
UTIL_OBJECTS=Util.o Util.hi

all: ast prettyprinter util

ast: $(AST_OBJECTS)

prettyprinter: ast $(PRETTYPRINTER_OBJECTS)

meta: $(META_OBJECTS)

util: ast $(UTIL_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all ast examples meta prettyprinter util clean