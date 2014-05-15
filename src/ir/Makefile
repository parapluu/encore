HC=ghc
FLAGS=--make -c
EXAMPLES_OBJECTS=Examples.o Examples.hi

all: ast examples

ast:
	make -C AST

examples: ast $(EXAMPLES_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	make -C AST clean
	rm -f *.hi *.o *\~

.PHONY: all clean