HC=ghc
FLAGS=--make -c
IDENTIFIERS_OBJECTS=Identifiers.o Identifiers.hi

all: ast identifiers #east 

ast:
	make -C AST

#east:
#	make -C EAST

identifiers: $(IDENTIFIERS_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	make -C AST clean
	rm -f *.hi *.o *\~

.PHONY: all clean