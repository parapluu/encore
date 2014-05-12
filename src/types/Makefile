HC=ghc
FLAGS=--make -c -i$(INC_PATH)
INC_PATH=../ir
TYPES_OBJECTS=Types.o Types.hi
ENVIRONMENT_OBJECTS=Environment.o Environment.hi
TYPECHECKER_OBJECTS=Typechecker.o Typechecker.hi

all: types environment typechecker

types: $(TYPES_OBJECTS)

environment: $(ENVIRONMENT_OBJECTS)

typechecker: $(TYPECHECKER_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all types environment typechecker clean 