HC=ghc
FLAGS=--make -c -i$(INC_PATH)
INC_PATH=../ir
TYPES_OBJECTS=Types.o Types.hi
ENVIRONMENT_OBJECTS=Environment.o Environment.hi
TYPECHECKER_OBJECTS=Typechecker.o Typechecker.hi
TYPEERROR_OBJECTS=TypeError.o TypeError.hi

all: types environment typechecker typeerror

types: $(TYPES_OBJECTS)

environment: $(ENVIRONMENT_OBJECTS)

typechecker: $(TYPECHECKER_OBJECTS)

typeerror: $(TYPEERROR_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all types environment typechecker typeerror clean 