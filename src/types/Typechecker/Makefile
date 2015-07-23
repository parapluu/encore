HC=ghc
FLAGS=--make -c -i$(INC_PATH)
SRC_PATH=../..
INC_PATH=..:$(SRC_PATH)/ir
ENVIRONMENT_OBJECTS=Environment.o Environment.hi
TYPECHECKER_OBJECTS=Typechecker.o Typechecker.hi
TYPEERROR_OBJECTS=TypeError.o TypeError.hi
CAPTURECHECKER_OBJECTS=Capturechecker.o Capturechecker.hi

all: types environment typechecker typeerror capturechecker

environment: $(ENVIRONMENT_OBJECTS)

typechecker: $(TYPECHECKER_OBJECTS)

capturechecker: $(CAPTURECHECKER_OBJECTS)

typeerror: $(TYPEERROR_OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all types environment typechecker typeerror clean 