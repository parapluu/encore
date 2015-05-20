HC=ghc
FLAGS=--make -c -i$(INC_PATH)
SRC_PATH=../..
INC_PATH=../:$(SRC_PATH)/ir:$(SRC_PATH)/types:$(SRC_PATH)/parser
OBJECTS=CCodeNames.hi CCodeNames.o Header.hi Header.o ClassDecl.hi ClassDecl.o Context.hi Context.o Expr.hi Expr.o Main.hi Main.o MethodDecl.hi MethodDecl.o Program.hi Program.o Type.hi Type.o Typeclasses.hi Typeclasses.o Closure.hi Closure.o

all: $(OBJECTS)

%.o %.hi : %.hs
	$(HC) $< $(FLAGS)

clean:
	rm -f *.hi *.o *\~

.PHONY: all clean
