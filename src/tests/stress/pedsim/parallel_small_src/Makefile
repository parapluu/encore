CC=clang
TARGET=parallel_small
INC=/home/sten/Desktop/build/encore/release/inc/
LIB=/home/sten/Desktop/build/encore/release/lib/*.a
FLAGS=-std=gnu11 -Wall -fms-extensions -Wno-format -Wno-microsoft -Wno-parentheses-equality -Wno-unused-variable -Wno-unused-value  -lpthread -ldl -lm -Wno-attributes /home/sten/Desktop/build/encore/release/lib/*.a -I /home/sten/Desktop/build/encore/release/inc/ -I . 
BENCH_FLAGS=-std=gnu11 -Wall -fms-extensions -Wno-format -Wno-microsoft -Wno-parentheses-equality -Wno-unused-variable -Wno-unused-value  -lpthread -ldl -lm -Wno-attributes /home/sten/Desktop/build/encore/release/lib/*.a -I /home/sten/Desktop/build/encore/release/inc/ -I . 
DEFINES=
DEPS=shared.c Agent.encore.c File.encore.c Quad_tree.encore.c Quad.encore.c Item.encore.c Box.encore.c Tiling_box.encore.c XML_lib.encore.c XML_node.encore.c Main.encore.c String.encore.c

all: $(TARGET)

$(TARGET): $(DEPS)
	$(CC) $(FLAGS) -I $(INC) -I .. $(DEPS) $(LIB) $(LIB) $(DEFINES) -o $(TARGET)

bench: $(DEPS)
	$(CC) $(BENCH_FLAGS) -I $(INC) -I .. $(DEPS) $(LIB) $(LIB) $(DEFINES) -o $(TARGET)

clean:
	rm -rf $(TARGET) $(TARGET).dSYM

.PHONY: all bench clean
