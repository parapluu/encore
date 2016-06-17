ifndef CC
  CC:=gcc
endif

OBJS:=embed_tl.o
CFLAGS:=-std=c11

all: $(OBJS)

%.o: %.c %.h
	$(CC) $(CFLAGS) $< -c

clean:
	rm -rf $(OBJS)

.PHONY: clean