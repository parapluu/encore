OBJS:=embed_tl.o

all: $(OBJS)

%.o: %.c %.h
	$(CC) $(CFLAGS) $< -c

clean:
	rm -rf $(OBJS)

.PHONY: clean