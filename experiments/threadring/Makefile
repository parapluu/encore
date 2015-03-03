CC=encorec
CFLAGS=

all: compile

clean: rm

compile: ootr.enc
	$(CC) $(CFLAGS) ootr.enc

encore: ootr.enc compile
	./ootr

java:
	cd java && javac threadring.java && \
	java  -server -XX:+TieredCompilation -XX:+AggressiveOpts threadring 50000000

racket:
	cd racket && racket threadring.racket 50000000

rm:
	-@rm -fr ootr.dSYM ootr_src ootr ootr.enc~ Makefile~ 2> /dev/null

.PHONY: encore java racket go
