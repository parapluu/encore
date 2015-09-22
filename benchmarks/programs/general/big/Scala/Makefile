WORKER_AMOUNT = 256
TIME = /usr/bin/time -f '%C\nwall-time: %E\nuser-time: %U \nsystem-time: %S\nload: %P'

all:
	scalac big.scala 
run:
	scala big $(WORKER_AMOUNT)
clean:
	rm -f *.class

.PHONY: all clean
