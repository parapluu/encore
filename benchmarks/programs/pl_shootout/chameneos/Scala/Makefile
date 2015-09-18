N = 800000
TIME = /usr/bin/time -f '%C\nwall-time: %E\nuser-time: %U \nsystem-time: %S\nload: %P'

all:
	scalac chameneos_redux.scala 
run:
	scala chameneosredux $(N)
clean:
	rm -f *.class

.PHONY: all clean
