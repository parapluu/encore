WORKER_AMOUNT = 2560
TIME = /usr/bin/time -f '%C\nwall-time: %E\nuser-time: %U \nsystem-time: %S\nload: %P'  

all:
	erlc +native +"{hipe, [o3]}" big.erl
run:
	erl -smp enable -noshell -run  big main $(WORKER_AMOUNT)

clean:
	rm -f *.beam

.PHONY: all clean
