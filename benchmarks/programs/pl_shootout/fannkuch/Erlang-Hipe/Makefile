MEETING_AMOUNT = 11
TIME = /usr/bin/time -f '%C\nwall-time: %E\nuser-time: %U \nsystem-time: %S\nload: %P'  
all:
	erlc +native +"{hipe, [o3]}" fannkuchredux.erl
run:
	erl -smp enable -noshell -run  fannkuchredux main $(MEETING_AMOUNT)
bench:
	$(TIME) erl -smp enable -noshell -run  fannkuchredux main $(MEETING_AMOUNT)
