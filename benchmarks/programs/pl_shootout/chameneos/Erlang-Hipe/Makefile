MEETING_AMOUNT = 80000
TIME = /usr/bin/time -f '%C\nwall-time: %E\nuser-time: %U \nsystem-time: %S\nload: %P'  
all:
	erlc +native +"{hipe, [o3]}" chameneos_redux.erl
run:
	erl -smp enable -noshell -run  chameneos_redux main $(MEETING_AMOUNT)
bench:
	$(TIME) erl -smp enable -noshell -run  chameneos_redux main $(MEETING_AMOUNT)
