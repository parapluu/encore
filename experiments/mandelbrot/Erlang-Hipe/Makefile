PLOT_SIZE = 16000
TIME = /usr/bin/time -f '%C\nwall-time: %E\nuser-time: %U \nsystem-time: %S\nload: %P'  
all:
	erlc +native +"{hipe, [o3]}" mandelbrot.erl
run:
	erl -smp enable -noshell -run  mandelbrot main $(PLOT_SIZE)

