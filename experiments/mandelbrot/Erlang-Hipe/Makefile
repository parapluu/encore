all:
	erlc +native +"{hipe, [o3]}" mandelbrot.erl
run:
	erl -smp enable -noshell -run  mandelbrot main 16000
