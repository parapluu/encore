PLOT_SIZE = 1600

all:
	-javac mandelbrot.java 
run:
	java  -server -XX:+TieredCompilation -XX:+AggressiveOpts mandelbrot $(PLOT_SIZE)
