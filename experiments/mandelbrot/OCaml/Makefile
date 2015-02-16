PLOT_SIZE=1600

all:
	ocamlopt -noassert -unsafe -fno-PIC -nodynlink -inline 100 unix.cmxa mandelbrot.ml -o mandelbrot.ocaml_run
run:
	./mandelbrot.ocaml_run $(PLOT_SIZE)
