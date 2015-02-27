#
#	This makefile should enable people to build and test all the versions  of the mandelbrot
#	plot for which the appropriate compilers are installed. 
#
#	run 'make bench' to perform the benchmark tests for the languages available on your system.
#	run 'make PLOT_SIZE=nnnnn bench' to run benchmarks with a specific plotsize.
#	
#	Note that the tree version of the encore program might not work for small values of PLOT_SIZE.
#


PLOT_SIZE = 1600
TIME = /usr/bin/time -f 'wall-time: %E\nuser-time: %U \nsystem-time: %S\nload: %P' 
ENC_TREE=mbr-tree
ENC_ROW=mbr
RACKET=Racket/mandelbrot.racket-4.racket
JAVA=java/mandelbrot.class
OCAML=OCaml/mandelbrot.ocaml_run
GCC=gcc9/gcc9
ERLANG=Erlang-Hipe/mandelbrot.beam

HAS_RACKET := $(shell racket --version 2>/dev/null)

all:
	@cd Erlang-Hipe; which erlc && make; true
	@cd gcc9; which erlc && make; true
	@cd java; which javac && make; true
	@cd OCaml; which ocamlopt && make; true
	@encorec -c mbr-tree.enc
	@encorec -c mbr.enc
	
encore-tree: mbr-tree.enc
	encorec -c mbr-tree.enc
	
encore: mbr.enc
	encorec -c mbr.enc
	
bench:
	
ifneq ("$(wildcard $(ENC_TREE))","")
	@echo '--==Encore (Tree)==--'
	@echo ' '
	@$(TIME)  ./mbr-tree $(PLOT_SIZE) > out.pbm
	@echo ' '
	@$(TIME)  ./mbr-tree $(PLOT_SIZE) > out.pbm
	@echo ' '
	@$(TIME)  ./mbr-tree $(PLOT_SIZE) > out.pbm
	@echo ''
endif

ifneq ("$(wildcard $(ENC_ROW))","")	
	@echo '--==Encore (Row)==--'
	@echo ' '
	@$(TIME) ./mbr $(PLOT_SIZE) > out.pbm
	@echo ' '
	@$(TIME)  ./mbr $(PLOT_SIZE) > out.pbm
	@echo ' '
	@$(TIME)  ./mbr $(PLOT_SIZE) > out.pbm
	@echo ' '
endif

ifdef HAS_RACKET
	@echo '--==Racket==--'
	@echo ' '
	@cd Racket; $(TIME)  racket mandelbrot.racket-4.racket $(PLOT_SIZE) > out.pbm 
	@echo ' '
	@cd Racket; $(TIME)  racket mandelbrot.racket-4.racket $(PLOT_SIZE) > out.pbm
	@echo ' '
	@cd Racket; $(TIME)  racket mandelbrot.racket-4.racket $(PLOT_SIZE) > out.pbm
	@echo ' '
endif
	
ifneq ("$(wildcard $(JAVA))","")
	@echo '--==Java==--'
	@echo ' '
	@cd java; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
	@cd java; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
	@cd java; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
endif 
	
ifneq ("$(wildcard $(OCAML))","")
	@echo '--==OCaml==--'
	@echo ' '
	@cd OCaml; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
	@cd OCaml; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
	@cd OCaml; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
endif 
	
ifneq ("$(wildcard $(GCC))","")
	@echo '--==C (gcc)==--'
	@echo ' '
	@cd gcc9; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
	@cd gcc9; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
	@cd gcc9; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
endif
	
ifneq ("$(wildcard $(ERLANG))","")
	@echo '--==Erlang Hipe==--'
	@echo ' '
	@cd Erlang-Hipe; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
	@cd Erlang-Hipe; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
	@echo ' '
	@cd Erlang-Hipe; $(TIME)  make PLOT_SIZE=$(PLOT_SIZE) run > out.pbm
endif
	