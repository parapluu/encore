SOURCE=Main
SCRIPT=trace.d

build:
	encorec $(SOURCE).enc

compile:
	cd $(SOURCE)_src && make && mv $(SOURCE) ../

run:
	sudo dtrace -s $(SCRIPT) -c ./$(SOURCE)

clean:
	rm $(SOURCE)

list:
	@sudo dtrace -l -P "pony*" -P "encore*" -c ./$(SOURCE)

gen:
	./probes.sh $(SOURCE)
