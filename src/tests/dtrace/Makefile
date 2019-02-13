SOURCE=None

build:
	encorec -c $(SOURCE)

compile:
	cd $(SOURCE)_src && make && mv $(SOURCE) ../

run:
	sudo dtrace -s dtrace.d -c ./$(SOURCE)

clean:
	rm $(SOURCE)

list:
	@sudo dtrace -l -P "pony*" -P "encore*" -c ./$(SOURCE)

gen:
	./probes.sh $(SOURCE)
