SOURCES=$(filter-out Foo.enc Bar.enc, $(wildcard *.enc))
OBJECTS=$(SOURCES:.enc=)
ENCOREC=../../../../release/encorec
FLAGS=--literate

all: $(OBJECTS)

%: %.enc
	$(ENCOREC) $(FLAGS) $<

clean:
	rm -f $(OBJECTS)