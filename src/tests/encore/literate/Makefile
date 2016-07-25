SOURCES=$(wildcard *.enc)
OBJECTS=$(SOURCES:.enc=)
ENCOREC=encorec
FLAGS=--literate

all: $(OBJECTS)

%: %.enc
	$(ENCOREC) $(FLAGS) $<

clean:
	rm -f $(OBJECTS)