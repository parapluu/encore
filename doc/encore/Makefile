BUILD=build
DEST=--dest-name encore
INIT=index.scrbl
SCRBLS=$(shell find . -name "[a-zA-Z]*.scrbl")

all: build/html/encore.html build/pdf/encore.pdf

build/html/encore.html: $(SCRBLS)
	@scribble --dest $(BUILD)/html $(DEST) $(INIT);

build/pdf/encore.pdf: $(SCRBLS)
	@scribble --pdf --dest $(BUILD)/pdf $(DEST) $(INIT);

clean:
	@rm -fr $(BUILD)

.PHONY: all clean
