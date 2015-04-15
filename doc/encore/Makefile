BUILD=build
DEST=--dest-name encore
INIT=index.scrbl
SCRBLS=$(shell find . -name "[a-zA-Z]*.scrbl")

all: build/html/encore.html build/pdf/encore.pdf

html: build/html/encore.html
pdf: build/pdf/encore.pdf

build/html/encore.html: $(SCRBLS)
	@scribble --info-out getting-started.sxref --dest $(BUILD)/html/getting-started --dest-name getting-started lang/getting-started/getting-started.scrbl;
	@scribble ++info-in getting-started.sxref --dest $(BUILD)/html $(DEST) $(INIT);
	
	@rm *.sxref

build/pdf/encore.pdf: $(SCRBLS)
	
	@scribble --info-out getting-started.sxref --pdf --dest $(BUILD)/pdf --dest-name getting-started lang/getting-started/getting-started.scrbl;
	@scribble  ++info-in getting-started.sxref --pdf --dest $(BUILD)/pdf $(DEST) $(INIT);
	
	@rm *.sxref

clean:
	@rm -fr $(BUILD)

.PHONY: all clean
