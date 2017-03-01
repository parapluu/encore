#
# makefile for SVM_ENCORE
#
# VHPhuc, 2016
#

ENCOREC = encorec
FILE = svm_encore

all: clean svm_encore

tidy: 
	rm -rf $(FILE).dSYM
	rm -rf $(FILE)_src

clean: tidy
	rm -f $(FILE)

help:   info

info:
	@echo
	@echo "make for SVM-ENCORE               Huu-Phuc Vo, 2016"
	@echo
	@echo "USAGE: make [clean | help | tidy | svm_encore "
	@echo 
	@echo "    svm_encore          builds the svm_encore module"
	@echo "    help                prints help for using makefile"
	@echo "    all (default)       builds svm_encore"
	@echo "    clean               removes target files"
	@echo "    tidy                removes svm_encore_src and .dSYM folder"
	@echo

svm_encore: 
	$(ENCOREC) -c $(FILE).enc

