CC    = gcc
MAKE  = make
SE    = dist/./svm_encore
LEARN = dist/./svm_learn
CLF   = dist/./svm_classify

SVML_SRC   = src/svm_light
ENCORE_SRC = src/encore_svm
DIST       = dist
LIB        = lib

TEST_HIGGS = test/higgs
TEST_EX1   = test/ex-svms
TEST_CVT   = test/converter

LIB_SO    = libsvm_encore.so

all: svm_light_api svm_lib svm_encore distribute

tidy:
	$(MAKE) -C $(SVML_SRC) tidy
	$(MAKE) -C $(ENCORE_SRC) tidy

clean: tidy
	rm -f $(DIST)/*
	rm -f $(LIB)/$(LIB_SO)
	$(MAKE) -C $(SVML_SRC) clean

svm_light_api:
	$(MAKE) -C $(SVML_SRC) clean
	$(MAKE) -C $(SVML_SRC)

svm_lib: svm_light_api
	$(MAKE) -C $(SVML_SRC) libsvm_encore

svm_encore:
	$(MAKE) -C $(ENCORE_SRC) all

distribute:
	mv $(SVML_SRC)/svm_learn    $(DIST)
	mv $(SVML_SRC)/svm_classify $(DIST)
	mv $(ENCORE_SRC)/svm_encore $(DIST)
	mv $(SVML_SRC)/$(LIB_SO)    $(LIB)/

test_all: test_convert test_ex1 test_higgs

test_convert:
	$(SE) convert -ei $(TEST_CVT)/train.csv -eo $(TEST_CVT)/train.svm -vv

test_ex1: test_ex1_encore test_ex1_svm

test_ex1_encore:
	$(SE) learn -ei $(TEST_EX1)/example1/train.dat -eo $(TEST_EX1)/example1/model_encore -vv
	$(SE) classify -ei $(TEST_EX1)/example1/test.dat -im $(TEST_EX1)/example1/model_encore -eo $(TEST_EX1)/example1/result_encore.out -vv

test_ex1_svm:
	$(LEARN) $(TEST_EX1)/example1/train.dat $(TEST_EX1)/example1/model_svm
	$(CLF) $(TEST_EX1)/example1/test.dat $(TEST_EX1)/example1/model_svm $(TEST_EX1)/example1/result_svm.out

test_higgs: test_higgs_convert test_higgs_encore test_higgs_svm

test_higgs_convert:
	$(SE) convert -ei $(TEST_HIGGS)/training_higgs.csv -eo $(TEST_HIGGS)/training_higgs.svm -vv
	$(SE) convert -ei $(TEST_HIGGS)/test_higgs.csv -eo $(TEST_HIGGS)/test_higgs.svm -vv

# Encore svm version
test_higgs_encore:
	$(SE) learn -ei $(TEST_HIGGS)/training_higgs.svm -eo $(TEST_HIGGS)/model_encore -vv
	$(SE) classify -ei $(TEST_HIGGS)/test_higgs.svm -im $(TEST_HIGGS)/model_encore -eo $(TEST_HIGGS)/result_encore.out -vv

# Original version.
test_higgs_svm:
	$(LEARN) $(TEST_HIGGS)/training_higgs.svm $(TEST_HIGGS)/model_svm
	$(CLF) $(TEST_HIGGS)/test_higgs.svm $(TEST_HIGGS)/model_svm $(TEST_HIGGS)/result_svm.out

help:
	@echo "SVM-ENCORE               Huu-Phuc Vo, 2016"
	@echo "---------------------------------------------------------------------"
	@echo "Thanks to SVMLight's authors for the open source library."
	@echo "---------------------------------------------------------------------"
	@echo "USAGE: make [all | help | clean | tidy"
	@echo "             svm_light_api | svm_encore | svm_lib | distribute"
	@echo "             test_all | test_convert | test_ex1 | test_higgs"
	@echo "    svm_light_api  builds learning, classfying libraries"
	@echo "    svm_encore     builds the svm_encore module"
	@echo "    svm_lib        builds shared object library that can be linked"
	@echo "    distribute     arranges library and executables to folders"
	@echo "    help           prints help for using makefile"
	@echo "    all (default)  builds and distributes libraries and executables"
	@echo "    clean          removes .o and target files"
	@echo "    tidy           removes all libraries and executables"
	@echo
	@echo "    test_all       tests convert(), train(), and classify()"
	@echo "    test_convert   tests convert() from csv to svml format"
	@echo "    test_ex1       runs example-1 2,000 events with 9947 features"
	@echo "    test_higgs     runs higgs boson higgs test"
	@echo "---------------------------------------------------------------------"
	@echo "USAGE: "
	@echo "  svm_encore [convert | learn | classify] [-eh | -lib]"
	@echo "             -eh        print help"
	@echo "             -lib       specify dynamic linking library manually"
	@echo
	@echo "    convert  [-chunk n] [-ei csv_file] [-eo svml_file]"
	@echo "             none       convert from csv to svm format"
	@echo "             -chunk -1  join from n file into one"
	@echo "             -chunk n   (n>1) split file into n files"
	@echo
	@echo "    learn    [options] -ei in_file  -eo model_file"
	@echo
	@echo "    classify [options] -ei in_file  -eo out_file  -im model_file"
	@echo
	@echo "             [options]  using all 28 flags of svmlight"
	@echo "---------------------------------------------------------------------"
