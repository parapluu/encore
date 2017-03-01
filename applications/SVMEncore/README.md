Overview
========

In this project, we aim to transform the sequential *SVMLight* to
parallel version using Encore programming language. The report firstly
introduces the background of the project such as object-oriented
parallel programming language Encore, ATLAS Higgs Boson dataset, and *SVMLight* library. Then the approach of how to achieve the goal and the implementation will be shown. Ultimately, sequential and parallel versions of *SVMLight* can be evaluated by different *ATLAS Higgs Boson* datasets. Afterward we can come up with conclusions about the correctness, data scalability, and performance
between two versions.

Usage
==========

    ---------------------------------------------------------------------
    SVM-ENCORE               Huu-Phuc Vo, 2016
    ---------------------------------------------------------------------
    Thanks to SVMLight's authors for the open source library.
    ---------------------------------------------------------------------
    USAGE:
      make    [all  |  help | clean | tidy
              svm_light_api | svm_encore   | svm_lib  | distribute
              test_all      | test_convert | test_ex1 | test_higgs
        svm_light_api  builds learning, classfying libraries
        svm_encore     builds the svm_encore module
        svm_lib        builds shared object library that can be linked
        distribute     arranges library and executables to folders
        help           prints help for using makefile
        all (default)  builds and distributes libraries and executables
        clean          removes .o and target files
        tidy           removes all libraries and executables

        test_all       tests convert(), train(), and classify()
        test_convert   tests convert() from csv to svml format
        test_ex1       runs example-1 2,000 events with 9947 features
        test_higgs     runs higgs boson higgs test
    ---------------------------------------------------------------------
    USAGE:
      svm_encore [convert | learn | classify] [-eh | -lib]
                 -eh        print help
                 -lib       specify dynamic linking library manually

        convert  [-chunk n] [-ei csv_file] [-eo svml_file]
                 none       convert from csv to svm format
                 -chunk -1  join from n file into one
                 -chunk n   (n>1) split file into n files

        learn    [options] -ei in_file  -eo model_file

        classify [options] -ei in_file  -eo out_file  -im model_file

                 [options]  using all 28 flags of svmlight
    ---------------------------------------------------------------------

  ***Please refer to Appendix for more information of [options]***

Background
==========

Firstly, the dataset of ATLAS Higgs Boson Machine Learning Challenge
2014 will be used for training and testing the library. The
goal of the Higgs training data is to distinguish a signal process which
produces Higgs bosons from a background process which does not
. Secondly, among several classification and regression
approaches, *Support Vector Machine (SVM)* is one of
the representatives since its popularity. With the given training
dataset which is marked as belonging to one or two categories, a model
will be built by SVM to predict whether a new input belongs to one
category or the other. Thirdly, we have been developing an
object-oriented parallel programming language called Encore
that supports developers to achieve parallelism-by-default using active
and passive objects. Active objects are instances of active classes, and
can possess their own single logical thread of control with asynchronous
method calls. Whereas the passive objects are constructed from passive
classes, and do not have a thread of control. In addition, method calls
on passive objects will be executed synchronously. Passive objects can
be asynchronously passed between active objects as data.

Approach
========

Our approach is to transform an SVM library from sequential version to
the parallel one by taking advantage of Encore programming language.
*SVMLight* has been chosen to transform from sequential to
parallel library since it is among the top 3 popular SVM libraries that
are widely used. In order to construct the parallel *SVMLight* library
in Encore language, both training and testing ATLAS Higgs Boson datasets
should be represented as passive objects. At the same time, sequential
methods of the *SVMLight* library can be correspondingly implemented as
parallel methods of active objects in Encore. An interface of *SVMLight*
is implemented and packed in a dynamic library in C so that the Encore
programming language can load and invoke the functions from the library.

Contributions
=============

The aim of the project is to make a parallel version of *SVMLight* using
object-oriented parallel programming language Encore. There are three
main phases to construct parallel *Encore-SVM* version, *converting*,
*learning* and *classifying*. Among three phases, the parallelism can be
applied to *classifying* and *converting* phases as all the events in
the testing converting datasets are independent. The *learning* phase,
however, is difficult to parallelize since all the events in the
training dataset are dependently used to construct the learned model.

***Contributions*** The following contributions are achieved in the
project:

1.  Propose an approach that can convert the csv data format to svm data
    format.

2.  Implement an *Encore-SVM* version with partial parallel functions
    (*converting* and *classifying*).

3.  Classify signal to background processes from the given Higgs dataset
    using *Encore-SVM* version.

4.  Validate the performance and accuracy of original *SVMLight* and
    *Encore-SVM* versions.

There are 33 different features in Higgs Boson training dataset
including label feature which is `s` for signal or `b` for background
value. When converting the features from *csv* format, all values are
converted exactly the same, but the value of label is changed to `+1`
with `s` and `-1` with `b`. The *svmlight* format is defined as below. Each features/values pairs are separated by a space
character and ordered by increasing feature number.

    <line>    .=. <target> <feature>:<value> ... <feature>:<value> # <info>
        <target>  .=. +1 | -1 | 0 | <float>
        <feature> .=. <integer> | qid
        <value>   .=. <float>
        <info>    .=. <string>

The given Higgs Boson datasets are firstly converted from *csv* format
to *svmlight* format. At the moment, the FileIO functions such as
reading and writing files are missing in Encore, so they are implemented
and packed in the IO Encore library. The IO library can run in 2
different modes, parallel and sequential. By default, the sequential
mode will be activated, the parallel mode that will automatically split
the input file into n-chunks, however, can be specified by using
`-chunk` flag. Each feature of `convert` is specified by given values of
`-chunk` flag.

    ./svm_encore convert -chunk k -ei train.csv -eo train.svm -vv
                    k = 1 : convert from csv to svmlight.
                    k > 1 : convert and split to n files.
                    k < 1 : merge n files to 1 file.
            -ei : input file.
            -eo : output file.
            -vv : verbosity mode.

The categories of kernel function can be decided by passing kernel
options such as linear (by default), polynomial, radial basis function
expression, sigmoid tanh, or user defined kernel. Additional information
of kernel can be added to the string `<info>`. The kernel can be
customized by other implementation of the kernel function in
`custom_kernel()` that currently returns `1.0` by default.

In order to make parallel version of the *SVMLight* in Encore, a C-*API*
for both *learning* and *classifying* is necessary for creating an `.so`
dynamic library with all methods that can be invoked from Encore. The
two main functions of *SVMLight*, *svm\_learn* and *svm\_classify*, are
used to train the model from given input data, and produce the result by
taking the trained model and testing data as inputs, respectively. The
*Encore-SVM* can also take all 28-`[options]` arguments for learning
phase exactly as the original version.

Implementation
==============

This section explains the implementation of convert, svm\_learn, and
svm\_classify features. In Encore, active classes instantiate active
objects that can possess their single logical thread of control with
asynchronous method calls. When executing, the main program parses the
given arguments and stores in passive object *Params*. Then the
corresponding commands such as *svm\_encore svm\_learn*, *svm\_encore
svm\_classify*, or *svm\_encore convert* can be executed.

To obtain the parallelism, the following active and passive classes are
being implemented.

***Active classes*** The instances of those active classes are active
objects as their method calls can be run asynchronously.

1.  <span>*Converter.enc*</span>: The converter takes the parameters
    from *Params.enc*, and has three different functions

    -   `convert()`: convert file from `csv` to `svm` format without
        splitting file.

    -   `split()`: convert to `svm` format and split input file into
        n-chunks.

    -   `join()`: merge n-chunks to 1 file only.

2.  <span>*Core.enc*</span>: This is the bridge that connects with the
    *.so* library. It takes the method name, the arguments, and sends
    the method call to the *.so* library.

    -   `load()`: loads the dynamic shared library with the given path.

    -   `call()`: sends the method call to shared library by specifying
        method names, and number of arguments. If the number of argument
        is `0`, the method call has no return value.

3.  <span>*Main.enc*</span> The main object takes the given arguments to
    initialize the *Param* object. Then the shared library is able to be
    loaded, and the *svm\_encore svm\_learn*, *svm\_encore
    svm\_classify*, or *svm\_encore convert* function can be extended
    depending on the given options.

***Passive class*** The passive object *Params* is used to send to and
receive from other active objects as data.

1.  <span>*Params.enc*</span> The passive object is stored all 28
    parameters of *svm\_encore svm\_learn*, 3 parameters of *svm\_encore
    svm\_classify*, and 14 parameters of *svm\_encore*. Those parameters
    can be used by all active objects aforementioned.

Validation
==========

Firstly, in order to validate the correctness and the accuracy of
*Encore-SVM*, 3 examples of *SVMLight* library will be used to evaluate
and compare the results between original and parallel versions. Figure
[fig:ex1-train] and [fig:ex1-test] illustrates the training and testing
datasets with 1000 positive and 1000 negative examples in the training
phase, and with 600 test examples, respectively.

Secondly, the Higgs Boson training and classifying datasets will be used
for training and classifying. Due to the limitation of *SVMLight*, it
takes approximately `12 hours` for training the Higgs Bosson datasets with
each implementations.

Thirdly, the performance between two versions is not much different when
training and classifying with the small datasets as shown in Example
[fig:ex1-train] and [fig:ex1-test]. However, the performance of
*Encore-SVM* in classifying phase is better than original version when
testing *55,000* events in the large Higgs Boson dataset, and *170,000*
support vectors in the model. The execution time of *Encore-SVM* is
`0.17 seconds`, of *SVMLight* is `0.22 seconds`.

Conclusion
==========

*Encore-SVM* is a parallel implementation of *SVMLight* that
is implemented using object-oriented parallel programming language
Encore. It takes advantage of Encore language such as active objects
that can send and receive messages asynchronously. The parallelism is
partially obtained in the converting phase, and classifying phase. The
*Encore-SVM* implementation is centered around the Higgs Boson datasets
that contain large training and testing data. The accuracy and
performance are firstly checked by using enclosing examples of
*SVMLight*, and then comparing the results between original and parallel
versions. The large Higgs Boson training and testing datasets expose the
limitation of *SVMLight* and *Encore-SVM*, it took more than 8 hours to
train the model with the given training dataset. The implementation of
*Encore-SVM* also requires a lot of time since additional features of
Encore language are unavailable at the moment and need to be developed
before used. In summary, the project shows the possibility, performance
and accuracy of applying Encore programming language to machine learning
in general, and *SVMLight* in particular. The Encore will be considered
to develop more necessary features and case studies. We will also
investigate how the Encore language can be extended in the future to
further support the developer in writing correct parallel programs.

Appendix
==========

***SVM_Learn*** Available [options] that can be used in both *SVMLight* and *SVMEncore*

  `svm_learn [options] example_file model_file`


  General options:

           -?          - this help
           -v [0..3]   - verbosity level (default 1)

  Learning options:

           -z {c,r,p}  - select between classification (c), regression (r), and
                         preference ranking (p) (see [Joachims, 2002c])
                         (default classification)
           -c float    - C: trade-off between training error
                         and margin (default [avg. x*x]^-1)
           -w [0..]    - epsilon width of tube for regression
                         (default 0.1)
           -j float    - Cost: cost-factor, by which training errors on
                         positive examples outweight errors on negative
                         examples (default 1) (see [Morik et al., 1999])
           -b [0,1]    - use biased hyperplane (i.e. x*w+b0) instead
                         of unbiased hyperplane (i.e. x*w0) (default 1)
           -i [0,1]    - remove inconsistent training examples
                         and retrain (default 0)

  Performance estimation options:

           -x [0,1]    - compute leave-one-out estimates (default 0)
                         (see [5])
           -o ]0..2]   - value of rho for XiAlpha-estimator and for pruning
                         leave-one-out computation (default 1.0)
                         (see [Joachims, 2002a])
           -k [0..100] - search depth for extended XiAlpha-estimator
                         (default 0)

  Transduction options (see [Joachims, 1999c], [Joachims, 2002a]):

           -p [0..1]   - fraction of unlabeled examples to be classified
                         into the positive class (default is the ratio of
                         positive and negative examples in the training data)

  Kernel options:

           -t int      - type of kernel function:
                          0: linear (default)
                          1: polynomial (s a*b+c)^d
                          2: radial basis function exp(-gamma ||a-b||^2)
                          3: sigmoid tanh(s a*b + c)
                          4: user defined kernel from kernel.h
           -d int      - parameter d in polynomial kernel
           -g float    - parameter gamma in rbf kernel
           -s float    - parameter s in sigmoid/poly kernel
           -r float    - parameter c in sigmoid/poly kernel
           -u string   - parameter of user defined kernel

  Optimization options (see [Joachims, 1999a], [Joachims, 2002a]):

           -q [2..]    - maximum size of QP-subproblems (default 10)
           -n [2..q]   - number of new variables entering the working set
                         in each iteration (default n = q). Set n<q to prevent
                         zig-zagging.
           -m [5..]    - size of cache for kernel evaluations in MB (default 40)
                         The larger the faster...
           -e float    - eps: Allow that error for termination criterion
                         [y [w*x+b] - 1] = eps (default 0.001)
           -h [5..]    - number of iterations a variable needs to be
                         optimal before considered for shrinking (default 100)
           -f [0,1]    - do final optimality check for variables removed by
                         shrinking. Although this test is usually positive, there
                         is no guarantee that the optimum was found if the test is
                         omitted. (default 1)
           -y string   -> if option is given, reads alphas from file with given
                          and uses them as starting point. (default 'disabled')
           -# int      -> terminate optimization, if no progress after this
                          number of iterations. (default 100000)

  Output options:

           -l char     - file to write predicted labels of unlabeled examples
                         into after transductive learning
           -a char     - write all alphas to this file after learning (in the
                         same order as in the training set)

  ***SVM_CLASSIFY***  Available [options] that can be used in both *SVMLight* and *SVMEncore*

  `svm_classify [options] example_file model_file output_file`

  Available options are:

           -h         Help.
           -v [0..3]  Verbosity level (default 2).
           -f [0,1]   0: old output format of V1.0
                      1: output the value of decision function (default)
