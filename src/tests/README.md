
# Tests

This directory contains all tests and the `test` script.

## Creating a Test

A test is a pair of an encore program, exactly one of three kinds of test
specification files, and an optional file detailing how to run the test:

### Specification files

A test specfication tells the framework what the test should do. It is a file in
the same directory, and with the same name as the test program file
(`testname.enc`), but a different file ending.

There are three kinds of specification files, two of which are supported right
now, and one of which may be supported in the future:

 - `testname.out` -- the most common test specification. It contains the exact
   output that is expected from running the program. In order for the test to
   pass, the program must compile.
 
 - `testname.fail` -- some tests ensure that the type checker does what it
   should. In these tests, `testname.fail` contains one regular expression per
   line that must appear in the compiler output. In order for the test to pass,
   the program must *fail* to compile.

 - `testname.chk` -- Currently not used/implemented.

### Running a Test With Input

Tests that need to be run in a special way (like with certain command line
flags) can optionally provide a file `testname.run` that must contain, on one
line, the command to run the test properly.

## Creating a Test Suite

Any directory under `src/tests` that contains tests is a test suite and will be
run by calling `./test` in this directory.

## Running Selected Test Suites

The `test` script can take either the name of a test (no `.enc` extension) or a
directory as a parameter. In the former case, it will run that test, in the
latter case, it will run all tests in that directory.

Example:

```
   .. $ ./test encore/modules # will only run the tests within encore/modules
```

## Disabling Tests and Other Ignored Files

To temporarily disable a test from running or to ignore any other
`.enc` files in the test directory hierarchy, 
add the tests names to the [IGNORED_FILES.grep](IGNORED_FILES.grep) file. Be careful not to
add any blank lines.

## Test output

You will get nicer output if you have the `tree` tool on your path.