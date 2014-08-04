mylittlepony
============

Preliminary explorations for the Encore compiler.

### Build Instructions

Make sure that you have `doxygen` (for documentation), `premake4`, an up-to-date
`clang` and `ghc` in your path.

We're using:

 - doxygen v1.8.6
 - clang:

    Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)
    Target: x86_64-apple-darwin13.1.0
    Thread model: posix

 - ghc: The Glorious Glasgow Haskell Compilation System, version 7.8.20140130
 - premake4 (Premake Build Script Generator) 4.3

Then run:

    cd mylittlepony
    make
    make doc
    make test

Now we recommend that you add the `release` directory to your `PATH` environment variable -- this will allow you to invoke the compiler by just saying

    $ encorec my_file.enc

in any directory. To do this, add the line to your `~/.bashrc` file:

    export PATH="${HOME}/code/mylittlepony/release:${PATH}"

Now you can compile a program by using:

    $ encorec -clang my_file.enc

Or, you can use a .enc-file as a script by adding `#! /usr/bin/env encorec -run` as its FIRST line. After you made the file executable:

    $ chmod u+x my_file.enc

..you can execute it:

    $ ./my_file.enc

This will compile the file -- and run it.

You can find some files in the [https://github.com/parapluu/mylittlepony/tree/master/programs](programs) directory
