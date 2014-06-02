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

Then run:

    cd mylittlepony
    make
    make doc
    make test

