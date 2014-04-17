mylittlepony
============

Preliminary explorations for the Encore compiler.

## Change Log
### First Program compiles

It runs. Barely, but it runs.
This will need heavy refactoring, lots of stuff is still hard-coded, duplicated.

Demo:

    cd mylittlepony/experiments/
    make
    cat Hello.pony.c
    ./Hello
    
#### It doesn't compile!

If the runtime doesn't compile, try `brew install premake` and make sure to have an up-to-date clang version:

    ../experiments $ clang --version
    Apple LLVM version 5.1 (clang-503.0.38) (based on LLVM 3.4svn)
    Target: x86_64-apple-darwin13.1.0
    Thread model: posix
