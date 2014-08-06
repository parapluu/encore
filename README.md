mylittlepony
============

Preliminary explorations for the Encore compiler.



## Build Instructions

Make sure that you have `doxygen` (for documentation), `premake4`, an up-to-date
`clang` and `ghc` in your path.

### Step 1: Premiminaries

We're using:

 - doxygen v1.8.6
 - clang:

    Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)
    Target: x86_64-apple-darwin13.1.0
    Thread model: posix

 - ghc: The Glorious Glasgow Haskell Compilation System, version 7.6.3
 - premake4 (Premake Build Script Generator) 4.3

#### Installing preliminaries on OS X

##### Installing homebrew
Go to http://brew.sh/, the instructions there work nicely. Make sure that your normal user is an admin (that you can use `sudo`). You should not need `sudo` to *use* `brew` in the future.

##### Installing `doxygen`

Run: `brew update; brew install doxygen`

##### Installing `clang`:
Run: `brew update; brew install llvm`

##### Installing `ghc`

You need at least version `7.6.3`.

- If you have an older version of `ghc` installed with `homebrew`: get rid of it by saying `brew uninstall haskell-platform; brew uninstall ghc`.
- If you have an older version of `ghc` installed downloaded from the haskell webpage: you need to remove it. Here is a discussion thread on how to do that: http://www.haskell.org/pipermail/haskell-cafe/2011-March/090170.html Warning: we did not test this, and even if we did: every computer is configured differently. In the future: please use homebrew for every installation where a formula is available. It allows you to uninstall stuff easily once you don't need it any more.

Then install the newest version, saying `brew update; brew install haskell-platform`.

#### Installing preliminaries on Linux

It's only tested on Ubuntu 14.04, and hopefully it works as well on other
distributions based on Ubuntu or Debian.

    # sync with the online repo source
    apt-get update
    # set up the building infrastructure
    apt-get install -y clang g++ premake4 haskell-platform
    # if you also want to see the HTML doc
    apt-apt install -y doxygen

#### Step 2: Compiling and installing mylittlepony

    cd mylittlepony
    make
    make test

#### Step 3: Adding `encorec` to the path

We recommend that you add the `release` directory to your `PATH` environment variable -- this will allow you to invoke the compiler by just saying

    $ encorec my_file.enc

in any directory. To do this, add this line to your `~/.bashrc` file, inserting the proper path for `<SOME_DIR>`:

    export PATH="<SOME_DIR>/mylittlepony/release:${PATH}"

### Compiling and Runnining Encore Programs

Now you can compile a program by using:

    $ encorec -clang my_file.enc

Or, you can use a .enc-file as a script by adding `#! /usr/bin/env encorec -run` as its FIRST line. After you made the file executable:

    $ chmod u+x my_file.enc

..you can execute it:

    $ ./my_file.enc

This will compile the file -- and run it.

You can find some example programs in the [programs](https://github.com/parapluu/mylittlepony/tree/master/programs]) directory.

Have fun!
