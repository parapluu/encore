encorec
============

The Encore compiler.

There are two ways of installing the Encore compiler. The first ([**Playing around with Encore**](https://github.com/parapluu/encore/blob/master/README.md#playing-around-with-encore)) uses a virtual box image and it is easy getting the compiler up and running. The disadvantage of this approach is that benchmarks are meaningless. The second approach ([**Building Encore from Source**](https://github.com/parapluu/encore/blob/master/README.md#building-encore-from-source)) is a proper installation of all the dependencies (Haskell, llvm, etc).

This readme concludes with a discussion of the Encore command line arguments and details of the documentation.

## Playing around with Encore

Would you like to play around with Encore without having to install all the dependencies? Now you can!

1. Install [VirtualBox](https://www.virtualbox.org/wiki/Downloads) and [Vagrant](https://www.vagrantup.com/).

2. `git clone git@github.com:parapluu/encore.git`

3. `cd encore`

4. `make vagrant` (you need to run this make target only the first time you download the project)

This installs the Encore compiler in a Virtual Machine (VM).

At this point, you have a Ubuntu VM working for you. You will work on your localhost (marked as `localhost$`) and compile on the VM (marked as `vm$`).

To connect to the VM:

    localhost$ vagrant ssh

From the VM, compile using Encore:

    vm$ encorec -clang example.enc

To exit the VM:

    vm$ exit

After playing around with Encore, you want to suspend or halt the machine
(so that it does not consume resources).

In your local machine:

    localhost$ vagrant halt

or

    localhost$ vagrant suspend

To start and connect again to the VM:

    localhost$ vagrant up && vagrant ssh


## Building Encore from Source

Make sure that you have `doxygen` (for documentation), `premake4`, an up-to-date
`clang` and `ghc` in your path.

### Step 1: Preliminaries

We're using:

 - doxygen v1.8.6
 - clang:
    Apple LLVM version 7.0.0 (clang-700.0.72)
    Target: x86_64-apple-darwin14.5.0
    Thread model: posix
 - g++ 4.8
 - ghc: The Glorious Glasgow Haskell Compilation System, version 7.10.2
 - premake4 (Premake Build Script Generator) 4.3
 - `scribble` -- only for building the documentation


In the below you find instructions for installing the preliminaries  on OS X and on linux.

#### Installing the preliminaries on OS X

##### Installing homebrew

Go to http://brew.sh/, the instructions there work nicely. Make sure that your normal user is an admin (that you can use `sudo`). You should not need `sudo` to *use* `brew` in the future.

##### Installing `doxygen`

Run: `brew update; brew install doxygen`

##### Installing `scribble`

Go to the [Racket Download page](http://racket-lang.org/download/),
download and install racket. The `scribble` tool comes with the racket
distribution.

##### Installing `clang`:

Run: `brew update; brew install llvm`

##### Installing `premake4`

Go to the[Premake4 Download page](http://premake.github.io/download.html),
download and install `premake4`. 

##### Installing `ghc`

You need at least version `7.10.2`.

- If you have an older version of `ghc` installed with `homebrew`: get rid of it by saying `brew uninstall haskell-platform; brew uninstall ghc`.
- If you have an older version of `ghc` installed downloaded from the haskell webpage: you need to remove it. Here is a discussion thread on how to do that: http://www.haskell.org/pipermail/haskell-cafe/2011-March/090170.html Warning: we did not test this, and even if we did: every computer is configured differently. In the future: please use homebrew for every installation where a formula is available. It allows you to uninstall stuff easily once you don't need it any more.

Then install the newest version:

```
brew update && brew install cabal-install && cabal update && brew install ghc
```

##### Installing `premake4`:

Run: `brew update; brew install premake`

#### Installing the preliminaries on Linux

It's only tested on Ubuntu 14.04, and hopefully it works as well on other
distributions based on Ubuntu or Debian.


    # add sources
    sudo add-apt-repository -y ppa:hvr/ghc
    # update
    sudo apt-get update
    # set up the building infrastructure
    sudo apt-get install -y clang lldb-3.5 g++ make premake4 zlib1g-dev ghc-7.10.2 cabal-install-1.22 racket doxygen
	# update cabal packages
	cabal update && cabal install cabal-install


##### Version checking

Due to the incomplete support for C++11 in gcc
[4.7](https://gcc.gnu.org/gcc-4.7/cxx0x_status.html), the minimal version of gcc
is [4.8](https://gcc.gnu.org/gcc-4.8/cxx0x_status.html).

    gcc/g++: ~> 4.8

#### Step 2: Compiling and installing encore

    cd encore
    make
    make test

#### Step 3: Adding `encorec` to the path

We recommend that you add the `release` directory to your `PATH` environment variable -- this will allow you to invoke the compiler by just saying

    $ encorec my_file.enc

in any directory. To do this, add this line to your `~/.bashrc` file, inserting the proper path for `<SOME_DIR>`:

    export PATH="<SOME_DIR>/encore/release:${PATH}"

### Step 4: Compiling and Running Encore Programs

Now you can compile a program by using

    $ encorec my_file.enc

or, if you you are calling C from your code, by using:

    $ encorec -clang my_file.enc

Then, you can run the executable, as usual, through

   ./my_file

Alternatively, you can use a .enc-file as a script by adding `#! /usr/bin/env encorec -run` as its FIRST line. After you made the file executable:

    $ chmod u+x my_file.enc

..you can execute it:

    $ ./my_file.enc

This will compile the file -- and run it.

You can find some example programs in the [programs](https://github.com/parapluu/encore/tree/master/programs) directory.

Have fun!

## `encorec` options

Running `encorec foo.enc` will typecheck the source and produce the executable
`foo`. The following options are supported:

* `-c` -- Keep intermediate C-files
* `-tc` -- Typecheck only (don't produce an executable)
* `-o [file]` -- Specify output file
* `-run` -- Run the program and remove the executable
* `-clang` -- Use clang to build the executable (default)
* `-AST` -- Output the parsed AST as text to `foo.AST`
* `-TypedAST` -- Output the typechecked AST as text to `foo.TAST`


## Language Specification

Update the language specification whenever you change the Encore compiler.

#### Dependencies

In order to update the language specification, first you need to:

  1. download the [Racket language](http://download.racket-lang.org/)
  2. clone the project upscale (https://github.com/fxpl/upscale)
  3. cd upscale/doc/encore
  4. make

After following the instructions above, you should have a folder under
`upscale/doc/encore` named `build` that contains the documentation in
pdf and html.

#### Update the language specification

The language specification has the following directory structure:

```
├── Makefile
├── README.md
├── index.scrbl
├── lang
│    ├── client-side
│    │   └── index.scrbl (todo)
│    ├── getting-started
│    │   └── index.scrbl
│    ├── module-system
│    │   └── index.scrbl (todo)
│    ├── semantics
│    │   └── index.scrbl (todo)
│    └── syntax
│        ├── index.scrbl
│        └── grammar.scrbl (ongoing work)
└── extras
     ├── README.md
     └── math
         └── ...
```

Update the files relevant to your changes. For instance, if you decide to
introduce a new keyword in the language, such as `repeat`, you would need to
update the file `grammar.scbl` in `upscale/doc/encore/lang/syntax/`.

The easiest way (for now) to know what to modify is by generating the html,
reading the section relevant to you and updating it.
