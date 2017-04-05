```
||:encorec:||
```

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
Your Encore code needs to be placed inside the `encore` folder (the VM is restricted to work only inside that folder).

To connect to the VM:

    localhost$ vagrant ssh

This command will connect you to the VM (user: `vagrant`, password: `vagrant`).

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
`clang` and `stack` in your path.

### Installing on Debian based Linux distros

To install Encore on a Debian based Linux distribution you can use the `debian-install.sh` script.
To perform a full install run the script with the `-f` flag to completely install all dependencies,
setup the correct Haskell version and build and install Encore.

If you do not want the script to alter your `$PATH` variable or change the Haskell version use the
`-h` flag to see available options.

### Installing on OS X

#### Step 1: Preliminaries

We're using:

 - doxygen v1.8.6
 - clang:
    Apple LLVM version 7.0.2 (clang-700.1.81)
    Target: x86_64-apple-darwin15.6.0
    Thread model: posix
 - pcre2-10.21 (for Regex library)
 - stack (the haskell build tool)
 - premake4 (Premake Build Script Generator) 4.3

Below you find instructions for installing the preliminaries on OS X.

##### Installing homebrew

Go to http://brew.sh/, the instructions there work nicely. Make sure that your normal user is an admin (that you can use `sudo`). You should not need `sudo` to *use* `brew` in the future.

##### Installing `doxygen`

Run: `brew update; brew install doxygen`

##### Installing `clang`:

Run: `brew update; brew install llvm`

##### Installing `premake4`

Go to the [Premake4 Download page](http://premake.github.io/download.html),
download and install `premake4`.

Alternatively, run: `brew update; brew install premake`

##### Installing `stack`

If you have homebrew, you can run `brew install haskell-stack`. Otherwise,
use these [installation instructions](http://docs.haskellstack.org/en/stable/README/#how-to-install).
You might have to run `stack update` in order to get the latest version
of all the dependencies.

##### Installing `pcre2`

Run: `brew update; brew install pcre2`

#### Step 2: Compiling and installing encore

    cd encore
    make
    make test

#### Step 3: Adding `encorec` to the path

We recommend that you add the `release` directory to your `PATH` environment variable -- this will allow you to invoke the compiler by just saying

    $ encorec my_file.enc

in any directory. To do this, add this line to your `~/.bashrc` file, inserting the proper path for `<SOME_DIR>`:

    export PATH="<SOME_DIR>/encore/release:${PATH}"

### Compiling and Running Encore Programs

This instructions are the same for any Linux distribution and OS X.

You can compile a program by using

    $ encorec my_file.enc

Then, you can run the executable, as usual, through

   ./my_file

Alternatively, you can use a .enc-file as a script by adding `#! /usr/bin/env encorec --run` as its FIRST line. After you made the file executable:

    $ chmod u+x my_file.enc

..you can execute it:

    $ ./my_file.enc

This will compile the file -- and run it.

You can find some example programs in the [programs](https://github.com/parapluu/encore/tree/development/src/tests/encore/basic) directory.

Have fun!

## `encorec` options

Running `encorec foo.enc` will typecheck the source and produce the executable
`foo`. The following options are supported:

```
  --import [dirs]   | -I [dirs] colon separated list of directories in which to look for modules.
  --out-file [file] | -o [file] Specify output file.
  --generate-c      | -c        Outputs intermediate C fields in separate source tree.
  --debug           | -g        Inserts debugging symbols in executable. Use with -c for improved debugging experience.
  --type-check      | -tc       Only type check program, do not produce an executable.
  --literate        |           Literate programming mode. Code blocks are delimited by '#+begin_src' and '#+end_src'.
  --verbose         | -v        Print debug information during compiler stages.
  --optimize N      | -O N      Optimise produced executable. N=0,1,2 or 3.
  --profile         | -pg       Embed profiling information in the executable.
  --run             |           Compile and run the program, but do not produce executable file.
  --no-gc           |           DEBUG: disable GC and use C-malloc for allocation.
  --help            |           Display this information.
```
