#lang scribble/manual

@title[#:tag "getting-started"]{Getting Up and Running}

There are two ways of installing the Encore compiler. The first (@secref{building-vm})
uses a virtual box image and it is easy getting the compiler up and running. The
disadvantage of this approach is that benchmarks are meaningless. The second approach
(@secref{building-source}) is a proper installation of all the dependencies (Haskell, llvm, etc).

@section[#:tag "building-vm"]{Playing Around with Encore}

@itemlist[#:style 'ordered
  @item{Download the compiler:
    @codeblock|{
      git clone git@github.com:parapluu/encore.git
    }|
  }

  @item{Install @hyperlink["https://www.virtualbox.org/wiki/Downloads" "VirtualBox"] and @hyperlink["https://www.vagrantup.com/"]{Vagrant}}

  @item{Install the Encore compiler
    @codeblock|{
      cd encore
      make vagrant
    }|
    This installs the Encore compiler in a Virtual Machine (VM). From now on, we are going
    to refer to your local machine as @code{localhost$} and to the VM as @code{vm$}.

    This process may take about 15 to 20 min (internet required).
  }

  @item{After installing Encore in a VM, you can connect to the VM by:
    @codeblock|{
      localhost$ vagrant ssh
    }|
  }

  @item{From the VM, you can compile and run programs:
    @codeblock|{
      vm$ encorec example.enc
    }|
  }

  @item{To exit the VM:
    @codeblock|{
      vm$ exit
    }|
  }

  @item{If you are not going to work in Encore for a couple of days, you should halt or suspend the VM:
    @codeblock|{
      localhost$ vagrant halt
    }|
    or
    @codeblock|{
      localhost$ vagrant suspend
    }|

    @code{halt} stops the VM as in turning off the computer while @code{suspend} saves the current state of the VM and allows you to get a
    faster boot up next time you run @code{vagrant up}.
  }

  @item{To connect to the VM:
    @codeblock|{
      localhost$ vagrant up && vagrant ssh
    }|
  }
]

You can work on your @code{localhost} machine with your favorite editor and use the
terminal with the @code{vm} only for compilation purposes.

Please take into account that your local folder is shared with the VM, so any files you remove from the VM will be removed
in your localhost as well.

Furthermore, if you compile a program inside the @code{VM}, this won't run in your local machine since the compiled
version is not cross-platform.

@section[#:tag "building-source"]{Build Encore from source}

Make sure that you have @code{doxygen} (for documentation), @code{premake4}, an up-to-date @code{clang} and @code{ghc} in your path.

@subsection{Dependencies}
Make sure you have installed:

@itemlist[#:style 'ordered
  @item{@code{doxygen v1.8.6}}
  @item{@code{clang}: Apple LLVM version 7.0.0 (clang-700.0.72) Target: x86_64-apple-darwin14.5.0 Thread model: posix}
  @item{@code{g++ 4.8}}
  @item{@code{ghc} The Glorious Glasgow Haskell Compilation System, version 7.10.2}
  @item{@code{premake4} (Premake Build Script Generator) 4.3}
]

@subsubsection[#:style 'unnumbered]{Installing dependencies on Mac OS X}

@bold{Installing homebrew}

Go to @hyperlink["http://brew.sh/"]{http://brew.sh/}, the instructions there work nicely.
Make sure that your normal user is an admin (that you can use @code{sudo}). You should
not need @code{sudo} to use @code{brew} in the future.

@bold{Installing @code{doxygen}}

Run: @code{brew update; brew install doxygen}


@bold{Installing @code{clang}}

Run: @code{brew update; brew install llvm}

@bold{Installing ghc}

You need version @code{7.10.2}.

If you have an older version of @code{ghc} installed with @code{homebrew}: get rid of
it by saying:

@codeblock|{
brew uninstall haskell-platform; brew uninstall ghc
}|

If you have an older version of @code{ghc} installed downloaded from the haskell webpage:
you need to remove it. Here is a @hyperlink["discussion thread on how to do that"]{http://www.haskell.org/pipermail/haskell-cafe/2011-March/090170.html}.

@bold{Warning}: we did not test this, and even if we did, every computer is configured differently.
In the future: please use homebrew for every installation where a formula is available.
It allows you to uninstall stuff easily once you don't need it any more.

Then install the newest version:

@codeblock|{
brew update && brew install cabal && cabal install cabal-install && brew install ghc
}|

@subsubsection[#:style 'unnumbered]{Installing dependencies on Linux}

It's only tested on Ubuntu 14.04 and hopefully it works on other distributions based on Ubuntu or Debian.

@codeblock|{
sudo add-apt-repository -y ppa:hvr/ghc

sudo apt-get update

sudo apt-get install -y clang lldb-3.5 g++ make premake4 zlib1g-dev \
                        ghc-7.10.2 cabal-install-1.22 racket doxygen

cabal update && cabal install cabal-install

ln -s /usr/bin/cabal-1.22 /usr/bin/cabal
ln -s /usr/bin/lldb-3.5 /usr/bin/lldb

export PATH=/opt/ghc/7.10.2/bin:$PATH
cabal update
}|

@subsection{Compiling and installing Encore}

@codeblock|{
cd encore
make
make test
}|

@subsection{Adding @code{encorec} to the path}

We recommend that you add the @code{release} directory to your @code{PATH} environment variable.
This will allow you to invoke the compiler:

@codeblock|{
  $ encorec my_file.enc
}|

in any directory. To do this, add this line to your @code{~/.bashrc} file, inserting
the proper path for @code{<SOME_DIR>}:

@codeblock|{
export PATH="<SOME_DIR>/encore/release:${PATH}"
}|

@subsection{Compiling and Running Encore Programs}

Now you can compile a program by using
@codeblock|{
$ encorec my_file.enc
}|

This will produce an executable that you can run as usual:

@codeblock|{
./my_file
}|

Alternatively, you can use the .enc-file as a script by adding:

@codeblock|{
#! /usr/bin/env encorec -run
}|

as its FIRST line. After you made the file executable:

@codeblock|{
$ chmod u+x my_file.enc
}|

you can execute it:
@codeblock|{
$ ./my_file.enc
}|

This will compile the file, run it and remove the executable.

You can find some example programs in the programs directory @code{encore/programs}.

@section{Emacs Support}

@subsection{encore-mode}

Some extra support for emacs from Elias Castegren; add the following
to @literal{~./emacs} or @literal{~/.emacs.d/init.el}

@codeblock|{
    (add-to-list 'load-path "PATH/TO/encore/emacs")
    (require 'encore-mode)
}|

where @code{PATH/TO} is the path to the folder containing the
@code{encore} folder.

If you also want better automatic indentation, you can additionally add:

@codeblock|{
    (add-to-list "PATH/TO/encore/emacs/dtrt-indent-20140325.1330/")
    (require 'dtrt-indent)
    (dtrt-indent-mode 1)
}|

You can add compilation support by adding the lines:

@codeblock|{
    (add-hook 'encore-mode-hook (lambda () (global-set-key (kbd "C-c C-c") 'compile)))
    (add-hook 'encore-mode-hook (lambda () (global-set-key (kbd "C-c C-n") 'next-error)))
    (add-hook 'encore-mode-hook (lambda () (global-set-key (kbd "C-c C-p") 'previous-error)))
}|

This allows you to compile an @tt{encore} source file by pressing
@tt{C-c C-c} in emacs. If there are errors, you can jump to them by
pressing @tt{C-c C-n} (to jump to the next error) or @tt{C-c C-p} (to
jump to the previous error.

@subsection{yas-minor-mode}

If you are a user of @tt{yas-minor-mode}, you can find snippet files
in @tt{PATH/TO/encore/emacs/yas}. You can use them by executing:

@verbatim{
cd ~/.emacs.d/snippets/ # (the snippets directory may differ on your system)
ln -s PATH/TO/encore/emacs/yas/encore-mode .
}
