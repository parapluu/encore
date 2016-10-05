#!/usr/bin/env bash
set -e
quiet=''
replace='false'
set_path='false'
build='false'

while getopts 'qrbphf' flag; do
  case "${flag}" in
    q) quiet='-qq' ;;
    r) replace='true' ;;
    s) set_path='true' ;;
    b) build='true' ;;
    f) build='true'; set_path='true';replace='true';;
    h)  echo "Available flags are:";\
            echo "-q  Quiet. Run in quiet mode, I.e don't print as much stuff";
            echo "-r  Replace ghc and cabal. Will change the path \$PATH";
            echo "    to point to ghc version 7.10.3 and cabal to version 1.22.";
            echo "-s  Adds Encore to the \$PATH variable. Has no effect unless the \`-b\` flag is used.";
            echo "-b  Builds Encore.";
            echo "-h  Print this message and exit.";
            echo "-f  Perform a full install. Equivalent to \`-rsb\`";exit;;
    *) echo "Unexpected option ${flag}. Use \`-h\` to see available options"; exit ;;
  esac
done

echo "--== ADDING REPOS ==--"
sudo add-apt-repository -y ppa:hvr/ghc

echo "--== UPDATING REPOS ==--"
sudo apt-get update $quiet

echo "--== INSTALLING STUFF ==--"
sudo apt-get install -y $quiet\
  clang\
  lldb-3.5\
  g++\
  make\
  premake4\
  zlib1g-dev \
  ghc-7.10.3\
  cabal-install-1.22\
  racket\
  doxygen

wget ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre2-10.21.tar.bz2
tar xvf pcre2-10.21.tar.bz2
cd pcre2-10.21
./configure --prefix=/usr
make
sudo make install

if $replace
then
  if ((hash ghc 2>/dev/null) && (ghc --version | grep -q 7.10.3))
  then
    echo "--==HASKELL VERSION IS CORRECT==--"
  else
    echo "--==SETTING HASKELL PATH==--"
    echo 'export PATH=/opt/ghc/7.10.3/bin:$PATH' >> $HOME/.profile
    export PATH=/opt/ghc/7.10.3/bin:$PATH
  fi

  if ((hash cabal 2>/dev/null) && (cabal --version | grep -q 1.22))
  then
      echo "--==CABAL VERSION IS CORRECT==--"
  else
    echo "--==SETTING CABAL PATH==--"
    echo 'export PATH=$HOME/.cabal/bin:/opt/cabal/1.22/bin:$PATH' >> $HOME/.profile
    export PATH=$HOME/.cabal/bin:/opt/cabal/1.22/bin:$PATH
  fi

  echo "--== UPDATING CABAL ==--"
  cabal update && cabal install cabal-install
fi

if $build
then
  echo "--== BUILDING ENCORE ==--"
  make

  if $set_path
  then
    echo "--== SETTING UP ENCORE PATHS ==--"
    echo export PATH=$(pwd)/release:\$PATH >> $HOME/.profile
    export PATH=$(pwd)/release:$PATH
  fi
fi

echo "--== DONE! ==--"
echo "Good luck, have fun!"
