!#/bin/bash
echo "--== ADDING REPOS == --" &&
sudo add-apt-repository -y ppa:hvr/ghc &&

echo "--== UPDATING REPOS == --" &&
sudo apt-get update &&

echo "--== INSTALLING STUFF == --" &&
sudo apt-get install -y clang lldb-3.5 g++ make premake4 zlib1g-dev \
ghc-7.10.2 cabal-install-1.22 racket doxygen &&

if ((hash ghc 2>/dev/null) && (ghc --version | grep -q 7.10.2))
then
  echo "--==HASKELL VERSION IS CORRECT==--"
else
  echo "--==SETTING HASKELL PATH==--"
  echo 'export PATH=/opt/ghc/7.10.2/bin:$PATH' >> $HOME/.bashrc
  export PATH=/opt/ghc/7.10.2/bin:$PATH
fi

if ((hash cabal 2>/dev/null) && (cabal --version | grep -q 1.22))
then
    echo "--==CABAL VERSION IS CORRECT==--"
else
  echo "--==SETTING CABAL PATH==--"
  echo 'export PATH=$HOME/.cabal/bin:/opt/cabal/1.22/bin:$PATH' >> $HOME/.bashrc
  export PATH=$HOME/.cabal/bin:/opt/cabal/1.22/bin:$PATH
fi

echo "--== UPDATING CABAL ==--" &&
cabal update && cabal install cabal-install &&
echo "--== BUILDING ENCORE ==--" &&
make &&
echo "--== TESTING ==--" &&
make test
echo "--== SETTING UPP ENCORE PATHS ==--" &&

echo export PATH=$(pwd)/release:\$PATH >> $HOME/.bashrc
export PATH=$(pwd)/release:$PATH

echo "--== DONE! ==--"
echo "Good luck, have fun!"
