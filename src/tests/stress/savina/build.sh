#!/bin/zsh
for d in $(ls -d */); do
    cd "$d"; encorec -c *.enc; cd ..
done
