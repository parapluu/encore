#!/bin/zsh

sudo dtrace -l -P "encore*" -P "pony*" -c ./$1 > tmp

enc_probes="$(awk '/encore/ {printf "%s %s\n", $4, $5}' tmp)"
enc_probes="$(echo $enc_probes | awk '!/_enc/ {printf "%s\n", $0}')"
pon_probes="$(awk '/pony/ {printf "%s %s\n", $4, $5}' tmp)"

echo "Encore probes:" > probes.txt
echo "Name Probe\n $enc_probes" | awk '{printf "%s %s\n", $2, $1}' | column -t >> probes.txt
echo "\nPony probes:" >> probes.txt
echo "Name Probe $pon_probes" | awk '{printf "%s %s\n", $2, $1}' | column -t >> probes.txt

rm tmp
