#!/bin/bash
echo "Your kernel version is $(uname -r)."
echo {a..j}{0..9} | tr [a..z] [A..Z] | tr 0 _

greeting=こんにちは
echo ${greeting/は/わ}
greeting=こんばんわ
echo ${greeting//ん/う}
echo ${greeting:4}
