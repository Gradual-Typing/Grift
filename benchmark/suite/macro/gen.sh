#!/bin/sh

s=""
for i in `seq 0 9999`; do
    x=$(( ( RANDOM % 1000 )  + 1 ))
    s=$"$s\n(gvect-set! a $i $x)"
done

echo -e $s >> log
