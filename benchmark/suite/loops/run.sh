#!/bin/sh

loop=999999999
TIMEFORMAT=%R
name=loop
logfile=$name.csv
gcclog=gcc.log
gambitclog=gambitc.log

rm -f $logfile

gsc -prelude "(declare (standard-bindings)) (declare (block))" -exe -cc-options "-O3 
-funroll-loops" s$name.scm
gcc c$name.c -O3 -std=c11 -funroll-loops -o c$name
for i in `seq 1 $1`;
do
    { time ./c$name $loop >/dev/null 1 ; } 2>> $gcclog
    { time ./s$name >/dev/null 1 ; } 2>> $gambitclog
done

read gcc_std gcc_mean <<< $( cat $gcclog | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read gambitc_std gambitc_mean <<< $( cat $gambitclog | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )

echo "$gcc_mean $gambitc_mean" > $logfile

rm -f $gcclog $gambitclog
