#!/bin/sh

gloop=999999
loop=99999999
schmldir=/u/dalmahal/lattice/Schml
memlimit=9999999999
#---------------------------------------------------------------------

name=loop
dir=$schmldir/benchmark/suite/micro/$name
datadir=$dir/data
outdir=$dir/output
tmpdir=$dir/tmp
logfile=$datadir/$name.csv
TIMEFORMAT=%R

echo "Benchmarking empty loops"

# create the data and tmp directories if they do not exist
mkdir -p $datadir $tmpdir

cp $dir/src/* $tmpdir

# compile Schml source files, then enter the src directory
cd $schmldir
racket benchmark.rkt $tmpdir $memlimit
cd $tmpdir

# compile scheme source files
gsc -exe -cc-options "-O3 -funroll-loops" s$name.scm

# compile C source files
# gcc c$name.c -O3 -std=c11 -funroll-loops -o c$name.o

# normalize to the cost of iteration in nano seconds.
c1=$(echo "1000000000/$gloop" | bc -l)
# c2=$(echo "1000000000/$loop" | bc -l)
c3=$(echo "1000000/$loop" | bc -l) # gambitC's time returns result in ms

for i in `seq 1 $1`;
do
    ./s$name $loop | sed -n '3,3s/ *\([0-9]*\).*/\1/p' | awk -v c="$c3" '{print ($1 = $1*c) " " $2}' >> s$name.log
    # ./c${name}.o $loop | sed -n 's/.*: \([0-9]*\)/\1/p' | awk -v c="$c2" '{print ($1 = $1*c) " " $2}' >> c$name.log
    echo $gloop | ./${name}.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{print ($1 = $1*c) " " $2}' >> ${name}.log
    echo "finished run #$i"
done

# read gcc_std gcc_mean <<< $( cat c$name.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read gambitc_std gambitc_mean <<< $( cat s$name.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read schml_std schml_mean <<< $( cat $name.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )

echo "$gambitc_mean $schml_mean" > $logfile
