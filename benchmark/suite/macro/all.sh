#!/bin/sh

schmldir=/u/dalmahal/lattice/Schml
loops=$1
iters=10000
precision=3
# --------------------------------------------------------------------

testdir=$schmldir/benchmark/suite
datadir=$schmldir/benchmark/suite/man-lattice/data
outdir=$schmldir/benchmark/suite/man-lattice/output
tmpdir=$schmldir/benchmark/suite/man-lattice/tmp
miscdir=$schmldir/benchmark/suite/man-lattice/misc
TIMEFORMAT=%R

static_mem_limit=999999
#---------------------------------------------------------------------

logfile=$datadir/log.csv

name=output

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

cp src/* $tmpdir

# for monotonic support
cd /u/dalmahal/Schml

racket benchmark.rkt $tmpdir 999999

cd $tmpdir

sum=0
for i in seq loops; do
    t=$(echo $iters | ./quicksort_worstcase_static.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q')
    sum=$(echo "scale=$precision;$sum+$t" | bc)
done
avg11=$(echo "scale=$precision;$sum/$loops" | bc)

sum=0
for i in seq loops; do
    t=$(echo $iters | ./quicksort_worstcase_static.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q')
    sum=$(echo "scale=$precision;$sum+$t" | bc)
done
avg12=$(echo "scale=$precision;$sum/$loops" | bc)




echo "iterations,time coercions, time twosomes" > $logfile
paste -d"," log1 log2 log3 >> $logfile
