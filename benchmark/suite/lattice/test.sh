#!/bin/sh

# nAnnotizer ranges between 0 and 1 (not 100)

nbins=10
nsamples=10
deliverable=3
usable=10
nAnnotizerbin=/u/dalmahal/nAnnotizer/.stack-work/install/x86_64-linux/lts-3.4/7.10.2/bin/./nAnnotizer
griftdir=/u/dalmahal/Grift
# --------------------------------------------------------------------

testdir=$griftdir/benchmark/suite
datadir=$griftdir/benchmark/suite/lattice/data
outdir=$griftdir/benchmark/suite/lattice/output
tmpdir=$griftdir/benchmark/suite/lattice/tmp
miscdir=$griftdir/benchmark/suite/lattice/misc
TIMEFORMAT=%R
let "n=nbins*nsamples"
let "nx=n*6/10"

for f in $tmpdir/*.grift; do
    path="${f%.*}";name=$(basename "$path")
    logfile1=$datadir/${name}1.log
    logfile2=$datadir/${name}1.csv
    logfile3=$datadir/${name}2.log
    logfile4=$datadir/${name}2.csv
    
done

declare -A x1 x2
for i in `seq 1 20`; do
    x1[$i]=1
    x2[$i]=0
done
f=1

if test "${x1[$f]+isset}"; then x1[${f}]=$((x1[${f}]+1)); else x1[${f}]=1; fi
for key in ${!x1[@]}; do
    echo $key ${x1[$key]}
done
