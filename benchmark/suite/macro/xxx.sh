#!/bin/sh
set -euo pipefail

iters=10000
nsamples=1000
deliverable=3
usable=10
schmldir=$SCHML_DIR
# --------------------------------------------------------------------

testdir=$schmldir/benchmark/suite/macro
datadir=$testdir/data
outdir=$testdir/output
tmpdir=$testdir/tmp
miscdir=$testdir/misc
TIMEFORMAT=%R
let "nx=nsamples*6/10"
n=1000

# common lines
LIN="set arrow from 1,$nx to 20,$nx nohead lt 3 dashtype 2 lc rgb \"brown\" lw 2;\
     set arrow from 10,graph(0,0) to 10,graph(1,1) nohead lc rgb \"orange\" lw 2; \
     set arrow from 3,graph(0,0) to 3,graph(1,1) nohead lc rgb \"green\" lw 2"

# f=$tmpdir/quicksort_worstcase_static.schml
f=$tmpdir/matmult_static.schml
path="${f%.*}";name=$(basename "$path")
logfile1=$datadir/${name}1.log
logfile2=$datadir/${name}1.csv
logfile3=$datadir/${name}2.log
logfile4=$datadir/${name}2.csv

gnuplot -e "set datafile separator \",\"; set term tikz standalone color; "`
	   `"set output '$outdir/${name}.tex'; "`
	   `"set border back; "`
	   `"set multiplot layout 2,2 rowsfirst;"`
	   `"set title \"\"; "`
	   `"set xrange [0:20]; set yrange [0:${n}]; "`
	   `"set xtics nomirror (\"1x\" 1, \"6x\" 6, \"10x\" 10, \"15x\" 15, \"20x\" 20); "`
	   `"set ytics nomirror; "`
	   `"$LIN; plot '$logfile2' using 1:2 with lines lw 3 lc rgb \"blue\" title '' smooth cumulative; "`
	   `"$LIN; plot '$logfile4' using 1:2 with lines lw 3 lc rgb \"blue\" title '' smooth cumulative"

# compile tex code

cp $miscdir/* $tmpdir
cp $outdir/${name}.tex $tmpdir
cd $tmpdir
lualatex --interaction=nonstopmode ${name}.tex
mv ${name}.pdf $outdir

# file duplicate files
# find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate
