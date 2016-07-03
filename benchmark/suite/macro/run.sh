#!/bin/sh
set -euo pipefail

schmldir=$SCHML_DIR
ranges=(10 100 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)
file=quicksort_partialvectdyn
# --------------------------------------------------------------------

testdir=$schmldir/benchmark/suite
datadir=$schmldir/benchmark/suite/man-lattice/data
outdir=$schmldir/benchmark/suite/man-lattice/output
tmpdir=$schmldir/benchmark/suite/man-lattice/tmp
miscdir=$schmldir/benchmark/suite/man-lattice/misc
TIMEFORMAT=%R

logfile=$datadir/log.csv

name=worstcase

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

cp src/* $tmpdir
cd $schmldir
racket benchmark.rkt $tmpdir

cd $tmpdir
for n in ${ranges[*]}; do
    echo $n >> log1
    echo $n | ./$file.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' >> log2
    echo $n | ./$file.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' >> log3
done

echo "iterations,time coercions, time twosomes" > $logfile
paste -d"," log1 log2 log3 >> $logfile

gnuplot -e "set datafile separator \",\"; set term tikz standalone color; "`
	   `"set output '$outdir/${name}.tex'; "`
	   `"set border back; "`
	   `"set key left top;"`
	   `"set title \"\"; "`
	   `"set xrange [0:10000]; set yrange [0:5100]; "`
	   `"set xlabel \"length of the array\"; "`
	   `"set ylabel \"time in seconds\"; "`
	   `"set xtics nomirror (10,100,1000,5000,8000,10000); "`
	   `"set ytics nomirror; "`
	   `"plot '$logfile' using 1:2 with lp lw 3 lc rgb \"blue\" title 'Coercions', "`
	   `"'$logfile' using 1:3 with lp lw 3 lc rgb \"red\" title 'Type-based casts'"

# compile tex code

cp $miscdir/* $tmpdir
cp $outdir/${name}.tex $tmpdir
cd $tmpdir
lualatex --interaction=nonstopmode ${name}.tex
mv ${name}.pdf $outdir
