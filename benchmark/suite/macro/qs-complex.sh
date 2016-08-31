#!/bin/sh
set -euo pipefail

loops=5
precision=5
schml_mem_limit=9999999999

schmldir=$SCHML_DIR
ranges=(10 100 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)
file=quicksort_worstcase
# --------------------------------------------------------------------
date=`date +%Y_%m_%d_%H_%M_%S`

testdir=$schmldir/benchmark/suite/macro
qscomplexdir=$testdir/quicksort-complex/$date
datadir=$qscomplexdir/data
outdir=$qscomplexdir/output
tmpdir=$qscomplexdir/tmp
miscdir=$testdir/misc
srcdir=$testdir/src
paramfile=$qscomplexdir/params.txt
TIMEFORMAT=%R

logfile=$datadir/log.csv

schml_regex="sed -n 's/.*): \\([0-9]\\+\\)/\\1/p'"

name=worstcase

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

avg()
{
    local avg_sum=0
    local avg_i
    for avg_i in `seq $loops`; do
	avg_t=$(eval $1)
	avg_sum=$(echo "scale=$precision;$avg_sum+$avg_t" | bc)
    done
    RETURN=$(echo "scale=$precision;$avg_sum/$loops" | bc)
}

cp src/partial/$file.schml $tmpdir
cd $schmldir
racket $schmldir/benchmark/benchmark.rkt $tmpdir $schml_mem_limit

cd $tmpdir
for n in ${ranges[*]}; do
    echo $n >> log1
    cd $tmpdir
    c="echo $n | ./$file.o1 | $schml_regex"
    avg "$c"
    echo $RETURN >> log2

    c="echo $n | ./$file.o2 | $schml_regex"
    avg "$c"
    echo $RETURN >> log3
    echo "finished" $n
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
