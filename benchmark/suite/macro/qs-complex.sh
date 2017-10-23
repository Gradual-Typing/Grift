#!/bin/sh
set -euo pipefail

loops=5
precision=5
grift_mem_limit=9999999999

griftdir=$SCHML_DIR
ranges=(10 100 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)
file=quicksort_worstcase
# --------------------------------------------------------------------
date=`date +%Y_%m_%d_%H_%M_%S`

testdir=$griftdir/benchmark/suite/macro
qscomplexdir=$testdir/quicksort-complex/$date
datadir=$qscomplexdir/data
outdir=$qscomplexdir/output
tmpdir=$qscomplexdir/tmp
miscdir=$testdir/misc
srcdir=$testdir/src
paramfile=$qscomplexdir/params.txt
TIMEFORMAT=%R

logfile=$datadir/log.csv

grift_regex="sed -n 's/.*): \\([0-9]\\+\\)/\\1/p'"

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

cp src/partial/$file.grift $tmpdir
cd $griftdir
racket $griftdir/benchmark/benchmark.rkt $tmpdir $grift_mem_limit

cd $tmpdir
for n in ${ranges[*]}; do
    echo $n >> log1
    cd $tmpdir
    c="echo $n | ./$file.o1 | $grift_regex"
    avg "$c"
    echo $RETURN >> log2

    c="echo $n | ./$file.o2 | $grift_regex"
    avg "$c"
    echo $RETURN >> log3
    echo "finished" $n
done

echo "iterations,time coercions, time twosomes" > $logfile
paste -d"," log1 log2 log3 >> $logfile

gnuplot -e "set datafile separator \",\"; set term pngcairo enhanced color font 'Verdana,10'; "`
	   `"set output '$outdir/${name}.png'; "`
	   `"set key left top;"`
	   `"set title \"Coercions vs Type-based casts performance comparison for quicksort worst case\"; "`
	   `"set xlabel \"length of the array\"; "`
	   `"set ylabel \"time in seconds\"; "`
	   `"set xtics nomirror; "`
	   `"set ytics nomirror; "`
	   `"set logscale y;"`
	   `"set format y \"%.4f\";"`
	   `"plot '$logfile' using 1:2 with lp lw 3 title 'Coercions', "`
	   `"'$logfile' using 1:3 with lp lw 3 title 'Type-based casts'"

gnuplot -e "set datafile separator \",\"; set term pngcairo enhanced color font 'Verdana,10'; "`
	   `"set output '$outdir/${name}_coercions_fitting.png'; "`
	   `"set key left top;"`
	   `"f(x) = a*x**2 + b*x + c;"`
	   `"set title \"Coercions runtimes fitted by a 2nd degree polynomial\"; "`
	   `"set xlabel \"length of the array\"; "`
	   `"set ylabel \"time in seconds\"; "`
	   `"set xtics nomirror; "`
	   `"set ytics nomirror; "`
	   `"fit f(x) '$logfile' using 1:2 via a, b, c;"`
	   `"plot '$logfile' using 1:2 with lp lw 3 title 'Coercions', "`
	   `"f(x) ls 4 title '2nd-degree polynomial'"

gnuplot -e "set datafile separator \",\"; set term pngcairo enhanced color font 'Verdana,10'; "`
	   `"set output '$outdir/${name}_typebased_fitting.png'; "`
	   `"set key left top;"`
	   `"f(x) = a*x**3 + b*x**2 + c*x + d;"`
	   `"set title \"Type-based casts runtimes fitted by a 3nd degree polynomial\"; "`
	   `"set xlabel \"length of the array\"; "`
	   `"set ylabel \"time in seconds\"; "`
	   `"set xtics nomirror; "`
	   `"set ytics nomirror; "`
	   `"fit f(x) '$logfile' using 1:3 via a, b, c, d;"`
	   `"set style line 2 lw 3 lt 2;"`
	   `"plot '$logfile' using 1:3 with lp ls 2 title 'Type-based casts', "`
	   `"f(x) ls 4 title  '3rd-degree polynomial'"
