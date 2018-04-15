#!/bin/sh
set -euo pipefail

. lib/runtime.sh

c1=17
c2=7

LOOPS=1
PRECISION=5

griftdir=$GRIFT_DIR
ranges=(10 100 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)
file=quicksort
# --------------------------------------------------------------------
date=`date +%Y_%m_%d_%H_%M_%S`

TEST_DIR=$griftdir/benchmark/suite/macro
LIB_DIR="$TEST_DIR/lib"
qscomplexdir=$TEST_DIR/quicksort-complex/$date
datadir=$qscomplexdir/data
outdir=$qscomplexdir/output
tmpdir=$qscomplexdir/tmp
miscdir=$TEST_DIR/misc
srcdir=$TEST_DIR/src
paramfile=$qscomplexdir/params.txt
TIMEFORMAT=%R

logfile1="$datadir/$c1.csv"
logfile2="$datadir/$c2.csv"

grift_regex="sed -n 's/.*): \\([0-9]\\+\\)/\\1/p'"

name=worstcase

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

cp src/partial/$file.grift $tmpdir
cd $griftdir

racket "$griftdir/benchmark/benchmark.rkt" --cast-profiler -s "$c1 $c2" "$tmpdir"

cd $tmpdir
echo "iterations,runtime,total values allocated,total casts,longest proxy chain,total proxies accessed,total uses,function total values allocated,vector total values allocated,ref total values allocated,tuple total values allocated,function total casts,vector total casts, ref total casts,tuple total casts,function longest proxy chain,vector longest proxy chain,ref longest proxy chain,tuple longest proxy chain,function total proxies accessed,vector total proxies accessed,ref total proxies accessed,tuple total proxies accessed,function total uses,vector total uses,ref total uses,tuple total uses,injects casts,projects casts"\
     > "$logfile1"
echo "iterations,runtime,total values allocated,total casts,longest proxy chain,total proxies accessed,total uses,function total values allocated,vector total values allocated,ref total values allocated,tuple total values allocated,function total casts,vector total casts, ref total casts,tuple total casts,function longest proxy chain,vector longest proxy chain,ref longest proxy chain,tuple longest proxy chain,function total proxies accessed,vector total proxies accessed,ref total proxies accessed,tuple total proxies accessed,function total uses,vector total uses,ref total uses,tuple total uses,injects casts,projects casts"\
     > "$logfile2"

function run()
{
    # the configuration index
    local c="$1";       shift
    local input="$1";   shift
    local output="$1";  shift
    local logfile="$1"; shift
    
    local b="${tmpdir}/${file}.o${c}"
    local bname="$(basename $b)"
    avg "$b" "$input" "static" "$output" "${b}.runtimes"
    printf "${RETURN}" >> "$logfile"
    rm "${b}.runtimes"

    eval "cat ${input} | ${b}.prof.o" > /dev/null 2>&1
    mv "$bname.prof.o.prof" "${b}.prof"
    printf "," >> "$logfile"
    # ignore first and last rows and sum the values across all
    # columns in the profile into one column and transpose it into
    # a row
    sed '1d;$d' "${b}.prof" | awk -F, '{print $2+$3+$4+$5+$6+$7}'\
        | paste -sd "," - | xargs echo -n >> "$logfile"
    echo -n "," >> "$logfile"
    # ignore the first row and the first column and stitsh together
    # all rows into one row
    sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
        | awk -F, '{print $1","$2","$3","$4}' | paste -sd "," -\
        | xargs echo -n >> "$logfile"
    echo -n "," >> "$logfile"
    # writing injections
    sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
        | awk -F, 'FNR == 2 {print $5}' | xargs echo -n >> "$logfile"
    echo -n "," >> "$logfile"
    # writing projections
    sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
        | awk -F, 'FNR == 2 {print $6}' >> "$logfile"
}

for n in ${ranges[*]}; do
    input=$n;
    for ((i=$n-1;i>=0;i--)); do
	input="$input $i";
    done
    echo $input > "${tmpdir}/input"
    input="${tmpdir}/input"
    echo $(($n-1)) > "${tmpdir}/output"
    
    printf "${n}," >> "$logfile1"
    printf "${n}," >> "$logfile2"

    run "$c1" "$input" "${tmpdir}/output" "$logfile1"
    run "$c2" "$input" "${tmpdir}/output" "$logfile2"
    
    echo "finished" $n
done

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
	   `"plot '$logfile1' using 1:2 with lp lw 3 title 'Coercions', "`
	   `"'$logfile2' using 1:2 with lp lw 3 title 'Type-based casts'"

gnuplot -e "set datafile separator \",\"; set term pngcairo enhanced color font 'Verdana,10'; "`
	   `"set output '$outdir/${name}_coercions_fitting.png'; "`
	   `"set key left top;"`
	   `"f(x) = a*x**2 + b*x + c;"`
	   `"set title \"Coercions runtimes fitted by a 2nd degree polynomial\"; "`
	   `"set xlabel \"length of the array\"; "`
	   `"set ylabel \"time in seconds\"; "`
	   `"set xtics nomirror; "`
	   `"set ytics nomirror; "`
	   `"fit f(x) '$logfile1' using 1:2 via a, b, c;"`
	   `"plot '$logfile1' using 1:2 with lp lw 3 title 'Coercions', "`
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
	   `"fit f(x) '$logfile2' using 1:2 via a, b, c, d;"`
	   `"set style line 2 lw 3 lt 2;"`
	   `"plot '$logfile2' using 1:2 with lp ls 2 title 'Type-based casts', "`
	   `"f(x) ls 4 title  '3rd-degree polynomial'"
