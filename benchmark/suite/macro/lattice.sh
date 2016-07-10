#!/bin/sh
set -euo pipefail

iters=1000
nsamples=1000
deliverable=3
usable=10
nAnnotizerbin=nAnnotizer
schmldir=$SCHML_DIR
# --------------------------------------------------------------------

testdir=$schmldir/benchmark/suite/macro
datadir=$testdir/data
outdir=$testdir/output
tmpdir=$testdir/tmp
miscdir=$testdir/misc
TIMEFORMAT=%R
let "nx=nsamples*6/10"

# common lines
LIN="set arrow from 1,$nx to 20,$nx nohead lt 3 dashtype 2 lc rgb \"brown\" lw 2;\
     set arrow from 10,graph(0,0) to 10,graph(1,1) nohead lc rgb \"orange\" lw 2; \
     set arrow from 3,graph(0,0) to 3,graph(1,1) nohead lc rgb \"green\" lw 2"

compute_slowdown_index () {
    # floor and if it is integer, subtract 1
    x1=$(echo "$1/$2" | bc -l | awk '{printf "%f\n", $0}')
    x2=$(echo ${x1%.*})
    if (( $(bc <<< "$x1 == $x2") ))
    then
	let x2="$x2-1"
    fi
    echo ${x2#-}; }

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

cp src/static/* $tmpdir
cd $schmldir
racket benchmark.rkt $tmpdir/ 9999999999

for f in $tmpdir/*.schml; do
    path="${f%.*}";name=$(basename "$path")
    logfile1=$datadir/${name}1.log
    logfile2=$datadir/${name}1.csv
    logfile3=$datadir/${name}2.log
    logfile4=$datadir/${name}2.csv
    statict1=$(echo $iters | ${path}.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q')
    statict2=$(echo $iters | ${path}.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q')
    # $nAnnotizerbin $path 1 1 1
    # racket benchmark.rkt $path 9999999999
    # dynt1="$(time ( $path/100/0.o1 ) 2>&1 1>/dev/null )"
    # dynt2="$(time ( $path/100/0.o2 ) 2>&1 1>/dev/null )"
    # rm -rf $path
    # r1=$(echo "$statict1/$dynt1" | bc -l)
    # r2=$(echo "$statict2/$dynt2" | bc -l)
    # printf "%s: ratio1:%s\t ratio2:%s\n" $name $r1 $r2
    
    $nAnnotizerbin $path $nsamples
    cd $schmldir
    racket benchmark.rkt $path/ 9999999999
    
    declare -A x1 x2
    for i in `seq 0 20`; do
	x1[$i]=0
	x2[$i]=0
    done
    echo "name,precision,time,slowdown" > $logfile1
    n=0
    for b in $(find $path -name '*.o1'); do
	let n=n+1
	echo $b
	binpath="${b%.*}";bname=$(basename "$binpath")
	p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < $binpath.schml)
	t=$(echo $iters | $b | sed -n 's/.*: \([0-9]\+\)/\1/p')
	echo $t
	f=$(echo "$t/$statict1" | bc -l | awk '{printf "%.2f\n", $0}')
	echo $f
	printf "%d,%.2f,%.2f,%.2f\n" $bname $p $t $f >> $logfile1
	if test "${x1[$f]+isset}"; then x1[${f}]=$((x1[${f}]+1)); else x1[${f}]=1; fi
    done
    for key in ${!x1[@]}; do
	printf "%.2f, %d\n" $key ${x1[$key]} >> $logfile2
    done
    sort -g $logfile2 -o $logfile2

    echo "name,precision,time,slowdown" > $logfile3
    for b in $(find $path -name '*.o2'); do
	binpath="${b%.*}"
	echo $b
	p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < $binpath.schml)
	t=$(echo $iters | $b | sed -n 's/.*: \([0-9]\+\)/\1/p')
	echo $t
	f=$(echo "$t/$statict2" | bc -l | awk '{printf "%.2f\n", $0}')
	echo $f
	printf "%d,%.2f,%.2f,%.2f\n" $bname $p $t $f >> $logfile3
	if test "${x2[$f]+isset}"; then x2[${f}]=$((x2[${f}]+1)); else x2[${f}]=1; fi
    done
    for key in ${!x2[@]}; do
	printf "%.2f, %d\n" $key ${x2[$key]} >> $logfile4
    done
    sort -g $logfile4 -o $logfile4
    
    # gnuplot -e "set datafile separator \",\"; set term tikz standalone color; "`
    # 	   `"set output '$outdir/${name}.tex'; "`
    # 	   `"set border back; "`
    # 	   `"set multiplot layout 2,2 rowsfirst;"`
    # 	   `"set title \"\"; "`
    # 	   `"set xrange [0:20]; set yrange [0:${n}]; "`
    # 	   `"set xtics nomirror (\"1x\" 1, \"6x\" 6, \"10x\" 10, \"15x\" 15, \"20x\" 20); "`
    # 	   `"set ytics nomirror; "`
    # 	   `"$LIN; plot '$logfile2' using 1:2 with lines lw 3 lc rgb \"blue\" title '' smooth cumulative; "`
    # 	   `"$LIN; plot '$logfile4' using 1:2 with lines lw 3 lc rgb \"blue\" title '' smooth cumulative"

    # compile tex code

    # cp $miscdir/* $tmpdir
    # cp $outdir/${name}.tex $tmpdir
    # cd $tmpdir
    # lualatex --interaction=nonstopmode ${name}.tex
    # mv ${name}.pdf $outdir
done

# file duplicate files
# find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate
