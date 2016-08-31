#!/bin/sh

# This script needs the following list of programs to be installed:
# dynamizer
# clang
# gambit-c
# racket
# imagemagick
# gnuplot

set -euo pipefail

loops=5
precision=5
schml_mem_limit1=999999
schml_mem_limit2=9999999999

#---------------------------------------------------------------------
# Obselete params
deliverable=3
usable=10
#---------------------------------------------------------------------
# quicksort worstcase parameters
quicksort_worstcase_array_len=1000
quicksort_worstcase_nsamples=1000

# quicksort bestcase parameters
quicksort_bestcase_array_len=1000
quicksort_bestcase_nsamples=1000

# matrix multiplication parameters
matmult_mat_side_len=200
matmult_nsamples=1000

# --------------------------------------------------------------------
date=`date +%Y_%m_%d_%H_%M_%S`

schmldir=$SCHML_DIR
testdir=$schmldir/benchmark/suite/macro
latticedir=$testdir/lattice-dev/$date
datadir=$latticedir/data
outdir=$latticedir/output
tmpdir=$latticedir/tmp
miscdir=$testdir/misc
srcdir=$testdir/src
paramfile=$latticedir/params.txt
TIMEFORMAT=%R
# let "nx=nsamples*6/10"

# scaling racket and gambit timing from milliseconds to seconds
c1=$(echo "1/1000" | bc -l)

schml_regex="sed -n 's/.*): \\([0-9]\\+\\)/\\1/p'"

# common lines in the plots
# LIN="set arrow from 1,$nx to 20,$nx nohead lt 3 dashtype 2 lc rgb \"brown\" lw 2;\
    #      set arrow from 10,graph(0,0) to 10,graph(1,1) nohead lc rgb \"orange\" lw 2; \
    #      set arrow from 3,graph(0,0) to 3,graph(1,1) nohead lc rgb \"green\" lw 2"

# --------------------------------------------------------------------
# compiling and preparing running commands

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

# copying the benchmarks to a temporary directory
cp -r $srcdir/* $tmpdir

printf "Date\t\t:%s\n" "$date" >> $paramfile
MYEMAIL="`id -un`@`hostname -f`"
printf "Machine\t\t:%s\n" "$MYEMAIL" >> $paramfile
schml_ver=$(git rev-parse HEAD)
printf "Schml ver.\t:%s\n" "$schml_ver" >> $paramfile
clang_ver=$(clang --version | sed -n 's/clang version \([0-9]*.[0-9]*.[0-9]*\) .*/\1/p;q')
printf "Clang ver.\t:%s\n" "$clang_ver" >> $paramfile
gambit_ver=$(gsc -v | sed -n 's/v\([0-9]*.[0-9]*.[0-9]*\) .*/\1/p;q')
printf "Gambit ver.\t:%s\n" "$gambit_ver" >> $paramfile
racket_ver=$(racket -v | sed -n 's/.* v\([0-9]*.[0-9]*\).*/\1/p;q')
printf "Racket ver.\t:%s\n" "$racket_ver" >> $paramfile

# --------------------------------------------------------------------
# preparing the quicksort worstcase benchmark
benchmark=quicksort_worstcase

printf "Benchmark\t:%s\n" "$benchmark" >> $paramfile
printf "Arg_array_len\t:%s\n" "$quicksort_worstcase_array_len" >> $paramfile
printf "Arg_nsamples\t:%s\n" "$quicksort_worstcase_nsamples" >> $paramfile

benchmark_rkt="$tmpdir/rkt/$benchmark"
raco make $benchmark_rkt.rkt
quicksort_worstcase_rkt_command="racket $benchmark_rkt.rkt $quicksort_worstcase_array_len"

benchmark_clang="$tmpdir/c/$benchmark"
clang $benchmark_clang.c -O3 -o $benchmark_clang.o
quicksort_worstcase_clang_command="$benchmark_clang.o $quicksort_worstcase_array_len"

benchmark_gambit="$tmpdir/gambit/$benchmark"
gsc -exe -cc-options -O3 $benchmark_gambit.scm
quicksort_worstcase_gambit_command="$benchmark_gambit $quicksort_worstcase_array_len"

quicksort_worstcase_schml_arg=$quicksort_worstcase_array_len

# ---------------------------------
# preparing the quicksort bestcase benchmark
benchmark=quicksort_bestcase

printf "Benchmark\t:%s\n" "$benchmark" >> $paramfile
printf "Arg_array_len\t:%s\n" "$quicksort_bestcase_array_len" >> $paramfile
printf "Arg_nsamples\t:%s\n" "$quicksort_bestcase_nsamples" >> $paramfile

benchmark_rkt="$tmpdir/rkt/$benchmark"
raco make $benchmark_rkt.rkt
quicksort_bestcase_rkt_command="racket $benchmark_rkt.rkt $quicksort_bestcase_array_len"

benchmark_clang="$tmpdir/c/$benchmark"
clang $benchmark_clang.c -O3 -o $benchmark_clang.o
quicksort_bestcase_clang_command="$benchmark_clang.o $quicksort_bestcase_array_len"

benchmark_gambit="$tmpdir/gambit/$benchmark"
gsc -exe -cc-options -O3 $benchmark_gambit.scm
quicksort_bestcase_gambit_command="$benchmark_gambit $quicksort_bestcase_array_len"

quicksort_bestcase_schml_arg=$quicksort_bestcase_array_len

s=""
f=""
n=$(($quicksort_bestcase_array_len-1))
for i in `seq 0 $n`; do
    # x=$(perl -e 'print int rand $quicksort_bestcase_array_len, "\n"; ')
    x=$(( ( RANDOM % $quicksort_bestcase_array_len )  + 1 ))
    f="$f\n$x"
    s="$s\n(gvector-set! a $i $x)"
done

echo -e $f > $tmpdir/nums

echo "s/INITIALIZE-VECTOR/$s/" | sed -f - -i  $tmpdir/static/$benchmark.schml
echo "s/INITIALIZE-VECTOR/$s/" | sed -f - -i  $tmpdir/dyn/$benchmark.schml

# ---------------------------------
# preparing the matmult benchmark
benchmark=matmult

printf "Benchmark\t:%s\n" "$benchmark" >> $paramfile
printf "Mat_side_len\t:%s\n" "$matmult_mat_side_len" >> $paramfile
printf "Arg_nsamples\t:%s\n" "$matmult_nsamples" >> $paramfile

benchmark_rkt="$tmpdir/rkt/$benchmark"
raco make $benchmark_rkt.rkt
matmult_rkt_command="racket $benchmark_rkt.rkt $matmult_mat_side_len"

benchmark_clang="$tmpdir/c/$benchmark"
clang $benchmark_clang.c -O3 -o $benchmark_clang.o
matmult_clang_command="$benchmark_clang.o $matmult_mat_side_len"

benchmark_gambit="$tmpdir/gambit/$benchmark"
gsc -exe -cc-options -O3 $benchmark_gambit.scm
matmult_gambit_command="$benchmark_gambit $matmult_mat_side_len"

matmult_schml_arg=$matmult_mat_side_len
#---------------------------------------------------------------------

cd $schmldir
racket $schmldir/benchmark/benchmark.rkt $tmpdir/static/ $schml_mem_limit1
racket $schmldir/benchmark/benchmark.rkt $tmpdir/dyn/ $schml_mem_limit2

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


usability_code=""
scatter_code=""
in=0
for f in $tmpdir/static/*.schml; do
    path="${f%.*}";name=$(basename "$path")
    # parameters are benchmark dependent
    if [ "$name" = "quicksort_worstcase" ]; then
	command1=$quicksort_worstcase_rkt_command
	command2=$quicksort_worstcase_clang_command
	command3=$quicksort_worstcase_gambit_command
	schml_arg=$quicksort_worstcase_schml_arg
	working_nsamples=$quicksort_worstcase_nsamples
    elif [ "$name" = "quicksort_bestcase" ]; then
	command1=$quicksort_bestcase_rkt_command
	command2=$quicksort_bestcase_clang_command
	command3=$quicksort_bestcase_gambit_command
	schml_arg=$quicksort_bestcase_schml_arg
	working_nsamples=$quicksort_bestcase_nsamples
    elif [ "$name" = "matmult" ]; then
	command1=$matmult_rkt_command
	command2=$matmult_clang_command
	command3=$matmult_gambit_command
	schml_arg=$matmult_schml_arg
	working_nsamples=$matmult_nsamples
    fi

    logfile1=$datadir/${name}1.log
    logfile2=$datadir/${name}1.csv
    logfile3=$datadir/${name}2.log
    logfile4=$datadir/${name}2.csv

    names[$in]=$(echo $name | tr _ " " | sed -e "s/\b\(.\)/\u\1/g" | tr " " "-")

    cd $tmpdir
    c="$command1 | sed -n 's/cpu time: \\([0-9]*\\) .*/\\1/p;q' | awk -v c="$c1" -v p="$precision" '{printf(\"%.*f\\n\", p,(\$1*c))}'"
    avg "$c"
    baseline=$RETURN

    c="$command2 | $schml_regex"
    avg "$c"
    ct=$RETURN
    cr=$(echo "$ct/$baseline" | bc -l | awk -v p="$precision" '{printf "%.*f\n",p, $0}')
    
    c="$command3 | sed -n '3,3s/ *\\([0-9]*\\).*/\\1/p' | awk -v c="$c1" -v p="$precision"  '{printf(\"%.*f\\n\",p, \$1*c)}'"
    avg "$c"
    gt=$RETURN
    gr=$(echo "$gt/$baseline" | bc -l | awk -v p="$precision" '{printf "%.*f\n",p, $0}')
    
    dynamizer_out=$(dynamizer $path ${working_nsamples} | sed -n 's/.* \([0-9]\+\) .* \([0-9]\+\) .*/\1 \2/p')
    type_constructor_count=$(echo $dynamizer_out | sed -n 's/[0-9]\+.\([0-9]\+\)/\1/p')
    less_precise_count=$(echo $dynamizer_out | sed -n 's/\([0-9]\+\).*/\1/p')
    cd $schmldir
    racket $schmldir/benchmark/benchmark.rkt $path/ $schml_mem_limit2
    
    echo "name,precision,time,slowdown" > $logfile1
    n=0
    for b in $(find $path -name '*.o1'); do
	let n=n+1
	echo $b $n
	binpath="${b%.*}";bname=$(basename "$binpath")
	p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < $binpath.schml)
	c="echo $schml_arg | $b | $schml_regex"
	avg "$c"
	t=$RETURN	
	# echo $t
	f=$(echo "$t/$baseline" | bc -l | awk -v p="$precision" '{printf "%.*f\n", p,$0}')
	echo $f
	printf "%d,%.2f,%.${precision}f,%.2f\n" $bname $p $t $f >> $logfile1
    done

    n=0
    echo "name,precision,time,slowdown" > $logfile3
    for b in $(find $path -name '*.o2'); do
	let n=n+1
	binpath="${b%.*}";bname=$(basename "$binpath")
	echo $b $n
	p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < $binpath.schml)
	c="echo $schml_arg | $b | $schml_regex"
	avg "$c"
	t=$RETURN
	# echo $t
	f=$(echo "$t/$baseline" | bc -l | awk -v p="$precision" '{printf "%.*f\n", p,$0}')
	echo $f
	printf "%d,%.2f,%.${precision}f,%.2f\n" $bname $p $t $f >> $logfile3
    done

    cut -d, -f4 $logfile1 | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > $logfile2
    cut -d, -f4 $logfile3 | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > $logfile4
    
    min_c=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' $logfile2)
    min_tb=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' $logfile4)

    max_c=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' $logfile2)
    max_tb=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' $logfile4)

    read std_c mean_c <<< $( cat $logfile2 | cut -d, -f1 | awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    read std_tb mean_tb <<< $( cat $logfile4 | cut -d, -f1 | awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )

    c11="echo $schml_arg | $tmpdir/static/$name.o1 | $schml_regex"
    avg "$c11"
    t11=$RETURN
    c12="echo $schml_arg | $tmpdir/dyn/$name.o1 | $schml_regex"
    avg "$c12"
    t12=$RETURN
    typed_untyped_ratio_c=$(echo "$t11/$t12" | bc -l | awk -v p="$precision" '{printf "%.2f\n",$0}')

    c21="echo $schml_arg | $tmpdir/static/$name.o2 | $schml_regex"
    avg "$c21"
    t21=$RETURN
    c22="echo $schml_arg | $tmpdir/dyn/$name.o2 | $schml_regex"
    avg "$c22"
    t22=$RETURN
    typed_untyped_ratio_tb=$(echo "$t21/$t22" | bc -l | awk -v p="$precision" '{printf "%.2f\n",$0}')

    cr_t=$(echo $cr | awk '{printf "%.2f\n",$0}');gr_t=$(echo $gr | awk '{printf "%.2f\n",$0}')
    lpc_t=$(echo "$less_precise_count/1000000000" | bc -l | awk '{printf "%.2f\n",$0}')
    
    usability_code="$usability_code;unset tics;unset border;set yrange [0:1];set xrange [0:1];\
     	   LABEL = \"{/Verdana:Bold ${names[$in]}}\nlattice size\n\nClang\nGambit-C\n\ntyped/untyped ratio\nmin. slowdown\nmax. slowdown\nmean slowdown\";\
	   set label $((in+1)) at .8,1 LABEL front right font 'Verdana,13';plot sqrt(-1);unset label $((in+1));\
	   unset tics;unset border;set yrange [0:1];set xrange [0:2.5];\
     	   LABEL = \"(${type_constructor_count} type nodes)\n${lpc_t} B\n\n${cr_t}x\n${gr_t}x\n\n${typed_untyped_ratio_tb}x\t${typed_untyped_ratio_c}x\n${min_tb}x\t${min_c}x\n${max_tb}x\t${max_c}x\n${mean_tb}x\t${mean_c}x\";\
	   set label $((in+1)) at 0,1 LABEL front left font 'Verdana,13';plot sqrt(-1);unset label $((in+1));\
	   set multiplot next;\
    	   set border 15 back; \
    	   set title \"\"; \
    	   set xrange [0:10]; set yrange [0:${n}]; \
    	   set xtics nomirror (\"1x\" 1,\"\" 2,\"\" 3,\"\" 4,\"\" 5, \"6x\" 6,\"\" 7, \"\" 8, \"\" 9, \"10x\" 10, \"15x\" 15, \"20x\" 20); \
    	   set ytics nomirror 0,200; \
	   set arrow from 1,graph(0,0) to 1,graph(1,1) nohead lc rgb \"black\" lw 2; \
	   set rmargin 2;\
    	   plot '$logfile2' using 1:2 with lines lw 2 lc rgb \"red\" title '' smooth cumulative, \
    	   '$logfile4' using 1:2 with lines lw 2 lc rgb \"blue\" title '' smooth cumulative; unset arrow; set multiplot next"

    scatter_code="$scatter_code;set yrange [0:100];\
    	   set title \"${names[$in]}\"; \
	   set xlabel \"slowdown\";\
	   set ylabel \"How much of the code is untyped\";\
    	   plot '$logfile1' using 4:2 with points title 'Coercions', \
    	   '$logfile3' using 4:2 with points title 'Type-based casts'"

    let in=in+1
done

legend="ofs_x = 0.5;\
dx = 40;\
set yrange [0:4];set xrange [0:20];unset tics; unset border;\
set rmargin 0;set lmargin 0;\
set label $((in+1)) at .5,4.5 \"x-axis: slowdown\" front left font 'Verdana,16';\
set label $((in+2)) at 5,4.5 \"y-axis: # configs\" front left font 'Verdana,16';\
set label $((in+3)) at 11.3,4.5 \"Coercions\" front left font 'Verdana,16';set arrow from 14,4.5 to 15,4.5 nohead lc rgb \"red\";\
set label $((in+4)) at 15.5,4.5 \"Type-based\" front left font 'Verdana,16';set arrow from 18.5,4.5 to 19.5,4.5 nohead lc rgb \"blue\";\
plot sqrt(-1);"

gnuplot -e "w = 300 * ($in+1);set datafile separator \",\"; set terminal pngcairo size 880,w "`
      	    `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '$outdir/usability.png'; "`
	   `"x = $in + 1;set multiplot layout x,5 margins 0.15,.95,0.1,0.98 spacing 0,0.08; unset key $usability_code"`
	   `";$legend"

let h=($in+1)*300-270
convert $outdir/usability.png -crop 880x$h new.png
rm new-1.png;mv new-0.png $outdir/usability.png


gnuplot -e "w = 400 * $in;set datafile separator \",\"; set terminal pngcairo size 500,w "`
      	    `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '$outdir/scatter.png'; "`
	   `"set multiplot layout $in,1 $scatter_code"
