#!/bin/sh

# This script needs the following list of programs to be installed:
# dynamizer
# clang
# gambit-c
# racket
# imagemagick
# gnuplot

# It takes the following arguments:
# number of samples
# number of times each program will run

set -euo pipefail

loops=${2:-5}
precision=5
grift_mem_limit1=999999
grift_mem_limit2=9999999999

#---------------------------------------------------------------------
# Obselete params
deliverable=3
usable=10
#---------------------------------------------------------------------
# quicksort worstcase parameters
quicksort_worstcase_array_len=1000
quicksort_worstcase_nsamples=${1:-1000}

# quicksort bestcase parameters
quicksort_bestcase_array_len=1000
quicksort_bestcase_nsamples=${1:-1000}

# matrix multiplication parameters
matmult_mat_side_len=200
matmult_nsamples=${1:-1000}

# --------------------------------------------------------------------
date=`date +%Y_%m_%d_%H_%M_%S`

griftdir=$GRIFT_DIR
testdir=$griftdir/benchmark/suite/macro
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

grift_regex="sed -n 's/.*): \\([0-9]\\+\\)/\\1/p'"

# common lines in the plots
# LIN="set arrow from 1,$nx to 20,$nx nohead lt 3 dashtype 2 lc rgb \"brown\" lw 2;\
    #      set arrow from 10,graph(0,0) to 10,graph(1,1) nohead lc rgb \"orange\" lw 2; \
    #      set arrow from 3,graph(0,0) to 3,graph(1,1) nohead lc rgb \"green\" lw 2"

# --------------------------------------------------------------------
# compiling and preparing running commands

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir/cumperflattice
mkdir -p $outdir/perflattice

# copying the benchmarks to a temporary directory
cp -r $srcdir/* $tmpdir

printf "Date\t\t:%s\n" "$date" >> $paramfile
MYEMAIL="`id -un`@`hostname -f`"
printf "Machine\t\t:%s\n" "$MYEMAIL" >> $paramfile
grift_ver=$(git rev-parse HEAD)
printf "Grift ver.\t:%s\n" "$grift_ver" >> $paramfile
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

quicksort_worstcase_grift_arg=$quicksort_worstcase_array_len

# ---------------------------------
# preparing the quicksort bestcase benchmark
benchmark=quicksort_bestcase

printf "Benchmark\t:%s\n" "$benchmark" >> $paramfile
printf "Arg_array_len\t:%s\n" "$quicksort_bestcase_array_len" >> $paramfile
printf "Arg_nsamples\t:%s\n" "$quicksort_bestcase_nsamples" >> $paramfile

f=""
s=""
n=$(($quicksort_bestcase_array_len-1))
for i in `seq 0 $n`; do
    # x=$(perl -e 'print int rand $quicksort_bestcase_array_len, "\n"; ')
    x=$(( ( RANDOM % $quicksort_bestcase_array_len )  + 1 ))
    f="$f\n$x"
    s="$s $x"
done

echo -e $f > $tmpdir/nums
f="$quicksort_bestcase_array_len $s"

benchmark_rkt="$tmpdir/rkt/$benchmark"
raco make $benchmark_rkt.rkt
quicksort_bestcase_rkt_command="racket $benchmark_rkt.rkt $quicksort_bestcase_array_len"

benchmark_clang="$tmpdir/c/$benchmark"
clang $benchmark_clang.c -O3 -o $benchmark_clang.o
quicksort_bestcase_clang_command="$benchmark_clang.o $quicksort_bestcase_array_len"

benchmark_gambit="$tmpdir/gambit/$benchmark"
gsc -exe -cc-options -O3 $benchmark_gambit.scm
quicksort_bestcase_gambit_command="$benchmark_gambit $quicksort_bestcase_array_len"

quicksort_bestcase_grift_arg=$f

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

matmult_grift_arg=$matmult_mat_side_len
#---------------------------------------------------------------------

cd $griftdir
racket $griftdir/benchmark/benchmark.rkt $tmpdir/static/ $grift_mem_limit1
racket $griftdir/benchmark/benchmark.rkt $tmpdir/dyn/ $grift_mem_limit2

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

in=0
for f in $tmpdir/static/*.grift; do
    path="${f%.*}";name=$(basename "$path")
    # parameters are benchmark dependent
    if [ "$name" = "quicksort_worstcase" ]; then
	command1=$quicksort_worstcase_rkt_command
	command2=$quicksort_worstcase_clang_command
	command3=$quicksort_worstcase_gambit_command
	grift_arg=$quicksort_worstcase_grift_arg
	working_nsamples=$quicksort_worstcase_nsamples
    elif [ "$name" = "quicksort_bestcase" ]; then
	command1=$quicksort_bestcase_rkt_command
	command2=$quicksort_bestcase_clang_command
	command3=$quicksort_bestcase_gambit_command
	grift_arg=$quicksort_bestcase_grift_arg
	working_nsamples=$quicksort_bestcase_nsamples
    elif [ "$name" = "matmult" ]; then
	command1=$matmult_rkt_command
	command2=$matmult_clang_command
	command3=$matmult_gambit_command
	grift_arg=$matmult_grift_arg
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

    c="$command2 | $grift_regex"
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
    cd $griftdir
    racket $griftdir/benchmark/benchmark.rkt $path/ $grift_mem_limit2
    
    echo "name,precision,time,slowdown" > $logfile1
    n=0
    for b in $(find $path -name '*.o1'); do
	let n=n+1
	echo $b $n
	binpath="${b%.*}";bname=$(basename "$binpath")
	p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < $binpath.grift)
	c="echo $grift_arg | $b | $grift_regex"
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
	p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < $binpath.grift)
	c="echo $grift_arg | $b | $grift_regex"
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

    c11="echo $grift_arg | $tmpdir/static/$name.o1 | $grift_regex"
    avg "$c11"
    t11=$RETURN
    c12="echo $grift_arg | $tmpdir/dyn/$name.o1 | $grift_regex"
    avg "$c12"
    t12=$RETURN
    typed_untyped_ratio_c=$(echo "$t11/$t12" | bc -l | awk -v p="$precision" '{printf "%.2f\n",$0}')

    c21="echo $grift_arg | $tmpdir/static/$name.o2 | $grift_regex"
    avg "$c21"
    t21=$RETURN
    c22="echo $grift_arg | $tmpdir/dyn/$name.o2 | $grift_regex"
    avg "$c22"
    t22=$RETURN
    typed_untyped_ratio_tb=$(echo "$t21/$t22" | bc -l | awk -v p="$precision" '{printf "%.2f\n",$0}')

    cr_t=$(echo $cr | awk '{printf "%.2f\n",$0}');gr_t=$(echo $gr | awk '{printf "%.2f\n",$0}')
    lpc_t=$(echo "$less_precise_count/1000000000" | bc -l | awk '{printf "%.2f\n",$0}')
    
    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '$outdir/cumperflattice/${names[$in]}.png';"`
	   `"set border 15 back;"`
      	   `"set title \"${names[$in]}\";"`
    	   `"set xrange [0:10]; set yrange [0:${n}];"`
    	   `"set xtics nomirror (\"1x\" 1,\"2x\" 2,\"3x\" 3,\"4x\" 4,\"5x\" 5, \"6x\" 6,\"7x\" 7, \"8x\" 8, \"9x\" 9, \"10x\" 10, \"15x\" 15, \"20x\" 20);"`
    	   `"set ytics nomirror 0,200;"`
	   `"set arrow from 1,graph(0,0) to 1,graph(1,1) nohead lc rgb \"black\" lw 2;"`
    	   `"plot '$logfile2' using 1:2 with lines lw 2 title 'Coercions' smooth cumulative,"`
    	   `"'$logfile4' using 1:2 with lines lw 2 title 'Type-based casts' smooth cumulative"

    echo "\begin{tabular}{|l|l|l|}
\hline
\textbf{${names[$in]}} & \multicolumn{2}{l|}{(${type_constructor_count} type nodes)} \\\ \hline
lattice size                & \multicolumn{2}{l|}{${lpc_t} B}         \\\ \hline
\multicolumn{3}{|l|}{}                                             \\\ \hline
Clang                       & \multicolumn{2}{l|}{${cr_t}x}           \\\ \hline
Gambit-C                    & \multicolumn{2}{l|}{${gr_t}x}           \\\ \hline
\multicolumn{3}{|l|}{}                                             \\\ \hline
typed/untyped ratio         & ${typed_untyped_ratio_tb}x             & ${typed_untyped_ratio_c}x            \\\ \hline
min. slowdown               & ${min_tb}x             & ${min_c}x            \\\ \hline
max. slowdown               & ${max_tb}x             & ${max_c}x            \\\ \hline
mean slowdown               & ${mean_tb}x             & ${mean_c}x            \\\ \hline
\end{tabular}
\end{table}" > $outdir/cumperflattice/${names[$in]}.tex

    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '$outdir/perflattice/${names[$in]}.png';"`
    	   `"set yrange [0:100];"`
    	   `"set title \"${names[$in]}\";"`
	   `"set xlabel \"slowdown\";"`
	   `"set ylabel \"How much of the code is typed\";"`
    	   `"plot '$logfile1' using 4:(100-\$2) with points title 'Coercions',"`
    	   `"'$logfile3' using 4:(100-\$2) with points title 'Type-based casts'"

    let in=in+1
done
