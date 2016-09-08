#!/bin/sh
set -euo pipefail

date="2016_08_24_11_49_42"
type_constructor_count=41
less_precise_count=31104000000

loops=1
precision=6
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

# --------------------------------------------------------------------
# preparing the quicksort worstcase benchmark
benchmark=quicksort_worstcase

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

quicksort_bestcase_schml_arg=$f

# ---------------------------------
# preparing the matmult benchmark

benchmark=matmult

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

avg()
{
    local avg_sum=0
    local avg_i
    # echo $1
    for avg_i in `seq $loops`; do
	avg_t=$(eval $1)
	avg_sum=$(echo "scale=$precision;$avg_sum+$avg_t" | bc)
    done
    RETURN=$(echo "scale=$precision;$avg_sum/$loops" | bc)
    # echo $RETURN
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

    cut -d, -f4 $logfile1 | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > $logfile2
    cut -d, -f4 $logfile3 | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > $logfile4
    
    min_c=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' $logfile2)
    min_tb=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' $logfile4)

    max_c=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' $logfile2)
    max_tb=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' $logfile4)

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
typed/untyped ratio         & ${typed_untyped_ratio_tb}x             & ${typed_untyped_ratio_tb}x            \\\ \hline
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


# gnuplot -e "w = 400 * $in;set datafile separator \",\"; set terminal pngcairo size 500,w "`
#       	    `"enhanced color font 'Verdana,10' ;"`
#     	   `"set output '$outdir/scatter.png'; "`
# 	   `"set multiplot layout $in,1 $scatter_code"

#---------------------------------------------------------------------

#     plot_code="$plot_code;set key off;unset tics; unset border;set yrange [0:5];set xrange [0:5];\
#      	   LABEL = \"${names[$in]}\nlattice size\n\ntyped/untyped ratio (coercions)\ntyped/untyped ratio (type-based)\n\nmin overhead (coercions)\nmin overhead (type-based)\n\nmax overhead (coercions)\nmax overhead (type-based)\n\nmean overhead (coercions)\nmean overhead (type-based)\";\
# 	   set label $((in+1)) at 3,3 LABEL front right;set size .3,1;plot sqrt(-1);unset label $((in+1));\
# 	   set key off;unset tics; unset border;set yrange [0:5];set xrange [0:5];\
#      	   LABEL = \"(${type_constructor_count} type nodes)\n${less_precise_count}\n\n${typed_untyped_ratio_c}x\n${typed_untyped_ratio_tb}x\n\n${min_c}x\n${min_tb}x\n\n${max_c}x\n${max_tb}x\n\n${mean_c}x\n${mean_tb}x\";\
# 	   set label $((in+1)) at 3,3 LABEL front right;set size .3,1;plot sqrt(-1);unset label $((in+1));\
#     	   set border 15 back; \
#     	   set title \"\"; \
#     	   set xrange [0:10]; set yrange [0:${n}]; \
#     	   set xtics nomirror (\"1x\" 1, \"6x\" 6, \"10x\" 10, \"15x\" 15, \"20x\" 20); \
#     	   set ytics nomirror 0,200; \
# 	   set arrow from $cr,graph(0,0) to $cr,graph(1,1) nohead lc rgb \"cyan\" lw 2; \
# 	   set arrow from $gr,graph(0,0) to $gr,graph(1,1) nohead lc rgb \"orange\" lw 2; \
# 	   set arrow from 1,graph(0,0) to 1,graph(1,1) nohead lc rgb \"black\" lw 2; \
# 	   set size 1,.6;\
#     	   plot '$logfile2' using 1:2 with lines lw 3 lc rgb \"red\" title '' smooth cumulative, \
#     	   '$logfile4' using 1:2 with lines lw 3 lc rgb \"blue\" title '' smooth cumulative; unset arrow"

#     let in=in+1
# done

# gnuplot -e "set datafile separator \",\"; set term tikz standalone color font '\small\sf';"`
#     	   `"set output '$outdir/results.tex'; "`
# 	   `"set multiplot layout $in, 3; set tmargin 5 $plot_code"


# cp $miscdir/* $tmpdir
# cp $outdir/results.tex $tmpdir
# cd $tmpdir
# lualatex --interaction=nonstopmode results.tex
# mv results.pdf $outdir
