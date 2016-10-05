#!/bin/sh
set -euo pipefail

loops=20
precision=4
schml_mem_limit1=999999
schml_mem_limit2=999999
schml_mem_limit3=999999
# --------------------------------------------------------------------
# quicksort worstcase parameters
quicksort_worstcase_array_len=10000

# quicksort bestcase parameters
quicksort_bestcase_array_len=10000

# matrix multiplication parameters
matmult_mat_side_len=300

# --------------------------------------------------------------------
date=`date +%Y_%m_%d_%H_%M_%S`

schmldir=$SCHML_DIR
testdir=$schmldir/benchmark/suite/macro
extremesdir=$testdir/extremes/$date
datadir=$extremesdir/data
outdir=$extremesdir/output
tmpdir=$extremesdir/tmp
miscdir=$testdir/misc
srcdir=$testdir/src
paramfile=$extremesdir/params.txt
TIMEFORMAT=%R
#---------------------------------------------------------------------

# scaling racket and gambit timing from milliseconds to seconds
c1=$(echo "1/1000" | bc -l)

schml_regex="sed -n 's/.*): \\([0-9]\\+\\)/\\1/p'"

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


# ---------------------------------
# preparing the quicksort worstcase benchmark
benchmark=quicksort_worstcase

printf "Benchmark\t:%s\n" "$benchmark" >> $paramfile
printf "Arg_array_len\t:%s\n" "$quicksort_worstcase_array_len" >> $paramfile

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
echo "s/INITIALIZE-VECTOR/$s/" | sed -f - -i  $tmpdir/partial/$benchmark.schml
echo "s/INITIALIZE-VECTOR/$s/" | sed -f - -i  $tmpdir/dyn/$benchmark.schml

# ---------------------------------
# preparing the matmult benchmark
benchmark=matmult

printf "Benchmark\t:%s\n" "$benchmark" >> $paramfile
printf "Mat_side_len\t:%s\n" "$matmult_mat_side_len" >> $paramfile

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

# compiling all Schml source files
cd $schmldir
racket $schmldir/benchmark/benchmark.rkt $tmpdir/static/ $schml_mem_limit1
racket $schmldir/benchmark/benchmark.rkt $tmpdir/partial/ $schml_mem_limit2
racket $schmldir/benchmark/benchmark.rkt $tmpdir/dyn/ $schml_mem_limit3

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

function join { local d=$1; shift; echo -n "$1"; shift; printf "%s" "${@/#/$d}"; }

in=0
for f in $tmpdir/static/*.schml; do
    path="${f%.*}";name=$(basename "$path")
    # parameters are benchmark dependent
    if [ "$name" = "quicksort_worstcase" ]; then
	command1=$quicksort_worstcase_rkt_command
	command2=$quicksort_worstcase_clang_command
	command3=$quicksort_worstcase_gambit_command
	schml_arg=$quicksort_worstcase_schml_arg
    elif [ "$name" = "quicksort_bestcase" ]; then
	command1=$quicksort_bestcase_rkt_command
	command2=$quicksort_bestcase_clang_command
	command3=$quicksort_bestcase_gambit_command
	schml_arg=$quicksort_bestcase_schml_arg
    elif [ "$name" = "matmult" ]; then
	command1=$matmult_rkt_command
	command2=$matmult_clang_command
	command3=$matmult_gambit_command
	schml_arg=$matmult_schml_arg
    fi
    
    path="${f%.*}";name=$(basename "$path")
    logfile1=$datadir/${name}1.log
    logfile2=$datadir/${name}1.csv
    logfile3=$datadir/${name}2.log
    logfile4=$datadir/${name}2.csv


    names[$in]=$(echo $name | tr _ " " | sed -e "s/\b\(.\)/\u\1/g" | tr " " "-")

    cd $tmpdir
    c="$command1 | sed -n 's/cpu time: \\([0-9]*\\) .*/\\1/p;q' | awk -v c="$c1" -v p="$precision" '{printf(\"%.*f\\n\", p,(\$1*c))}'"
    avg "$c"
    rkt[$in]=$RETURN

    c="$command2 | $schml_regex"
    avg "$c"
    clang[$in]=$RETURN
    
    c="$command3 | sed -n '3,3s/ *\\([0-9]*\\).*/\\1/p' | awk -v c="$c1" -v p="$precision"  '{printf(\"%.*f\\n\",p, \$1*c)}'"
    avg "$c"
    gambit[$in]=$RETURN
    
    c="echo $schml_arg | $tmpdir/static/$name.o1 | $schml_regex"
    avg "$c"
    sc1[$in]=$RETURN

    c="echo $schml_arg | $tmpdir/partial/$name.o1 | $schml_regex"
    avg "$c"
    sc2[$in]=$RETURN

    c="echo $schml_arg | $tmpdir/dyn/$name.o1 | $schml_regex"
    avg "$c"
    sc3[$in]=$RETURN
    
    c="echo $schml_arg | $tmpdir/static/$name.o2 | $schml_regex"
    avg "$c"
    stb1[$in]=$RETURN

    c="echo $schml_arg | $tmpdir/partial/$name.o2 | $schml_regex"
    avg "$c"
    stb2[$in]=$RETURN
    
    c="echo $schml_arg | $tmpdir/dyn/$name.o2 | $schml_regex"
    avg "$c"
    stb3[$in]=$RETURN
    
    echo "finished " ${names[$in]}
    let in=in+1
done

header=$(printf '|l%.0s' eval {1..$in})

namesx=$(join \& ${names[@]})
clangx=$(join \& ${clang[@]})
stb1x=$(join \& ${stb1[@]})
sc1x=$(join \& ${sc1[@]})
stb2x=$(join \& ${stb2[@]})
sc2x=$(join \& ${sc2[@]})
stb3x=$(join \& ${stb3[@]})
sc3x=$(join \& ${sc3[@]})
rktx=$(join \& ${rkt[@]})
gambitx=$(join \& ${gambit[@]})
i1=$((in+1))

echo "\begin{tabular}{|l$header|}
\hline
                   & \multicolumn{$in}{c|}{Time(s)} \\\ \hline
                   & $namesx    \\\ \hline
\multicolumn{$i1}{|c|}{Statically typed}                         \\\ \hline
Clang C            & $clangx       \\\ \hline
Schml              & $stb1x        \\\ \hline
Schml (coercions)  & $sc1x        \\\ \hline
\multicolumn{$i1}{|c|}{Partially typed}                          \\\ \hline
Schml              & $stb2x         \\\ \hline
Schml (coercions)  & $sc2x         \\\ \hline
\multicolumn{$i1}{|c|}{Dynamically typed}                        \\\ \hline
Gambit Scheme      & $gambitx        \\\ \hline
Racket             & $rktx        \\\ \hline
Schml              & $stb3x         \\\ \hline
Schml (coercions)  & $sc3x         \\\ \hline
\end{tabular}" > $outdir/extremes.tex
