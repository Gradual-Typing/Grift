#!/bin/sh

# This script needs the following list of programs to be installed:
# dynamizer
# $CC
# gambit-c
# racket
# gnuplot

set -euo pipefail

declare -r PRECISION=5
declare -r CREGEX="sed -n 's/.*): \\([0-9]\\+\\)/\\1/p'"
declare -r DATE=`date +%Y_%m_%d_%H_%M_%S`
declare -r TEST_DIR="$SCHML_DIR/benchmark/suite/macro"
declare -r LATTICE_DIR="$TEST_DIR/lattice/$DATE"
declare -r DATA_DIR="$LATTICE_DIR/data"
declare -r OUT_DIR="$LATTICE_DIR/output"
declare -r TMP_DIR="$LATTICE_DIR/tmp"
declare -r SRC_DIR="$TEST_DIR/src"
declare -r PARAMS_LOG="$LATTICE_DIR/params.txt"
declare -r LIBGC="$SCHML_DIR/src/backend-c/runtime/boehm-gc-install/lib/libgc.a"
declare -r LIBS="$LIBGC -pthread -lm"
TIMEFORMAT=%R

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $RETURN - the runtime for the racket benchmark
get_racket_runtime()
{
    local benchmark="$1"; shift
    local benchmark_path="${TMP_DIR}/rkt/${benchmark}"
    local cache_file="${benchmark_path}.cache"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	local benchmark_args="$1"; shift
	# scaling racket timing from milliseconds to seconds
	# local c1=$(echo "1/1000" | bc -l)
	raco make "${benchmark_path}.rkt"
	# | sed -n 's/cpu time: \\([0-9]*\\) .*/\\1/p;q' | awk -v c="$c1" -v p="$PRECISION" '{printf(\"%.*f\\n\", p,(\$1*c))}'
	avg "racket ${benchmark_path}.rkt" "$benchmark_args"
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $RETURN - the slowdown factor for the C benchmark
get_c_slowdown()
{
    local benchmark="$1"; shift
    local benchmark_path="${TMP_DIR}/c/${benchmark}"
    local cache_file="${benchmark_path}.cache"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	local benchmark_args="$1"; shift
	get_racket_runtime "$benchmark" "$benchmark_args"
	local baseline="$RETURN";
	$CC "${benchmark_path}.c" ${LIBS} "-I${SCHML_DIR}/src/backend-c/runtime/boehm-gc-install/include" -O3 -o "${benchmark_path}.o"
	# ${CREGEX}
	avg "${benchmark_path}.o" "$benchmark_args"
	local ct="$RETURN"
	local cr=$(echo "${ct}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n",p, $0}')
	RETURN=$(echo "$cr" | awk '{printf "%.2f\n",$0}')
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $RETURN - the slowdown factor for the C benchmark
get_gambit_slowdown()
{
    local benchmark="$1"; shift
    local benchmark_path="${TMP_DIR}/gambit/${benchmark}"
    local cache_file="${benchmark_path}.cache"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	local benchmark_args="$1"; shift
	get_racket_runtime "$benchmark" "$benchmark_args"
	local baseline="$RETURN";
	gsc -exe -cc-options -O3 "${benchmark_path}.scm"
	# scaling gambit timing from milliseconds to seconds
	local c1=$(echo "1/1000" | bc -l)
	# | sed -n '3,3s/ *\\([0-9]*\\).*/\\1/p' | awk -v c="$c1" -v p="$PRECISION"  '{printf(\"%.*f\\n\",p, \$1*c)}'
	avg "${benchmark_path}" "$benchmark_args"
	local ct="$RETURN"
	local cr=$(echo "${ct}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n",p, $0}')
	RETURN=$(echo "$cr" | awk '{printf "%.2f\n",$0}')
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - binary to run
# $2 - arguments
# $RETURN - average runtime
avg()
{
    local bin="$1"; shift
    local arg="$1"; shift
    local c="time ( echo ${arg} | ${bin} ) 2>&1 1>/dev/null"
    local avg_sum=0
    local avg_i avg_t
    for avg_i in `seq $LOOPS`; do
	avg_t=$(eval "$c")
	avg_sum=$(echo "scale=${PRECISION};${avg_sum}+${avg_t}" | bc)
    done
    RETURN=$(echo "scale=${PRECISION};${avg_sum}/${LOOPS}" | bc)
}

# $1 - logfile index
# $2 - $path
# $3 - space-separated benchmark arguments
# $4 - aux name
# $RETURN - number of configurations
run_config()
{
    # arguments
    local i="$1";        shift
    local path="$1";     shift
    local args="$1";     shift
    local aux_name="$1"; shift

    
    local disk_aux_name="" print_aux_name=""
    if [[ ! -z "${aux_name}" ]]; then
	disk_aux_name="_${aux_name}"
	print_aux_name=" (${aux_name})"
    fi
    
    local name=$(basename "$path")
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${i}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${i}.csv"
    local n=0 b

    get_racket_runtime "$name" "$args"
    local baseline="$RETURN"
    
    echo "name,precision,time,slowdown" > "$logfile1"
    local b
    for b in $(find "$path" -name "*.o$i"); do
	let n=n+1
	local binpath="${b%.*}"
	local bname=$(basename "$binpath")
	local p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < "${binpath}.schml")
	# $CREGEX
	avg "$b" "$args"
	local t="$RETURN"
	local f=$(echo "${t}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n", p,$0}')
	echo $n $b $f
	printf "%d,%.2f,%.${PRECISION}f,%.2f\n" $bname $p $t $f >> $logfile1
    done
    cut -d, -f4 "$logfile1" | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > "$logfile2"
    RETURN="$n"
}

# $1 - config index
# $2 - $name
# $3 - space-separated benchmark arguments
# RETURN - the ratio between the runtimes of the typed and untyped versions of a benchmark
compute_typed_untyped_ratio()
{
    # arguments
    local i="$1";              shift
    local name="$1";           shift
    
    local cache_file="$TMP_DIR/${name}_typed_untyped_ratio${i}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	local benchmark_args="$1"; shift
	# ${CREGEX}
	avg "${TMP_DIR}/static/./${name}.o${i}" "$benchmark_args"
	t11="$RETURN"
	avg "${TMP_DIR}/dyn/./${name}.o${i}" "$benchmark_args"
	t12="$RETURN"
	RETURN=$(echo "${t11}/${t12}" | bc -l | awk -v p="$PRECISION" '{printf "%.2f\n",$0}')
        echo "$RETURN" > $cache_file
    fi
}

# $1 - first config index
# $2 - second config index
# $3 - $path
# $4 - space-separated benchmark arguments
# $5 - $lpc_t
# $6 - number of type nodes
# $7 - printed name of the benchmark
# $8 - description of config 1
# $9 - description of config 2
# $10 - aux name
gen_output()
{
    # arguments
    local c1="$1";                     shift
    local c2="$1";                     shift
    local path="$1";                   shift
    local benchmark_args="$1";         shift
    local lpc_t="$1";                  shift
    local type_constructor_count="$1"; shift
    local printname="$1";              shift
    local c1d="$1";                    shift
    local c2d="$1";                    shift
    local aux_name="$1";               shift

    local disk_aux_name="" print_aux_name=""
    if [[ ! -z "${aux_name}" ]]; then
	disk_aux_name="_${aux_name}"
	print_aux_name=" (${aux_name})"
    fi

    local name=$(basename "$path")
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${c1}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${c1}.csv"
    local logfile3="${DATA_DIR}/${name}${disk_aux_name}${c2}.log"
    local logfile4="${DATA_DIR}/${name}${disk_aux_name}${c2}.csv"

    # run systems other than Schml
    get_c_slowdown "$name" "$benchmark_args"
    local cr_t="$RETURN"
    get_gambit_slowdown "$name" "$benchmark_args"
    local gr_t="$RETURN"

    compute_typed_untyped_ratio "$c1" "$name" "$benchmark_args"
    local typed_untyped_ratio1="$RETURN"
    compute_typed_untyped_ratio "$c2" "$name" "$benchmark_args"
    local typed_untyped_ratio2="$RETURN"

    # run Schml configurations
    run_config "$c1" "$path" "$benchmark_args" "$aux_name"
    run_config "$c2" "$path" "$benchmark_args" "$aux_name"
    local n="$RETURN"
    
    local min1=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' "$logfile2")
    local min2=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' "$logfile4")
    local max1=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' "$logfile2")
    local max2=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' "$logfile4")
    local std1 std2 mean1 mean2
    read std1 mean1 <<< $( cat "$logfile2" | cut -d, -f1 | awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    read std2 mean2 <<< $( cat "$logfile4" | cut -d, -f1 | awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )

    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '${OUT_DIR}/cumperflattice/${name}${disk_aux_name}_${c1}_${c2}.png';"`
	   `"set border 15 back;"`
      	   `"set title \"${printname}${print_aux_name}\";"`
    	   `"set xrange [0:10]; set yrange [0:${n}];"`
    	   `"set xtics nomirror (\"1x\" 1,\"2x\" 2,\"3x\" 3,\"4x\" 4,\"5x\" 5, \"6x\" 6,\"7x\" 7, \"8x\" 8, \"9x\" 9, \"10x\" 10, \"15x\" 15, \"20x\" 20);"`
    	   `"set ytics nomirror 0,200;"`
	   `"set arrow from 1,graph(0,0) to 1,graph(1,1) nohead lc rgb \"black\" lw 2;"`
    	   `"plot '${logfile2}' using 1:2 with lines lw 2 title 'Coercions' smooth cumulative,"`
    	   `"'${logfile4}' using 1:2 with lines lw 2 title 'Type-based casts' smooth cumulative"

    echo "\begin{tabular}{|l|l|l|}
\hline
\textbf{${printname}${aux_name}} & \multicolumn{2}{l|}{(${type_constructor_count} type nodes)} \\\ \hline
lattice size                & \multicolumn{2}{l|}{${lpc_t} B}         \\\ \hline
\multicolumn{3}{|l|}{}                                             \\\ \hline
Clang                       & \multicolumn{2}{l|}{${cr_t}x}           \\\ \hline
Gambit-C                    & \multicolumn{2}{l|}{${gr_t}x}           \\\ \hline
\multicolumn{3}{|l|}{}                                             \\\ \hline
typed/untyped ratio         & ${typed_untyped_ratio2}x             & ${typed_untyped_ratio1}x            \\\ \hline
min. slowdown               & ${min2}x             & ${min1}x            \\\ \hline
max. slowdown               & ${max2}x             & ${max1}x            \\\ \hline
mean slowdown               & ${mean2}x             & ${mean1}x            \\\ \hline
\end{tabular}
\end{table}" > "${OUT_DIR}/cumperflattice/${name}${disk_aux_name}_${c1}_${c2}.tex"

    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '${OUT_DIR}/perflattice/${name}${disk_aux_name}_${c1}_${c2}.png';"`
    	   `"set yrange [0:100];"`
    	   `"set title \"${printname}${print_aux_name}\";"`
	   `"set xlabel \"slowdown\";"`
	   `"set ylabel \"How much of the code is typed\";"`
    	   `"plot '${logfile1}' using 4:(100-\$2) with points title '${c1d}',"`
    	   `"'${logfile3}' using 4:(100-\$2) with points title '${c2d}'"
}

# $1 - first config index
# $2 - second config index
# $3 - benchmark filename without extension
# $4 - space-separated benchmark arguments
# $5 - nsamples
# $6 - input name
run_benchmark()
{
    local c1="$1";             shift
    local c2="$1";             shift
    local name="$1";           shift
    local benchmark_args="$1"; shift
    local nsamples="$1";       shift
    local aux_name="$1";       shift

    local lattice_path="${TMP_DIR}/static/${name}"
    local benchmark_file="${lattice_path}.schml"
    local printname=$(echo "$name" | tr _ " " | sed -e "s/\b\(.\)/\u\1/g" | tr " " "-")
    
    printf "Benchmark\t:%s\n" "$name" >> "$PARAMS_LOG"
    printf "Args\t:%s\n" "$benchmark_args" >> "$PARAMS_LOG"

    local lattice_file="${lattice_path}/out"
    if [ ! -f $lattice_file ]; then
	rm -rf "$lattice_path"
	# generate and compile sampled lattice
	local dynamizer_out=$(dynamizer "$lattice_path" "$nsamples" | sed -n 's/.* \([0-9]\+\) .* \([0-9]\+\) .*/\1 \2/p')
	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${lattice_path}/"
	echo "$dynamizer_out" > $lattice_file
    fi
    dynamizer_out=$(cat "$lattice_file")
    local type_constructor_count=$(echo "$dynamizer_out" | sed -n 's/[0-9]\+.\([0-9]\+\)/\1/p')
    local less_precise_count=$(echo "$dynamizer_out" | sed -n 's/\([0-9]\+\).*/\1/p')
    local lpc_t=$(echo "${less_precise_count}/1000000000" | bc -l | awk '{printf "%.2f\n",$0}')

    gen_output $c1 $c2 "$lattice_path" "$benchmark_args" "$lpc_t" "$type_constructor_count" "$printname" "Coercions" "Type-Based" "$aux_name"
}

# $1 - first config index
# $2 - second config index
# $3 - nsamples
run_experiment()
{
    local c1="$1";       shift
    local c2="$1";       shift
    local nsamples="$1"; shift

    local qs_bc_arg="1000"
    local s=""
    local n=$(($qs_bc_arg-1)) i
    for i in `seq 0 $n`; do
	x=$(( ( RANDOM % $qs_bc_arg )  + 1 ))
	s="$s$x "
    done
    qs_bc_arg="\"$qs_bc_arg $s\""
    run_benchmark $c1 $c2 "quicksort" "$qs_bc_arg" "$nsamples" "bestcase"
    
    local qs_wc_arg="1000"
    local s=""
    local n=$(($qs_wc_arg-1)) i
    for i in `seq 0 $n`; do
	x=$(( n-i ))
	s="$s$x "
    done
    qs_wc_arg="\"$qs_wc_arg $s\""
    run_benchmark $c1 $c2 "quicksort" "$qs_wc_arg" "$nsamples" "worstcase"
    
    run_benchmark $c1 $c2 "matmult" "200" "$nsamples" ""

    run_benchmark $c1 $c2 "n-body" "1000000" "$nsamples" ""
    echo "finished experiment comparing" $c1 "vs" $c2
}

# $1 - number of samples
# $2 - number of times each program will run
main()
{
    USAGE="Usage: $0 nsamples loops n_1,n_2 ... n_n"
    if [ "$#" == "0" ]; then
	echo "$USAGE"
	exit 1
    fi
    local nsamples="$1"; shift
    LOOPS="$1";          shift
    
    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$TMP_DIR"
    mkdir -p "$OUT_DIR/cumperflattice"
    mkdir -p "$OUT_DIR/perflattice"
    # copying the benchmarks to a temporary directory
    cp -r ${SRC_DIR}/* $TMP_DIR

    # logging
    printf "Date\t\t:%s\n" "$DATE" >> "$PARAMS_LOG"
    MYEMAIL="`id -un`@`hostname -f`"
    printf "Machine\t\t:%s\n" "$MYEMAIL" >> "$PARAMS_LOG"
    schml_ver=$(git rev-parse HEAD)
    printf "Schml ver.\t:%s\n" "$schml_ver" >> "$PARAMS_LOG"
    clang_ver=$(clang --version | sed -n 's/clang version \([0-9]*.[0-9]*.[0-9]*\) .*/\1/p;q')
    printf "Clang ver.\t:%s\n" "$clang_ver" >> "$PARAMS_LOG"
    gambit_ver=$(gsc -v | sed -n 's/v\([0-9]*.[0-9]*.[0-9]*\) .*/\1/p;q')
    printf "Gambit ver.\t:%s\n" "$gambit_ver" >> "$PARAMS_LOG"
    racket_ver=$(racket -v | sed -n 's/.* v\([0-9]*.[0-9]*\).*/\1/p;q')
    printf "Racket ver.\t:%s\n" "$racket_ver" >> "$PARAMS_LOG"
    printf "loops:\t:%s\n" "$LOOPS" >> "$PARAMS_LOG"
    printf "nsamples\t:%s\n" "$nsamples" >> "$PARAMS_LOG"

    cd "$SCHML_DIR"
    racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${TMP_DIR}/static/"
    racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${TMP_DIR}/dyn/"

    local i j
    if [ "$#" == "1" ]; then
	local config="$1";   shift
	for i in `seq ${config}`; do
	    for j in `seq ${i} ${config}`; do
		if [ ! $i -eq $j ]; then
		    run_experiment $i $j $nsamples
		fi
	    done
	done
    else
	while (( "$#" )); do
	    i=$1; shift
	    j=$1; shift
	    run_experiment $i $j $nsamples
	done
    fi
    echo "done."
}

main "$@"
