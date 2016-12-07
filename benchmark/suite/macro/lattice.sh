#!/bin/sh

# This script needs the following list of programs to be installed:
# dynamizer
# clang
# gambit-c
# racket
# gnuplot

set -euo pipefail
declare -r PRECISION=5
TIMEFORMAT=%R

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_racket_runtime()
{
    local benchmark="$1"; shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1"; shift
    
    local benchmark_path="${TMP_DIR}/rkt/${benchmark}"
    local cache_file="${benchmark_path}${disk_aux_name}.cache"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	raco make "${benchmark_path}.rkt"
	avg "racket ${benchmark_path}.rkt" "$benchmark_args"
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the slowdown factor for the C benchmark
get_c_slowdown()
{
    local benchmark="$1"; shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1"; shift
    
    local benchmark_path="${TMP_DIR}/c/${benchmark}"
    local cache_file="${benchmark_path}${disk_aux_name}.cache"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	get_racket_runtime "$benchmark" "$benchmark_args" "$disk_aux_name"
	local baseline="$RETURN";
	clang "${benchmark_path}.c" "${SCHML_DIR}/src/backend-c/runtime/boehm-gc-install/lib/libgc.a" -I"${SCHML_DIR}/src/backend-c/runtime/boehm-gc-install/include" -pthread -lm -O3 -o "${benchmark_path}.o"
	avg "${benchmark_path}.o" "$benchmark_args"
	local ct="$RETURN"
	local cr=$(echo "${ct}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n",p, $0}')
	RETURN=$(echo "$cr" | awk '{printf "%.2f\n",$0}')
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the slowdown factor for the C benchmark
get_gambit_slowdown()
{
    local benchmark="$1"; shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1"; shift
    
    local benchmark_path="${TMP_DIR}/gambit/${benchmark}"
    local cache_file="${benchmark_path}${disk_aux_name}.cache"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	get_racket_runtime "$benchmark" "$benchmark_args" "$disk_aux_name"
	local baseline="$RETURN";
	gsc -exe -cc-options -O3 "${benchmark_path}.scm"
	avg "${benchmark_path}" "$benchmark_args"
	local ct="$RETURN"
	local cr=$(echo "${ct}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n",p, $0}')
	RETURN=$(echo "$cr" | awk '{printf "%.2f\n",$0}')
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - binary to run
# $2 - stdin arguments
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
# $4 - disk aux name
# $RETURN - number of configurations
run_config()
{
    local i="$1";             shift
    local path="$1";          shift
    local args="$1";          shift
    local disk_aux_name="$1"; shift
    
    local name=$(basename "$path")
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${i}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${i}.csv"
    local cache_file="${TMP_DIR}/static/${name}${disk_aux_name}${i}.cache"
    if [ -f $cache_file ]; then
	RETURN=$(cat "$cache_file")
    else
	local n=0 b
	get_racket_runtime "$name" "$args" "$disk_aux_name"
	local baseline="$RETURN"
	echo "name,precision,time,slowdown" > "$logfile1"
	for b in $(find "$path" -name "*.o$i"); do
	    let n=n+1
	    local binpath="${b%.*}"
	    local bname=$(basename "$binpath")
	    local p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < "${binpath}.schml")
	    avg "$b" "$args"
	    local t="$RETURN"
	    local f=$(echo "${t}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n", p,$0}')
	    echo $n $b $f
	    printf "%d,%.2f,%.${PRECISION}f,%.2f\n" $bname $p $t $f >> $logfile1
	done
	cut -d, -f4 "$logfile1" | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > "$logfile2"
	RETURN="$n"
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - config index
# $2 - $name
# $3 - space-separated benchmark arguments
# $4 - disk aux name
# RETURN - the ratio between the runtimes of the typed and untyped versions of a benchmark
compute_typed_untyped_ratio()
{
    local i="$1";              shift
    local name="$1";           shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local cache_file="${TMP_DIR}/${name}${disk_aux_name}${i}_typed_untyped_ratio"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
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
# $5 - output of dynamizer
# $6 - printed name of the benchmark
# $7 - disk aux name
gen_output()
{
    local c1="$1";                     shift
    local c2="$1";                     shift
    local path="$1";                   shift
    local benchmark_args="$1";         shift
    local dynamizer_out="$1";          shift
    local printname="$1";              shift
    local disk_aux_name="$1";          shift

    local type_constructor_count=$(echo "$dynamizer_out" | sed -n 's/[0-9]\+.\([0-9]\+\)/\1/p')
    local less_precise_count=$(echo "$dynamizer_out" | sed -n 's/\([0-9]\+\).*/\1/p')
    local lpc_t=$(echo "${less_precise_count}/1000000000" | bc -l | awk '{printf "%.2f\n",$0}')

    local name=$(basename "$path")
    local disk_name="${name}${disk_aux_name}_${c1}_${c2}"
    cum_perf_lattice_fig="${OUT_DIR}/cumperflattice/${disk_name}.png"
    cum_perf_lattice_tbl="${OUT_DIR}/cumperflattice/${disk_name}.tex"
    perf_lattice_fig="${OUT_DIR}/perflattice/${disk_name}.png"
    
    # if [[ ! -f "$cum_perf_lattice_fig" || ! -f "$cum_perf_lattice_tbl" || ! -f "$perf_lattice_fig" ]]; then
    rm -f "$cum_perf_lattice_fig" "$cum_perf_lattice_tbl" "$perf_lattice_fig"
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${c1}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${c1}.csv"
    local logfile3="${DATA_DIR}/${name}${disk_aux_name}${c2}.log"
    local logfile4="${DATA_DIR}/${name}${disk_aux_name}${c2}.csv"

    get_c_slowdown "$name" "$benchmark_args" "$disk_aux_name"
    local cr_t="$RETURN"
    get_gambit_slowdown "$name" "$benchmark_args" "$disk_aux_name"
    local gr_t="$RETURN"

    compute_typed_untyped_ratio "$c1" "$name" "$benchmark_args" "$disk_aux_name"
    local typed_untyped_ratio1="$RETURN"
    compute_typed_untyped_ratio "$c2" "$name" "$benchmark_args" "$disk_aux_name"
    local typed_untyped_ratio2="$RETURN"

    run_config "$c1" "$path" "$benchmark_args" "$disk_aux_name"
    run_config "$c2" "$path" "$benchmark_args" "$disk_aux_name"
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
    	   `"set output '${cum_perf_lattice_fig}';"`
	   `"set border 15 back;"`
      	   `"set title \"${printname}\";"`
    	   `"set xrange [0:10]; set yrange [0:${n}];"`
    	   `"set xtics nomirror (\"1x\" 1,\"2x\" 2,\"3x\" 3,\"4x\" 4,\"5x\" 5, \"6x\" 6,\"7x\" 7, \"8x\" 8, \"9x\" 9, \"10x\" 10, \"15x\" 15, \"20x\" 20);"`
    	   `"set ytics nomirror 0,200;"`
	   `"set arrow from 1,graph(0,0) to 1,graph(1,1) nohead lc rgb \"black\" lw 2;"`
    	   `"plot '${logfile2}' using 1:2 with lines lw 2 title 'Config ${c1}' smooth cumulative,"`
    	   `"'${logfile4}' using 1:2 with lines lw 2 title 'Config ${c2}' smooth cumulative"

    echo "\begin{tabular}{|l|l|l|}
\hline
\textbf{${printname}} & \multicolumn{2}{l|}{(${type_constructor_count} type nodes)} \\\ \hline
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
\end{table}" > "${cum_perf_lattice_tbl}"

    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '${perf_lattice_fig}';"`
    	   `"set yrange [0:100];"`
    	   `"set title \"${printname}\";"`
	   `"set xlabel \"slowdown\";"`
	   `"set ylabel \"How much of the code is typed\";"`
    	   `"plot '${logfile1}' using 4:(100-\$2) with points title 'Config ${c1}',"`
    	   `"'${logfile3}' using 4:(100-\$2) with points title 'Config ${c2}'"
    # fi
}

# $1 - first config index
# $2 - second config index
# $3 - benchmark filename without extension
# $4 - space-separated benchmark arguments
# $5 - nsamples
# $6 - aux name
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

    local disk_aux_name="" print_aux_name=""
    if [[ ! -z "${aux_name}" ]]; then
	disk_aux_name="_${aux_name}"
	print_aux_name=" (${aux_name})"
    fi
    
    local print_name="$(echo "$name" | tr _ " " | sed -e "s/\b\(.\)/\u\1/g" | tr " " "-")${print_aux_name}"

    local benchmark_args_file="${TMP_DIR}/${name}${disk_aux_name}.args"
    if [ -f benchmark_args_file ]; then
	benchmark_args=$(cat "$benchmark_args_file")
    else
	printf "Benchmark\t:%s\n" "$name" >> "$PARAMS_LOG"
	printf "Args\t\t:%s\n" "$benchmark_args" >> "$PARAMS_LOG"
	echo "$benchmark_args" > "$benchmark_args_file"
    fi

    local lattice_file="${lattice_path}/out"
    if [ ! -f "$lattice_file" ]; then
	rm -rf "$lattice_path"
	local dynamizer_out=$(dynamizer "$lattice_path" "$nsamples" | sed -n 's/.* \([0-9]\+\) .* \([0-9]\+\) .*/\1 \2/p')
	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${lattice_path}/"
	echo "$dynamizer_out" > "$lattice_file"
    fi
    dynamizer_out=$(cat "$lattice_file")

    gen_output $c1 $c2 "$lattice_path" "$benchmark_args" "$dynamizer_out" "$print_name" "$disk_aux_name"
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

    run_benchmark $c1 $c2 "n-body" "10000" "$nsamples" ""

    echo "finished experiment comparing" $c1 "vs" $c2
}

main()
{
    USAGE="Usage: $0 nsamples loops [fresh|date] n_1,n_2 ... n_n"
    if [ "$#" == "0" ]; then
	echo "$USAGE"
	exit 1
    fi
    local nsamples="$1"; shift
    LOOPS="$1";          shift
    date="$1";           shift

    if [ "$date" == "fresh" ]; then
	declare -r DATE=`date +%Y_%m_%d_%H_%M_%S`
    else
	declare -r DATE="$date"
	if [ ! -d "$SCHML_DIR/benchmark/suite/macro/lattice/$DATE" ]; then
	    echo "Directory not found"
	    exit 1
	fi
    fi
    
    declare -r TEST_DIR="$SCHML_DIR/benchmark/suite/macro"
    declare -r LATTICE_DIR="$TEST_DIR/lattice/$DATE"
    declare -r DATA_DIR="$LATTICE_DIR/data"
    declare -r OUT_DIR="$LATTICE_DIR/output"
    declare -r TMP_DIR="$LATTICE_DIR/tmp"
    declare -r SRC_DIR="$TEST_DIR/src"
    declare -r PARAMS_LOG="$LATTICE_DIR/params.txt"

    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$TMP_DIR"
    mkdir -p "$OUT_DIR/cumperflattice"
    mkdir -p "$OUT_DIR/perflattice"

    if [ "$date" == "fresh" ]; then
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
	printf "loops:\t\t:%s\n" "$LOOPS" >> "$PARAMS_LOG"
	printf "nsamples\t:%s\n" "$nsamples" >> "$PARAMS_LOG"

	cd "$SCHML_DIR"
	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${TMP_DIR}/static/"
	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${TMP_DIR}/dyn/"
    fi

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
