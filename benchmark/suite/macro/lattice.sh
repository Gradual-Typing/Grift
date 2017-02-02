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

# $1 - baseline system
# $2 - logfile index
# $3 - $path
# $4 - space-separated benchmark arguments
# $5 - disk aux name
# $RETURN - number of configurations
run_config()
{
    local baseline_system="$1"; shift
    local i="$1";               shift
    local path="$1";            shift
    local args="$1";            shift
    local disk_aux_name="$1";   shift
    
    local name=$(basename "$path")
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${i}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${i}.csv"
    local cache_file="${TMP_DIR}/static/${name}${disk_aux_name}${i}.cache"
    if [ -f $cache_file ]; then
	RETURN=$(cat "$cache_file")
    else
	local n=0 b
	$baseline_system "$name" "$args" "$disk_aux_name"
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

# $1 - baseline system
# $2 - first config index
# $3 - second config index
# $4 - $path
# $5 - space-separated benchmark arguments
# $6 - output of dynamizer
# $7 - printed name of the benchmark
# $8 - disk aux name
gen_output()
{
    local baseline_system="$1"; shift
    local c1="$1";              shift
    local c2="$1";              shift
    local path="$1";            shift
    local benchmark_args="$1";  shift
    local dynamizer_out="$1";   shift
    local printname="$1";       shift
    local disk_aux_name="$1";   shift

    local type_constructor_count=$(echo "$dynamizer_out" | sed -n 's/[0-9]\+.\([0-9]\+\)/\1/p')
    local less_precise_count=$(echo "$dynamizer_out" | sed -n 's/\([0-9]\+\).*/\1/p')
    local lpc_t=$(echo "${less_precise_count}/1000000000" | bc -l | awk '{printf "%.2f\n",$0}')

    local name=$(basename "$path")

    local config_str=$(racket "${SCHML_DIR}/benchmark/config_str.rkt" -c $c1 $c2)
    local c1t=$(echo $config_str | sed -n 's/\(.*\),.*,.*/\1/p;q')
    local c2t=$(echo $config_str | sed -n 's/.*,\(.*\),.*/\1/p;q')
    local ct=$(echo $config_str | sed -n 's/.*,.*,\(.*\)/\1/p;q')
    local disk_name="${name}${disk_aux_name}_${ct}"
    
    cum_perf_lattice_fig="${OUT_DIR}/cumperflattice/${disk_name}.png"
    cum_perf_lattice_tbl="${OUT_DIR}/cumperflattice/${disk_name}.tex"
    perf_lattice_fig="${OUT_DIR}/perflattice/${disk_name}.png"
    
    # if [[ ! -f "$cum_perf_lattice_fig" || ! -f "$cum_perf_lattice_tbl" || ! -f "$perf_lattice_fig" ]]; then
    rm -f "$cum_perf_lattice_fig" "$cum_perf_lattice_tbl" "$perf_lattice_fig"
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${c1}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${c1}.csv"
    local logfile3="${DATA_DIR}/${name}${disk_aux_name}${c2}.log"
    local logfile4="${DATA_DIR}/${name}${disk_aux_name}${c2}.csv"

    get_slowdown c $baseline_system "$name" "$benchmark_args" "$disk_aux_name"
    local cr_t="$RETURN"
    get_slowdown gambit $baseline_system "$name" "$benchmark_args" "$disk_aux_name"
    local gr_t="$RETURN"

    compute_typed_untyped_ratio "$c1" "$name" "$benchmark_args" "$disk_aux_name"
    local typed_untyped_ratio1="$RETURN"
    compute_typed_untyped_ratio "$c2" "$name" "$benchmark_args" "$disk_aux_name"
    local typed_untyped_ratio2="$RETURN"

    run_config $baseline_system "$c1" "$path" "$benchmark_args" "$disk_aux_name"
    run_config $baseline_system "$c2" "$path" "$benchmark_args" "$disk_aux_name"
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
    	   `"plot '${logfile2}' using 1:2 with lines lw 2 title '${c1t}' smooth cumulative,"`
    	   `"'${logfile4}' using 1:2 with lines lw 2 title '${c2t}' smooth cumulative"

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
    	   `"plot '${logfile1}' using 4:(100-\$2) with points title '${c1t}',"`
    	   `"'${logfile3}' using 4:(100-\$2) with points title '${c2t}'"
    # fi
}

# $1 - baseline system
# $2 - first config index
# $3 - second config index
# $4 - benchmark filename without extension
# $5 - space-separated benchmark arguments
# $6 - nsamples
# $7 - aux name
run_benchmark()
{
    local baseline_system="$1"; shift
    local c1="$1";              shift
    local c2="$1";              shift
    local name="$1";            shift
    local benchmark_args="$1";  shift
    local nsamples="$1";        shift
    local aux_name="$1";        shift

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
	echo "$dynamizer_out" > "$lattice_file"
    fi
    racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${lattice_path}/"
    dynamizer_out=$(cat "$lattice_file")

    gen_output $baseline_system $c1 $c2 "$lattice_path" "$benchmark_args" "$dynamizer_out" "$print_name" "$disk_aux_name"
}

# $1 - baseline system
# $2 - first config index
# $3 - second config index
# $4 - nsamples
run_experiment()
{
    local baseline_system="$1"; shift
    local c1="$1";              shift
    local c2="$1";              shift
    local nsamples="$1";        shift

    local bs_bc_arg="\"$(cat "${INPUT_DIR}/blackscholes/in_64K.txt")\""
    run_benchmark $baseline_system $c1 $c2 "blackscholes" "$bs_bc_arg" "$nsamples" ""
    
    local qs_bc_arg="\"$(cat "${INPUT_DIR}/quicksort/in_rand1000.txt")\""
    run_benchmark $baseline_system $c1 $c2 "quicksort" "$qs_bc_arg" "$nsamples" "bestcase"
    
    local qs_wc_arg="\"$(cat "${INPUT_DIR}/quicksort/in_descend1000.txt")\""
    run_benchmark $baseline_system $c1 $c2 "quicksort" "$qs_wc_arg" "$nsamples" "worstcase"
    
    run_benchmark $baseline_system $c1 $c2 "matmult" "200" "$nsamples" ""

    run_benchmark $baseline_system $c1 $c2 "n-body" "10000" "$nsamples" ""

    run_benchmark $baseline_system $c1 $c2 "fft" "65536" "$nsamples" ""

    # convert "*_${c1}_${c2}.png" -append "${c1}_${c2}".png

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
    local date="$1";     shift

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
    declare -r EXP_DIR="$TEST_DIR/lattice/$DATE"
    declare -r DATA_DIR="$EXP_DIR/data"
    declare -r OUT_DIR="$EXP_DIR/output"
    declare -r TMP_DIR="$EXP_DIR/tmp"
    declare -r SRC_DIR="$TEST_DIR/src"
    declare -r INPUT_DIR="$TEST_DIR/inputs"
    declare -r PARAMS_LOG="$EXP_DIR/params.txt"

    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$TMP_DIR"
    mkdir -p "$OUT_DIR/cumperflattice"
    mkdir -p "$OUT_DIR/perflattice"

    . "lib/runtime.sh"

    cd "$SCHML_DIR"

    local baseline_system=get_racket_runtime

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
	chezscheme_ver=$(scheme --version 2>&1)
	printf "ChezScheme ver.\t:%s\n" "$chezscheme_ver" >> "$PARAMS_LOG"
	printf "loops:\t\t:%s\n" "$LOOPS" >> "$PARAMS_LOG"
	printf "nsamples\t:%s\n" "$nsamples" >> "$PARAMS_LOG"

	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${TMP_DIR}/static/"
	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${TMP_DIR}/dyn/"
    fi

    local i j
    if [ "$#" == "1" ]; then
	local config="$1";   shift
	for i in `seq ${config}`; do
	    for j in `seq ${i} ${config}`; do
		if [ ! $i -eq $j ]; then
		    run_experiment $baseline_system $i $j $nsamples
		fi
	    done
	done
    else
	while (( "$#" )); do
	    i=$1; shift
	    j=$1; shift
	    run_experiment $baseline_system $i $j $nsamples
	done
    fi
    echo "done."
}

main "$@"
