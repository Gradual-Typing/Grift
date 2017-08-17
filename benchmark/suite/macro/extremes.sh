#!/bin/sh
set -uo pipefail
trap 's=$?; echo "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

declare -r PRECISION=5
TIMEFORMAT=%R

# $1 - baseline system
# $2 - benchmark filename without extension
# $3 - space-separated benchmark arguments
# $4 - aux name
# $5 - static/dyn/partial
# $6 - logfile full path
write_grift_speedups()
{
    local baseline_system="$1"; shift
    local name="$1";            shift
    local benchmark_args="$1";  shift
    local disk_aux_name="$1";   shift
    local dir="$1";             shift
    local logfile="$1";         shift
    
    for config_index in ${configs[@]}; do
	get_grift_speedup $baseline_system "${TMP_DIR}/${dir}/${name}" "$benchmark_args" "$disk_aux_name" $config_index
	printf ",$RETURN" >> $logfile
    done
}

# $1 - static baseline system
# $2 - dynamic baseline system
# $3 - benchmark filename without extension
# $4 - space-separated benchmark arguments
# $5 - aux name
run_benchmark()
{
    local baseline_system_static="$1";  shift
    local baseline_system_dynamic="$1"; shift
    local name="$1";                    shift
    local benchmark_args="$1";          shift
    local aux_name="$1";                shift

    local logfile1="${DATA_DIR}/static.log"
    local logfile2="${DATA_DIR}/dyn.log"
    local logfile3="${DATA_DIR}/partial.log"

    local disk_aux_name="" print_aux_name=""
    if [[ ! -z "${aux_name}" ]]; then
	disk_aux_name="_${aux_name}"
	print_aux_name="(${aux_name})"
    fi

    local benchmark_args_file="${TMP_DIR}/${name}${disk_aux_name}.args"
    if [ -f benchmark_args_file ]; then
	benchmark_args=$(cat "$benchmark_args_file")
    else
	printf "Benchmark\t:%s\n" "$name" >> "$PARAMS_LOG"
	printf "Args\t\t:%s\n" "$benchmark_args" >> "$PARAMS_LOG"
	echo "$benchmark_args" > "$benchmark_args_file"
    fi

    printf "$name$print_aux_name" >> "$logfile1"
    write_grift_speedups $baseline_system_static "$name" "$benchmark_args" "$disk_aux_name" static "$logfile1"
    printf "\n" >> "$logfile1"
    printf "$name$print_aux_name" >> $logfile2
    write_grift_speedups $baseline_system_dynamic "$name" "$benchmark_args" "$disk_aux_name" dyn "$logfile2"
    get_speedup racket $baseline_system_dynamic "$name" "$benchmark_args" "$disk_aux_name"
    printf ",$RETURN" >> $logfile2
    get_speedup chezscheme $baseline_system_dynamic "$name" "$benchmark_args" "$disk_aux_name"
    printf ",$RETURN" >> $logfile2
    printf "\n" >> "$logfile2"

    # local partial_path="${TMP_DIR}/partial/${name}"
    # if [ -f "${partial_path}.grift" ]; then
    # 	echo -n "$name$print_aux_name" >> "$logfile3"
    # 	write_grift_speedups $baseline_system_dynamic "$name" "$benchmark_args" "$disk_aux_name" partial "$logfile3"
    # fi
    
    echo "finished ${name}${print_aux_name}"
}

# $1 - static or dyn
gen_fig()
{
    local mode="$1"; shift
    local sys="$1";  shift
    
    local logfile="${DATA_DIR}/${mode}.log"
    local outfile="${OUT_DIR}/${mode}.png"
    local N=$(head -1 "${logfile}" | sed 's/[^,]//g' | wc -c)

    rm -rf "$outfile"
    
    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '${outfile}';"`
	   `"set border 15 back;"`
           `"set style data histogram;"`
           `"set style histogram cluster gap 1;"`
           `"set style fill pattern border -1;"`
           `"set boxwidth 0.9;"`
	   `"set key left;"`
	   `"set logscale y;"`
	   `"set ylabel \"Speedup with respect to ${sys} in logarithmic scale\";"`
           `"set title \"\";"`
	   `"set xtic rotate by -45 scale 0;"`
	   `"set grid ytics;"`
           `"set ytics add (\"1\" 1);"`
           `"plot '${logfile}' using 2:xtic(1) title col,"`
      	   `"for [i=3:$N] \"\" using i title columnheader(i)"
}


# $1 - static baseline system
# $2 - dynamic baseline system
run_experiment()
{
    local baseline_system_static="$1";  shift
    local baseline_system_dynamic="$1"; shift
    
    local logfile1="${DATA_DIR}/static.log"
    local logfile2="${DATA_DIR}/dyn.log"
    local logfile3="${DATA_DIR}/partial.log"

    local config_str=$(racket "${SCHML_DIR}/benchmark/config_str.rkt" --indices-to-names "$configs")

    echo "name,${config_str}" > "$logfile1"
    echo "name,${config_str},racket,chezscheme" > "$logfile2"
    echo "name,${config_str}" > "$logfile3"

    local arr_bc_arg="\"$(cat "${INPUT_DIR}/array/slow.txt")\""
    run_benchmark $baseline_system_static $baseline_system_dynamic "array" "$arr_bc_arg" ""
    
    local tak_bc_arg="\"$(cat "${INPUT_DIR}/tak/slow.txt")\""
    run_benchmark $baseline_system_static $baseline_system_dynamic "tak" "$tak_bc_arg" ""

    run_benchmark $baseline_system_static $baseline_system_dynamic "ray" "" ""

    local bs_bc_arg="\"$(cat "${INPUT_DIR}/blackscholes/in_64K.txt")\""
    run_benchmark $baseline_system_static $baseline_system_dynamic "blackscholes" "$bs_bc_arg" ""
    
    run_benchmark $baseline_system_static $baseline_system_dynamic "matmult" "400" ""
    
    local qs_wc_arg="\"$(cat "${INPUT_DIR}/quicksort/in_descend10000.txt")\""
    run_benchmark $baseline_system_static $baseline_system_dynamic "quicksort" "$qs_wc_arg" ""

    run_benchmark $baseline_system_static $baseline_system_dynamic "fft" "65536" ""

    run_benchmark $baseline_system_static $baseline_system_dynamic "n-body" "100000" ""

    gen_fig static STLC
    gen_fig dyn Gambit
    # gen_fig partial
}

main()
{
    USAGE="Usage: $0 loops date config_n ..."
    if [ "$#" -le "2" ]; then
        echo "$USAGE"
        exit 1
    fi
    LOOPS="$1";          shift
    local date="$1";     shift
    configs=("$@")

    
    if [ "$date" == "fresh" ]; then
	declare -r DATE=`date +%Y_%m_%d_%H_%M_%S`
    else
	declare -r DATE="$date"
	if [ ! -d "$GRIFT_DIR/benchmark/suite/macro/extremes/$DATE" ]; then
	    echo "Directory not found"
	    exit 1
	fi
    fi

    GRIFT_DIR=${GRIFT_DIR:=`pwd`/../../..}
    
    declare -r TEST_DIR="$GRIFT_DIR/benchmark/suite/macro"
    declare -r EXP_DIR="$TEST_DIR/extremes/$DATE"
    declare -r DATA_DIR="$EXP_DIR/data"
    declare -r OUT_DIR="$EXP_DIR/output"
    declare -r TMP_DIR="$EXP_DIR/tmp"
    declare -r SRC_DIR="$TEST_DIR/src"
    declare -r INPUT_DIR="$TEST_DIR/inputs"
    declare -r PARAMS_LOG="$EXP_DIR/params.txt"

    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$TMP_DIR"
    mkdir -p "$OUT_DIR"

    . "lib/runtime.sh"

    cd "$GRIFT_DIR"

    if [ "$date" == "fresh" ]; then
	# copying the benchmarks to a temporary directory
	cp -r ${SRC_DIR}/* $TMP_DIR

	# logging
	printf "Date\t\t:%s\n" "$DATE" >> "$PARAMS_LOG"
	MYEMAIL="`id -un`@`hostname -f`"
	printf "Machine\t\t:%s\n" "$MYEMAIL" >> "$PARAMS_LOG"
	grift_ver=$(git rev-parse HEAD)
	printf "Grift ver.\t:%s\n" "$grift_ver" >> "$PARAMS_LOG"
	clang_ver=$(clang --version | sed -n 's/clang version \([0-9]*.[0-9]*.[0-9]*\) .*/\1/p;q')
	printf "Clang ver.\t:%s\n" "$clang_ver" >> "$PARAMS_LOG"
	gambit_ver=$(gsc -v | sed -n 's/v\([0-9]*.[0-9]*.[0-9]*\) .*/\1/p;q')
	printf "Gambit ver.\t:%s\n" "$gambit_ver" >> "$PARAMS_LOG"
	racket_ver=$(racket -v | sed -n 's/.* v\([0-9]*.[0-9]*\).*/\1/p;q')
	printf "Racket ver.\t:%s\n" "$racket_ver" >> "$PARAMS_LOG"
	chezscheme_ver=$(scheme --version 2>&1)
	printf "ChezScheme ver.\t:%s\n" "$chezscheme_ver" >> "$PARAMS_LOG"
	printf "loops:\t\t:%s\n" "$LOOPS" >> "$PARAMS_LOG"
    fi

    run_experiment get_static_schml_runtime get_gambit_runtime
    echo "done."
}

main "$@"
