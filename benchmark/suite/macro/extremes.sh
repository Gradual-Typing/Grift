#!/bin/sh

set -euo pipefail
declare -r PRECISION=5
TIMEFORMAT=%R

function join { local d=$1; shift; echo -n "$1"; shift; printf "%s" "${@/#/$d}"; }

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

    get_schml_slowdown $baseline_system_static "${TMP_DIR}/static/${name}" "$benchmark_args" "$disk_aux_name"
    local ss="${RETURN[@]}"
    echo "$name$print_aux_name",$(join \, ${ss[@]}) >> $logfile1
    
    get_slowdown gambit $baseline_system_dynamic "$name" "$benchmark_args" "$disk_aux_name"
    local gs=$RETURN
    get_slowdown chezscheme $baseline_system_dynamic "$name" "$benchmark_args" "$disk_aux_name"
    local css=$RETURN
    get_schml_slowdown $baseline_system_dynamic "${TMP_DIR}/dyn/${name}" "$benchmark_args" "$disk_aux_name"
    local sd="${RETURN[@]}"
    echo "$name$print_aux_name",$gs,$css,$(join \, ${sd[@]}) >> $logfile2
    
    echo "finished " "${name}${print_aux_name}"
}

# $1 - static or dyn
gen_fig()
{
    local mode="$1"; shift
    
    local logfile="${DATA_DIR}/${mode}.log"
    local outfile="${OUT_DIR}/${mode}.png"
    local N=$(head -1 "${logfile}" | sed 's/[^,]//g' | wc -c)
    
    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '${outfile}';"`
	   `"set border 15 back;"`
           `"set style data histogram;"`
           `"set style histogram cluster gap 1;"`
           `"set style fill pattern border -1;"`
           `"set boxwidth 0.9;"`
	   `"set key left;"`
           `"set title \"\";"`
	   `"set xtic rotate by -45 scale 0;"`
	   `"set grid ytics;"`
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
    echo "name,schml$(join ,schml ${SCHML_CONFIGS[@]})" > "$logfile1"
    echo "name,gambit,chezscheme,schml$(join ,schml ${SCHML_CONFIGS[@]})" > "$logfile2"

    local qs_bc_arg="\"$(cat "${INPUT_DIR}/quicksort/in_rand10000.txt")\""
    run_benchmark $baseline_system_static $baseline_system_dynamic "quicksort" "$qs_bc_arg" "bestcase"
    
    local qs_wc_arg="\"$(cat "${INPUT_DIR}/quicksort/in_descend10000.txt")\""
    run_benchmark $baseline_system_static $baseline_system_dynamic "quicksort" "$qs_wc_arg" "worstcase"
    
    run_benchmark $baseline_system_static $baseline_system_dynamic "matmult" "200" ""

    run_benchmark $baseline_system_static $baseline_system_dynamic "n-body" "100000" ""

    gen_fig static
    gen_fig dyn
}

main()
{
    USAGE="Usage: $0 loops"
    if [ "$#" == "0" ]; then
	echo "$USAGE"
	exit 1
    fi
    LOOPS="$1";          shift

    declare -r DATE=`date +%Y_%m_%d_%H_%M_%S`
    declare -r TEST_DIR="$SCHML_DIR/benchmark/suite/macro"
    declare -r EXP_DIR="$TEST_DIR/extremes/$DATE"
    declare -r DATA_DIR="$EXP_DIR/data"
    declare -r OUT_DIR="$EXP_DIR/output"
    declare -r TMP_DIR="$EXP_DIR/tmp"
    declare -r SRC_DIR="$TEST_DIR/src"
    declare -r INPUT_DIR="$TEST_DIR/inputs"
    declare -r PARAMS_LOG="$EXP_DIR/params.txt"

    declare -r SCHML_CONFIGS=(1 2 3 4)

    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$TMP_DIR"
    mkdir -p "$OUT_DIR"

    . "lib/runtime.sh"

    cd "$SCHML_DIR"
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

    run_experiment get_c_runtime get_racket_runtime
    echo "done."
}

main "$@"
