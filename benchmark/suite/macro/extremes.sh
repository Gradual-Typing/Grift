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
        get_grift_speedup $baseline_system "${TMP_DIR}/${dir}/${name}"\
                          "$benchmark_args" "$disk_aux_name" $config_index
        printf ",$RETURN" >> $logfile
        echo "grift $config_index speedup: $RETURN"
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
    local input_file="$1";              shift
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
    local input="$(cat ${INPUT_DIR}/${name}/${input_file})"
    if [ -f benchmark_args_file ]; then
        local old_input=$(cat "$benchmark_args_file")
        if [ ! $old_input == $input ]; then
            echo "input changed mid test" 1>&2
            exit 1
        fi
    else
        printf "Benchmark\t:%s\n" "$name" >> "$PARAMS_LOG"
        printf "Args\t\t:%s\n" "$input" >> "$PARAMS_LOG"
        echo "$input" > "$benchmark_args_file"
    fi

    # Record the runtime of Statically Typed Varients
    printf "$name$print_aux_name" >> "$logfile1"
    write_grift_speedups $baseline_system_static "$name" "$input_file"\
                         "$disk_aux_name" static "$logfile1"
    # Typed Racket
    printf "Typed Racket speedup: "
    get_speedup typed_racket $baseline_system_static\
                "$name" "$input_file" "$disk_aux_name"
    printf ",$RETURN" >> $logfile1
    echo "$RETURN"
    
    # OCaml
    get_speedup ocaml $baseline_system_static\
                "$name" "$input_file" "$disk_aux_name"
    printf ",$RETURN" >> $logfile1    
    echo "OCaml speedup: $RETURN"

    
    printf "\n" >> "$logfile1"
    printf "$name$print_aux_name" >> $logfile2
    
    write_grift_speedups $baseline_system_dynamic "$name" "$input_file"\
                         "$disk_aux_name" dyn "$logfile2"

    # The systems to compare against
    get_speedup gambit $baseline_system_dynamic\
                "$name" "$input_file" "$disk_aux_name"
    printf ",$RETURN" >> $logfile2
    echo "Gambit Speedup: $RETURN"
    
    get_speedup chezscheme $baseline_system_dynamic\
                "$name" "$input_file" "$disk_aux_name"
    printf ",$RETURN" >> $logfile2
    echo "Chez speedup: $RETURN"
    
    printf "\n" >> "$logfile2"

    echo "finished ${name}${print_aux_name}"
}

# $1 - static or dyn
gen_fig()
{
    local mode="$1"; shift
    local sys="$1";  shift
    local outfile_name="$1"; shift
    local key_position="$1"; shift
    local ymin="$1"; shift
    local ymax="$1"; shift

    local logfile="${DATA_DIR}/${mode}.log"
    local outfile="${OUT_DIR}/${outfile_name}.png"
    local N=$(head -1 "${logfile}" | sed 's/[^,]//g' | wc -c)

    rm -rf "$outfile"
    
    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `" noenhanced color font 'Verdana,26' ;"`
            `"set output '${outfile}';"`
                `"set border 15 back;"`
            `"set yrange [${ymin}:${ymax}];"`
            `"set logscale y;"`
            `"set key font 'Verdana,20';"`
            `"set style data histogram;"`
            `"set style histogram cluster gap 1;"`
            `"set style fill pattern border -1;"`
            `"load '${LIB_DIR}/dark-colors.pal';"`
            `"set boxwidth 0.9;"` 
            `"set ylabel \"  Speedup with respect to ${sys}\";"`
            `"set title \"\";"`
            `"set xtic rotate by -45 scale 0;"`
            `"set grid ytics;"`
            `"set ytics add (\"1\" 1);"`
            `"plot '${logfile}' using 2:xtic(1) title col,"`
            `"  for [i=3:$N] \"\" "`
            `"using i title columnheader(i) ls (i-1)"
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

    local config_str=$(racket "${GRIFT_DIR}/benchmark/config_str.rkt" \
                              --name-end " Grift" --names $configs)
    local shared_str=$(racket "${GRIFT_DIR}/benchmark/config_str.rkt" \
                              --name-sep "_" --common $configs)
    
    echo "name,${config_str},Typed-Racket,OCaml" > "$logfile1"
    echo "name,${config_str},Gambit,Chez Scheme" > "$logfile2"
    echo "name,${config_str}" > "$logfile3"

    # run_benchmark $baseline_system_static $baseline_system_dynamic\
        #               "array" "slow.txt" ""
    
    run_benchmark $baseline_system_static $baseline_system_dynamic\
                  "tak" "slow.txt" ""

    run_benchmark $baseline_system_static $baseline_system_dynamic\
                  "ray" "empty.txt" ""

    run_benchmark $baseline_system_static $baseline_system_dynamic\
                  "blackscholes" "in_64K.txt" ""
    
    run_benchmark $baseline_system_static $baseline_system_dynamic\
                  "matmult" "400.txt" ""
    
    run_benchmark $baseline_system_static $baseline_system_dynamic\
                  "quicksort" "in_descend10000.txt" ""

    run_benchmark $baseline_system_static $baseline_system_dynamic\
                  "fft" "slow.txt" ""

    run_benchmark $baseline_system_static $baseline_system_dynamic\
                  "n_body" "slow.txt" ""

    local gmlog1=$(racket "${LIB_DIR}/geometric-mean.rkt" $logfile1)
    local gmlog2=$(racket "${LIB_DIR}/geometric-mean.rkt" $logfile2)
    echo "$gmlog1" > $logfile1
    echo "$gmlog2" > $logfile2

    gen_fig static "Static Grift" "${shared_str}_static" "right" "" ""
    gen_fig dyn Racket "${shared_str}_dynamic" "right" "0.02" "11"
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
    configs="$@"

    GRIFT_DIR=${GRIFT_DIR:=`pwd`/../../..}
    
    if [ "$date" == "fresh" ]; then
        declare -r DATE=`date +%Y_%m_%d_%H_%M_%S`
    elif [ "$date" == "test" ]; then
        declare -r DATE="test"
    else
        declare -r DATE="$date"
        if [ ! -d "$GRIFT_DIR/benchmark/suite/macro/extremes/$DATE" ]; then
            echo "Directory not found"
            exit 1
        fi
    fi

    declare -r TEST_DIR="$GRIFT_DIR/benchmark/suite/macro"
    declare -r EXP_DIR="$TEST_DIR/extremes/$DATE"
    declare -r DATA_DIR="$EXP_DIR/data"
    declare -r OUT_DIR="$EXP_DIR/outputs"
    declare -r TMP_DIR="$EXP_DIR/tmp"
    declare -r SRC_DIR="$TEST_DIR/src"
    declare -r INPUT_DIR="$TEST_DIR/inputs"
    declare -r OUTPUT_DIR="$TEST_DIR/outputs"
    declare -r PARAMS_LOG="$EXP_DIR/params.txt"
    declare -r LIB_DIR="$TEST_DIR/lib"

    # Check to see if all is right in the world
    if [ ! -d $TEST_DIR ]; then
        echo "test directory not found" 1>&2
        exit 1
    elif [ ! -d $SRC_DIR ]; then
        echo "source directory not found" 1>&2
        exit 1
    elif [ ! -d $INPUT_DIR ]; then
        echo "input directory not found" 1>&2
        exit 1
    elif [ ! -d $OUTPUT_DIR ]; then
        echo "output directory not found" 1>&2
        exit 1
    elif [ ! -d $LIB_DIR ]; then
        echo "lib directory not found" 1>&2
        exit 1
    fi
    
    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$OUT_DIR"

    . "lib/runtime.sh"

    cd "$GRIFT_DIR"

    if [ ! -d $TMP_DIR ]; then
        # copying the benchmarks to a temporary directory
        cp -r $SRC_DIR $TMP_DIR

        
        
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

    run_experiment get_static_grift_runtime get_racket_runtime
    echo "done."
}

main "$@"
