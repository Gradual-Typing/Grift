#!/bin/bash

# computes speedup ranges for different configurations in the internal comparisons
# it expects the same directory structure as the one for lattice_bins where
# the main directory is named temp and it needs temp/data, temp/tmp/dyn, temp/tmp/static,
# and maybe temp/tmp/racket if you assume racket as the baseline

function main()
{
    declare -r TEST_DIR="${GRIFT_DIR}/benchmark/suite/macro"
    declare -r LIB_DIR="$TEST_DIR/lib"
    declare -r LB_DIR="$TEST_DIR/lattice_bins"
    declare -r EXP_DIR="$LB_DIR/temp"
    declare -r DATA_DIR="$EXP_DIR/data"
    declare -r OUT_DIR="$EXP_DIR/output"
    declare -r SRC_DIR="$EXP_DIR/src/partial"
    declare -r TMP_DIR="$EXP_DIR/tmp"

    if [ ! -d "$EXP_DIR" ]; then
        echo "$EXP_DIR" "Directory not found"
        exit 1
    fi

    . ${TEST_DIR}/lib/runtime.sh

    local baseline_system=get_dyn_grift_17_runtime
    # local baseline_system=get_racket_runtime

    run_double 19 17
    run_double 17 7
    run_double 17 13
    run_double 17 8
    
    run_single $baseline_system 19
    run_single $baseline_system 17
    run_single $baseline_system 7
}

function run_double()
{
    local c1="$1"; shift
    local c2="$1"; shift

    MIN_SPEEDUP=1000000
    MAX_SPEEDUP=0
    MEAN_SPEEDUP=0
	
    local config_str=$(racket "${GRIFT_DIR}/benchmark/config_str.rkt" -c $c1 $c2)
    local c1t=$(echo $config_str | sed -n 's/\(.*\),.*,.*/\1/p;q')
    local c2t=$(echo $config_str | sed -n 's/.*,\(.*\),.*/\1/p;q')
    local ct=$(echo $config_str | sed -n 's/.*,.*,\(.*\)/\1/p;q')

    # Blackscholes
    run_two_benchmarks $c1 $c2 "$ct" "$c1t" "$c2t" "blackscholes" "in_4K.txt"
    
    # Quicksort
    run_two_benchmarks $c1 $c2 "$ct" "$c1t" "$c2t" "quicksort" "in_descend1000.txt"
    
    # Matrix Multiplication
    run_two_benchmarks $c1 $c2 "$ct" "$c1t" "$c2t" "matmult" "400.txt"
    
    # N Body Simulation
    run_two_benchmarks $c1 $c2 "$ct" "$c1t" "$c2t" "n_body" "slow.txt"
    
    # Fast Fourier Transform
    run_two_benchmarks $c1 $c2 "$ct" "$c1t" "$c2t" "fft" "slow.txt"
    
    # Scheme Array Benchmark
    run_two_benchmarks $c1 $c2 "$ct" "$c1t" "$c2t" "array" "slow.txt"
    
    # Tak
    run_two_benchmarks $c1 $c2 "$ct" "$c1t" "$c2t" "tak" "slow.txt"

    run_two_benchmarks $c1 $c2 "$ct" "$c1t" "$c2t" "ray" "empty.txt"

    echo "finished comparing $c1t to $c2t with $ct, where speedups range from "\
	 $MIN_SPEEDUP " to " $MAX_SPEEDUP ", with a mean of" $MEAN_SPEEDUP
}

function run_single()
{
    local baseline_system="$1"; shift
    local c="$1"; shift

    MIN_SPEEDUP=1000000
    MAX_SPEEDUP=0

    local config_str=$(racket "${GRIFT_DIR}/benchmark/config_str.rkt" -c $c $c)
    local ct=$(echo $config_str | sed -n 's/.*,.*,\(.*\)/\1/p;q')

    # Blackscholes
    run_benchmark $baseline_system $c "$ct" "blackscholes" "in_4K.txt"
    
    # Quicksort
    run_benchmark $baseline_system $c "$ct" "quicksort" "in_descend1000.txt"
    
    # Matrix Multiplication
    run_benchmark $baseline_system $c "$ct" "matmult" "400.txt"
    
    # N Body Simulation
    run_benchmark $baseline_system $c "$ct" "n_body" "slow.txt"
    
    # Fast Fourier Transform
    run_benchmark $baseline_system $c "$ct" "fft" "slow.txt"
    
    # Scheme Array Benchmark
    run_benchmark $baseline_system $c "$ct" "array" "slow.txt"
    
    # Tak
    run_benchmark $baseline_system $c "$ct" "tak" "slow.txt"

    run_benchmark $baseline_system $c "$ct" "ray" "empty.txt"

    echo "finished running" $c ", where speedups range from " $MIN_SPEEDUP " to " $MAX_SPEEDUP
}

function run_benchmark()
{
    local baseline_system="$1"; shift
    local c="$1"; shift
    local ct="$1"; shift
    local name="$1"; shift
    local input_file="$1";  shift

    local disk_aux_name=""
    local logfile="${DATA_DIR}/${name}${c}.log"

    if [ ! -f "$logfile" ]; then
	return
    fi

    local max_rt=$(tail -n +2 "$logfile" | cut -f3 -d"," | sort -n | tail -1)
    local min_rt=$(tail -n +2 "$logfile" | cut -f3 -d"," | sort -n | head -1)

    $baseline_system "$name" "$input_file" "$disk_aux_name"
    local baseline="$RETURN"

    local min_speedup=$(echo "${baseline} ${max_rt}" | awk '{printf "%.5f\n", $1 / $2}')
    local max_speedup=$(echo "${baseline} ${min_rt}" | awk '{printf "%.5f\n", $1 / $2}')

    if [[ $(echo $min_speedup'<'$MIN_SPEEDUP | bc -l) -eq 1 ]]; then
	MIN_SPEEDUP=$min_speedup
    fi

    if [[ $(echo $max_speedup'>'$MAX_SPEEDUP | bc -l) -eq 1 ]]; then
	MAX_SPEEDUP=$max_speedup
    fi
}

function run_two_benchmarks()
{
    local c1="$1"; shift
    local c2="$1"; shift
    local ct="$1"; shift
    local c1t="$1"; shift
    local c2t="$1"; shift
    local name="$1"; shift
    local input_file="$1";  shift

    local disk_aux_name=""
    local logfile1="${DATA_DIR}/${name}${c1}.log"
    local logfile2="${DATA_DIR}/${name}${c2}.log"
    local analysis_dir="${OUT_DIR}/analysis/${ct}"

    if [ ! -f "$logfile1" ]; then
	return
    fi

    if [ ! -f "$logfile2" ]; then
	return
    fi

    mkdir -p "$analysis_dir"

    local tmp1_logfile="tmp1"
    local tmp2_logfile="tmp2"
    local tmp3_logfile="tmp3"
    local tmp4_logfile="tmp4"
    local tmp5_logfile="tmp5"
    local tmp6_logfile="tmp6"
    local tmp7_logfile="tmp7"
    local speedup_logfile="${analysis_dir}/${name}.log"

    local logfile3="${logfile1}.sorted1"
    local logfile4="${logfile2}.sorted1"

    # delete the extension from file names in the logfile so that we can sort them
    #tail -n +2 "$logfile1" | sed "s/.o${c1}//1" | sort -k1 -n -t, > "${logfile3}"
    tail -n +2 "$logfile1" | sort -k1 -n -t, > "${logfile3}"
    tail -n +2 "$logfile2" | sort -k1 -n -t, > "${logfile4}"

    # extract the runtime columns (3rd) from each logfile and put them together
    # in a temporary file
    awk -F"," 'BEGIN { OFS = "," } {print $3}' "$logfile3" > "$tmp1_logfile"
    awk -F"," 'BEGIN { OFS = "," } {print $3}' "$logfile4" > "$tmp2_logfile"
    paste -d , "$tmp1_logfile" "$tmp2_logfile" > "$tmp3_logfile"

    # compute speedup of the first config over the second
    awk  -F "," '{printf "%4.2f\n", $2/$1 }' "$tmp3_logfile" > "$tmp4_logfile"

    # extract file names
    awk -F"," 'BEGIN { OFS = "," } {print $1}' "$logfile3" > "$tmp5_logfile"

    echo "filename,${c1t} runtime,${c2t} runtime,speedup of ${c1t} over ${c2t}" > "$speedup_logfile"
    paste -d , "$tmp5_logfile" "$tmp3_logfile" "$tmp4_logfile" >> "$speedup_logfile"
    
    local max_speedup=$(sort -n "$tmp4_logfile" | tail -1)
    local min_speedup=$(sort -n "$tmp4_logfile" | head -1)

    local mean1=$(awk '{ total += $1 } END { print total/NR }' "$tmp1_logfile")
    local mean2=$(awk '{ total += $1 } END { print total/NR }' "$tmp2_logfile")

    local mean_speedup=$(echo "${mean2} ${mean1}" | awk '{printf "%.5f\n", $1 / $2}')

    if [[ $(echo $min_speedup'<'$MIN_SPEEDUP | bc -l) -eq 1 ]]; then
	MIN_SPEEDUP=$min_speedup
    fi

    if [[ $(echo $max_speedup'>'$MAX_SPEEDUP | bc -l) -eq 1 ]]; then
	MAX_SPEEDUP=$max_speedup
    fi

    if [[ $(echo $mean_speedup'>'$MEAN_SPEEDUP | bc -l) -eq 1 ]]; then
	MEAN_SPEEDUP=$mean_speedup
    fi

    echo $name $mean_speedup $min_speedup $max_speedup

    rm "$tmp1_logfile" "$tmp2_logfile" "$tmp3_logfile" "$tmp4_logfile"
}

main "$@"
