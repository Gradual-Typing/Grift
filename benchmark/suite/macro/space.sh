#!/home/dalmahal/sys/bin/bash
# needs bash >= 4.3
set -euo pipefail


function run_config()
{
    # the configuration index
    local c="$1";              shift
    local -n __input_sizes=$1; shift
    local -n __inputs=$1;      shift
    local -n __outputs=$1;     shift
    local logfile="$1";        shift

    
    local b="${TMP_DIR}/partial/${name}/0.o${c}"
    local bname="$(basename $b)"

    echo "iterations,runtime,total values allocated,total casts,longest proxy chain,total proxies accessed,total uses,function total values allocated,vector total values allocated,ref total values allocated,tuple total values allocated,function total casts,vector total casts, ref total casts,tuple total casts,function longest proxy chain,vector longest proxy chain,ref longest proxy chain,tuple longest proxy chain,function total proxies accessed,vector total proxies accessed,ref total proxies accessed,tuple total proxies accessed,function total uses,vector total uses,ref total uses,tuple total uses,injects casts,projects casts"\
	 > "$logfile"

    for ((i=0;i<${#__inputs[@]};++i)); do
	local input="${__inputs[i]}"
	local output="${__outputs[i]}"
	local input_size="${__input_sizes[i]}"
	
	echo $input > "${TMP_DIR}/input"
	echo $output > "${TMP_DIR}/output"
	input="${TMP_DIR}/input"
	output="${TMP_DIR}/output"
	
	printf "${input_size}," >> "$logfile"
	
	avg "$b" "$input" "static" "$output" "${b}.runtimes"
	printf "${RETURN}" >> "$logfile"
	rm "${b}.runtimes"

	eval "cat ${input} | ${b}.prof.o" > /dev/null 2>&1
	mv "$bname.prof.o.prof" "${b}.prof"
	printf "," >> "$logfile"
	# ignore first and last rows and sum the values across all
	# columns in the profile into one column and transpose it into
	# a row
	sed '1d;$d' "${b}.prof" | awk -F, '{print $2+$3+$4+$5+$6+$7}'\
            | paste -sd "," - | xargs echo -n >> "$logfile"
	echo -n "," >> "$logfile"
	# ignore the first row and the first column and stitsh together
	# all rows into one row
	sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
            | awk -F, '{print $1","$2","$3","$4}' | paste -sd "," -\
            | xargs echo -n >> "$logfile"
	echo -n "," >> "$logfile"
	# writing injections
	sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
            | awk -F, 'FNR == 2 {print $5}' | xargs echo -n >> "$logfile"
	echo -n "," >> "$logfile"
	# writing projections
	sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
            | awk -F, 'FNR == 2 {print $6}' >> "$logfile"
	echo "finished " $name $c " " $input_size
    done
}

function run_benchmark()
{
    local name="$1";          shift
    local c1="$1";            shift
    local c2="$1";            shift
    local -n _input_sizes=$1; shift
    local -n _inputs=$1;      shift
    local -n _outputs=$1;     shift
    
    local logfile1="$DATA_DIR/${name}${c1}.csv"
    local logfile2="$DATA_DIR/${name}${c2}.csv"

    cd ${GRIFT_DIR}

    racket "${GRIFT_DIR}/benchmark/benchmark.rkt" --cast-profiler -s "$c1 $c2" "${TMP_DIR}/partial/${name}"

    cd "${TMP_DIR}/partial/${name}"
    
    run_config "$c1" _input_sizes _inputs _outputs "$logfile1"
    run_config "$c2" _input_sizes _inputs _outputs "$logfile2"
}

function run_experiment()
{
    local c1="$1"; shift
    local c2="$1"; shift

    local input_sizes=()
    local inputs=()
    local outputs=()

    local inputs=(10000000 20000000 30000000 40000000 50000000 60000000 70000000 80000000 90000000 100000000 200000000 300000000)
    for n in ${inputs[*]}; do
	input_sizes+=("$n")
	outputs+=("#t")
    done
    run_benchmark "cps-even-odd" $c1 $c2 input_sizes inputs outputs
    
    ranges=(10 100 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)
    input_sizes=()
    inputs=()
    outputs=()
    for n in ${ranges[*]}; do
	input=$n;
	for ((i=$n-1;i>=0;i--)); do
	    input="$input $i";
	done
	inputs+=("$input")
	input_sizes+=("$n")
	outputs+=($(($n-1)))
    done

    run_benchmark quicksort $c1 $c2 input_sizes inputs outputs
}

function main()
{
    . lib/runtime.sh

    c1=17
    c2=7

    LOOPS=1
    PRECISION=5

    date=`date +%Y_%m_%d_%H_%M_%S`

    TEST_DIR=${GRIFT_DIR}/benchmark/suite/macro
    LIB_DIR="$TEST_DIR/lib"
    SPACE_DIR=$TEST_DIR/space/$date
    DATA_DIR=$SPACE_DIR/data
    OUT_DIR=$SPACE_DIR/output
    TMP_DIR=$SPACE_DIR/tmp
    SRC_DIR=$TEST_DIR/src/partial

    # create the result directory if it does not exist
    mkdir -p $DATA_DIR
    mkdir -p $TMP_DIR
    mkdir -p $OUT_DIR

    cp -r ${SRC_DIR} $TMP_DIR

    run_experiment $c1 $c2
}

main "$@"
