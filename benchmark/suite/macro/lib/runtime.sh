# $1 - Name of the benchmark
# $2 - file containing the input to the benchmark
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_racket_runtime()
{
    # name of the benchmark
    local benchmark="$1";      shift
    # input file
    local input="$1"; shift
    # I am not sure what this is. -andre
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/racket/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        raco make "${benchmark_path}.rkt"
        avg "racket ${benchmark_path}.rkt"\
            "${INPUT_DIR}/${benchmark}/${input}"\
            "racket" "${OUTPUT_DIR}/racket/${benchmark}/${input}" \
            "$runtimes_file"
        echo "$RETURN" > "$cache_file"
    fi
}

# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_typed_racket_runtime()
{
    local benchmark="$1";      shift
    local input="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/typed_racket/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        raco make "${benchmark_path}.rkt"
        avg "racket ${benchmark_path}.rkt"\
            "${INPUT_DIR}/${benchmark}/${input}"\
            "typed_racket" "${OUTPUT_DIR}/typed_racket/${benchmark}/${input}"\
            "$runtimes_file"
        echo "$RETURN" > "$cache_file"
    fi
}

# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_ocaml_runtime()
{
    local benchmark="$1";      shift
    local input="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/ocaml/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	make -C "$(dirname $benchmark_path)"
	avg "${benchmark_path}"\
            "${INPUT_DIR}/${benchmark}/${input}"\
            "ocaml" "${OUTPUT_DIR}/ocaml/${benchmark}/${input}"\
            "$runtimes_file"
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_chezscheme_runtime()
{
    local benchmark="$1";      shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/chezscheme/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        echo "(compile-program \"${benchmark_path}.ss\")" | scheme -q
        avg "${benchmark_path}.so" "${INPUT_DIR}/${benchmark}/${input}"\
            "chezscheme" "${OUTPUT_DIR}/chezscheme/${benchmark}/${input}"\
            "$runtimes_file"
        echo "$RETURN" > "$cache_file"
    fi
}


# run the static variant of the grift compiler on a static
# benchmark and return the average runtime creating
# a couple utility files along the way.
# It relies on the TMP_DIR variable being set and the static
# varient of the benchmark being located in TMP_DIR/static/
# directory.
# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the average runtime
get_static_grift_runtime()
{
    local benchmark="$1";      shift
    local input="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/static/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.static.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.static.runtime"

    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        "${GRIFT_DIR}/main.rkt" \
            "--static" \
            "-O" "3" \
            "-o" "${benchmark_path}.static" \
            "${benchmark_path}.grift"

        avg "${benchmark_path}.static" "${INPUT_DIR}/${benchmark}/${input}"\
            "static" "${OUTPUT_DIR}/static/${benchmark}/${input}"\
            "$runtimes_file"
        
        echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark file path without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the Grift benchmark
# This function is out of date and isn't used anywhere
get_grift_runtimes()
{
    local benchmark_path="$1"; shift
    local input="$1"; shift
    local disk_aux_name="$1";  shift

    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        readarray RETURN < "$cache_file"
    else
	racket "${GRIFT_DIR}/benchmark/benchmark.rkt" "${benchmark_path}.grift"
        local configs=($(racket "${GRIFT_DIR}/benchmark/config_str.rkt" -i))
        for i in ${configs[@]}; do
            avg "${benchmark_path}.o${i}" "${input}" "$runtimes_file"
            echo "$RETURN" >> "$cache_file"
        done
        readarray RETURN < "$cache_file"
    fi
}

# $1 - benchmark file path without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $4 - configuration index
# $RETURN - the runtime for the Grift benchmark
get_grift_runtime()
{
    local benchmark_path="$1"; shift
    local input="$1"; shift
    local disk_aux_name="$1";  shift
    local config_index="$1";   shift
    local benchmark="$(basename $benchmark_path)"
    
    local runtimes_file="${benchmark_path}${disk_aux_name}${config_index}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}${config_index}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        racket "${GRIFT_DIR}/benchmark/benchmark.rkt"\
               --config $config_index "${benchmark_path}.grift"
        avg "${benchmark_path}.o${config_index}"\
            "${INPUT_DIR}/${benchmark}/${input}"\
            "static" "${OUTPUT_DIR}/static/${benchmark}/${input}"\
            "$runtimes_file"
        echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the slowdown factor for the C benchmark
# This function is out of date and we no longer compare against c
get_c_runtime()
{
    local benchmark="$1";      shift
    local input="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/c/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        clang "${benchmark_path}.c" "${GRIFT_DIR}/src/backend-c/runtime/boehm-gc-install/lib/libgc.a" -I"${GRIFT_DIR}/src/backend-c/runtime/boehm-gc-install/include" -pthread -lm -O3 -o "${benchmark_path}.o"
        avg "${benchmark_path}.o" "${input}" "$runtimes_file"
        echo "$RETURN" > "$cache_file"
    fi
}



# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the slowdown factor for the C benchmark
get_gambit_runtime()
{
    local benchmark="$1";      shift
    local input="$1";          shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/gambit/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        gsc -prelude "(declare (standard-bindings) (block))"\
            -exe -cc-options -O3 "${benchmark_path}.scm"
        avg "${benchmark_path}" "${INPUT_DIR}/${benchmark}/${input}"\
            "gambit" "${OUTPUT_DIR}/gambit/${benchmark}/${input}" \
            "$runtimes_file"
        echo "$RETURN" > "$cache_file"
    fi
}

# $1 - baseline system
# $2 - benchmark file path without extension
# $3 - space-separated benchmark arguments
# $4 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_grift_slowdowns()
{
    local baseline_system="$1"; shift
    local benchmark_path="$1";  shift
    local benchmark_args="$1";  shift
    local disk_aux_name="$1";   shift
    
    local cache_file="${benchmark_path}${disk_aux_name}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        readarray RETURN < "$cache_file"
    else
        local benchmark=$(basename "$benchmark_path")
        $baseline_system "$benchmark" "$benchmark_args" "$disk_aux_name"
        local baseline="$RETURN" st sr;
        get_grift_runtimes "$benchmark_path" "$benchmark_args" "$disk_aux_name"
        st=${RETURN[@]}
        for st in ${st[@]}; do
            sr=$(echo "${st}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n",p, $0}')
            RETURN=$(echo "$sr" | awk '{printf "%.2f\n",$0}')
            echo "$RETURN" >> "$cache_file"
        done
        readarray RETURN < "$cache_file"
    fi
}

# $1 - baseline system
# $2 - benchmark file path without extension
# $3 - file containing the benchmark input
# $4 - disk aux name
# $5 - configuration index
# $RETURN - the runtime for the racket benchmark
get_grift_slowdown()
{
    local baseline_system="$1"; shift
    local benchmark_path="$1";  shift
    local input="$1";  shift
    local disk_aux_name="$1";   shift
    local config_index="$1";    shift
    
    local name="${benchmark_path}${disk_aux_name}${config_index}"
    local cache_file="${name}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        local benchmark=$(basename "$benchmark_path")
        $baseline_system "$benchmark" "${input}" "$disk_aux_name"
        local baseline="$RETURN" st sr;
        get_grift_runtime "$benchmark_path" "${input}"\
			  "$disk_aux_name" $config_index
        local st="$RETURN";
        RETURN=$(echo "${st} ${baseline}" | awk '{printf "%.2f", $1 \ $2}')
        echo "$RETURN" >> "$cache_file"
    fi
}

# $1 - baseline system
# $2 - benchmark file path without extension
# $3 - file containing the benchmark input
# $4 - disk aux name
# $5 - configuration index
# $RETURN - the runtime for the racket benchmark
get_grift_speedup()
{
    local baseline_system="$1"; shift
    local benchmark_path="$1";  shift
    local input="$1";  shift
    local disk_aux_name="$1";   shift
    local config_index="$1";    shift

    # Is this cache_file supose to be called .slowdown_ ?
    local name="${benchmark_path}${disk_aux_name}${config_index}"
    local cache_file="${name}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        local benchmark=$(basename "$benchmark_path")
        $baseline_system "$benchmark" "${input}" "$disk_aux_name"
        local baseline="$RETURN" st sr;
        get_grift_runtime "$benchmark_path" "${input}" "$disk_aux_name" $config_index
        local st="$RETURN";
        RETURN=$(echo "${baseline} ${st}" | awk '{printf "%.2f\n", $1 / $2}')
        echo "$RETURN" >> "$cache_file"
    fi
}

# $1 - system
# $2 - baseline system
# $3 - benchmark filename without extension
# $4 - file containing the input to the benchmark
# $5 - disk aux name
# $6 - aux information
# $RETURN - the slowdown factor for that system benchmark
get_slowdown()
{
    local system="$1";          shift
    local baseline_system="$1"; shift
    local benchmark="$1";       shift
    local input="$1";  shift
    local disk_aux_name="$1";   shift
    
    local name="${TMP_DIR}/${system}/${benchmark}${disk_aux_name}"
    local cache_file="${name}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        $baseline_system "$benchmark" "${input}" "$disk_aux_name"
        local baseline="$RETURN";
        "get_${system}_runtime" "$benchmark" "${input}" "$disk_aux_name"
        local ct="$RETURN"
        local cr=$(echo "${ct}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n",p, $0}')
        RETURN=$(echo "$cr" | awk '{printf "%.2f\n",$0}')
        echo "$RETURN" > "$cache_file"
    fi
}

# $1 - system
# $2 - baseline system
# $3 - benchmark filename without extension
# $4 - space-separated benchmark arguments
# $5 - disk aux name
# $6 - aux information
# $RETURN - the slowdown factor for that system benchmark
get_speedup()
{
    local system="$1";          shift
    local baseline_system="$1"; shift
    local benchmark="$1";       shift
    local input="$1";  shift
    local disk_aux_name="$1";   shift
    
    local name="${TMP_DIR}/${system}/${benchmark}${disk_aux_name}"
    local cache_file="${name}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        $baseline_system "$benchmark" "${input}" "$disk_aux_name"
        local baseline="$RETURN";
        "get_${system}_runtime" "$benchmark" "${input}" "$disk_aux_name"
        local ct="$RETURN"
        RETURN=$(echo "${baseline} ${ct}" | awk '{printf "%.2f", $1 / $2}')
        echo "$RETURN" > "$cache_file"
    fi
}

# $RETURN - average runtime
avg()
{
    # $1 A shell command that will execute the benchmark
    # This command cannot redirect the io of the benchmark
    local cmd="$1";             shift

    # $2 A path to the input communicated to the benchmark on stdin
    local input_file="$1";      shift

    # $3 The language being benchmarked
    local lang="$1";      shift

    # $4 A path to the expected output of the benchmark on stdout
    local output_file="$1";     shift
    
    # $5 A path to the file where the runtimes will be saved
    # this path is also used to make tempory files to save io.
    local runtimes_file="$1";   shift
    
    # extend the command to write to standard temp files and pipe input
    local tmp_out="${runtimes_file}.tmp.out"
    local tmp_err="${runtimes_file}.tmp.err"
    run_cmd="cat ${input_file} | ${cmd} 1> ${tmp_out} 2> ${tmp_err}"
    
    
    local loops_done="0"
    # if the there is no file then we have done no loops
    if [ -f $runtimes_file ]; then
        # one loop per line
        loops_done="$(cat ${runtimes_file} | sed '/^\s*$/d' | wc -l)"
    fi
    
    # if not enough runtimes have occurred
    for avg_i in `seq $(expr $LOOPS - ${loops_done})`; do
        eval "$run_cmd"
        local cmd_status=$?
        if [ $cmd_status -ne 0 ]; then
            echo "benchmark $cmd errored" 1>&2
            cat ${tmp_err} 1>&2
            exit 1
        else
            local pt_out="$(racket ${LIB_DIR}/parse-time.rkt \
                                   --lang ${lang} \
                                   --in ${tmp_out} \
                                   --expect ${output_file})"
            local pt_status=$?
            if [ $pt_status -ne 0 ] || [ -z $pt_out ]; then
                echo "Failed to parse output of $cmd" 1>&2
                exit 1
            else
                rm $tmp_out $tmp_err
                echo $pt_out >> $runtimes_file
            fi
        fi
    done
    
    RETURN="$(racket ${LIB_DIR}/mean.rkt -d $PRECISION -f $runtimes_file)"
    if [ $? -ne 0 ] || [ -z $RETURN ]; then
        echo "Failed mean" 1>&2
        exit 1
    fi
}

# $1 - logfile
# $2 - column index
# $RETURN - geometric mean of the fifth column in the logfile
geometric_mean()
{
    local logfile="$1"; shift
    local indx="$1";    shift
    RETURN=$(cat "$logfile" | sed 1d | cut -d, -f$indx | "${TEST_DIR}/lib/geo")
}

mean()
{
    local logfile="$1"; shift
    local indx="$1";    shift
    RETURN=$(cat "$logfile1" | sed 1d | awk -F',' -v indx=$indx '{sum+=$indx; ++n} END { print sum/n }')
}

runtime_mean()
{
    local logfile="$1"; shift
    mean "$logfile" 3
}

speedup_geometric_mean()
{
    local logfile="$1"; shift
    geometric_mean "$logfile" 5
}

runtime_geometric_mean()
{
    local logfile="$1"; shift
    geometric_mean "$logfile" 5
}
