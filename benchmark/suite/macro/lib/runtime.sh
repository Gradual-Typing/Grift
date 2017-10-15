
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_racket_runtime()
{
    local benchmark="$1";      shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/racket/internal/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	raco make "${benchmark_path}.rkt"
	racket_avg "racket ${benchmark_path}.rkt" "$benchmark_args" "$runtimes_file" "${benchmark_path}.tmp.txt"
	echo "$RETURN" > "$cache_file"
    fi
}

# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_typed_racket_runtime()
{
    local benchmark="$1";      shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/typed_racket/internal/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	raco make "${benchmark_path}.rkt"
	racket_avg "racket ${benchmark_path}.rkt" "$benchmark_args" "$runtimes_file" "${benchmark_path}.tmp.txt"
	echo "$RETURN" > "$cache_file"
    fi
}

# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_ocaml_runtime()
{
    local benchmark="$1";      shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/ocaml/ocaml_${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	ocamlopt -O3 -o "${benchmark_path}" "${benchmark_path}.ml"
    avg "${benchmark_path}" "$benchmark_args" "$runtimes_file"
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
	avg "${TMP_DIR}/chezscheme/./${benchmark}.so" "$benchmark_args" "$runtimes_file"
	echo "$RETURN" > "$cache_file"
    fi
}


# run the static variant of the schml compiler on a static
# benchmark and return the average runtime creating
# a couple utility files along the way.
# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the average runtime
get_static_schml_runtime()
{
    local benchmark="$1";      shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/static/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        "${SCHML_DIR}/main.rkt" \
               --static -O 3\
               -o "${benchmark_path}.o" \
               "${benchmark_path}.schml"
        
	local configs=($(racket "${SCHML_DIR}/benchmark/config_str.rkt" -i))
        avg "${benchmark_path}.o" "$benchmark_args" "$runtimes_file"
        echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark file path without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the Grift benchmark
get_grift_runtimes()
{
    local benchmark_path="$1"; shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift

    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        readarray RETURN < "$cache_file"
    else
	racket "${GRIFT_DIR}/benchmark/benchmark.rkt" "${benchmark_path}.grift"
	local configs=($(racket "${GRIFT_DIR}/benchmark/config_str.rkt" -i))
	for i in ${configs[@]}; do
	    avg "${benchmark_path}.o${i}" "$benchmark_args" "$runtimes_file"
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
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    local config_index="$1";   shift

    local runtimes_file="${benchmark_path}${disk_aux_name}${config_index}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}${config_index}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	racket "${GRIFT_DIR}/benchmark/benchmark.rkt" --config $config_index "${benchmark_path}.grift"
	avg "${benchmark_path}.o${config_index}" "$benchmark_args" "$runtimes_file"
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the slowdown factor for the C benchmark
get_c_runtime()
{
    local benchmark="$1";      shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/c/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	clang "${benchmark_path}.c" "${GRIFT_DIR}/src/backend-c/runtime/boehm-gc-install/lib/libgc.a" -I"${GRIFT_DIR}/src/backend-c/runtime/boehm-gc-install/include" -pthread -lm -O3 -o "${benchmark_path}.o"
	avg "${benchmark_path}.o" "$benchmark_args" "$runtimes_file"
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
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/gambit/${benchmark}"
    local runtimes_file="${benchmark_path}${disk_aux_name}.runtimes"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	gsc -prelude "(declare (standard-bindings) (block))" -exe -cc-options -O3 "${benchmark_path}.scm"
	avg "${benchmark_path}" "$benchmark_args" "$runtimes_file"
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
# $3 - space-separated benchmark arguments
# $4 - disk aux name
# $5 - configuration index
# $RETURN - the runtime for the racket benchmark
get_grift_slowdown()
{
    local baseline_system="$1"; shift
    local benchmark_path="$1";  shift
    local benchmark_args="$1";  shift
    local disk_aux_name="$1";   shift
    local config_index="$1";    shift
    
    local cache_file="${benchmark_path}${disk_aux_name}${config_index}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	local benchmark=$(basename "$benchmark_path")
	$baseline_system "$benchmark" "$benchmark_args" "$disk_aux_name"
	local baseline="$RETURN" st sr;
	get_grift_runtime "$benchmark_path" "$benchmark_args" "$disk_aux_name" $config_index
	local st="$RETURN";
	RETURN=$(echo "${st} ${baseline}" | awk '{printf "%.2f", $1 \ $2}')
	echo "$RETURN" >> "$cache_file"
    fi
}

# $1 - baseline system
# $2 - benchmark file path without extension
# $3 - space-separated benchmark arguments
# $4 - disk aux name
# $5 - configuration index
# $RETURN - the runtime for the racket benchmark
get_grift_speedup()
{
    local baseline_system="$1"; shift
    local benchmark_path="$1";  shift
    local benchmark_args="$1";  shift
    local disk_aux_name="$1";   shift
    local config_index="$1";    shift

    # Is this cache_file supose to be called .slowdown_ ?
    local cache_file="${benchmark_path}${disk_aux_name}${config_index}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	local benchmark=$(basename "$benchmark_path")
	$baseline_system "$benchmark" "$benchmark_args" "$disk_aux_name"
	local baseline="$RETURN" st sr;
	get_grift_runtime "$benchmark_path" "$benchmark_args" "$disk_aux_name" $config_index
	local st="$RETURN";
	RETURN=$(echo "${baseline} ${st}" | awk '{printf "%.2f\n", $1 / $2}')
	echo "$RETURN" >> "$cache_file"
    fi
}

# $1 - system
# $2 - baseline system
# $3 - benchmark filename without extension
# $4 - space-separated benchmark arguments
# $5 - disk aux name
# $6 - aux information
# $RETURN - the slowdown factor for that system benchmark
get_slowdown()
{
    local system="$1";          shift
    local baseline_system="$1"; shift
    local benchmark="$1";       shift
    local benchmark_args="$1";  shift
    local disk_aux_name="$1";   shift
    
    local benchmark_path="${TMP_DIR}/${system}/${benchmark}"
    local cache_file="${benchmark_path}${disk_aux_name}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	$baseline_system "$benchmark" "$benchmark_args" "$disk_aux_name"
	local baseline="$RETURN";
	"get_${system}_runtime" "$benchmark" "$benchmark_args" "$disk_aux_name"
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
    local benchmark_args="$1";  shift
    local disk_aux_name="$1";   shift
    
    local benchmark_path="${TMP_DIR}/${system}/${benchmark}"
    local cache_file="${benchmark_path}${disk_aux_name}.slowdown_${baseline_system}"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	$baseline_system "$benchmark" "$benchmark_args" "$disk_aux_name"
	local baseline="$RETURN";
	"get_${system}_runtime" "$benchmark" "$benchmark_args" "$disk_aux_name"
	local ct="$RETURN"
	RETURN=$(echo "${baseline} ${ct}" | awk '{printf "%.2f", $1 / $2}')
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - binary to run
# $2 - stdin arguments
# $3 - path to the file that will record individual runtimes
# $RETURN - average runtime
avg()
{
    local bin="$1";           shift
    local arg="$1";           shift
    local runtimes_file="$1"; shift
    local c="time ( echo ${arg} | ${bin} ) 2>&1 1>/dev/null"
    local avg_sum=0.0
    local avg_i avg_t
    for avg_i in `seq $LOOPS`; do
	avg_t=$(eval "$c")
	echo $avg_t >> "$runtimes_file"
	avg_sum=$(echo "${avg_sum} ${avg_t}" | awk -v p="$PRECISION" '{printf "%.*f", p, $1 + $2}')
    done
    RETURN=$(echo "${avg_sum} ${LOOPS}" | awk -v p="$PRECISION" '{printf "%.*f", p, $1 / $2}')
}

# $1 - binary to run
# $2 - stdin arguments
# $3 - path to the file that will record individual runtimes
# $RETURN - average runtime
racket_avg()
{
    local bin="$1";             shift
    local arg="$1";             shift
    local runtimes_file="$1";   shift
    local output_tmp_file="$1"; shift
    local c="echo ${arg} | ${bin}"
    local avg_sum=0.0
    local avg_i avg_t output
    for avg_i in `seq $LOOPS`; do
    
    $(eval "$c > ${output_tmp_file}")
    avg_t=$(eval "cat ${output_tmp_file} | racket ${TEST_DIR}/lib/parse-racket-time.rkt")
	echo $avg_t >> "$runtimes_file"
	avg_sum=$(echo "${avg_sum} ${avg_t}" | awk -v p="$PRECISION" '{printf "%.*f", p, $1 + $2}')
    done
    RETURN=$(echo "${avg_sum} ${LOOPS}" | awk -v p="$PRECISION" '{printf "%.*f", p, $1 / $2}')
}

# $1 - logfile
# $RETURN - geometric mean of the fifth column in the logfile
speedup_geometric_mean()
{
    local logfile="$1"; shift
    RETURN=$(cat "$logfile" | sed 1d | cut -d, -f5 | "${TEST_DIR}/lib/geo")
}
