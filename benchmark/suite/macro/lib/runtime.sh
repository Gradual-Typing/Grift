# $1 - benchmark filename without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_racket_runtime()
{
    local benchmark="$1";      shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/rkt/${benchmark}"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
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
# $RETURN - the runtime for the racket benchmark
get_chezscheme_runtime()
{
    local benchmark="$1";      shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local benchmark_path="${TMP_DIR}/chezscheme/${benchmark}"
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	echo "(compile-program \"${benchmark_path}.ss\")" | scheme -q
	avg "${TMP_DIR}/chezscheme/./${benchmark}.so" "$benchmark_args"
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - benchmark file path without extension
# $2 - space-separated benchmark arguments
# $3 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_schml_runtime()
{
    local benchmark_path="$1"; shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        readarray RETURN < "$cache_file"
    else
	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${benchmark_path}.schml"
	for i in ${SCHML_CONFIGS[@]}; do
	    avg "${benchmark_path}.o${i}" "$benchmark_args"
	    echo "$RETURN" >> "$cache_file"
	done
	readarray RETURN < "$cache_file"
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
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	clang "${benchmark_path}.c" "${SCHML_DIR}/src/backend-c/runtime/boehm-gc-install/lib/libgc.a" -I"${SCHML_DIR}/src/backend-c/runtime/boehm-gc-install/include" -pthread -lm -O3 -o "${benchmark_path}.o"
	avg "${benchmark_path}.o" "$benchmark_args"
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
    local cache_file="${benchmark_path}${disk_aux_name}.runtime"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	gsc -exe -cc-options -O3 "${benchmark_path}.scm"
	avg "${benchmark_path}" "$benchmark_args"
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - baseline system
# $2 - benchmark file path without extension
# $3 - space-separated benchmark arguments
# $4 - disk aux name
# $RETURN - the runtime for the racket benchmark
get_schml_slowdown()
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
	get_schml_runtime "$benchmark_path" "$benchmark_args" "$disk_aux_name"
	st=${RETURN[@]}
	for st in ${st[@]}; do
	    sr=$(echo "${st}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n",p, $0}')
	    RETURN=$(echo "$sr" | awk '{printf "%.2f\n",$0}')
	    echo "$RETURN" >> "$cache_file"
	done
	readarray RETURN < "$cache_file"
    fi
}

# $1 - system
# $2 - baseline system
# $3 - benchmark filename without extension
# $4 - space-separated benchmark arguments
# $5 - disk aux name
# $RETURN - the slowdown factor for the C benchmark
get_slowdown()
{
    local system="$1";          shift
    local baseline_system="$1"; shift
    local benchmark="$1";       shift
    local benchmark_args="$1";  shift
    local disk_aux_name="$1";   shift
    
    local benchmark_path="${TMP_DIR}/gambit/${benchmark}"
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
