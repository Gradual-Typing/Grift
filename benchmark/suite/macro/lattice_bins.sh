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
	echo "name,precision,time,slowdown,speedup" > "$logfile1"
	for b in $(find "$path" -name "*.o$i"); do
	    let n=n+1
	    local binpath="${b%.*}"
	    local bname=$(basename "$binpath")
	    local p=$(sed -n 's/;; \([0-9]*.[0-9]*\)%/\1/p;q' < "${binpath}.grift")
	    echo $b
	    avg "$b" "$args" "${b}.runtimes"
	    local t="$RETURN"
	    local speedup=$(echo "${baseline}/${t}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n", p,$0}')
	    local slowdown=$(echo "${t}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n", p,$0}')
	    echo $n $b $speedup
	    printf "%s,%.2f,%.${PRECISION}f,%.2f,%.2f\n" $bname $p $t $slowdown $speedup >> $logfile1
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
	get_grift_runtime "${TMP_DIR}/static/${name}" "$benchmark_args" "$disk_aux_name" $i
	t11="$RETURN"
	get_grift_runtime "${TMP_DIR}/dyn/${name}" "$benchmark_args" "$disk_aux_name" $i
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

    local config_str=$(racket "${GRIFT_DIR}/benchmark/config_str.rkt" -c $c1 $c2)
    local c1t=$(echo $config_str | sed -n 's/\(.*\),.*,.*/\1/p;q')
    local c2t=$(echo $config_str | sed -n 's/.*,\(.*\),.*/\1/p;q')
    local ct=$(echo $config_str | sed -n 's/.*,.*,\(.*\)/\1/p;q')
    local disk_name="${name}${disk_aux_name}_$(echo "$ct" | tr " " "_")"
    
    cum_perf_lattice_fig="${OUT_DIR}/cumperflattice/${disk_name}.png"
    cum_perf_lattice_tbl="${OUT_DIR}/cumperflattice/${disk_name}.tex"
    perf_lattice_slowdown_fig="${OUT_DIR}/perflattice/slowdown/${disk_name}.png"
    perf_lattice_speedup_fig="${OUT_DIR}/perflattice/speedup/${disk_name}.png"
    perf_lattice_log_fig="${OUT_DIR}/perflattice/log/${disk_name}.png"
    perf_lattice_lin_fig="${OUT_DIR}/perflattice/log/lin_${disk_name}.png"
    
    # if [[ ! -f "$cum_perf_lattice_slowdown_fig" || ! -f "$cum_perf_lattice_tbl" || ! -f "$perf_lattice_slowdown_fig" ]]; then
    rm -f "$cum_perf_lattice_fig" "$cum_perf_lattice_tbl" "$perf_lattice_slowdown_fig"
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${c1}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${c1}.csv"
    local logfile3="${DATA_DIR}/${name}${disk_aux_name}${c2}.log"
    local logfile4="${DATA_DIR}/${name}${disk_aux_name}${c2}.csv"

    
    # get_speedup static_schml $baseline_system "$name" "$benchmark_args" "$disk_aux_name"
    # local cr_t="$RETURN"
    get_speedup gambit $baseline_system "$name" "$benchmark_args" "$disk_aux_name"
    local gr_t="$RETURN"

    compute_typed_untyped_ratio "$c1" "$name" "$benchmark_args" "$disk_aux_name"
    local typed_untyped_ratio1="$RETURN"
    compute_typed_untyped_ratio "$c2" "$name" "$benchmark_args" "$disk_aux_name"
    local typed_untyped_ratio2="$RETURN"

    run_config $baseline_system "$c1" "$path" "$benchmark_args" "$disk_aux_name"
    run_config $baseline_system "$c2" "$path" "$benchmark_args" "$disk_aux_name"
    local n="$RETURN"

    speedup_geometric_mean "$logfile1"
    g1="$RETURN"
    speedup_geometric_mean "$logfile3"
    g2="$RETURN"

    $baseline_system "$name" "$benchmark_args" "$disk_aux_name"
    local baseline_mean="$RETURN"

    get_static_schml_runtime "$name" "$benchmark_args" "$disk_aux_name"
    local static_mean="$RETURN"
    local static_speed_up=$(echo "${baseline_mean} ${static_mean}" | awk '{printf "%.2f", $1 / $2}')
    
    printf "%s:\t\t%d=%.2f\t%d=%.2f\n" $name $c1 $g1 $c2 $g2
    
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
    	   `"plot '${logfile2}' using 1:2 with lines lw 2 dt 4 title '${c1t}' smooth cumulative,"`
    	   `"'${logfile4}' using 1:2 with lines lw 2 dt 2 title '${c2t}' smooth cumulative"

    echo "\begin{tabular}{|l|l|l|}
\hline
\textbf{${printname}} & \multicolumn{2}{l|}{(${type_constructor_count} type nodes)} \\\ \hline
lattice size                & \multicolumn{2}{l|}{${lpc_t} B}         \\\ \hline
\multicolumn{3}{|l|}{}                                             \\\ \hline
Static Grift            & \multicolumn{2}{l|}{${static_speed_up}x}           \\\ \hline
Gambit-C                    & \multicolumn{2}{l|}{${gr_t}x}           \\\ \hline
\multicolumn{3}{|l|}{}                                             \\\ \hline
typed/untyped ratio         & ${typed_untyped_ratio2}x             & ${typed_untyped_ratio1}x            \\\ \hline
min. speedup               & ${min2}x             & ${min1}x            \\\ \hline
max. speedup               & ${max2}x             & ${max1}x            \\\ \hline
mean speedup               & ${mean2}x             & ${mean1}x            \\\ \hline
\end{tabular}
\end{table}" > "${cum_perf_lattice_tbl}"

    # gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
    #   	   `"enhanced color font 'Verdana,10' ;"`
    # 	   `"set output '${perf_lattice_slowdown_fig}';"`
	#        `"set key left;"`
    # 	   `"set xrange [0:100];"`
	#        `"set ytics add (\"\" 0, \"1\" 1);"`
	#        `"set ytics font \", 13\";"`
	#        `"set xtics font \", 13\";"`
	#        `"set grid ytics;"`
    # 	   `"set title \"${printname}\";"`
    #        `"set ylabel \"Slowdown with respect to Gambit\";"`
	#        `"set xlabel \"How much of the code is typed\";"`
    #        `"plot '${logfile1}' using 2:4 with points pointtype 6 lc rgb \"#3182bd\" title '${c1t}',"`
    #        `"'${logfile3}' using 2:4 with points pointtype 8 lc rgb \"#fdae6b\" title '${c2t}'"

    # gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
    #     	   `"enhanced color font 'Verdana,13' ;"`
    #            `"set output '${perf_lattice_speedup_fig}';"`
    #            `"set key outside center bottom font \",10\";"`
    # 	       `"set xrange [-5:105];"`
    # 	       `"set yrange [0.1:4];"`
	#            `"set ytics add (\"1\" 1, \"\" ${g1}, \"\" ${g2});"`
    # 	       `"set title \"${printname}\";"`
	#            `"set ylabel \"Speedup over Gambit\";"`
	#            `"set xlabel \"How much of the code is typed\";"`
	#            `"plot '${logfile1}' using 2:5 with points pointtype 6 lc rgb \"#0072B2\" title '${c1t}',"`
    # 	       `"'${logfile3}' using 2:5 with points pointtype 8 lc rgb \"#E69F00\" title '${c2t}',"`
	#            `"${g1} lw 2 dt 3 lc rgb \"#0072B2\" notitle '${c1t} mean',"`
	#            `"${g2} lw 2 dt 3 lc rgb \"#E69F00\" notitle '${c2t} mean',"`
    #            `"1 lw 2 dt 2 lc rgb \"black\" title 'No Gradual Overhead (Gambit Scheme)',"` 
    #            `"${static_speed_up} lw 1 dt 2 lc \"black\" title 'No Gradual Overhead (Grift STLC)';"
    
    
     gnuplot -e "set datafile separator \",\";"`
      	   `"set terminal pngcairo size 1280,960"`
              `" noenhanced color font 'Verdana,26' ;"`
    	   `"set output '${perf_lattice_log_fig}';"`
	       `"set key left font 'Verdana,20';"`
	       `"set logscale y;"`
    	   `"set xrange [-5:105];"`
    	   `"set yrange [0.01:100];"`
	       `"set ytics add (\"1\" 1, \"\" ${g1}, \"\" ${g2});"`
    	   `"set title \"${printname}\";"`
	       `"set ylabel \"Speedup over Gambit (Log scale)\";"`
	       `"set xlabel \"How much of the code is typed\";"`
	       `"plot '${logfile1}' using 2:5 with points"`
              `" pt 9 ps 3 lc rgb '#fdb863' title '${c1t}',"`
	       `"${g1} lw 4 dt 2 lc rgb '#fdb863' notitle '${c1t} mean',"`
    	   `"'${logfile3}' using 2:5 with points"`
              `" pt 6 ps 3 lc rgb '#5e3c99' title '${c2t}',"`
	       `"${g2} lw 4 dt 4 lc rgb '#5e3c99' notitle '${c2t} mean',"`
           `"1 lw 2 dt 2 lc rgb \"black\" title 'Gambit Scheme',"` 
           `"${static_speed_up} lw 1 dt 2 lc \"black\" title 'Static Grift';"


     gnuplot -e "set datafile separator \",\";"`
           `"set terminal pngcairo size 1280,960"`
                `" noenhanced color font 'Verdana,26' ;"`
    	   `"set output '${perf_lattice_lin_fig}';"`
	       `"set key left font 'Verdana,20';"`
    	   `"set xrange [-5:105];"`
           `"set xrange [0:];"`
	       `"set ytics add (\"1\" 1, \"\" ${g1}, \"\" ${g2});"`
    	   `"set title \"${printname}\";"`
	       `"set ylabel \"Speedup over Gambit\";"`
	       `"set xlabel \"How much of the code is typed\";"`
	       `"plot '${logfile1}' using 2:5 with points"` 
              `" pt 9 ps 3 lc rgb '#fdb863' title '${c1t}',"`
	       `"${g1} lw 4 dt 2 lc rgb '#fdb863' title '${c1t} mean',"`
    	   `"'${logfile3}' using 2:5 with points"`
              `" pt 6 ps 3 lc rgb '#5e3c99' title '${c2t}',"`
	       `"${g2} lw 4 dt 4 lc rgb '#5e3c99'  title '${c2t} mean',"`
           `"1 lw 2 dt 4 lc rgb \"black\" title 'Gambit Scheme',"` 
           `"${static_speed_up} lw 2 dt 2 lc \"black\" title 'Static Grift';"
     
    # RETURN=$(awk -v g1="$g1" -v g2="$g2" "BEGIN {printf \"%.2f\n\", g2/g1}")
}

##3182bd
##fdae6b

# $1 - baseline system
# $2 - first config index
# $3 - second config index
# $4 - benchmark filename without extension
# $5 - space-separated benchmark arguments
# $6 - nsamples
# $7 - nbins
# $8 - aux name
run_benchmark()
{
    local baseline_system="$1"; shift
    local c1="$1";              shift
    local c2="$1";              shift
    local name="$1";            shift
    local benchmark_args="$1";  shift
    local nsamples="$1";        shift
    local nbins="$1";           shift
    local aux_name="$1";        shift

    local lattice_path="${TMP_DIR}/partial/${name}"
    local benchmarks_path="${TMP_DIR}/static"
    local static_source_file="${benchmarks_path}/${name}.grift"
    local dyn_source_file="${TMP_DIR}/dyn/${name}.grift"

    local disk_aux_name="" print_aux_name=""
    if [[ ! -z "${aux_name}" ]]; then
	disk_aux_name="_${aux_name}"
	print_aux_name=" (${aux_name})"
    fi
    
    local print_name="$(echo "$name" | tr _ "-")${print_aux_name}"

    local benchmark_args_file="${TMP_DIR}/${name}${disk_aux_name}.args"
    if [ -f benchmark_args_file ]; then
	benchmark_args=$(cat "$benchmark_args_file")
    else
	printf "Benchmark\t:%s\n" "$name" >> "$PARAMS_LOG"
	printf "Args\t\t:%s\n" "$benchmark_args" >> "$PARAMS_LOG"
	echo "$benchmark_args" > "$benchmark_args_file"
    fi

    local lattice_file="${lattice_path}/out"
    if [ -f "$lattice_file" ]; then
    local dynamizer_out=$(cat "$lattice_file")
    else
    rm -rf "$lattice_path"
    rm -f "${lattice_path}.grift"
    cp "$static_source_file" "${lattice_path}.grift"
	local dynamizer_out=$(dynamizer "${lattice_path}.grift" "$nsamples" "$nbins" | sed -n 's/.* \([0-9]\+\) .* \([0-9]\+\) .*/\1 \2/p')
	echo "$dynamizer_out" > "$lattice_file"
    fi

    racket "${GRIFT_DIR}/benchmark/benchmark.rkt" -s "$c1 $c2" "${lattice_path}/"
    dynamizer_out=$(cat "$lattice_file")

    
    # check for/create/annotate 100% and 0%
    local benchmark_100_file="${lattice_path}/static.schml"
    if [ ! -f benchmark_100_file ]; then
        cp "$static_source_file" "$benchmark_100_file"
        sed -i '1i;; 100.00%' "$benchmark_100_file"
        echo "100% created"
    fi
    local benchmark_0_file="${lattice_path}/dyn.schml"
    if [ ! -f benchmark_0_file ]; then
        cp "$dyn_source_file" "$benchmark_0_file"
        sed -i '1i;; 0.0%' "$benchmark_0_file"
        echo "0% created"
    fi
    
    racket "${SCHML_DIR}/benchmark/benchmark.rkt" \
           -s "$c1 $c2" "${lattice_path}/"
    
    gen_output $baseline_system $c1 $c2 "$lattice_path" "$benchmark_args" "$dynamizer_out" "$print_name" "$disk_aux_name"
}

# $1 - baseline system
# $2 - first config index
# $3 - second config index
# $4 - nsamples
# $5 - nbins
run_experiment()
{
    local baseline_system="$1"; shift
    local c1="$1";              shift
    local c2="$1";              shift
    local nsamples="$1";        shift
    local nbins="$1";           shift

    local g=()

    local bs_bc_arg="\"$(cat "${INPUT_DIR}/blackscholes/in_4K.txt")\""
    run_benchmark $baseline_system $c1 $c2 "blackscholes" \
                  "$bs_bc_arg" "$nsamples" "$nbins" ""
    g+=($RETURN)
    
    local qs_wc_arg="\"$(cat "${INPUT_DIR}/quicksort/in_descend1000.txt")\""
    run_benchmark $baseline_system $c1 $c2 "quicksort" \
                  "$qs_wc_arg" "$nsamples" "$nbins" ""
    g+=($RETURN)
    
    run_benchmark $baseline_system \
                 $c1 $c2 "matmult" "200" "$nsamples" "$nbins" ""
    g+=($RETURN)

    run_benchmark $baseline_system \
                 $c1 $c2 "n_body" "10000" "$nsamples" "$nbins" ""
    g+=($RETURN)

    run_benchmark $baseline_system \
                 $c1 $c2 "fft" "32768" "$nsamples" "$nbins" ""
    g+=($RETURN)

    local arr_bc_arg="\"$(cat "${INPUT_DIR}/array/slow.txt")\""
    run_benchmark $baseline_system \
                 $c1 $c2 "array" "$arr_bc_arg" "$nsamples" "$nbins" ""
    g+=($RETURN)

    local tak_bc_arg="\"$(cat "${INPUT_DIR}/tak/slow.txt")\""
    run_benchmark $baseline_system $c1 $c2 "tak" "$tak_bc_arg" "$nsamples" "$nbins" ""
    g+=($RETURN)
    
    # # Dynamizer uses too much memory
    # #run_benchmark $baseline_system $c1 $c2 "ray" "" "$nsamples" "$nbins" ""

    IFS=$'\n'
    max=$(echo "${g[*]}" | sort -nr | head -n1)
    min=$(echo "${g[*]}" | sort -n | head -n1)
    

    echo "finished experiment comparing" $c1 "vs" $c2 ", where speedups range from " $min " to " $max
}

main()
{
    USAGE="Usage: $0 nsamples nbins loops [fresh|date] n_1,n_2 ... n_n"
    if [ "$#" == "0" ]; then
	echo "$USAGE"
	exit 1
    fi
    local nsamples="$1"; shift
    local nbins="$1"; shift
    LOOPS="$1";          shift
    local date="$1";     shift

    if [ "$date" == "fresh" ]; then
	declare -r DATE=`date +%Y_%m_%d_%H_%M_%S`
    else
	declare -r DATE="$date"
	if [ ! -d "$GRIFT_DIR/benchmark/suite/macro/lattice_bins/$DATE" ]; then
	    echo "Directory not found"
	    exit 1
	fi
    fi
    
    declare -r TEST_DIR="$GRIFT_DIR/benchmark/suite/macro"
    declare -r EXP_DIR="$TEST_DIR/lattice_bins/$DATE"
    declare -r DATA_DIR="$EXP_DIR/data"
    declare -r OUT_DIR="$EXP_DIR/output"
    declare -r TMP_DIR="$EXP_DIR/tmp"
    declare -r SRC_DIR="$TEST_DIR/src"
    declare -r INPUT_DIR="$TEST_DIR/inputs"
    declare -r PARAMS_LOG="$EXP_DIR/params.txt"

    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$TMP_DIR/partial"
    mkdir -p "$OUT_DIR/cumperflattice"
    mkdir -p "$OUT_DIR/perflattice/slowdown"
    mkdir -p "$OUT_DIR/perflattice/speedup"
    mkdir -p "$OUT_DIR/perflattice/log"

    . "lib/runtime.sh"

    cd "$GRIFT_DIR"

    local baseline_system=get_gambit_runtime
    local static_system=get_static_schml_runtime

    
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
	printf "nsamples\t:%s\n" "$nsamples" >> "$PARAMS_LOG"
	printf "nbins\t:%s\n" "$nbins" >> "$PARAMS_LOG"
    fi
    

    local i j
    if [ "$#" == "1" ]; then
	local config="$1";   shift
	for i in `seq ${config}`; do
	    for j in `seq ${i} ${config}`; do
		if [ ! $i -eq $j ]; then
		    run_experiment $baseline_system $i $j $nsamples $nbins
		fi
	    done
	done
    else
	while (( "$#" )); do
	    i=$1; shift
	    j=$1; shift
	    run_experiment $baseline_system $i $j $nsamples $nbins
	done
    fi
    echo "done."
}

main "$@"

# find . -name *quicksort_worstcase* | sed -e "p;s/quicksort_worstcase/quicksort/" | xargs -n2 mv
