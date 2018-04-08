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
    local input_file="$1";      shift
    local disk_aux_name="$1";   shift
    
    local name=$(basename "$path")
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${i}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${i}.csv"
    local cache_file="${TMP_DIR}/static/${name}${disk_aux_name}${i}.cache"
    local bs=$(find "$path" -name "*.o$i")
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        local n=0 b
        $baseline_system "$name" "$input_file" "$disk_aux_name"
        local baseline="$RETURN"
        if [ "$CAST_PROFILER" = true ]; then
            echo "name,precision,time,slowdown,speedup,total values allocated,total casts,longest proxy chain,total proxies accessed,total uses,function total values allocated,vector total values allocated,ref total values allocated,tuple total values allocated,function total casts,vector total casts, ref total casts,tuple total casts,function longest proxy chain,vector longest proxy chain,ref longest proxy chain,tuple longest proxy chain,function total proxies accessed,vector total proxies accessed,ref total proxies accessed,tuple total proxies accessed,function total uses,vector total uses,ref total uses,tuple total uses,injects casts,projects casts"\
                 > "$logfile1"
        else
            echo "name,precision,time,slowdown,speedup" > "$logfile1"
        fi
        for b in $(find "$path" -name "*.o$i"); do
            let n=n+1
            local binpath="${b%.*}"
            local p=$(sed -n 's/;; \([0-9]*.[0-9]*\)%/\1/p;q' \
                          < "${binpath}.grift")
            local bname="$(basename $b)"
            local input="${INPUT_DIR}/${name}/${input_file}"
            
            avg "$b" "$input"\
                "static" "${OUTPUT_DIR}/static/${name}/${input_file}"\
                "${b}.runtimes"
            local t="$RETURN"
            local speedup=$(echo "${baseline}/${t}" | \
                                bc -l | \
                                awk -v p="$PRECISION" '{printf "%.*f\n", p,$0}')
            local slowdown=$(echo "${t}/${baseline}" | \
                                 bc -l | \
                                 awk -v p="$PRECISION" '{printf "%.*f\n", p,$0}')
            echo $n $b $speedup
            printf "%s,%.2f,%.${PRECISION}f,%.${PRECISION}f,%.${PRECISION}f" \
                   $bname $p $t $slowdown $speedup >> "$logfile1"

            if [ "$CAST_PROFILER" = true ] ; then
                # run the cast profiler
                eval "cat ${input} | ${b}.prof.o" > /dev/null 2>&1
                mv "$bname.prof.o.prof" "${b}.prof"
                printf "," >> "$logfile1"
                # ignore first and last rows and sum the values across all
                # columns in the profile into one column and transpose it into
                # a row
                sed '1d;$d' "${b}.prof" | awk -F, '{print $2+$3+$4+$5+$6+$7}'\
                    | paste -sd "," - | xargs echo -n >> "$logfile1"
                echo -n "," >> "$logfile1"
                # ignore the first row and the first column and stitsh together
                # all rows into one row
                sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
                    | awk -F, '{print $1","$2","$3","$4}' | paste -sd "," -\
                    | xargs echo -n >> "$logfile1"
                echo -n "," >> "$logfile1"
                # writing injections
                sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
                    | awk -F, 'FNR == 2 {print $5}' | xargs echo -n >> "$logfile1"
                echo -n "," >> "$logfile1"
                # writing projections
                sed '1d;$d' "${b}.prof" | cut -f1 -d"," --complement\
                    | awk -F, 'FNR == 2 {print $6}' >> "$logfile1"
            else
                printf "\n" >> "$logfile1"
            fi
        done
        cut -d, -f4 "$logfile1" | \
            sed -n '1!p' | \
            sort | \
            uniq -c | \
            awk ' { t = $1; $1 = $2; $2 = t; print; } ' | \
            awk '{ $1=$1" ,";; print }' > "$logfile2"
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
    local input_file="$1"; shift
    local disk_aux_name="$1";  shift
    
    local cache_file="${TMP_DIR}/${name}${disk_aux_name}${i}_typed_untyped_ratio"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
        get_grift_runtime "${TMP_DIR}/static/${name}"\
                          "$input_file" "$disk_aux_name" $i
        t11="$RETURN"
        get_grift_runtime "${TMP_DIR}/dyn/${name}" \
                          "$input_file" "$disk_aux_name" $i
        t12="$RETURN"
        RETURN=$(echo "${t11}/${t12}" | \
                     bc -l | \
                     awk -v p="$PRECISION" '{printf "%.2f\n",$0}')
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
    local input_file="$1";      shift
    local dynamizer_out="$1";   shift
    local printname="$1";       shift
    local disk_aux_name="$1";   shift
    local ymin="$1"; shift
    local ymax="$1"; shift

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
    perf_lattice_log_fig="${OUT_DIR}/perflattice/log/${disk_name}_log.png"
    perf_lattice_lin_fig="${OUT_DIR}/perflattice/linear/${disk_name}_lin.png"
    casts_fig="${OUT_DIR}/casts/plot/${disk_name}.png"
    

    rm -f "$cum_perf_lattice_fig" "$cum_perf_lattice_tbl" "$perf_lattice_slowdown_fig"
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${c1}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${c1}.csv"
    local logfile3="${DATA_DIR}/${name}${disk_aux_name}${c2}.log"
    local logfile4="${DATA_DIR}/${name}${disk_aux_name}${c2}.csv"

    get_speedup gambit $baseline_system "$name" "$input_file" "$disk_aux_name"
    local gr_t="$RETURN"

    compute_typed_untyped_ratio "$c1" "$name" "$input_file" "$disk_aux_name"
    local typed_untyped_ratio1="$RETURN"
    compute_typed_untyped_ratio "$c2" "$name" "$input_file" "$disk_aux_name"
    local typed_untyped_ratio2="$RETURN"

    run_config $baseline_system "$c1" "$path" "$input_file" "$disk_aux_name"
    run_config $baseline_system "$c2" "$path" "$input_file" "$disk_aux_name"
    local n="$RETURN"

    speedup_geometric_mean "$logfile1"
    g1="$RETURN"
    runtime_mean "$logfile1"
    rt1="$RETURN"
    
    speedup_geometric_mean "$logfile3"
    g2="$RETURN"
    runtime_mean "$logfile3"
    rt2="$RETURN"
    
    $baseline_system "$name" "$input_file" "$disk_aux_name"
    local baseline_mean="$RETURN"

    get_static_grift_runtime "$name" "$input_file" "$disk_aux_name"
    local static_mean="$RETURN"
    local static_speed_up=$(echo "${baseline_mean} ${static_mean}" | \
                                awk '{printf "%.2f", $1 / $2}')
    
    printf "geometric means %s:\t\t%d=%.4f\t%d=%.4f\n" $name $c1 $g1 $c2 $g2
    
    racket ${LIB_DIR}/csv-set.rkt --add "$name , $c1 , $g1"\
           --add "$name , $c2 , $g2"\
           --in "$GMEANS" --out "$GMEANS"

    local min1=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}'\
                     "$logfile2")
    local min2=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}'\
                     "$logfile4")
    local max1=$(awk -v max=0 \
                     '{if($1>max){want=$1; max=$1}}END{print want}'\
                     "$logfile2")
    local max2=$(awk -v max=0 \
                     '{if($1>max){want=$1; max=$1}}END{print want}'\
                     "$logfile4")
    local std1 std2 mean1 mean2
    read std1 mean1 <<< $(cat "$logfile2" |\
                              cut -d, -f1 |\
                              awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    read std2 mean2 <<< $(cat "$logfile4" |\
                              cut -d, -f1 |\
                              awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    

    local DPURPLE='#7b3294'
    local DGREEN='#008837'
    local SYELLOW='#fdb863'
    local SPURPLE='#5e3c99'
    local color1="$DGREEN"
    local color2="$DPURPLE"


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
                `"     '${logfile4}' using 1:2 with lines lw 2 dt 2 title '${c2t}' smooth cumulative"

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

    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
                `"enhanced color font 'Verdana,10' ;"`
                `"set output '${perf_lattice_slowdown_fig}';"`
                `"set key left font \"Verdana,20\";"`
                `"set xrange [0:100];"`
                `"set ytics add (\"\" 0, \"1\" 1);"`
                `"set ytics font \", 13\";"`
                `"set xtics font \", 13\";"`
                `"set grid ytics;"`
                `"set title \"${printname}\";"`
                `"set ylabel \"Slowdown with respect to Gambit\";"`
                `"set xlabel \"How much of the code is typed\";"`
                `"plot '${logfile1}' using 2:4 with points pointtype 6 lc rgb \"#3182bd\" title '${c1t}',"`
                `"'${logfile3}' using 2:4 with points pointtype 8 lc rgb \"#fdae6b\" title '${c2t}'"

    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
                `"enhanced color font 'Verdana,13' ;"`
                `"set output '${perf_lattice_speedup_fig}';"`
                `"set key left font \"Verdana,20\";"`
                `"set xrange [-5:105];"`
                `"set yrange [0.1:4];"`
                `"set ytics add (\"1\" 1, \"\" ${g1}, \"\" ${g2});"`
                `"set title \"${printname}\";"`
                `"set ylabel \"Speedup over Gambit\";"`
                `"set xlabel \"How much of the code is typed\";"`
                `"plot '${logfile1}' using 2:5 with points "`
                `"   pt 6 lc rgb '$color1' title '${c1t}',"`
                `"'${logfile3}' using 2:5 with points pointtype 8 lc rgb '$color1' title '${c2t}',"`
                `"${g1} lw 2 dt 3 lc rgb '$color2' notitle '${c1t} mean',"`
                `"${g2} lw 2 dt 3 lc rgb '$color2' notitle '${c2t} mean',"`
                `"1 lw 2 dt 2 lc rgb \"black\" title 'No Gradual Overhead (Gambit Scheme)',"` 
                `"${static_speed_up} lw 1 dt 2 lc \"black\" title 'No Gradual Overhead (Grift STLC)';"
    
    
    gnuplot -e "set datafile separator \",\";"`
                `"set terminal pngcairo size 1280,960"`
                `"   noenhanced color font 'Verdana,26' ;"`
                `"set output '${perf_lattice_log_fig}';"`
                `"set key left font 'Verdana,20';"`
                `"set logscale y;"`
                `"set xrange [-5:105];"` 
                `"set yrange [${ymin}:${ymax}];"` 
                `"set ytics add ('1' 1, '' ${g1}, '' ${g2});"`
                `"set ytics add (${ymin});"` 
                `"set ytics add (${ymax});"` 
                `"set title \"${printname}\";"`
                `"set ylabel \"Speedup over Racket (Log scale)\";"`
                `"set xlabel \"How much of the code is typed\";"`
                `"plot '${logfile1}' using 2:5 with points"`
                `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
                `"${g1} lw 4 dt 2 lc rgb '$color1' title '${c1t} mean',"`
                `"'${logfile3}' using 2:5 with points"`
                `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}',"`
                `"${g2} lw 4 dt 4 lc rgb '$color2' title '${c2t} mean',"`
                `"1 lw 2 dt 2 lc rgb \"black\" title 'Racket',"` 
                `"${static_speed_up} lw 1 dt 2 lc \"black\" title 'Static Grift';"


    gnuplot -e "set datafile separator \",\";"`
                `"set terminal pngcairo size 1280,960"`
                `"   noenhanced color font 'Verdana,26' ;"`
                `"set output '${perf_lattice_lin_fig}';"`
                `"set key left font 'Verdana,20';"`
                `"set xrange [-5:105];"`
                `"set yrange [${ymin}:${ymax}];"` 
                `"set ytics add ('1' 1, '' ${g1}, '' ${g2});"`
                `"set ytics add (${ymin});"` 
                `"set ytics add (${ymax});"` 
                `"set title \"${printname}\";"`
                `"set ylabel \"Speedup over Racket\";"`
                `"set xlabel \"How much of the code is typed\";"`
                `"plot '${logfile1}' using 2:5 with points"` 
                `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
                `"${g1} lw 4 dt 2 lc rgb '$color1' title '${c1t} mean',"`
                `"'${logfile3}' using 2:5 with points"`
                `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}',"`
                `"${g2} lw 4 dt 4 lc rgb '$color2'  title '${c2t} mean',"`
                `"1 lw 2 dt 4 lc rgb \"black\" title 'Racket',"` 
                `"${static_speed_up} lw 2 dt 2 lc \"black\" title 'Static Grift';"

    if [ "$CAST_PROFILER" = true ]; then
        gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,1900"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${casts_fig}';"`
            `"set lmargin at screen 0.15;"`
            `"set rmargin at screen 0.95;"`
            `"TOP=0.95;"`
            `"DY = 0.29;"`
            `"set multiplot;"`
            `"set yrange [0:*];"`
            `"set xlabel \"How much of the code is typed\";"`
            `"set ylabel \"Longest proxy chain\";"`
            `"set tmargin at screen TOP-2*DY;"`
            `"set bmargin at screen TOP-3*DY;"`
            `"unset key;"`
            `"set xtics nomirror;"`
            `"plot '${logfile1}' using 2:8 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 2:8 with points"`
            `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}';"`
            `"unset xlabel;"`
            `"set format x '';"`
            `"set ylabel \"Runtime casts count\" offset -3;"`
            `"set tmargin at screen TOP-DY;"`
            `"set bmargin at screen TOP+0.02-2*DY;"`
            `"unset key;"`
            `"set yrange [1:*];"`
            `"set format y \"10^{%T}\";"`
            `"set logscale y;"`
            `"plot '${logfile1}' using 2:7 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 2:7 with points"`
            `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}';"`
            `"set key bottom left font 'Verdana,20';"`
            `"set tmargin at screen TOP;"`
            `"set bmargin at screen TOP+0.02-DY;"`
            `"set title \"${printname}\";"`
            `"set yrange [*:*];"`
            `"set ylabel \"Runtime in seconds\" offset 0;"`
            `"unset format y;"`
            `"plot '${logfile1}' using 2:3 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"${rt1} lw 2 dt 3 lc rgb '$color2' notitle '${c1t} mean',"`
            `"'${logfile3}' using 2:3 with points"`
            `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}',"`
            `"${rt2} lw 2 dt 3 lc rgb '$color2' notitle '${c2t} mean',"`
            `"${baseline_mean} lw 2 dt 2 lc rgb \"black\" title 'Racket',"`
            `"${static_mean} lw 2 dt 2 lc \"blue\" title 'Static Grift';"
    fi
}

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
    local input_file="$1";  shift
    local nsamples="$1";        shift
    local nbins="$1";           shift
    local aux_name="$1";        shift
    local ymin="$1"; shift
    local ymax="$1"; shift

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

    local lattice_file="${lattice_path}/out"
    local dynamizer_out=""

    if [ -f "$lattice_file" ]; then
        dynamizer_out=$(cat "$lattice_file")
    else
        rm -rf "$lattice_path"
        rm -f "${lattice_path}.grift"
        cp "$static_source_file" "${lattice_path}.grift"

        dynamizer_out=$(dynamizer "${lattice_path}.grift"\
                                  --samples "$nsamples" --bins "$nbins" | \
                            sed -n 's/.* \([0-9]\+\) .* \([0-9]\+\) .*/\1 \2/p')
        echo "$dynamizer_out" > "$lattice_file"
    fi

    
    # check for/create/annotate 100% and 0%
    local benchmark_100_file="${lattice_path}/static.grift"
    if [ ! -f benchmark_100_file ]; then
        cp "$static_source_file" "$benchmark_100_file"
        sed -i '1i;; 100.00%' "$benchmark_100_file"
        echo "100% created"
    fi
    local benchmark_0_file="${lattice_path}/dyn.grift"
    if [ ! -f benchmark_0_file ]; then
        cp "$dyn_source_file" "$benchmark_0_file"
        sed -i '1i;; 0.0%' "$benchmark_0_file"
        echo "0% created"
    fi
    
    if [ "$CAST_PROFILER" = true ] ; then
        racket "${GRIFT_DIR}/benchmark/benchmark.rkt" --cast-profiler \
               -s "$c1 $c2" "${lattice_path}/"
    else
        racket "${GRIFT_DIR}/benchmark/benchmark.rkt"\
               -s "$c1 $c2" "${lattice_path}/"
    fi

    gen_output $baseline_system $c1 $c2 "$lattice_path" "$input_file"\
               "$dynamizer_out" "$print_name" "$disk_aux_name" "$ymin" "$ymax"
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
    
    # Blackscholes
    local bs_yminf="${DATA_DIR}/bs_ymin_${c1}_${c2}.txt"
    local bs_ymin=""
    if [ -f $bs_yminf ]; then
        bs_ymin="$(cat $bs_yminf)"
    fi
    local bs_ymaxf="${DATA_DIR}/bs_ymax_${c1}_${c2}.txt"
    local bs_ymax=""
    if [ -f $bs_ymaxf ]; then
        bs_ymax="$(cat $bs_ymaxf)";
    fi
    run_benchmark $baseline_system $c1 $c2 "blackscholes" \
                  "in_4K.txt" "$nsamples" "$nbins" ""\
                  "$bs_ymin" "$bs_ymax" 
    g+=($RETURN)
    
    # Quicksort
    local qs_yminf="${DATA_DIR}/qs_ymin_${c1}_${c2}.txt"
    local qs_ymin=""
    if [ -f $qs_yminf ]; then
        qs_ymin="$(cat $qs_yminf)"
    fi
    local qs_ymaxf="${DATA_DIR}/qs_ymax_${c1}_${c2}.txt"
    local qs_ymax=""
    if [ -f $qs_ymaxf ]; then
        qs_ymax="$(cat $qs_ymaxf)";
    fi
    run_benchmark $baseline_system $c1 $c2 "quicksort" \
                  "in_descend1000.txt" "$nsamples" "$nbins" ""\
                  "$qs_ymin" "$qs_ymax" 
    g+=($RETURN)
    
    # Matrix Multiplication
    local mm_yminf="${DATA_DIR}/mm_ymin_${c1}_${c2}.txt"
    local mm_ymin=""
    if [ -f $mm_yminf ]; then
        mm_ymin="$(cat $mm_yminf)"
    fi
    local mm_ymaxf="${DATA_DIR}/mm_ymax_${c1}_${c2}.txt"
    local mm_ymax=""
    if [ -f $mm_ymaxf ]; then
        mm_ymax="$(cat $mm_ymaxf)";
    fi
    run_benchmark $baseline_system $c1 $c2 "matmult"\
                  "400.txt" "$nsamples" "$nbins" ""\
                  "$mm_ymin" "$mm_ymax"
    g+=($RETURN)
    
    # N Body Simulation
    local nb_yminf="${DATA_DIR}/nb_ymin_${c1}_${c2}.txt"
    local nb_ymin=""
    if [ -f $nb_yminf ]; then
        nb_ymin="$(cat $nb_yminf)"
    fi
    local nb_ymaxf="${DATA_DIR}/nb_ymax_${c1}_${c2}.txt"
    local nb_ymax=""
    if [ -f $nb_ymaxf ]; then
        nb_ymax="$(cat $nb_ymaxf)";
    fi
    run_benchmark $baseline_system $c1 $c2 "n_body"\
                  "slow.txt" "$nsamples" "$nbins" ""\
                  "$nb_ymin" "$nb_ymax"

    g+=($RETURN)
    
    # Fast Fourier Transform
    local fft_yminf="${DATA_DIR}/fft_ymin_${c1}_${c2}.txt"
    local fft_ymin=""
    if [ -f $fft_yminf ]; then
        fft_ymin="$(cat $fft_yminf)"
    fi
    local fft_ymaxf="${DATA_DIR}/fft_ymax_${c1}_${c2}.txt"
    local fft_ymax=""
    if [ -f $fft_ymaxf ]; then
        fft_ymax="$(cat $fft_ymaxf)";
    fi
    run_benchmark $baseline_system $c1 $c2 "fft"\
                  "slow.txt" "$nsamples" "$nbins" ""\
                  "$fft_ymin" "$fft_ymax"
    g+=($RETURN)
    
    # Scheme Array Benchmark
    local arr_yminf="${DATA_DIR}/arr_ymin_${c1}_${c2}.txt"
    local arr_ymin=""
    if [ -f $arr_yminf ]; then
        arr_ymin="$(cat $arr_yminf)"
    fi
    local arr_ymaxf="${DATA_DIR}/arr_ymax_${c1}_${c2}.txt"
    local arr_ymax=""
    if [ -f $arr_ymaxf ]; then
        arr_ymax="$(cat $arr_ymaxf)";
    fi
    run_benchmark $baseline_system $c1 $c2 "array" \
                  "slow.txt" "$nsamples" "$nbins" ""\
                  "$arr_ymin" "$arr_ymin"
    g+=($RETURN)
    
    # Tak
    local tak_yminf="${DATA_DIR}/tak_ymin_${c1}_${c2}.txt"
    local tak_ymin=""
    if [ -f $tak_yminf ]; then
        tak_ymin="$(cat $tak_yminf)"
    fi
    local tak_ymaxf="${DATA_DIR}/tak_ymax_${c1}_${c2}.txt"
    local tak_ymax=""
    if [ -f $tak_ymaxf ]; then
        tak_ymax="$(cat $tak_ymaxf)";
    fi
    run_benchmark $baseline_system $c1 $c2 "tak"\
                  "slow.txt" "$nsamples" "$nbins" ""\
                  "$tak_ymin" "$tak_ymax"
    g+=($RETURN)

    local ray_yminf="${DATA_DIR}/ray_ymin_${c1}_${c2}.txt"
    local ray_ymin=""
    if [ -f $ray_yminf ]; then
        ray_ymin="$(cat $ray_yminf)"
    fi
    local ray_ymaxf="${DATA_DIR}/ray_ymax_${c1}_${c2}.txt"
    local ray_ymax=""
    if [ -f $ray_ymaxf ]; then
        ray_ymax="$(cat $ray_ymaxf)";
    fi
    run_benchmark $baseline_system $c1 $c2 "ray"\
                  "empty.txt" "$nsamples" "$nbins" ""\
                  "$ray_ymin" "$ray_ymax"
    g+=($RETURN)
    

    IFS=$'\n'
    max=$(echo "${g[*]}" | sort -nr | head -n1)
    min=$(echo "${g[*]}" | sort -n | head -n1)
    
    echo "finished experiment comparing" $c1 "vs" $c2 \
         ", where speedups range from " $min " to " $max
}

main()
{
    USAGE="Usage: $0 nsamples nbins loops cast_profiler? [fresh|date] n_1,n_2 ... n_n"
    if [ "$#" == "0" ]; then
        echo "$USAGE"
        exit 1
    fi
    local nsamples="$1"; shift
    local nbins="$1";    shift
    LOOPS="$1";          shift
    CAST_PROFILER="$1";  shift
    local date="$1";     shift
    
    GRIFT_DIR=${GRIFT_DIR:=`pwd`/../../..}
    
    declare -r TEST_DIR="$GRIFT_DIR/benchmark/suite/macro"
    declare -r LB_DIR="$TEST_DIR/lattice_bins"
    if [ "$date" == "fresh" ]; then
        declare -r DATE=`date +%Y_%m_%d_%H_%M_%S`
        mkdir -p "$LB_DIR/$DATE"
    elif [ "$date" == "test" ]; then
        declare -r DATE="test"
        if [ ! -d "$LB_DIR/$DATE" ]; then
            mkdir -p "$LB_DIR/$DATE"
        fi
    else
        declare -r DATE="$date"
        if [ ! -d "$LB_DIR/$DATE" ]; then
            echo "$LB_DIR/$DATE" "Directory not found"
            exit 1
        fi
    fi
    
    declare -r EXP_DIR="$LB_DIR/$DATE"
    declare -r DATA_DIR="$EXP_DIR/data"
    declare -r OUT_DIR="$EXP_DIR/output"
    declare -r GMEANS="${OUT_DIR}/geometric-means.csv"
    declare -r TMP_DIR="$EXP_DIR/tmp"
    declare -r SRC_DIR="$TEST_DIR/src"
    declare -r INPUT_DIR="$TEST_DIR/inputs"
    declare -r OUTPUT_DIR="$TEST_DIR/outputs"
    declare -r LIB_DIR="$TEST_DIR/lib"
    declare -r PARAMS_LOG="$EXP_DIR/params.txt"
    
    # Check to see if all is right in the world
    if [ ! -d $TEST_DIR ]; then
        echo "directory not found: $TEST_DIR" 1>&2
        exit 1
    elif [ ! -d $EXP_DIR ]; then
        echo "Directory not found: $EXP_DIR"
        exit 1
    elif [ ! -d $SRC_DIR ]; then
        echo "directory not found: $SRC_DIR" 1>&2
        exit 1
    elif [ ! -d $INPUT_DIR ]; then
        echo "directory not found: $INPUT_DIR" 1>&2
        exit 1
    elif [ ! -d $OUTPUT_DIR ]; then
        echo "directory not found: $OUTPUT_DIR" 1>&2
        exit 1
    elif [ ! -d $LIB_DIR ]; then
        echo "directory not found: $LIB_DIR" 1>&2
        exit 1
    fi
    
    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$OUT_DIR/cumperflattice"
    mkdir -p "$OUT_DIR/perflattice/slowdown"
    mkdir -p "$OUT_DIR/perflattice/speedup"
    mkdir -p "$OUT_DIR/perflattice/log"
    mkdir -p "$OUT_DIR/perflattice/linear"
    mkdir -p "$OUT_DIR/casts/plot"
    rm -f $GMEANS 
    touch $GMEANS

    . "lib/runtime.sh"

    cd "$GRIFT_DIR"

    local baseline_system=get_racket_runtime
    local static_system=get_static_grift_runtime
    
    
    if [ ! -d $TMP_DIR ]; then
        # copying the benchmarks to a temporary directory
        cp -r ${SRC_DIR} $TMP_DIR
        mkdir -p "$TMP_DIR/partial"
        
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

    racket ${LIB_DIR}/csv-set.rkt -i $GMEANS --config-names 1 \
           --si 2 \
           -o ${OUT_DIR}/gm-total.csv 
    racket ${LIB_DIR}/csv-set.rkt -i $GMEANS --config-names 1 \
           --si 2 --su 0 \
           -o ${OUT_DIR}/gm-benchmart.csv
    racket ${LIB_DIR}/csv-set.rkt -i $GMEANS --config-names 1 \
           --si 2 --su 1 \
           -o ${OUT_DIR}/gm-config.csv
    
    
    echo "done."


}

main "$@"

# find . -name *quicksort_worstcase* | sed -e "p;s/quicksort_worstcase/quicksort/" | xargs -n2 mv
