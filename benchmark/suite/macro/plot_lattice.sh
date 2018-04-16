#!/bin/sh

function main()
{
    TEST_DIR=${GRIFT_DIR}/benchmark/suite/macro
    LIB_DIR="$TEST_DIR/lib"
    LATTICE_DIR=$TEST_DIR/lattice_bins/temp
    DATA_DIR=$LATTICE_DIR/data
    OUT_DIR=$LATTICE_DIR/output
    SRC_DIR=$TEST_DIR/src/partial

    . ${TEST_DIR}/lib/runtime.sh

    DPURPLE='#7b3294'
    DGREEN='#008837'
    SYELLOW='#fdb863'
    SPURPLE='#5e3c99'
    color1="$DGREEN"
    color2="$DPURPLE"

    plot 19 17 "Monotonic" "Proxied"
    plot 17 7 "Coercions" "Type-Based casts"
}

function plot()
{
    local c1="$1"; shift
    local c2="$1"; shift
    local c1t="$1"; shift
    local c2t="$1"; shift
    
    plot_benchmark "quicksort" $c1 $c2 "$c1t" "$c2t"
    plot_benchmark "fft" $c1 $c2 "$c1t" "$c2t"
    plot_benchmark "blackscholes" $c1 $c2 "$c1t" "$c2t"
    plot_benchmark "matmult" $c1 $c2 "$c1t" "$c2t"
    plot_benchmark "n_body" $c1 $c2 "$c1t" "$c2t"
    plot_benchmark "tak" $c1 $c2 "$c1t" "$c2t"
    plot_benchmark "array" $c1 $c2 "$c1t" "$c2t"
    plot_benchmark "ray" $c1 $c2 "$c1t" "$c2t"
}

function plot_benchmark()
{
    local name="$1"; shift
    local c1="$1"; shift
    local c2="$1"; shift
    local c1t="$1"; shift
    local c2t="$1"; shift

    local plot_dir="${OUT_DIR}/${c1}_${c2}/${name}"
    local alls_dir="${OUT_DIR}/${c1}_${c2}/alls"
    local rt_casts_dir="${OUT_DIR}/${c1}_${c2}/rt_casts"

    mkdir -p "$plot_dir"
    mkdir -p "$alls_dir"
    mkdir -p "$rt_casts_dir"

    local lpc_fig="${plot_dir}/lpc.png"
    local rt_fig="${plot_dir}/rt.png"
    local mono_fig="${plot_dir}/mono.png"
    local casts_fig="${plot_dir}/casts.png"
    local all_fig="${plot_dir}/all.png"

    local logfile1="${DATA_DIR}/${name}${c1}.log.sorted"
    local logfile3="${DATA_DIR}/${name}${c2}.log.sorted"

    if [ ! -f "${DATA_DIR}/${name}${c1}.log" ]; then
	return
    fi

    if [ ! -f "${DATA_DIR}/${name}${c2}.log" ]; then
	return
    fi
    
    sort -k2 -n -t, "${DATA_DIR}/${name}${c1}.log" > "${logfile1}"
    sort -k2 -n -t, "${DATA_DIR}/${name}${c2}.log" > "${logfile3}"

    print_aux_name=""
    printname="$(echo "$name" | tr _ "-")${print_aux_name}"

    speedup_geometric_mean "$logfile1"
    g1="$RETURN"
    runtime_mean "$logfile1"
    rt1="$RETURN"

    speedup_geometric_mean "$logfile3"
    g2="$RETURN"
    runtime_mean "$logfile3"
    rt2="$RETURN"

    dyn_mean=$(cat "${LATTICE_DIR}/dyn/${name}${disk_aux_name}17.runtime")
    static_mean=$(cat "${LATTICE_DIR}/static/${name}${disk_aux_name}.static.runtime")

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   noenhanced color font 'Verdana,26' ;"`
            `"set output '${lpc_fig}';"`
            `"set key left font 'Verdana,20';"`
            `"set title \"${printname}\";"`
	    `"set logscale y;"`
            `"set xlabel \"How much of the code is typed\";"`
            `"set ylabel \"Longest proxy chain\";"`
            `"plot '${logfile1}' using 2:8 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 2:8 with points"`
            `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}'"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${rt_fig}';"`
            `"set key top right box vertical width 1 height 1 maxcols 1 spacing 1 font 'Verdana,20';"`
            `"set title \"${printname}\";"`
   	    `"stats '${logfile1}' nooutput;"`
	    `"set xrange [0:STATS_records+10];"`
	    `"divby=STATS_records/4;"`
	    `"set xtics ('0%%' 0, '%%25' divby, '%%50' divby*2, '%%75' divby*3, '%%100' divby*4) nomirror;"`
            `"set xlabel \"How much of the code is typed\";"`
            `"set ylabel \"Runtime in seconds\";"`
	    `"set palette maxcolors 2;"`
	    `"set palette model RGB defined ( 0 'red', 1 '$color2' );"`
	    `"unset colorbox;"`
            `"plot '${logfile1}' using 0:3 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 0:3:( \$8 > 50 ? 0 : 1 ) with points"`
            `"   pt 6 ps 3 palette title '${c2t}',"`
            `"${static_mean} lw 2 dt 2 lc \"blue\" title 'Static Grift',"`
            `"${dyn_mean} lw 2 dt 2 lc \"red\" title 'Dynamic Grift';"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${casts_fig}';"`
            `"set key left font 'Verdana,20';"`
            `"set title \"${printname}\";"`
	    `"set logscale y;"`
            `"set xlabel \"How much of the code is typed\";"`
            `"set ylabel \"Runtime casts count\";"`
            `"plot '${logfile1}' using 2:7 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 2:7 with points"`
            `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}'"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,1500"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${mono_fig}';"`
            `"set lmargin at screen 0.15;"`
	    `"set rmargin at screen 0.95;"`
	    `"TOP=0.95;"`
	    `"DY = 0.45;"`
	    `"set multiplot;"`
            `"set xlabel \"How much of the code is typed\";"`
	    `"unset ylabel;"`
	    `"unset key;"`
	    `"stats '${logfile1}' nooutput;"`
	    `"set xrange [0:STATS_records+10];"`
	    `"divby=STATS_records/4;"`
	    `"set xtics ('0%%' 0, '%%25' divby, '%%50' divby*2, '%%75' divby*3, '%%100' divby*4) nomirror;"`
	    `"max(x,y) = (x > y) ? x : y;"`
	    `"set format x '';"`
	    `"set yrange [0:*];"`
            `"set label 2 \"Runtime casts count\" at screen 0.02,0.25 rotate by 90;"`
	    `"set tmargin at screen TOP-DY;"`
	    `"set bmargin at screen TOP+0.02-2*DY;"`
	    `"unset key;"`
            `"plot '${logfile1}' using 0:7 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 0:7 with points"`
            `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}';"`
	    `"unset xtics; unset xlabel;"`
            `"set key top right box vertical width 1 height 1 maxcols 1 spacing 1 font 'Verdana,20';"`
	    `"set tmargin at screen TOP;"`
	    `"set bmargin at screen TOP+0.02-DY;"`
            `"set title \"${printname}\";"`
            `"set label 3 \"Runtime in seconds\" at screen 0.02,0.7 rotate by 90;"`
	    `"set palette maxcolors 2;"`
	    `"set palette model RGB defined ( 0 'red', 1 '$color2' );"`
	    `"unset colorbox;"`
            `"plot '${logfile1}' using 0:3 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 0:3:( \$8 > 50 ? 0 : 1 ) with points"`
            `"   pt 6 ps 3 palette title '${c2t}',"`
            `"${static_mean} lw 2 dt 2 lc \"blue\" title 'Static Grift',"`
            `"${dyn_mean} lw 2 dt 2 lc \"red\" title 'Dynamic Grift';"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,1900"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${all_fig}';"`
            `"set lmargin at screen 0.15;"`
	    `"set rmargin at screen 0.95;"`
	    `"TOP=0.95;"`
	    `"DY = 0.29;"`
	    `"set multiplot;"`
            `"set xlabel \"How much of the code is typed\";"`
	    `"unset ylabel;"`
            `"set label 1 \"Longest proxy chain\" at screen 0.02,0.15 rotate by 90;"`
	    `"set tmargin at screen TOP-2*DY;"`
	    `"set bmargin at screen TOP-3*DY;"`
	    `"unset key;"`
	    `"stats '${logfile1}' nooutput;"`
	    `"set xrange [0:STATS_records+10];"`
	    `"divby=STATS_records/4;"`
	    `"set xtics ('0%%' 0, '%%25' divby, '%%50' divby*2, '%%75' divby*3, '%%100' divby*4) nomirror;"`
	    `"max(x,y) = (x > y) ? x : y;"`
	    `"set yrange [0:*];"`
            `"plot '${logfile1}' using 0:(max(\$19, (max(\$20, \$21)))) with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
	    `"'${logfile3}' using 0:(max(\$19, (max(\$20, \$21)))) with points"`
            `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}';"`
	    `"unset xtics;"`
	    `"unset xlabel;"`
	    `"set format x '';"`
	    `"set yrange [0:*];"`
            `"set label 2 \"Runtime casts count\" at screen 0.02,0.45 rotate by 90;"`
	    `"set tmargin at screen TOP-DY;"`
	    `"set bmargin at screen TOP+0.02-2*DY;"`
	    `"unset key;"`
            `"plot '${logfile1}' using 0:7 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 0:7 with points"`
            `"   pt 6 ps 3 lc rgb '$color2' title '${c2t}';"`
            `"set key top right box vertical width 1 height 1 maxcols 1 spacing 1 font 'Verdana,20';"`
	    `"set tmargin at screen TOP;"`
	    `"set bmargin at screen TOP+0.02-DY;"`
            `"set title \"${printname}\";"`
            `"set label 3 \"Runtime in seconds\" at screen 0.02,0.75 rotate by 90;"`
	    `"set palette maxcolors 2;"`
	    `"set palette model RGB defined ( 0 'red', 1 '$color2' );"`
	    `"unset colorbox;"`
            `"plot '${logfile1}' using 0:3 with points"` 
            `"   pt 9 ps 3 lc rgb '$color1' title '${c1t}',"`
            `"'${logfile3}' using 0:3:( \$8 > 50 ? 0 : 1 ) with points"`
            `"   pt 6 ps 3 palette title '${c2t}',"`
            `"${static_mean} lw 2 dt 2 lc \"blue\" title 'Static Grift',"`
            `"${dyn_mean} lw 2 dt 2 lc \"red\" title 'Dynamic Grift';"

    cp "${all_fig}" "${alls_dir}/${name}.png"
    cp "${mono_fig}" "${rt_casts_dir}/${name}.png"
}

main "$@"
