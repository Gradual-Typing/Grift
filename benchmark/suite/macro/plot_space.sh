#!/bin/sh
set -euo pipefail

# Script to plot figures for the benchmarks with space issues when the input is
# varied.

function main()
{
    . lib/runtime.sh


    date="2018_04_15_21_25_29"

    LOOPS=1
    PRECISION=5

    TEST_DIR=${GRIFT_DIR}/benchmark/suite/macro
    LIB_DIR="$TEST_DIR/lib"
    SPACE_DIR=$TEST_DIR/space/$date
    DATA_DIR=$SPACE_DIR/data
    OUT_DIR=$SPACE_DIR/output
    TMP_DIR=$SPACE_DIR/tmp
    SRC_DIR=$TEST_DIR/src/partial

    DPURPLE='#7b3294'
    DGREEN='#008837'
    SYELLOW='#fdb863'
    SPURPLE='#5e3c99'
    color1="$DGREEN"
    color2="$DPURPLE"

    plot 17 7
}

function plot()
{
    local c1="$1"; shift
    local c2="$1"; shift

    local config_str=$(racket "${GRIFT_DIR}/benchmark/config_str.rkt" -c $c1 $c2)
    local c1t=$(echo $config_str | sed -n 's/\(.*\),.*,.*/\1/p;q')
    local c2t=$(echo $config_str | sed -n 's/.*,\(.*\),.*/\1/p;q')
    local ct=$(echo $config_str | sed -n 's/.*,.*,\(.*\)/\1/p;q')

    plot_benchmark "quicksort" $c1 $c2 "$c1t" "$c2t" "quicksort" "Array length"
    plot_benchmark "cps-even-odd" $c1 $c2 "$c1t" "$c2t" "csp-even-odd" "Input"
}

function plot_benchmark()
{
    local name="$1"; shift
    local c1="$1"; shift
    local c2="$1"; shift
    local c1t="$1"; shift
    local c2t="$1"; shift
    local printname="$1"; shift
    local x_axis_label="$1"; shift

    plot_comparison "$name" $c1 $c2 "$c1t" "$c2t" "$printname" "$x_axis_label"
    plot_config "$name" $c1 "$c1t" "$printname" "$x_axis_label" 1
    plot_config "$name" $c2 "$c2t" "$printname" "$x_axis_label" 2
}

function plot_config()
{
    local name="$1"; shift
    local c="$1"; shift
    local ct="$1"; shift
    local printname="$1"; shift
    local x_axis_label="$1"; shift
    local line_style="$1"; shift

    local plot_dir="${OUT_DIR}/${name}/${ct}"

    mkdir -p "$plot_dir"

    logfile="$DATA_DIR/${name}${c}.csv"

    local lpc_fig="${plot_dir}/lpc.png"
    local rt_fig="${plot_dir}/rt.png"
    local casts_fig="${plot_dir}/casts.png"
    local all_fig="${plot_dir}/all.png"
    local fit1_fig="$plot_dir/poly2fitting.png"
    local fit2_fig="$plot_dir/poly3fitting.png"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${rt_fig}';"`
            `"set key top left box vertical width 1 height 1 maxcols 1 spacing 1 font 'Verdana,20';"`
            `"set title \"${printname}\";"`
	    `"set xlabel \"${x_axis_label}\"; "`
	    `"set ylabel \"Runtime in seconds\"; "`
	    `"set yrange [0:*];"`
	    `"set xtics nomirror; "`
	    `"set ytics nomirror; "`
	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 5 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
	    `"plot '$logfile' using 1:2 with lp ls ${line_style} title '${ct}'"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   noenhanced color font 'Verdana,26' ;"`
            `"set output '${lpc_fig}';"`
            `"set key bottom right font 'Verdana,20';"`
            `"set title \"${printname}\";"`
            `"set xlabel \"${x_axis_label}\";"`
            `"set ylabel \"Longest proxy chain\";"`
	    `"max(x,y) = (x > y) ? x : y;"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 5 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
            `"plot '${logfile}' using 1:(max(\$16, (max(\$17, \$18)))) with lp ls ${line_style} title '${ct}'"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${casts_fig}';"`
            `"set key bottom right font 'Verdana,20';"`
            `"set title \"${printname}\";"`
            `"set xlabel \"${x_axis_label}\";"`
            `"set ylabel \"Runtime casts count\";"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 5 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
            `"plot '${logfile}' using 1:4 with lp ls ${line_style} title '${ct}'"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,1900"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${all_fig}';"`
            `"set lmargin at screen 0.15;"`
	    `"set rmargin at screen 0.95;"`
	    `"TOP=0.95;"`
	    `"DY = 0.29;"`
	    `"set multiplot;"`
            `"set xlabel \"${x_axis_label}\";"`
	    `"unset ylabel;"`
            `"set label 1 \"Longest proxy chain\" at screen 0.02,0.15 rotate by 90;"`
	    `"set tmargin at screen TOP-2*DY;"`
	    `"set bmargin at screen TOP-3*DY;"`
	    `"unset key;"`
	    `"max(x,y) = (x > y) ? x : y;"`
	    `"set yrange [0:*];"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 dt 2 lw 2 pt 5 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
            `"plot '${logfile}' using 1:(max(\$16, (max(\$17, \$18)))) with lp ls ${line_style} title '${ct}';"`
	    `"unset xtics;"`
	    `"unset xlabel;"`
	    `"set format x '';"`
	    `"set yrange [0:*];"`
            `"set label 2 \"Runtime casts count\" at screen 0.02,0.45 rotate by 90;"`
	    `"set tmargin at screen TOP-DY;"`
	    `"set bmargin at screen TOP+0.02-2*DY;"`
	    `"unset key;"`
            `"plot '${logfile}' using 1:4 with lp ls ${line_style} title '${ct}';"`
            `"set key top left box vertical width 1 height 1 maxcols 1 spacing 1 font 'Verdana,20';"`
	    `"set tmargin at screen TOP;"`
	    `"set bmargin at screen TOP+0.02-DY;"`
            `"set title \"${printname}\";"`
            `"set label 3 \"Runtime in seconds\" at screen 0.02,0.75 rotate by 90;"`
	    `"plot '$logfile' using 1:2 with lp ls ${line_style} title '${ct}';"

    gnuplot -e "set datafile separator \",\";"`
                `"set terminal pngcairo size 1280,960"`
                `"   enhanced color font 'Verdana,26' ;"`
    	   `"set output '${fit1_fig}'; "`
    	   `"set key left top;"`
    	   `"f(x) = a*x**2 + b*x + c;"`
    	   `"set title \"${printname} \"; "`
    	   `"set xlabel \"${x_axis_label}\"; "`
    	   `"set ylabel \"Runtime in seconds\"; "`
           `"set yrange [0:*];"`
    	   `"set xtics nomirror; "`
    	   `"set ytics nomirror; "`
    	   `"fit f(x) '$logfile' using 1:2 via a, b, c;"`
    	   `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
    	   `"set pointintervalbox 3;"`
    	   `"plot '$logfile' using 1:2 with lp ls ${line_style} title '${ct}', "`
    	   `"f(x) ls 4 title '2nd-degree polynomial'"

    gnuplot -e "set datafile separator \",\";"`
                `"set terminal pngcairo size 1280,960"`
                `"   enhanced color font 'Verdana,26' ;"`
    	   `"set output '${fit2_fig}'; "`
    	   `"set key left top;"`
    	   `"f(x) = a*x**3 + b*x**2 + c*x + d;"`
    	   `"set title \"${printname}\"; "`
    	   `"set xlabel \"${x_axis_label}\"; "`
    	   `"set ylabel \"Runtime in seconds\"; "`
	   `"set yrange [0:*];"`
    	   `"set xtics nomirror; "`
    	   `"set ytics nomirror; "`
    	   `"fit f(x) '$logfile' using 1:2 via a, b, c, d;"`
    	   `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
    	   `"set pointintervalbox 3;"`
    	   `"plot '$logfile' using 1:2 with lp ls ${line_style} title '${ct}', "`
    	   `"f(x) ls 4 title  '3rd-degree polynomial'"
}

function plot_comparison()
{
    local name="$1"; shift
    local c1="$1"; shift
    local c2="$1"; shift
    local c1t="$1"; shift
    local c2t="$1"; shift
    local printname="$1"; shift
    local x_axis_label="$1"; shift

    local plot_dir="${OUT_DIR}/${name}"

    mkdir -p "$plot_dir"
    
    logfile1="$DATA_DIR/${name}${c1}.csv"
    logfile2="$DATA_DIR/${name}${c2}.csv"

    local lpc_fig="${plot_dir}/lpc.png"
    local rt_fig="${plot_dir}/rt.png"
    local casts_fig="${plot_dir}/casts.png"
    local all_fig="${plot_dir}/all.png"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${rt_fig}';"`
            `"set key top left box vertical width 1 height 1 maxcols 1 spacing 1 font 'Verdana,20';"`
            `"set title \"${printname}\";"`
	    `"set xlabel \"${x_axis_label}\"; "`
	    `"set ylabel \"Runtime in seconds\"; "`
	    `"set yrange [0:*];"`
	    `"set xtics nomirror; "`
	    `"set ytics nomirror; "`
	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 5 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
	    `"plot '$logfile1' using 1:2 with lp ls 1 title '${c1t}', "`
	    `"'$logfile2' using 1:2 with lp ls 2 title '${c2t}'"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   noenhanced color font 'Verdana,26' ;"`
            `"set output '${lpc_fig}';"`
            `"set key bottom right font 'Verdana,20';"`
            `"set title \"${printname}\";"`
            `"set xlabel \"${x_axis_label}\";"`
            `"set ylabel \"Longest proxy chain\";"`
	    `"max(x,y) = (x > y) ? x : y;"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 5 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
            `"plot '${logfile1}' using 1:(max(\$16, (max(\$17, \$18)))) with lp ls 1 title '${c1t}',"`
            `"'${logfile2}' using 1:(max(\$16, (max(\$17, \$18)))) with lp ls 2 title '${c2t}'"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${casts_fig}';"`
            `"set key bottom right font 'Verdana,20';"`
            `"set title \"${printname}\";"`
            `"set xlabel \"${x_axis_label}\";"`
            `"set ylabel \"Runtime casts count\";"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 5 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
            `"plot '${logfile1}' using 1:4 with lp ls 1 title '${c1t}',"`
            `"'${logfile2}' using 1:4 with lp ls 2 title '${c2t}'"

    gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,1900"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${all_fig}';"`
            `"set lmargin at screen 0.15;"`
	    `"set rmargin at screen 0.95;"`
	    `"TOP=0.95;"`
	    `"DY = 0.29;"`
	    `"set multiplot;"`
            `"set xlabel \"${x_axis_label}\";"`
	    `"unset ylabel;"`
            `"set label 1 \"Longest proxy chain\" at screen 0.02,0.15 rotate by 90;"`
	    `"set tmargin at screen TOP-2*DY;"`
	    `"set bmargin at screen TOP-3*DY;"`
	    `"unset key;"`
	    `"max(x,y) = (x > y) ? x : y;"`
	    `"set yrange [0:*];"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 dt 2 lw 2 pt 5 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
            `"plot '${logfile1}' using 1:(max(\$16, (max(\$17, \$18)))) with lp ls 1 title '${c1t}',"`
            `"'${logfile2}' using 1:(max(\$16, (max(\$17, \$18)))) with lp ls 2 title '${c2t}';"`
	    `"unset xtics;"`
	    `"unset xlabel;"`
	    `"set format x '';"`
	    `"set yrange [0:*];"`
            `"set label 2 \"Runtime casts count\" at screen 0.02,0.45 rotate by 90;"`
	    `"set tmargin at screen TOP-DY;"`
	    `"set bmargin at screen TOP+0.02-2*DY;"`
	    `"unset key;"`
            `"plot '${logfile1}' using 1:4 with lp ls 1 title '${c1t}',"`
            `"'${logfile2}' using 1:4 with lp ls 2 title '${c2t}';"`
            `"set key top left box vertical width 1 height 1 maxcols 1 spacing 1 font 'Verdana,20';"`
	    `"set tmargin at screen TOP;"`
	    `"set bmargin at screen TOP+0.02-DY;"`
            `"set title \"${printname}\";"`
            `"set label 3 \"Runtime in seconds\" at screen 0.02,0.75 rotate by 90;"`
	    `"plot '$logfile1' using 1:2 with lp ls 1 title '${c1t}', "`
	    `"'$logfile2' using 1:2 with lp ls 2 title '${c2t}'"
}

main "$@"
