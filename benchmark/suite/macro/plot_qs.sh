#!/bin/sh
set -euo pipefail

. lib/runtime.sh


date="2018_04_15_01_58_24"
c1=17
c2=7

c1t="Coercions"
c2t="Type-Based casts"

LOOPS=1
PRECISION=5

griftdir=$GRIFT_DIR
name=quicksort
printname="$name"
# --------------------------------------------------------------------

TEST_DIR=$griftdir/benchmark/suite/macro
LIB_DIR="$TEST_DIR/lib"
qscomplexdir=$TEST_DIR/quicksort-complex/$date
datadir=$qscomplexdir/data
outdir=$qscomplexdir/output
tmpdir=$qscomplexdir/tmp
miscdir=$TEST_DIR/misc
srcdir=$TEST_DIR/src
paramfile=$qscomplexdir/params.txt
TIMEFORMAT=%R

lpc_fig=${outdir}/lpc.png
rt_fig=${outdir}/rt.png
casts_fig=${outdir}/casts.png
all_fig=${outdir}/all.png

DPURPLE='#7b3294'
DGREEN='#008837'
SYELLOW='#fdb863'
SPURPLE='#5e3c99'
color1="$DGREEN"
color2="$DPURPLE"

logfile1="$datadir/$c1.csv"
logfile2="$datadir/$c2.csv"

grift_regex="sed -n 's/.*): \\([0-9]\\+\\)/\\1/p'"

cd $tmpdir

gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${rt_fig}';"`
            `"set key top left box vertical width 1 height 1 maxcols 1 spacing 1 font 'Verdana,20';"`
            `"set title \"${printname}\";"`
	   `"set xlabel \"Array length\"; "`
	   `"set ylabel \"Runtime in seconds\"; "`
	   `"set xtics nomirror; "`
	   `"set ytics nomirror; "`
	   `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	   `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	   `"set pointintervalbox 3;"`
	   `"plot '$logfile1' using 1:2 with lp ls 1 title '${c1t}', "`
	   `"'$logfile2' using 1:2 with lp ls 2 title '${c2t}'"

gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
	   `"set output '$outdir/${name}_coercions_fitting.png'; "`
	   `"set key left top;"`
	   `"f(x) = a*x**2 + b*x + c;"`
	   `"set title \"${printname} \"; "`
	   `"set xlabel \"Array length\"; "`
	   `"set ylabel \"Runtime in seconds\"; "`
	   `"set xtics nomirror; "`
	   `"set ytics nomirror; "`
	   `"fit f(x) '$logfile1' using 1:2 via a, b, c;"`
	   `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	   `"set pointintervalbox 3;"`
	   `"plot '$logfile1' using 1:2 with lp ls 1 title '${c1t}', "`
	   `"f(x) ls 4 title '2nd-degree polynomial'"

gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
	   `"set output '$outdir/${name}_typebased_fitting.png'; "`
	   `"set key left top;"`
	   `"f(x) = a*x**3 + b*x**2 + c*x + d;"`
	   `"set title \"${printname}\"; "`
	   `"set xlabel \"Array length\"; "`
	   `"set ylabel \"Runtime in seconds\"; "`
	   `"set xtics nomirror; "`
	   `"set ytics nomirror; "`
	   `"fit f(x) '$logfile2' using 1:2 via a, b, c, d;"`
	   `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	   `"set pointintervalbox 3;"`
	   `"plot '$logfile2' using 1:2 with lp ls 2 title '${c2t}', "`
	   `"f(x) ls 4 title  '3rd-degree polynomial'"

gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   noenhanced color font 'Verdana,26' ;"`
            `"set output '${lpc_fig}';"`
            `"set key bottom right font 'Verdana,20';"`
            `"set title \"${printname}\";"`
            `"set xlabel \"Array length\";"`
            `"set ylabel \"Longest proxy chain\";"`
	    `"max(x,y) = (x > y) ? x : y;"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set pointintervalbox 3;"`
            `"plot '${logfile1}' using 1:(max(\$16, (max(\$17, \$18)))) with lp ls 1 title '${c1t}',"`
            `"'${logfile2}' using 1:(max(\$16, (max(\$17, \$18)))) with lp ls 2 title '${c2t}'"

gnuplot -e "set datafile separator \",\";"`
            `"set terminal pngcairo size 1280,960"`
            `"   enhanced color font 'Verdana,26' ;"`
            `"set output '${casts_fig}';"`
            `"set key bottom right font 'Verdana,20';"`
            `"set title \"${printname}\";"`
            `"set xlabel \"Array length\";"`
            `"set ylabel \"Runtime casts count\";"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
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
            `"set xlabel \"Array length\";"`
	    `"unset ylabel;"`
            `"set label 1 \"Longest proxy chain\" at screen 0.02,0.15 rotate by 90;"`
	    `"set tmargin at screen TOP-2*DY;"`
	    `"set bmargin at screen TOP-3*DY;"`
	    `"unset key;"`
	    `"max(x,y) = (x > y) ? x : y;"`
	    `"set yrange [0:*];"`
      	    `"set style line 1 lc rgb '$color1' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
	    `"set style line 2 lc rgb '$color2' lt 1 lw 2 pt 7 pi -1 ps 1.5;"`
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
