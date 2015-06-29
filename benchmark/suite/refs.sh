#!/bin/sh
num=5
cd refs
echo "References"
./run.sh $num
cd ..;cd loops
echo "----------------------------------"
echo "Loop overhead"
./run.sh $num
cd ..

gnuplot -e "set datafile separator \",\"; set term pngcairo dashed; set output 'refs.png'; set title \"References read and write benchmark\"; set xrange [0:4]; set yrange [0:4]; set ytics 0,.2,4; set xtics 0,1,4; set ylabel \"time in seconds\"; set xlabel \"samples\"; set grid; set style line 1 lt 1 lc rgb \"blue\" dashtype 2; set style line 2 lt 1 lc rgb \"red\" dashtype 2; plot 'refs/ref.csv' every ::1 using 0:1 with linespoints title 'GCC pointers', 'refs/ref.csv' every ::1 using 0:2 with linespoints title \"GambitC set\!\", 'loops/loop.csv' every ::1 using 0:1 with linespoints ls 1 title 'GCC loop overhead', 'loops/loop.csv' every ::1 using 0:2 with linespoints ls 2 title 'GambitC loop overhead'"
sxiv refs.png
