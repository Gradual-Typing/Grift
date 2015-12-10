#!/bin/sh
num=5
cd refs
./run.sh $num
cd ..

# gnuplot -e "set datafile separator \",\"; set term pdfcairo dashed; "`
# 	   `"set output 'refs.pdf'; "`
# 	   `"set title \"References read and write benchmark\"; "`
# 	   `"set xrange [0:4]; set yrange [0:150]; set ytics 0,10,150; "`
# 	   `"set xtics 0,1,4; set ylabel \"time in seconds\"; "`
# 	   `"set xlabel \"samples\"; "`
# 	   `"plot 'refs/ref.csv' every ::1 using 0:1 with lines lc rgb \"blue\" title 'GCC pointers', "`
# 	   `"'refs/ref.csv' every ::1 using 0:2 with lines lc rgb \"red\" title \"GambitC set\!\", "`
# 	   `"'refs/ref.csv' every ::1 using 0:3 with lines title \"Schml Coercion Dyn guarded box\","`
# 	   `"'refs/ref.csv' every ::1 using 0:4 with lines title \"Schml Twosome Dyn guarded box\","`
# 	   `"'refs/ref.csv' every ::1 using 0:5 with lines title \"Schml Coercion (GRef Dyn) guarded box\","`
# 	   `"'refs/ref.csv' every ::1 using 0:6 with lines title \"Schml Twosome (GRef Dyn) guarded box\","`
# 	   `"'refs/ref.csv' every ::1 using 0:7 with lines title \"Schml Coercion (GRef Int) guarded box\","`
# 	   `"'refs/ref.csv' every ::1 using 0:8 with lines title \"Schml Twosome (GRef Int) guarded box\""
