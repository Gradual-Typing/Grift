#!/bin/sh

gnuplot -e "set datafile separator \",\"; set term pdfcairo; "`
	   `"set output 'refread.pdf'; "`
	   `"set title \"Reference read\"; "`
	   `"set xrange [0:10]; set yrange [0:.4]; set ytics 0,0.05,.4; "`
	   `"set xtics 0,1,10; set ylabel \"time in seconds\"; "`
	   `"set xlabel \"number of casts\"; "`
	   `"plot 'refread.csv' every ::1 using 1:2 with lines lc rgb \"blue\" title 'Coercions', "`
	   `"'refread.csv' every ::1 using 1:4 with lines lc rgb \"red\" title 'Twosomes'"

gnuplot -e "set datafile separator \",\"; set term pdfcairo; "`
	   `"set output 'refwrite.pdf'; "`
	   `"set title \"Reference write\"; "`
	   `"set xrange [0:10]; set yrange [0:.4]; set ytics 0,0.05,.4; "`
	   `"set xtics 0,1,10; set ylabel \"time in seconds\"; "`
	   `"set xlabel \"number of casts\"; "`
	   `"plot 'refwrite.csv' every ::1 using 1:2 with lines lc rgb \"blue\" title 'Coercions', "`
	   `"'refwrite.csv' every ::1 using 1:4 with lines lc rgb \"red\" title 'Twosomes'"
