set print "-"

if (!exists("tDataFile")) \
   print "variable tFile undefined"; \
   exit gnuplot;

if (!exists("cDataFile")) \
   print "variable cFile undefined"; \
   exit gnuplot;

if (!exist("oFile"))\
   print "variable oFile undefined"; \
   exit gnuplot;

if (!exist("titleString"))\
   print "variable titleString undefined";\
   exit gnuplot;

if (!exist("yMax"))\
   print "variable yMax undefined";\
   exit gnuplot;
   
set datafile separator ",";
set terminal pngcairo enhanced color font 'Verdana,10';
set output oFile;
set border 15 back;
set title titleString;
set xrange [0:10]; set yrange [0:yMax];
set xtics nomirror ("1x" 1,"2x" 2,"3x" 3,"4x" 4,"5x" 5,\
    	  	    "6x" 6,"7x" 7, "8x" 8, "9x" 9, "10x" 10,\
		    "15x" 15, "20x" 20);
set ytics nomirror 0,200;
set arrow from 1, graph(0,0) to 1, graph(1,1) nohead lc rgb "black" lw 2
plot cDataFile using 1:2 with lines lw 2  \
       title 'Coercions' smooth cumulative, \
     tDataFile using 1:2 with lines lw 2  \
       title 'Type-based casts' smooth cumulative