set print "-"

if (!exists("tDataFile")) \
   print "variable tDataFile undefined"; \
   exit gnuplot;

if (!exists("cDataFile")) \
   print "variable cDataFile undefined"; \
   exit gnuplot;

if (!exist("oFile"))\
   print "variable oFile undefined"; \
   exit gnuplot;

if (!exist("titleString"))\
   print "variable titleString undefined";\
   exit gnuplot;
   
set datafile separator ",";
set terminal pngcairo enhanced color font 'Verdana,10';
set output oFile;
set yrange [0:100];
set title titleString;
set xlabel "slowdown";
set ylabel "How much of the code is typed";
plot cDataFile using 6:(100-$2) with points title 'Coercions', \
     tDataFile using 6:(100-$2) with points title 'Type-based casts';