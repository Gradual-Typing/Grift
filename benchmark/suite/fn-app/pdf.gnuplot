set terminal pdf

if (!exists("inFile") || !exists("outFile"))\
   exit error "no input or output files"

set output outFile

set title "Cost of Applying Casted Function"


set xtics 5
set mxtics 1
set xlabel "number of function casts"

set ylabel "time (seconds)"
set autoscale

plot inFile index 0 u 1:2 t "Twosomes" w l lw 3,\
     inFile index 1 u 1:2 t "Coercions" w l lw 3



