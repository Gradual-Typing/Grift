set terminal wxt

if (!exists("inFile")) exit error "usage: make plot <date-hash.dat>"

set title "Cost of Applying Casted Function"


set xtics 5
set mxtics 1
set xlabel "number of function casts"

set ylabel "time (microseconds)"
set autoscale

plot   inFile index 0 u 1:2:3 title "Twosomes"  w errorbars  lw 2,\
       inFile index 1 u 1:2:3 title "Coercions" w errorbars  lw 2

pause -1 "Press ENTER to exit"

