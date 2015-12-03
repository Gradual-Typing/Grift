set terminal wxt

if (!exists("inFile")) exit error "usage: make plot <date-hash.dat>"

set title "Cost of Applying Casted Function"


set xtics 5
set mxtics 1
set xlabel "number of function casts"

set ylabel "time (seconds)"
set autoscale

plot   inFile index 0 u 1:2 t "Twosomes" w l lw 2,\
       inFile index 1 u 1:2 t "Coercions" w l lw 2

pause -1 "Press ENTER to exit"

