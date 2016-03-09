set terminal tikz color

if (!exists("inFile"))\
   exit error "no input file: try \"-e \"inFile='data.dat'\""

set output "fn-app.tex"

set title "Cost of Applying Casted Functions"

set xtics 5
set mxtics 1
set xlabel "number of function casts"

set ylabel "time ($\mu$s)"
set autoscale

plot inFile index 0 u 1:2 t "Function Representation using Twosomes" lw 2 ,\
     inFile index 1 u 1:2 t "Hybrid Representation using Coercions" lw 2



