#!/bin/sh

griftdir=/u/dalmahal/lattice/Schml
memlimit=9999999999
# --------------------------------------------------------------------

name=call
testdir=$griftdir/benchmark/suite/micro
dir=$testdir/dynamic/funcalls
datadir=$dir/data
outdir=$dir/output
tmpdir=$dir/tmp
logfile1=$outdir/${name}-gambit.csv
logfile2=$outdir/${name}-grift.csv
logfile3=$datadir/
TIMEFORMAT=%R

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

cp $dir/src/* $tmpdir

echo "Benchmarking function calls"

# compile Schml source files, then enter the tmp directory
cd $griftdir
racket benchmark.rkt $tmpdir $memlimit
cd $tmpdir

# compile scheme source files
gsc -exe -cc-options -O3 s$name.scm
gsc -exe -cc-options -O3 soverhead.scm

printf "n, Gambit Scheme mean, Gambit Scheme stddev\n" >> $logfile1
printf "n, coercions mean, coercions stddev, typebased mean, typebased stddev\n" >> $logfile2

# if Schml decided to return more verpose data about timing, remove q
size=10000000
# Schml segfault at bigger sizes
grift_size=10000000 #$(($size/1000))
# normalize to the cost of iteration in nano seconds.
c1=$(echo "1000000000/$grift_size" | bc -l)
c2=$(echo "1000000/$size" | bc -l)

# calculating overhead
for i in `seq 1 $1`; do
    echo $grift_size | ./overhead.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c))}' >> ${name}1.log
    echo $grift_size | ./overhead.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c))}' >> ${name}2.log
    ./soverhead $size | sed -n '3,3s/ *\([0-9]*\).*/\1/p' | awk -v c="$c2" '{printf("%.2f\n", ($1*c))}' >> s$name.log
    echo "finished run #$i"
done

read grift1overhead_std grift1overhead_mean <<< $( cat ${name}1.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read grift2overhead_std grift2overhead_mean <<< $( cat ${name}2.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read s1overhead_std s1overhead_mean <<< $( cat s$name.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )

rm ${name}1.log ${name}2.log s$name.log

for i in `seq 1 $1`; do
    echo $grift_size | ./${name}.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" -v o="$grift1overhead_mean" '{printf("%.2f\n", ($1*c)-o)}' >> ${name}1.log
    echo $grift_size | ./${name}.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" -v o="$grift2overhead_mean" '{printf("%.2f\n", ($1*c)-o)}' >> ${name}2.log
    ./s$name $size | sed -n '3,3s/ *\([0-9]*\).*/\1/p' | awk -v c="$c2" -v o="$s1overhead_mean"  '{printf("%.2f\n", ($1*c)-o)}' >> s$name.log
    echo "finished run #$i"
done
read s1_std s1_mean <<< $( cat s$name.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read grift1_std grift1_mean <<< $( cat ${name}1.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read grift2_std grift2_mean <<< $( cat ${name}2.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
printf "%s,%s,%s\n" $size $s1_mean $s1_std >> $logfile1
printf "%s,%s,%s,%s,%s\n" $grift_size $grift1_mean $grift1_std $grift2_mean $grift2_std >> $logfile2
# save the data
echo "GambitC,Schml coercions,Schml typebased" > ${logfile3}$size
paste -d"," s${name}.log ${name}1.log ${name}2.log >> ${logfile3}$size
rm -f s${name}.log ${name}1.log ${name}2.log

# print out the results
printf "\n%s - dynamic\n" $name
column -s, -t < $logfile1 | cat
column -s, -t < $logfile2 | cat

# cd ..
# python plot.py
