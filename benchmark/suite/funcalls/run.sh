#!/bin/sh


gloop=99999
casts=1
loop=99999999
schmldir=/u/dalmahal/Schml
# --------------------------------------------------------------------

name=call
testdir=$schmldir/benchmark/suite
dir=$testdir/funcalls
datadir=$dir/data
outdir=$dir/output
tmpdir=$dir/tmp
logfile1=$datadir/$name.csv
logfile5=$outdir/${name}-dynamic.csv
TIMEFORMAT=%R

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

# specialize all source files templates to a concrete number of
# iterations.
# cd $testdir/refs/src
# sed "s/OP-COUNT/$gloop/" < ${name}1-template> $tmpdir/${name}1.schml
# sed "s/OP-COUNT/$loop/" < c${name}-template> $tmpdir/c${name}.c

cp $dir/src/* $tmpdir

# calculate the overhead of empty loops
cd $testdir/loop
./run.sh $1
# read cloop <<< $(while read -a line; do echo -e "${line[0]}"; done < $testdir/loop/data/loop.csv)
read sloop <<< $(while read -a line; do echo -e "${line[1]}"; done < $testdir/loop/data/loop.csv)
read schmlloop <<< $(while read -a line; do echo -e "${line[2]}"; done < $testdir/loop/data/loop.csv)

echo "Benchmarking function calls across many systems"

# compile Schml source files, then enter the tmp directory
cd $schmldir
racket benchmark.rkt $tmpdir
cd $tmpdir

# compile scheme source files
gsc -exe -cc-options -O3 s$name.scm

# # compile C source files
# gcc c$name.c -O3 -std=c11 -o c$name.o

# normalize to the cost of iteration in nano seconds.
c1=$(echo "1000000000/$gloop" | bc -l)
# c2=$(echo "1000000000/$loop" | bc -l)
c3=$(echo "1000000/$loop" | bc -l)

# if Schml decided to return more verpose data about timing,
# remove ;q
for i in `seq 1 $1`;
do
    # { time ./s$name >/dev/null 1 ; } 2>> $gambitclog
    echo $gloop | ./${name}.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name}1.log
    echo $gloop | ./${name}.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name}2.log
    ./s$name $loop | sed -n '3,3s/ *\([0-9]*\).*/\1/p' | awk -v c="$c3" '{printf("%.2f\n", ($1*c)-sloop)}' >> s$name.log
    # ./c${name}.o | sed -n 's/.*: \([0-9]*\)/\1/p' | awk -v c="$c2" '{printf("%.2f\n", ($1*c)-cloop)}' >> c$name.log
    echo "finished run #$i"
done

# read c1_std c1_mean <<< $( cat c$name.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read s1_std s1_mean <<< $( cat s$name.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml1_std schml1_mean <<< $( cat ${name}1.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml2_std schml2_mean <<< $( cat ${name}2.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )

# echo ",mean,std dev" >> $logfile3
# printf "GCC,%s,%s\n" $c1_mean $c1_std >> $logfile3
# printf "Schml Coercion (GRef Int),%s,%s\n" $schml5_mean $schml5_std >> $logfile3
# printf "Schml Twosome (GRef Int),%s,%s\n" $schml6_mean $schml6_std >> $logfile3

echo ",mean,std dev" >> $logfile5
printf "GambitC,%s,%s\n" $s1_mean $s1_std >> $logfile5
printf "Schml (Coercion),%s,%s\n" $schml1_mean $schml1_std >> $logfile5
printf "Schml (Twosome),%s,%s\n" $schml2_mean $schml2_std >> $logfile5

# print out the results
# printf "\n%s - static\n" $name
# column -s, -t < $logfile3 | cat
printf "\n%s - dynamic\n" $name
column -s, -t < $logfile5 | cat

# save the data
echo "GambitC,Schml C dyn,Schml T dyn" > $logfile1
paste -d"," s${name}.log ${name}1.log ${name}2.log >> $logfile1
