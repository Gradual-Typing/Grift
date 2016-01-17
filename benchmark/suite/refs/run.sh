#!/bin/sh


gloop=999999
casts=1
loop=999999999
schmldir=/u/dalmahal/Schml
# --------------------------------------------------------------------

name1=refread
name2=refwrite
testdir=$schmldir/benchmark/suite
datadir=$schmldir/benchmark/suite/refs/data
outdir=$schmldir/benchmark/suite/refs/output
tmpdir=$schmldir/benchmark/suite/refs/tmp
logfile1=$datadir/$name1.csv
logfile2=$datadir/$name2.csv
logfile3=$outdir/$name1-static.csv
logfile4=$outdir/$name2-static.csv
logfile5=$outdir/$name1-dynamic.csv
logfile6=$outdir/$name2-dynamic.csv
TIMEFORMAT=%R

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

# specialize all source files templates to a concrete number of
# iterations.
cd $testdir/refs/src
sed "s/OP-COUNT/$gloop/" < ${name1}1-template> $tmpdir/${name1}1.schml
sed "s/OP-COUNT/$gloop/" < ${name2}1-template> $tmpdir/${name2}1.schml
sed "s/OP-COUNT/$gloop/" < ${name1}2-template> $tmpdir/${name1}2.schml
sed "s/OP-COUNT/$gloop/" < ${name2}2-template> $tmpdir/${name2}2.schml
sed "s/OP-COUNT/$gloop/" < ${name1}3-template> $tmpdir/${name1}3.schml
sed "s/OP-COUNT/$gloop/" < ${name2}3-template> $tmpdir/${name2}3.schml
sed "s/OP-COUNT/$loop/" < c${name1}-template> $tmpdir/c${name1}.c
sed "s/OP-COUNT/$loop/" < c${name2}-template> $tmpdir/c${name2}.c
sed "s/OP-COUNT/$loop/" < s${name1}-template> $tmpdir/s${name1}.scm
sed "s/OP-COUNT/$loop/" < s${name2}-template> $tmpdir/s${name2}.scm

# calculate the overhead of empty loops
cd $testdir/loop
./run.sh $1
read cloop <<< $(while read -a line; do echo -e "${line[0]}"; done < $testdir/loop/data/loop.csv)
read sloop <<< $(while read -a line; do echo -e "${line[1]}"; done < $testdir/loop/data/loop.csv)
read schmlloop <<< $(while read -a line; do echo -e "${line[2]}"; done < $testdir/loop/data/loop.csv)

echo "Benchmarking reference read and write operations across many systems"

# compile Schml source files, then enter the tmp directory
cd $schmldir
racket benchmark.rkt $tmpdir
cd $tmpdir

# compile scheme source files
#(declare (not safe))
gsc -prelude "(declare (standard-bindings)) (declare (block)) (declare (fixnum))" -exe -cc-options -O3 s$name1.scm
gsc -prelude "(declare (standard-bindings)) (declare (block)) (declare (fixnum))" -exe -cc-options -O3 s$name2.scm

# compile C source files
gcc c$name1.c -O3 -std=c11 -o c$name1.o
gcc c$name1.c -O3 -std=c11 -o c$name2.o

# normalize to the cost of iteration in nano seconds.
c1=$(echo "1000000000/$gloop" | bc -l)
c2=$(echo "1000000000/$loop" | bc -l)
c3=$(echo "1000000/$loop" | bc -l)

# if Schml decided to return more verpose data about timing,
# remove ;q
for i in `seq 1 $1`;
do
    ./${name1}1.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name1}1.log
    ./${name1}1.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name1}2.log
    ./${name1}2.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name1}3.log
    ./${name1}2.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name1}4.log
    ./${name1}3.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name1}5.log
    ./${name1}3.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name1}6.log
    ./${name2}1.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name2}1.log
    ./${name2}1.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name2}2.log
    ./${name2}2.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name2}3.log
    ./${name2}2.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name2}4.log
    ./${name2}3.o1 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name2}5.log
    ./${name2}3.o2 | sed -n 's/.*: \([0-9]*\)/\1/p;q' | awk -v c="$c1" '{printf("%.2f\n", ($1*c)-schmlloop)}' >> ${name2}6.log
    ./s$name1 | sed -n '2,2s/ *\([0-9]*\).*/\1/p' | awk -v c="$c3" '{printf("%.2f\n", ($1*c)-sloop)}' >> s$name1.log
    ./s$name2 | sed -n '2,2s/ *\([0-9]*\).*/\1/p' | awk -v c="$c3" '{printf("%.2f\n", ($1*c)-sloop)}' >> s$name2.log
    ./c${name1}.o | sed -n 's/.*: \([0-9]*\)/\1/p' | awk -v c="$c2" '{printf("%.2f\n", ($1*c)-cloop)}' >> c$name1.log
    ./c${name2}.o | sed -n 's/.*: \([0-9]*\)/\1/p' | awk -v c="$c2" '{printf("%.2f\n", ($1*c)-cloop)}' >> c$name2.log
    echo "finished run #$i"
done

read c1_std c1_mean <<< $( cat c$name1.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read c2_std c2_mean <<< $( cat c$name2.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read s1_std s1_mean <<< $( cat s$name1.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read s2_std s2_mean <<< $( cat s$name2.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml1_std schml1_mean <<< $( cat ${name1}1.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml2_std schml2_mean <<< $( cat ${name1}2.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml3_std schml3_mean <<< $( cat ${name1}3.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml4_std schml4_mean <<< $( cat ${name1}4.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml5_std schml5_mean <<< $( cat ${name1}5.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml6_std schml6_mean <<< $( cat ${name1}6.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml7_std schml7_mean <<< $( cat ${name2}1.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml8_std schml8_mean <<< $( cat ${name2}2.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml9_std schml9_mean <<< $( cat ${name2}3.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml10_std schml10_mean <<< $( cat ${name2}4.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml11_std schml11_mean <<< $( cat ${name2}5.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read schml12_std schml12_mean <<< $( cat ${name2}6.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )

echo ",mean,std dev" >> $logfile3
printf "GCC,%s,%s\n" $c1_mean $c1_std >> $logfile3
printf "Schml Coercion (GRef Int),%s,%s\n" $schml5_mean $schml5_std >> $logfile3
printf "Schml Twosome (GRef Int),%s,%s\n" $schml6_mean $schml6_std >> $logfile3

echo ",mean,std dev" >> $logfile4
printf "GCC,%s,%s\n" $c2_mean $c2_std >> $logfile4
printf "Schml Coercion (GRef Int),%s,%s\n" $schml11_mean $schml11_std >> $logfile4
printf "Schml Twosome (GRef Int),%s,%s\n" $schml12_mean $schml12_std >> $logfile4

echo ",mean,std dev" >> $logfile5
printf "GambitC,%s,%s\n" $s1_mean $s1_std >> $logfile5
printf "Schml Coercion Dyn,%s,%s\n" $schml1_mean $schml1_std >> $logfile5
printf "Schml Twosome Dyn,%s,%s\n" $schml2_mean $schml2_std >> $logfile5
printf "Schml Coercion (GRef Dyn),%s,%s\n" $schml3_mean $schml3_std >> $logfile5
printf "Schml Twosome (GRef Dyn),%s,%s\n" $schml4_mean $schml4_std >> $logfile5

echo ",mean,std dev" >> $logfile6
printf "GambitC,%s,%s\n" $s2_mean $s2_std >> $logfile6
printf "Schml Coercion Dyn,%s,%s\n" $schml7_mean $schml7_std >> $logfile6
printf "Schml Twosome Dyn,%s,%s\n" $schml8_mean $schml8_std >> $logfile6
printf "Schml Coercion (GRef Dyn),%s,%s\n" $schml9_mean $schml9_std >> $logfile6
printf "Schml Twosome (GRef Dyn),%s,%s\n" $schml10_mean $schml10_std >> $logfile6

# print out the results
printf "\n%s - static\n" $name1
column -s, -t < $logfile3 | cat
printf "\n%s - static\n" $name2
column -s, -t < $logfile4 | cat
printf "\n%s - dynamic\n" $name1
column -s, -t < $logfile5 | cat
printf "\n%s - dynamic\n" $name2
column -s, -t < $logfile6 | cat

# save the data
echo "GCC,GambitC,Schml C dyn,Schml T dyn,Schml C gref dyn,Schml T gref dyn,Schml C gref int,Schml T gref int" > $logfile1
paste -d"," c${name1}.log s${name1}.log ${name1}1.log ${name1}2.log ${name1}3.log ${name1}4.log ${name1}5.log ${name1}6.log >> $logfile1
echo "GCC,GambitC,Schml C dyn,Schml T dyn,Schml C gref dyn,Schml T gref dyn,Schml C gref int,Schml T gref int" > $logfile2
paste -d"," c${name2}.log s${name2}.log ${name2}1.log ${name2}2.log ${name2}3.log ${name2}4.log ${name2}5.log ${name2}6.log >> $logfile2

#fff () { gcc c1.c $1 -S -o c1.oo; cat c1.oo; }
