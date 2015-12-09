#!/bin/sh


gloop=999999
casts=1
loop=999999999
TIMEFORMAT=%R
name=ref
logfile=$name.csv
gcclog=gcc.log
gambitclog=gambitc.log
schmldir=/u/dalmahal/Schml
testdir=$schmldir/benchmark/suite/refs

rm -f $logfile $gcclog $gambitclog schml1.log schml2.log schml3.log schml4.log schml5.log schml6.log

sed "s/OP-COUNT/$gloop/" < ${name}1-template> ${name}1.schml
sed "s/OP-COUNT/$gloop/" < ${name}2-template> ${name}2.schml
sed "s/OP-COUNT/$gloop/" < ${name}3-template> ${name}3.schml
cd $schmldir
racket benchmark.rkt $testdir
cd $testdir

#i can not declare fixnum because the loop output is pretty big
#(declare (not safe))
gsc -prelude "(declare (standard-bindings)) (declare (block))" -exe -cc-options -O3 s$name.scm
gcc c$name.c -O3 -std=c11 -o c$name
for i in `seq 1 $1`;
do
    { time ./c$name $loop >/dev/null 1 ; } 2>> $gcclog
    { time ./s$name >/dev/null 1 ; } 2>> $gambitclog
    ./${name}1.o1 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' >> schml1.log
    ./${name}1.o2 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' >> schml2.log
    ./${name}2.o1 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' >> schml3.log
    ./${name}2.o2 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' >> schml4.log
    ./${name}3.o1 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' >> schml5.log
    ./${name}3.o2 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' >> schml6.log
    echo "finished run #$i"
done
awk -v gloop="$gloop" -v loop="$loop" '{print ($1 = $1*(loop/gloop)) " " $2}' schml1.log > schml1.tmp
awk -v gloop="$gloop" -v loop="$loop" '{print ($1 = $1*(loop/gloop)) " " $2}' schml2.log > schml2.tmp
awk -v gloop="$gloop" -v loop="$loop" '{print ($1 = $1*(loop/gloop)) " " $2}' schml3.log > schml3.tmp
awk -v gloop="$gloop" -v loop="$loop" '{print ($1 = $1*(loop/gloop)) " " $2}' schml4.log > schml4.tmp
awk -v gloop="$gloop" -v loop="$loop" '{print ($1 = $1*(loop/gloop)) " " $2}' schml5.log > schml5.tmp
awk -v gloop="$gloop" -v loop="$loop" '{print ($1 = $1*(loop/gloop)) " " $2}' schml6.log > schml6.tmp
mv schml1.tmp schml1.log
mv schml2.tmp schml2.log
mv schml3.tmp schml3.log
mv schml4.tmp schml4.log
mv schml5.tmp schml5.log
mv schml6.tmp schml6.log

cd ../loops
./run.sh $1
read gcc <<< $(while read -a line; do echo -e "${line[0]}"; done < loop.csv)
read gambit <<< $(while read -a line; do echo -e "${line[1]}"; done < loop.csv)
cd ../refs
awk -v gcc="$gcc" '{print ($1 = $1-gcc) " " $2}' $gcclog > $gcclog.tmp
awk -v gam="$gambit" '{print ($1 = $1-gam) " " $2}' $gambitclog > $gambitclog.tmp

mv $gcclog.tmp $gcclog
mv $gambitclog.tmp $gambitclog

read gcc_std gcc_mean <<< $( cat $gcclog | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read gambitc_std gambitc_mean <<< $( cat $gambitclog | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read schml1_std schml1_mean <<< $( cat schml1.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read schml2_std schml2_mean <<< $( cat schml2.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read schml3_std schml3_mean <<< $( cat schml3.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read schml4_std schml4_mean <<< $( cat schml4.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read schml5_std schml5_mean <<< $( cat schml5.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read schml6_std schml6_mean <<< $( cat schml6.log | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )

printf "GCC:\t\t\t\t\t mean=%s\t std=%s\n" $gcc_mean $gcc_std
printf "GambitC:\t\t\t\t mean=%s\t std=%s\n" $gambitc_mean $gambitc_std
printf "Schml Coercion Dyn:\t\t\t mean=%s\t std=%s\n" $schml1_mean $schml1_std
printf "Schml Twosome Dyn:\t\t\t mean=%s\t std=%s\n" $schml2_mean $schml2_std
printf "Schml Coercion (GRef Dyn):\t\t mean=%s\t std=%s\n" $schml3_mean $schml3_std
printf "Schml Twosome (GRef Dyn):\t\t mean=%s\t std=%s\n" $schml4_mean $schml4_std
printf "Schml Coercion (GRef Int):\t\t mean=%s\t std=%s\n" $schml5_mean $schml5_std
printf "Schml Twosome (GRef Int):\t\t mean=%s\t std=%s\n" $schml6_mean $schml6_std

echo "GCC,GambitC,Schml C dyn, Schml T dyn, Schml C gref dyn, Schml T gref dyn, Schml C gref int, Schml T gref int" > $logfile
paste -d"," $gcclog $gambitclog schml1.log schml2.log schml3.log schml4.log schml5.log schml6.log >> $logfile

rm -f $gcclog $gambitclog schml1.log schml2.log schml3.log schml4.log schml5.log schml6.log ref1.schml ref2.schml ref3.schml

#fff () { gcc c1.c $1 -S -o c1.oo; cat c1.oo; }
