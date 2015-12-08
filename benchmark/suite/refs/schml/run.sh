#!/bin/sh

name1=refread
name2=refwrite
name3=refcast
# a larger number will result in requesting more memory to be
# allocated by twosomes which is a very expensive operation
reads=999999
writes=999999
ncasts=9999999
casts=11
TIMEFORMAT=%R
logfile1=$name1.csv
logfile2=$name2.csv
logfile3=$name3.csv
schmldir=/u/dalmahal/Schml
testdir=$schmldir/benchmark/suite/refs/schml/

rm -f $logfile1 $name1.schml $logfile2 $name2.schml $logfile3 $name3.schml

cd $testdir
sed "s/CAST-COUNT/$casts/" < $name1-template> $name1
sed "s/OP-COUNT/$reads/" < $name1> $name1.schml
sed "s/CAST-COUNT/$casts/" < $name2-template> $name2
sed "s/OP-COUNT/$writes/" < $name2> $name2.schml
sed "s/CAST-COUNT/$ncasts/" < $name3-template> $name3.schml
rm -f $name1 $name2
cd $schmldir
racket benchmark.rkt $testdir
cd $testdir

echo "n,coercion-mean,coercion-std,twosome-mean,twosome-std" > $logfile1
echo "n,coercion-mean,coercion-std,twosome-mean,twosome-std" > $logfile2
echo "n,coercion-mean,coercion-std,twosome-mean,twosome-std" > $logfile3

# run the same experiment $1 times to compute the mean and std
for i in `seq 1 $1`;
do
    # layout the result at each number of casts in one row space separated
    ./$name1.o1 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' | paste -sd " " - >> data1
    ./$name1.o2 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' | paste -sd " " - >> data2
    ./$name2.o1 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' | paste -sd " " - >> data3
    ./$name2.o2 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' | paste -sd " " - >> data4
    ./$name3.o1 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' >> data5
    ./$name3.o2 | sed -n 's/.*: \([0-9].[0-9]*\)/\1/p' >> data6
done

read std1 mean1 <<< $( cat data5 | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
read std2 mean2 <<< $( cat data6 | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
echo "$ncasts,$mean1,$std1,$mean2,$std2" >> $logfile3

for i in `seq 0 $((casts-1))`;
do
    read std1 mean1 <<< $( while read -a line; do echo -e "${line[$i]}"; \
			   done < data1 | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
    read std2 mean2 <<< $( while read -a line; do echo -e "${line[$i]}"; \
			   done < data2 | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
    read std3 mean3 <<< $( while read -a line; do echo -e "${line[$i]}"; \
			   done < data3 | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
    read std4 mean4 <<< $( while read -a line; do echo -e "${line[$i]}"; \
			   done < data4 | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)" "(sum/var)}' )
    echo "$i,$mean1,$std1,$mean2,$std2" >> $logfile1
    echo "$i,$mean3,$std3,$mean4,$std4" >> $logfile2
done

# awk '{printf "%d, %s\n", NR-1, $0}' data >> $logfile1

rm -f data1 data2 data3 data4 data5 data6

cat $logfile3
