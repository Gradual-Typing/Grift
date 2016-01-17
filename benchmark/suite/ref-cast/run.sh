#!/bin/sh

# a larger number will result in requesting more memory to be
# allocated by twosomes which is a very expensive operation
reads=999999
writes=999999
ncasts=9999999
casts=11
schmldir=/u/dalmahal/Schml
# --------------------------------------------------------------------

name1=refread
name2=refwrite
name3=refcast
testdir=$schmldir/benchmark/suite
datadir=$schmldir/benchmark/suite/ref-cast/data
outdir=$schmldir/benchmark/suite/ref-cast/output
# contains files required by tikz to output correct figures
# created by lua gnuplot-tikz.lua style
miscdir=$schmldir/benchmark/suite/ref-cast/misc
tmpdir=$schmldir/benchmark/suite/ref-cast/tmp
logfile1=$datadir/$name1.csv
logfile2=$datadir/$name2.csv
logfile3=$datadir/$name3.csv
TIMEFORMAT=%R

echo "Benchmarking reference read and write operations with different\
     number of casts"

# create the result directory if it does not exist
mkdir -p $datadir
mkdir -p $tmpdir
mkdir -p $outdir

# specialize all source files templates to a concrete number of
# iterations.
cd $testdir/ref-cast/src
sed "s/CAST-COUNT/$casts/;s/OP-COUNT/$reads/" < $name1-template> $tmpdir/$name1.schml
sed "s/CAST-COUNT/$casts/;s/OP-COUNT/$writes/" < $name2-template> $tmpdir/$name2.schml
sed "s/CAST-COUNT/$ncasts/" < $name3-template> $tmpdir/$name3.schml

# compile Schml source files, then enter the src directory.
cd $schmldir
racket benchmark.rkt $tmpdir
cd $tmpdir

echo "n,coercion-mean,coercion-std,twosome-mean,twosome-std" > $logfile1
echo "n,coercion-mean,coercion-std,twosome-mean,twosome-std" > $logfile2
echo "coercion-mean,coercion-std,twosome-mean,twosome-std" > $logfile3

# normalize to the cost of iteration in nano seconds.
let "c1=1000000000/$reads"
let "c2=1000000000/$writes"
let "c3=1000000000/$ncasts"

# run the same experiment $1 times to compute the mean and std
for i in `seq 1 $1`;
do
    # layout the result at each number of casts in one row space separated
    ./$name1.o1 | sed -n 's/.*: \([0-9]*.[0-9]*\)/\1/p' | paste -sd " " - >> $tmpdir/data1
    ./$name1.o2 | sed -n 's/.*: \([0-9]*.[0-9]*\)/\1/p' | paste -sd " " - >> $tmpdir/data2
    ./$name2.o1 | sed -n 's/.*: \([0-9]*.[0-9]*\)/\1/p' | paste -sd " " - >> $tmpdir/data3
    ./$name2.o2 | sed -n 's/.*: \([0-9]*.[0-9]*\)/\1/p' | paste -sd " " - >> $tmpdir/data4
    ./$name3.o1 | sed -n 's/.*: \([0-9]*.[0-9]*\)/\1/p' >> $tmpdir/data5
    ./$name3.o2 | sed -n 's/.*: \([0-9]*.[0-9]*\)/\1/p' >> $tmpdir/data6
    echo "finished run #$i"
done

awk -v c="$c3" '{print ($1 = $1*c) " " $2}' $tmpdir/data5 > $tmpdir/data5.tmp; mv $tmpdir/data5.tmp $tmpdir/data5
awk -v c="$c3" '{print ($1 = $1*c) " " $2}' $tmpdir/data6 > $tmpdir/data6.tmp; mv $tmpdir/data6.tmp $tmpdir/data6
read std1 mean1 <<< $( cat $tmpdir/data5 | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
read std2 mean2 <<< $( cat $tmpdir/data6 | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )

echo "$mean1,$std1,$mean2,$std2" >> $logfile3
for i in `seq 0 $((casts-1))`;
do
    read std1 mean1 <<< $( while read -a line; do echo -e "${line[$i]}"; \
			   done < $tmpdir/data1 | awk -v c="$c1" '{print ($1 = $1*c) " " $2}' | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    read std2 mean2 <<< $( while read -a line; do echo -e "${line[$i]}"; \
			   done < $tmpdir/data2 | awk -v c="$c1" '{print ($1 = $1*c) " " $2}' | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    read std3 mean3 <<< $( while read -a line; do echo -e "${line[$i]}"; \
			   done < $tmpdir/data3 | awk -v c="$c2" '{print ($1 = $1*c) " " $2}' | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    read std4 mean4 <<< $( while read -a line; do echo -e "${line[$i]}"; \
			   done < $tmpdir/data4 | awk -v c="$c2" '{print ($1 = $1*c) " " $2}' | awk -v var=$1 '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    echo "$i,$mean1,$std1,$mean2,$std2" >> $logfile1
    echo "$i,$mean3,$std3,$mean4,$std4" >> $logfile2
done

# compile figures to tikz tex code

gnuplot -e "set datafile separator \",\"; set term tikz standalone color size 5in,3in; "`
	   `"set output '$outdir/$name1.tex'; "`
	   `"set title \"Reference read\"; "`
	   `"set xrange [0:10]; set yrange [0:300]; "`
	   `"set xtics 0,1,10; set ylabel \"time in ns\"; "`
	   `"set xlabel \"number of casts\"; "`
	   `"plot '$datadir/$name1.csv' every ::1 using 1:2 with linespoints lc rgb \"blue\" title 'Coercions', "`
	   `"'$datadir/$name1.csv' every ::1 using 1:4 with linespoints lc rgb \"red\" title 'Twosomes'"

gnuplot -e "set datafile separator \",\"; set term tikz standalone color size 5in,3in; "`
	   `"set output '$outdir/$name2.tex'; "`
	   `"set title \"Reference write\"; "`
	   `"set xrange [0:10]; set yrange [0:300]; "`
	   `"set xtics 0,1,10; set ylabel \"time in ns\"; "`
	   `"set xlabel \"number of casts\"; "`
	   `"plot '$datadir/$name2.csv' every ::1 using 1:2 with linespoints lc rgb \"blue\" title 'Coercions', "`
	   `"'$datadir/$name2.csv' every ::1 using 1:4 with linespoints lc rgb \"red\" title 'Twosomes'"

# compile tex code

cp $miscdir/* $tmpdir
cp $outdir/$name1.tex $tmpdir
cp $outdir/$name2.tex $tmpdir
cd $tmpdir
lualatex --interaction=nonstopmode $name1.tex
lualatex --interaction=nonstopmode $name2.tex
mv $name1.pdf $outdir
mv $name2.pdf $outdir

