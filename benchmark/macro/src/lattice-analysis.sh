for f in grift/partial/*/out/*/*.err; do
    echo $f
#     path="${f%.*}";name=$(basename "$path")
#     # parameters are benchmark dependent
#     if [ "$name" = "quicksort_worstcase" ]; then
# 	command1=$quicksort_worstcase_rkt_command
# 	command2=$quicksort_worstcase_clang_command
# 	command3=$quicksort_worstcase_gambit_command
# 	grift_arg=$quicksort_worstcase_grift_arg
# 	working_nsamples=$quicksort_worstcase_nsamples
#     elif [ "$name" = "quicksort_bestcase" ]; then
# 	command1=$quicksort_bestcase_rkt_command
# 	command2=$quicksort_bestcase_clang_command
# 	command3=$quicksort_bestcase_gambit_command
# 	grift_arg=$quicksort_bestcase_grift_arg
# 	working_nsamples=$quicksort_bestcase_nsamples
#     elif [ "$name" = "matmult" ]; then
# 	command1=$matmult_rkt_command
# 	command2=$matmult_clang_command
# 	command3=$matmult_gambit_command
# 	grift_arg=$matmult_grift_arg
# 	working_nsamples=$matmult_nsamples
#     fi

#     logfile1=$datadir/${name}1.log
#     logfile2=$datadir/${name}1.csv
#     logfile3=$datadir/${name}2.log
#     logfile4=$datadir/${name}2.csv

#     names[$in]=$(echo $name | tr _ " " | sed -e "s/\b\(.\)/\u\1/g" | tr " " "-")

#     cd $tmpdir
#     c="$command1 | sed -n 's/cpu time: \\([0-9]*\\) .*/\\1/p;q' | awk -v c="$c1" -v p="$precision" '{printf(\"%.*f\\n\", p,(\$1*c))}'"
#     avg "$c"
#     baseline=$RETURN

#     c="$command2 | $grift_regex"
#     avg "$c"
#     ct=$RETURN
#     cr=$(echo "$ct/$baseline" | bc -l | awk -v p="$precision" '{printf "%.*f\n",p, $0}')
    
#     c="$command3 | sed -n '3,3s/ *\\([0-9]*\\).*/\\1/p' | awk -v c="$c1" -v p="$precision"  '{printf(\"%.*f\\n\",p, \$1*c)}'"
#     avg "$c"
#     gt=$RETURN
#     gr=$(echo "$gt/$baseline" | bc -l | awk -v p="$precision" '{printf "%.*f\n",p, $0}')
    
#     dynamizer_out=$(dynamizer $path ${working_nsamples} | sed -n 's/.* \([0-9]\+\) .* \([0-9]\+\) .*/\1 \2/p')
#     type_constructor_count=$(echo $dynamizer_out | sed -n 's/[0-9]\+.\([0-9]\+\)/\1/p')
#     less_precise_count=$(echo $dynamizer_out | sed -n 's/\([0-9]\+\).*/\1/p')
#     cd $griftdir
#     racket $griftdir/benchmark/benchmark.rkt $path/ $grift_mem_limit2
    
#     echo "name,precision,time,slowdown" > $logfile1
#     n=0
#     for b in $(find $path -name '*.o1'); do
# 	let n=n+1
# 	echo $b $n
# 	binpath="${b%.*}";bname=$(basename "$binpath")
# 	p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < $binpath.grift)
# 	c="echo $grift_arg | $b | $grift_regex"
# 	avg "$c"
# 	t=$RETURN	
# 	# echo $t
# 	f=$(echo "$t/$baseline" | bc -l | awk -v p="$precision" '{printf "%.*f\n", p,$0}')
# 	echo $f
# 	printf "%d,%.2f,%.${precision}f,%.2f\n" $bname $p $t $f >> $logfile1
#     done

#     n=0
#     echo "name,precision,time,slowdown" > $logfile3
#     for b in $(find $path -name '*.o2'); do
# 	let n=n+1
# 	binpath="${b%.*}";bname=$(basename "$binpath")
# 	echo $b $n
# 	p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < $binpath.grift)
# 	c="echo $grift_arg | $b | $grift_regex"
# 	avg "$c"
# 	t=$RETURN
# 	# echo $t
# 	f=$(echo "$t/$baseline" | bc -l | awk -v p="$precision" '{printf "%.*f\n", p,$0}')
# 	echo $f
# 	printf "%d,%.2f,%.${precision}f,%.2f\n" $bname $p $t $f >> $logfile3
#     done

#     cut -d, -f4 $logfile1 | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > $logfile2
#     cut -d, -f4 $logfile3 | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > $logfile4
    
#     min_c=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' $logfile2)
#     min_tb=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' $logfile4)

#     max_c=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' $logfile2)
#     max_tb=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' $logfile4)

#     read std_c mean_c <<< $( cat $logfile2 | cut -d, -f1 | awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
#     read std_tb mean_tb <<< $( cat $logfile4 | cut -d, -f1 | awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )

#     c11="echo $grift_arg | $tmpdir/static/$name.o1 | $grift_regex"
#     avg "$c11"
#     t11=$RETURN
#     c12="echo $grift_arg | $tmpdir/dyn/$name.o1 | $grift_regex"
#     avg "$c12"
#     t12=$RETURN
#     typed_untyped_ratio_c=$(echo "$t11/$t12" | bc -l | awk -v p="$precision" '{printf "%.2f\n",$0}')

#     c21="echo $grift_arg | $tmpdir/static/$name.o2 | $grift_regex"
#     avg "$c21"
#     t21=$RETURN
#     c22="echo $grift_arg | $tmpdir/dyn/$name.o2 | $grift_regex"
#     avg "$c22"
#     t22=$RETURN
#     typed_untyped_ratio_tb=$(echo "$t21/$t22" | bc -l | awk -v p="$precision" '{printf "%.2f\n",$0}')

#     cr_t=$(echo $cr | awk '{printf "%.2f\n",$0}');gr_t=$(echo $gr | awk '{printf "%.2f\n",$0}')
#     lpc_t=$(echo "$less_precise_count/1000000000" | bc -l | awk '{printf "%.2f\n",$0}')
    
#     gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
#       	   `"enhanced color font 'Verdana,10' ;"`
#     	   `"set output '$outdir/cumperflattice/${names[$in]}.png';"`
# 	   `"set border 15 back;"`
#       	   `"set title \"${names[$in]}\";"`
#     	   `"set xrange [0:10]; set yrange [0:${n}];"`
#     	   `"set xtics nomirror (\"1x\" 1,\"2x\" 2,\"3x\" 3,\"4x\" 4,\"5x\" 5, \"6x\" 6,\"7x\" 7, \"8x\" 8, \"9x\" 9, \"10x\" 10, \"15x\" 15, \"20x\" 20);"`
#     	   `"set ytics nomirror 0,200;"`
# 	   `"set arrow from 1,graph(0,0) to 1,graph(1,1) nohead lc rgb \"black\" lw 2;"`
#     	   `"plot '$logfile2' using 1:2 with lines lw 2 title 'Coercions' smooth cumulative,"`
#     	   `"'$logfile4' using 1:2 with lines lw 2 title 'Type-based casts' smooth cumulative"

#     echo "\begin{tabular}{|l|l|l|}
# \hline
# \textbf{${names[$in]}} & \multicolumn{2}{l|}{(${type_constructor_count} type nodes)} \\\ \hline
# lattice size                & \multicolumn{2}{l|}{${lpc_t} B}         \\\ \hline
# \multicolumn{3}{|l|}{}                                             \\\ \hline
# Clang                       & \multicolumn{2}{l|}{${cr_t}x}           \\\ \hline
# Gambit-C                    & \multicolumn{2}{l|}{${gr_t}x}           \\\ \hline
# \multicolumn{3}{|l|}{}                                             \\\ \hline
# typed/untyped ratio         & ${typed_untyped_ratio_tb}x             & ${typed_untyped_ratio_c}x            \\\ \hline
# min. slowdown               & ${min_tb}x             & ${min_c}x            \\\ \hline
# max. slowdown               & ${max_tb}x             & ${max_c}x            \\\ \hline
# mean slowdown               & ${mean_tb}x             & ${mean_c}x            \\\ \hline
# \end{tabular}
# \end{table}" > $outdir/cumperflattice/${names[$in]}.tex

#     gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
#       	   `"enhanced color font 'Verdana,10' ;"`
#     	   `"set output '$outdir/perflattice/${names[$in]}.png';"`
#     	   `"set yrange [0:100];"`
#     	   `"set title \"${names[$in]}\";"`
# 	   `"set xlabel \"slowdown\";"`
# 	   `"set ylabel \"How much of the code is typed\";"`
#     	   `"plot '$logfile1' using 4:(100-\$2) with points title 'Coercions',"`
#     	   `"'$logfile3' using 4:(100-\$2) with points title 'Type-based casts'"

#     let in=in+1
done
