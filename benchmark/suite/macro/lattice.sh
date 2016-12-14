#!/bin/sh

# This script needs the following list of programs to be installed:
# dynamizer
# clang
# gambit-c
# racket
# gnuplot

set -euo pipefail
declare -r PRECISION=5
TIMEFORMAT=%R

# $1 - baseline system
# $2 - logfile index
# $3 - $path
# $4 - space-separated benchmark arguments
# $5 - disk aux name
# $RETURN - number of configurations
run_config()
{
    local baseline_system="$1"; shift
    local i="$1";               shift
    local path="$1";            shift
    local args="$1";            shift
    local disk_aux_name="$1";   shift
    
    local name=$(basename "$path")
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${i}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${i}.csv"
    local cache_file="${TMP_DIR}/static/${name}${disk_aux_name}${i}.cache"
    if [ -f $cache_file ]; then
	RETURN=$(cat "$cache_file")
    else
	local n=0 b
	$baseline_system "$name" "$args" "$disk_aux_name"
	local baseline="$RETURN"
	echo "name,precision,time,slowdown" > "$logfile1"
	for b in $(find "$path" -name "*.o$i"); do
	    let n=n+1
	    local binpath="${b%.*}"
	    local bname=$(basename "$binpath")
	    local p=$(sed -n 's/;; \([0-9]*.[0-9]*\) %/\1/p;q' < "${binpath}.schml")
	    avg "$b" "$args"
	    local t="$RETURN"
	    local f=$(echo "${t}/${baseline}" | bc -l | awk -v p="$PRECISION" '{printf "%.*f\n", p,$0}')
	    echo $n $b $f
	    printf "%d,%.2f,%.${PRECISION}f,%.2f\n" $bname $p $t $f >> $logfile1
	done
	cut -d, -f4 "$logfile1" | sed -n '1!p' | sort | uniq -c | awk ' { t = $1; $1 = $2; $2 = t; print; } ' | awk '{ $1=$1" ,";; print }' > "$logfile2"
	RETURN="$n"
	echo "$RETURN" > "$cache_file"
    fi
}

# $1 - config index
# $2 - $name
# $3 - space-separated benchmark arguments
# $4 - disk aux name
# RETURN - the ratio between the runtimes of the typed and untyped versions of a benchmark
compute_typed_untyped_ratio()
{
    local i="$1";              shift
    local name="$1";           shift
    local benchmark_args="$1"; shift
    local disk_aux_name="$1";  shift
    
    local cache_file="${TMP_DIR}/${name}${disk_aux_name}${i}_typed_untyped_ratio"
    if [ -f $cache_file ]; then
        RETURN=$(cat "$cache_file")
    else
	avg "${TMP_DIR}/static/./${name}.o${i}" "$benchmark_args"
	t11="$RETURN"
	avg "${TMP_DIR}/dyn/./${name}.o${i}" "$benchmark_args"
	t12="$RETURN"
	RETURN=$(echo "${t11}/${t12}" | bc -l | awk -v p="$PRECISION" '{printf "%.2f\n",$0}')
        echo "$RETURN" > $cache_file
    fi
}

# $1 - baseline system
# $2 - first config index
# $3 - second config index
# $4 - $path
# $5 - space-separated benchmark arguments
# $6 - output of dynamizer
# $7 - printed name of the benchmark
# $8 - disk aux name
gen_output()
{
    local baseline_system="$1"; shift
    local c1="$1";              shift
    local c2="$1";              shift
    local path="$1";            shift
    local benchmark_args="$1";  shift
    local dynamizer_out="$1";   shift
    local printname="$1";       shift
    local disk_aux_name="$1";   shift

    local type_constructor_count=$(echo "$dynamizer_out" | sed -n 's/[0-9]\+.\([0-9]\+\)/\1/p')
    local less_precise_count=$(echo "$dynamizer_out" | sed -n 's/\([0-9]\+\).*/\1/p')
    local lpc_t=$(echo "${less_precise_count}/1000000000" | bc -l | awk '{printf "%.2f\n",$0}')

    local name=$(basename "$path")
    local disk_name="${name}${disk_aux_name}_${c1}_${c2}"
    cum_perf_lattice_fig="${OUT_DIR}/cumperflattice/${disk_name}.png"
    cum_perf_lattice_tbl="${OUT_DIR}/cumperflattice/${disk_name}.tex"
    perf_lattice_fig="${OUT_DIR}/perflattice/${disk_name}.png"
    
    # if [[ ! -f "$cum_perf_lattice_fig" || ! -f "$cum_perf_lattice_tbl" || ! -f "$perf_lattice_fig" ]]; then
    rm -f "$cum_perf_lattice_fig" "$cum_perf_lattice_tbl" "$perf_lattice_fig"
    local logfile1="${DATA_DIR}/${name}${disk_aux_name}${c1}.log"
    local logfile2="${DATA_DIR}/${name}${disk_aux_name}${c1}.csv"
    local logfile3="${DATA_DIR}/${name}${disk_aux_name}${c2}.log"
    local logfile4="${DATA_DIR}/${name}${disk_aux_name}${c2}.csv"

    get_c_slowdown $baseline_system "$name" "$benchmark_args" "$disk_aux_name"
    local cr_t="$RETURN"
    get_gambit_slowdown $baseline_system "$name" "$benchmark_args" "$disk_aux_name"
    local gr_t="$RETURN"

    compute_typed_untyped_ratio "$c1" "$name" "$benchmark_args" "$disk_aux_name"
    local typed_untyped_ratio1="$RETURN"
    compute_typed_untyped_ratio "$c2" "$name" "$benchmark_args" "$disk_aux_name"
    local typed_untyped_ratio2="$RETURN"

    run_config $baseline_system "$c1" "$path" "$benchmark_args" "$disk_aux_name"
    run_config $baseline_system "$c2" "$path" "$benchmark_args" "$disk_aux_name"
    local n="$RETURN"
    
    local min1=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' "$logfile2")
    local min2=$(awk 'NR == 1 || $3 < min {line = $1; min = $3}END{print line}' "$logfile4")
    local max1=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' "$logfile2")
    local max2=$(awk -v max=0 '{if($1>max){want=$1; max=$1}}END{print want}' "$logfile4")
    local std1 std2 mean1 mean2
    read std1 mean1 <<< $( cat "$logfile2" | cut -d, -f1 | awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )
    read std2 mean2 <<< $( cat "$logfile4" | cut -d, -f1 | awk -v var=$n '{sum+=$1; sumsq+=$1*$1}END{printf("%.2f %.2f\n", sqrt(sumsq/NR - (sum/NR)**2), (sum/var))}' )

    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '${cum_perf_lattice_fig}';"`
	   `"set border 15 back;"`
      	   `"set title \"${printname}\";"`
    	   `"set xrange [0:10]; set yrange [0:${n}];"`
    	   `"set xtics nomirror (\"1x\" 1,\"2x\" 2,\"3x\" 3,\"4x\" 4,\"5x\" 5, \"6x\" 6,\"7x\" 7, \"8x\" 8, \"9x\" 9, \"10x\" 10, \"15x\" 15, \"20x\" 20);"`
    	   `"set ytics nomirror 0,200;"`
	   `"set arrow from 1,graph(0,0) to 1,graph(1,1) nohead lc rgb \"black\" lw 2;"`
    	   `"plot '${logfile2}' using 1:2 with lines lw 2 title 'Config ${c1}' smooth cumulative,"`
    	   `"'${logfile4}' using 1:2 with lines lw 2 title 'Config ${c2}' smooth cumulative"

    echo "\begin{tabular}{|l|l|l|}
\hline
\textbf{${printname}} & \multicolumn{2}{l|}{(${type_constructor_count} type nodes)} \\\ \hline
lattice size                & \multicolumn{2}{l|}{${lpc_t} B}         \\\ \hline
\multicolumn{3}{|l|}{}                                             \\\ \hline
Clang                       & \multicolumn{2}{l|}{${cr_t}x}           \\\ \hline
Gambit-C                    & \multicolumn{2}{l|}{${gr_t}x}           \\\ \hline
\multicolumn{3}{|l|}{}                                             \\\ \hline
typed/untyped ratio         & ${typed_untyped_ratio2}x             & ${typed_untyped_ratio1}x            \\\ \hline
min. slowdown               & ${min2}x             & ${min1}x            \\\ \hline
max. slowdown               & ${max2}x             & ${max1}x            \\\ \hline
mean slowdown               & ${mean2}x             & ${mean1}x            \\\ \hline
\end{tabular}
\end{table}" > "${cum_perf_lattice_tbl}"

    gnuplot -e "set datafile separator \",\"; set terminal pngcairo "`
      	   `"enhanced color font 'Verdana,10' ;"`
    	   `"set output '${perf_lattice_fig}';"`
    	   `"set yrange [0:100];"`
    	   `"set title \"${printname}\";"`
	   `"set xlabel \"slowdown\";"`
	   `"set ylabel \"How much of the code is typed\";"`
    	   `"plot '${logfile1}' using 4:(100-\$2) with points title 'Config ${c1}',"`
    	   `"'${logfile3}' using 4:(100-\$2) with points title 'Config ${c2}'"
    # fi
}

# $1 - baseline system
# $2 - first config index
# $3 - second config index
# $4 - benchmark filename without extension
# $5 - space-separated benchmark arguments
# $6 - nsamples
# $7 - aux name
run_benchmark()
{
    local baseline_system="$1"; shift
    local c1="$1";              shift
    local c2="$1";              shift
    local name="$1";            shift
    local benchmark_args="$1";  shift
    local nsamples="$1";        shift
    local aux_name="$1";        shift

    local lattice_path="${TMP_DIR}/static/${name}"
    local benchmark_file="${lattice_path}.schml"

    local disk_aux_name="" print_aux_name=""
    if [[ ! -z "${aux_name}" ]]; then
	disk_aux_name="_${aux_name}"
	print_aux_name=" (${aux_name})"
    fi
    
    local print_name="$(echo "$name" | tr _ " " | sed -e "s/\b\(.\)/\u\1/g" | tr " " "-")${print_aux_name}"

    local benchmark_args_file="${TMP_DIR}/${name}${disk_aux_name}.args"
    if [ -f benchmark_args_file ]; then
	benchmark_args=$(cat "$benchmark_args_file")
    else
	printf "Benchmark\t:%s\n" "$name" >> "$PARAMS_LOG"
	printf "Args\t\t:%s\n" "$benchmark_args" >> "$PARAMS_LOG"
	echo "$benchmark_args" > "$benchmark_args_file"
    fi

    local lattice_file="${lattice_path}/out"
    if [ ! -f "$lattice_file" ]; then
	rm -rf "$lattice_path"
	local dynamizer_out=$(dynamizer "$lattice_path" "$nsamples" | sed -n 's/.* \([0-9]\+\) .* \([0-9]\+\) .*/\1 \2/p')
	echo "$dynamizer_out" > "$lattice_file"
    fi
    racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${lattice_path}/"
    dynamizer_out=$(cat "$lattice_file")

    gen_output $baseline_system $c1 $c2 "$lattice_path" "$benchmark_args" "$dynamizer_out" "$print_name" "$disk_aux_name"
}

# $1 - baseline system
# $2 - first config index
# $3 - second config index
# $4 - nsamples
run_experiment()
{
    local baseline_system="$1"; shift
    local c1="$1";              shift
    local c2="$1";              shift
    local nsamples="$1";        shift

    local qs_bc_arg="1000"
    # local s=""
    # local n=$(($qs_bc_arg-1)) i
    # for i in `seq 0 $n`; do
    # 	x=$(( ( RANDOM % $qs_bc_arg )  + 1 ))
    # 	s="$s$x "
    # done
    qs_bc_arg="1000 507 878 629 30 278 193 694 490 616 130 462 972 197 528 941 321 11 591 184 229 247 268 328 449 466 743 871 301 221 756 411 860 698 457 218 681 849 763 350 466 833 968 848 160 906 59 506 633 18 373 806 399 896 503 666 769 253 341 240 314 899 406 935 815 109 726 550 741 540 27 249 72 307 868 541 249 686 387 949 75 235 550 528 273 29 609 293 146 477 432 47 678 732 807 567 550 741 752 753 269 884 638 24 467 854 976 814 29 609 24 172 764 806 9 782 325 237 259 65 135 374 677 165 950 151 302 686 784 588 7 305 140 460 476 528 240 873 525 178 182 388 764 189 260 65 696 759 923 985 925 142 440 981 181 375 650 914 688 614 611 181 291 427 153 914 744 750 205 517 417 7 223 743 721 13 687 324 446 724 295 677 427 168 847 12 333 386 984 296 527 52 665 252 115 764 945 599 480 318 978 540 146 246 826 417 106 104 130 313 473 177 733 960 959 754 349 886 645 83 284 843 75 155 47 714 236 383 269 717 469 336 663 483 594 469 208 449 489 117 925 294 685 766 464 881 230 521 782 638 15 353 813 232 664 54 642 145 965 118 548 578 454 372 161 523 2 545 642 426 923 581 385 397 224 669 572 338 909 758 936 965 537 116 139 427 897 455 292 929 704 459 238 172 512 238 811 358 724 426 219 720 27 32 529 140 570 575 820 746 338 97 43 522 94 244 85 506 521 112 537 471 759 205 859 491 266 104 187 885 902 116 685 15 922 185 7 430 196 983 278 210 71 151 348 674 612 416 296 650 210 540 108 392 912 713 12 328 145 669 886 165 923 603 220 1 537 760 257 261 41 921 979 800 928 692 658 888 146 410 616 922 167 259 412 583 405 977 719 654 335 646 1 664 900 768 886 449 627 213 914 144 486 113 273 829 386 818 728 796 582 897 685 957 960 101 572 143 970 542 148 129 418 473 981 674 957 927 641 333 678 446 523 410 906 437 765 911 254 439 947 780 557 284 150 347 828 800 508 161 924 30 597 482 961 30 733 561 958 657 985 20 369 210 473 881 172 108 136 789 810 238 461 91 508 397 892 217 328 652 406 355 925 790 335 846 702 64 871 266 291 63 360 449 343 230 401 852 724 87 152 936 694 41 291 859 529 336 28 970 522 336 177 422 216 357 664 165 228 777 792 796 517 59 318 870 350 926 834 458 904 506 130 860 401 62 985 703 723 278 429 790 67 912 65 464 77 477 925 362 968 890 251 703 843 996 766 497 930 41 55 195 169 220 376 557 582 839 539 606 698 897 618 706 887 930 276 680 536 477 38 374 601 902 12 544 636 51 638 684 350 513 585 693 470 648 936 661 473 349 605 740 845 306 283 184 635 164 235 584 235 422 594 39 864 708 154 165 371 720 795 280 691 399 86 177 731 621 539 803 72 515 959 251 303 697 448 908 744 208 16 862 207 793 475 655 18 224 13 415 10 206 712 828 694 416 797 862 397 131 864 97 933 624 290 954 915 70 479 289 5 326 345 697 346 810 142 393 593 682 700 928 330 534 422 290 660 456 853 983 631 828 706 221 339 875 270 418 556 321 441 959 572 24 628 943 761 375 772 793 505 561 607 599 922 139 324 728 93 299 333 588 717 88 328 61 471 344 592 366 523 883 10 793 896 779 770 784 73 988 863 909 855 203 495 273 946 841 31 802 579 477 629 819 654 284 931 859 55 254 629 164 620 432 206 404 735 704 743 450 826 70 922 70 350 321 626 324 814 11 709 411 423 829 356 411 827 604 334 250 656 346 629 682 653 755 554 388 683 570 408 315 265 943 508 585 508 75 902 601 471 931 331 429 99 907 208 481 620 724 928 517 94 104 337 61 597 952 458 143 362 788 180 194 737 867 532 572 642 500 184 971 102 824 548 805 442 156 68 980 144 52 846 423 358 750 542 769 900 723 781 72 518 487 350 90 915 360 208 332 568 168 364 946 450 398 698 570 895 413 446 787 53 165 170 298 968 652 663 933 590 702 860 32 292 792 544 861 192 884 825 876 386 845 116 8 490 700 22 480 259 564 292 859 172 991 776 90 569 11 501 978 948 169 176 44 713 671 821 237 317 357 469 503 874 907 199 486 481 616 923 307 425 32 316 818 812 600 847 830 471 372 675 670 4 368 778 7 198 790 167 456 275 977 330 454 563 54 635 10 638 8 67 653 4 34 692 452 59 562 390 708 612 525 493 304 36 330 870 683 514 733 755 409 488 291 705 972 382 621 76 "
    run_benchmark $baseline_system $c1 $c2 "quicksort" "$qs_bc_arg" "$nsamples" "bestcase"
    
    local qs_wc_arg="1000"
    local s=""
    local n=$(($qs_wc_arg-1)) i
    for i in `seq 0 $n`; do
	x=$(( n-i ))
	s="$s$x "
    done
    qs_wc_arg="\"$qs_wc_arg $s\""
    run_benchmark $baseline_system $c1 $c2 "quicksort" "$qs_wc_arg" "$nsamples" "worstcase"
    
    run_benchmark $baseline_system $c1 $c2 "matmult" "200" "$nsamples" ""

    run_benchmark $baseline_system $c1 $c2 "n-body" "10000" "$nsamples" ""

    # convert "*_${c1}_${c2}.png" -append "${c1}_${c2}".png

    echo "finished experiment comparing" $c1 "vs" $c2
}

main()
{
    USAGE="Usage: $0 nsamples loops [fresh|date] n_1,n_2 ... n_n"
    if [ "$#" == "0" ]; then
	echo "$USAGE"
	exit 1
    fi
    local nsamples="$1"; shift
    LOOPS="$1";          shift
    local date="$1";     shift

    if [ "$date" == "fresh" ]; then
	declare -r DATE=`date +%Y_%m_%d_%H_%M_%S`
    else
	declare -r DATE="$date"
	if [ ! -d "$SCHML_DIR/benchmark/suite/macro/lattice/$DATE" ]; then
	    echo "Directory not found"
	    exit 1
	fi
    fi
    
    declare -r TEST_DIR="$SCHML_DIR/benchmark/suite/macro"
    declare -r EXP_DIR="$TEST_DIR/lattice/$DATE"
    declare -r DATA_DIR="$EXP_DIR/data"
    declare -r OUT_DIR="$EXP_DIR/output"
    declare -r TMP_DIR="$EXP_DIR/tmp"
    declare -r SRC_DIR="$TEST_DIR/src"
    declare -r PARAMS_LOG="$EXP_DIR/params.txt"

    # create the result directory if it does not exist
    mkdir -p "$DATA_DIR"
    mkdir -p "$TMP_DIR"
    mkdir -p "$OUT_DIR/cumperflattice"
    mkdir -p "$OUT_DIR/perflattice"

    . "lib/runtime.sh"

    cd "$SCHML_DIR"

    local baseline_system=get_racket_runtime

    if [ "$date" == "fresh" ]; then
	# copying the benchmarks to a temporary directory
	cp -r ${SRC_DIR}/* $TMP_DIR

	# logging
	printf "Date\t\t:%s\n" "$DATE" >> "$PARAMS_LOG"
	MYEMAIL="`id -un`@`hostname -f`"
	printf "Machine\t\t:%s\n" "$MYEMAIL" >> "$PARAMS_LOG"
	schml_ver=$(git rev-parse HEAD)
	printf "Schml ver.\t:%s\n" "$schml_ver" >> "$PARAMS_LOG"
	clang_ver=$(clang --version | sed -n 's/clang version \([0-9]*.[0-9]*.[0-9]*\) .*/\1/p;q')
	printf "Clang ver.\t:%s\n" "$clang_ver" >> "$PARAMS_LOG"
	gambit_ver=$(gsc -v | sed -n 's/v\([0-9]*.[0-9]*.[0-9]*\) .*/\1/p;q')
	printf "Gambit ver.\t:%s\n" "$gambit_ver" >> "$PARAMS_LOG"
	racket_ver=$(racket -v | sed -n 's/.* v\([0-9]*.[0-9]*\).*/\1/p;q')
	printf "Racket ver.\t:%s\n" "$racket_ver" >> "$PARAMS_LOG"
	printf "loops:\t\t:%s\n" "$LOOPS" >> "$PARAMS_LOG"
	printf "nsamples\t:%s\n" "$nsamples" >> "$PARAMS_LOG"

	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${TMP_DIR}/static/"
	racket "${SCHML_DIR}/benchmark/benchmark.rkt" "${TMP_DIR}/dyn/"
    fi

    local i j
    if [ "$#" == "1" ]; then
	local config="$1";   shift
	for i in `seq ${config}`; do
	    for j in `seq ${i} ${config}`; do
		if [ ! $i -eq $j ]; then
		    run_experiment $baseline_system $i $j $nsamples
		fi
	    done
	done
    else
	while (( "$#" )); do
	    i=$1; shift
	    j=$1; shift
	    run_experiment $baseline_system $i $j $nsamples
	done
    fi
    echo "done."
}

main "$@"
