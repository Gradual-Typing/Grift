#!/bin/sh

set -euo pipefail

read_csv_field ()
{
    local DATAFILE="$1"; shift
    local benchmark="$1";shift
    local ind="$1";    shift
    sed -r 's/("[^",]+),([^",]+")/\1###\2/g' "$DATAFILE" | awk -v v=$ind -v u=$benchmark -F, '$1 == u {print $v}' | sed 's/###/,/g';
}

diff()
{
    local name="$1"; shift
    benchmarks=( $(tail -n +2 "$DATA2_DIR/$name.log" | cut -d ',' -f1 ) )
    for b in "${benchmarks[@]}"; do
	t2=$(read_csv_field "$DATA2_DIR/$name.log" $b $i)
	t1=$(read_csv_field "$DATA1_DIR/$name.log" $b $i)
	echo $b $name " become faster by " $(echo "${t1} ${t2}" | awk '{printf "%.2f", $1/$2}') "x"
    done
}

main()
{
    USAGE="Usage: date1 date2 [config index]"
    if [ "$#" == "0" ]; then
	echo "$USAGE"
	exit 1
    fi
    local expirement1="$1"; shift
    local expirement2="$1"; shift
    
    
    declare -r TEST_DIR="$GRIFT_DIR/benchmark/suite/macro"
    declare -r EXP1_DIR="$TEST_DIR/extremes/$expirement1"
    declare -r DATA1_DIR="$EXP1_DIR/data"
    declare -r OUT1_DIR="$EXP1_DIR/output"
    declare -r EXP2_DIR="$TEST_DIR/extremes/$expirement2"
    declare -r DATA2_DIR="$EXP2_DIR/data"
    declare -r OUT2_DIR="$EXP2_DIR/output"

    . "lib/runtime.sh"

    cd "$GRIFT_DIR"

    local i
    if [ "$#" == "1" ]; then
	i="$1"; shift
	i=$(($i + 1))
	diff static
	echo "--------------------------------------------------------"
	diff dyn
    fi
}

main "$@"
