#!/bin/sh

set -euo pipefail

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
    declare -r EXP1_DIR="$TEST_DIR/lattice_bins/$expirement1"
    declare -r DATA1_DIR="$EXP1_DIR/data"
    declare -r OUT1_DIR="$EXP1_DIR/output"
    declare -r EXP2_DIR="$TEST_DIR/lattice_bins/$expirement2"
    declare -r DATA2_DIR="$EXP2_DIR/data"
    declare -r OUT2_DIR="$EXP2_DIR/output"

    . "lib/runtime.sh"

    cd "$GRIFT_DIR"

    local i
    if [ "$#" == "1" ]; then
	i="$1"; shift
	for b in $(find "$DATA1_DIR" -name "*$i.log"); do
	    geometric_mean "$b" 3
	    t1="$RETURN"
	    local file=$(basename "$b")
	    local name="${file%.*}"
	    geometric_mean "$DATA2_DIR/$file" 3
	    t2="$RETURN"
	    echo $name " become faster by " $(echo "${t1} ${t2}" | awk '{printf "%.2f", $1/$2}') "x"
	done
    fi
}

main "$@"
