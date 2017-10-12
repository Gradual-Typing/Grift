#!/bin/sh

set -euo pipefail

create()
{
    local n="$1";shift
    local t1="$1";shift
    
    local t="$t1"
    local i
    for ((i=1;i<=n;i++)); do
	t="$t $t1"
    done
    RETURN="$t"
}

create_dyn_val()
{
    local n="$1";shift
    
    local t="(: 0 Dyn)"
    local i
    for ((i=1;i<=n;i++)); do
	t="$t (: $i Dyn)"
    done
    RETURN="$t"
}

create_gradual()
{
    local n="$1";shift
    local n1="$1";shift
    local t="$1";shift
    local t1="$1";shift
    
    local s="$t1"
    n1=$(($n1 - 1))
    local i
    for((i=1;i<=n;i++)); do
	if [ "$n1" -eq "0" ]; then
	    s="$s $t"
	else
	    s="$s $t1"
	    n1=$(($n1 - 1))
	fi
    done
    RETURN="$s"
}

main()
{
    USAGE="Usage: <number of casts>"
    if [ "$#" == "0" ]; then
	echo "$USAGE"
	exit 1
    fi
    
    local n="$1";shift
    n=$(($n - 1))
    
    create $n "Dyn"
    local t="$RETURN"
    create_dyn_val $n
    local v="$RETURN"
    echo "(define r : (MRef (Tuple $t)) (mbox (tuple $v)))" > mono.schml

    echo "(define sum : (MRef Int) (mbox 0))" >> mono.schml
    
    create_gradual $n 1 "Dyn" "Int"
    t="$RETURN"
    echo "(define r0 (: r (MRef (Tuple $t))))" >> mono.schml
    
    local j i
    for ((i=1;i<=n;i++)); do
	j=$(($i - 1))
	echo "(mbox-set! sum (+ (tuple-proj (munbox r$j) $i) (munbox sum)))" >> mono.schml
	create_gradual $n $(($i + 1)) "Dyn" "Int"
	t="$RETURN"
	echo "(define r$i (: r$j (MRef (Tuple $t))))" >> mono.schml
    done
    echo "(munbox sum)" >> mono.schml
}

main "$@"
