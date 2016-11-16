set -euo pipefail
#;set -o xtrace
date=`date +%Y_%m_%d_%H_%M_%S`
runs=`seq 1 10`

# compute the power set of string justapositions
function pow() {
    res=()
    for a in $1; do
	for b in $2; do
	    res+=("${a}${b}")
	done
    done
    echo "${res[@]}"
}

schmls=$(pow 'static/ dynamic/' 'coercions casts')
langs="c gambit racket ${schmls}"
tmpdir=tmp 

for lang in $langs; do
    echo $lang
    logout=$lang/logs/n-body.out
    logerr=$lang/logs/n-body.err
    for i in $runs; do
    	cat inputs/n-body | /usr/bin/time -f "time (sec) %e" \
    					  $lang/bin/n-body \
    					  >> $logout \
    					  2>> $logerr
    done
done

