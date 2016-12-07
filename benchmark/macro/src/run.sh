set -euo pipefail
#set -o xtrace
#The first argument is the number of runs
runs=`seq 1 ${1}`
#The second argument is the path to the binary
bin=${2}
prog=$(basename $bin)
#The third and fourth arguments are the output files
out=${3}
err=${4}
#The fifth argument is the input file path
in=${5}
time=/usr/bin/time

for i in $runs; do
  cat $in | $time -f "time (sec) %e" $bin >> $out 2>> $err
done

# compute the power set of string justapositions
# function pow() {
#     res=()
#     for a in $1; do
# 	for b in $2; do
# 	    res+=("${a}${b}")
# 	done
#     done
#     echo "${res[@]}"
# }

# schmls=$(pow 'static/ dynamic/' 'coercions casts')
# langs="c gambit racket ${schmls}"
# tmpdir=tmp 



