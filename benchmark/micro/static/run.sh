set -euo pipefail
IFS=$'\n\t'

iterations=${1:-100}
runs=${2:-100}

if [ ! -d tmp/out ]; then
    mkdir -p tmp/out;
fi

for exe in tmp/exe/*; do
    log=tmp/out/`basename $exe`;
    for i in `seq 1 $runs`; do
        echo $exe $i;
        echo $iterations | $exe >> $log;
    done
done
