set -euo pipefail
IFS=$'\n\t'

iterations=${1:-10000}
runs=${2:-1000}

if [ ! -d tmp/out ]; then
    mkdir -p tmp/out;
fi

for i in `seq 1 $runs`; do
    echo $i;
    for exe in tmp/exe/*; do
        log=tmp/out/`basename $exe`;
        echo $iterations | $exe >> $log;
    done
done
