set -euo pipefail

# Print array length followed by array_length number of random
# to a file.

array_length=$1
output_file=$2

n=$(($array_length-1))
echo $array_length > $output_file
for i in `seq 0 $n`; do
    echo $i >> $output_file
done
