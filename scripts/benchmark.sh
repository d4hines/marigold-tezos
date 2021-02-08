path=$(pwd)

mkdir -p ./benchmark_data

cd src/proto_alpha/lib_protocol/benchmark
benchmark="$path/_build/default/src/proto_alpha/lib_protocol/benchmark/main.exe"

date_str=$(git log -1 --format=%ct)
branch=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD)
if [ "$branch" = "HEAD" ]; then
    branch=$CI_COMMIT_BRANCH
fi
sha=$(git rev-parse --short HEAD)

# Match and clean the data
number="(\d|_)+(\.\d+)?"
unit="\w(s|w)"

# Replaces leading strings
# Swaps spaces with commas
# Add the data and output
mkdir -p benchmark_data
$benchmark -ascii | tee $path/benchmark_data/data.txt
cat $path/benchmark_data/data.txt | grep -oP ".+(\s+${number}${unit}){4}" | awk '{$1=$1};1' | awk '{$1=$1}1' OFS="," | awk "\$0=\"$date_str,$branch,$sha,\"\$0" > $path/benchmark_data/data.csv

cd $path

if [ "$1" = "--publish" ]; then
  # Fetch old results and append to file
  curl https://tezos-benchmark.surge.sh/data.csv >> benchmark_data/data.csv
  # Upload new results
  echo tezos-benchmark.surge.sh > benchmark_data/CNAME
  echo '*' > benchmark_data/CORS
  surge benchmark_data
fi
