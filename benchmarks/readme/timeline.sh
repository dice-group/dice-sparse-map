#!/usr/bin/env bash
# Runs the allocation timeline: every map builds n entries and is destroyed, in its own process.
#
#   benchmarks/readme/timeline.sh -b <timeline bin dir> -o <out dir> [-n <n>] [-r "<rounds>"] [-m <regex>]
#
#   -b  the directory with the dsm_readme_timeline_* binaries (<build dir>/benchmarks/readme/bin/timeline)
#   -o  the output directory. raw.txt gets one line per process (lines are appended), and every
#       recording writes <map>-r<round>.csv.
#   -n  the entries, default 10000000
#   -r  the rounds, default "1 2 3". Odd rounds run the maps in list order, even rounds in reverse.
#   -m  only the maps whose binary name matches this regex (grep -E)
#
# Per round and map, three processes run one after the other:
#   every 1     the recording binary, every event recorded, writes the CSV
#   every 1000  the recording binary, only every 1000th event recorded (the cost of the recording)
#   every 0     the _plain binary, without the replaced malloc (the runtime without any counting)
# A line of raw.txt is `r<round> <the line of the binary>`, or `r<round> FAILED <binary> exit=<code>`.
# With CORE set, every process runs under `taskset -c $CORE`.
set -uo pipefail
export LC_ALL=C

bin_dir="" out="" n=10000000 rounds="1 2 3" filter=""
while getopts "b:o:n:r:m:" opt; do
    case $opt in
        b) bin_dir=$OPTARG ;;
        o) out=$OPTARG ;;
        n) n=$OPTARG ;;
        r) rounds=$OPTARG ;;
        m) filter=$OPTARG ;;
        *) exit 1 ;;
    esac
done
if [ -z "$bin_dir" ] || [ -z "$out" ]; then
    echo "usage: $0 -b <timeline bin dir> -o <out dir> [-n n] [-r rounds] [-m regex]" >&2
    exit 1
fi
mkdir -p "$out"

mapfile -t binaries < <(find "$bin_dir" -maxdepth 1 -type f -name 'dsm_readme_timeline_*' ! -name '*_plain' -printf '%f\n' | sort)
if [ -n "$filter" ]; then
    mapfile -t binaries < <(printf '%s\n' "${binaries[@]}" | grep -E "$filter")
fi
if [ ${#binaries[@]} -eq 0 ]; then
    echo "no binaries in $bin_dir" >&2
    exit 1
fi

pin=()
if [ -n "${CORE:-}" ]; then
    pin=(taskset -c "$CORE")
fi

# run <round> <label> <command...>: runs one process and appends its line to raw.txt
run() {
    local round=$1 label=$2
    shift 2
    echo "$(date -Is) r$round $label load $(cut -d' ' -f1-3 /proc/loadavg)" >&2
    local output code
    output=$("${pin[@]}" "$@")
    code=$?
    if [ $code -eq 0 ] && [[ $output == *total_s* ]]; then
        echo "r$round $output" >>"$out/raw.txt"
    else
        echo "r$round FAILED $label exit=$code" >>"$out/raw.txt"
        echo "FAILED $label exit=$code" >&2
    fi
}

for round in $rounds; do
    order=("${binaries[@]}")
    if [ $((round % 2)) -eq 0 ]; then
        order=()
        for ((i = ${#binaries[@]} - 1; i >= 0; i--)); do
            order+=("${binaries[$i]}")
        done
    fi
    for name in "${order[@]}"; do
        map=${name#dsm_readme_timeline_std_u64_}
        run "$round" "$name" "$bin_dir/$name" timeline "$n" "$out/$map-r$round.csv"
        run "$round" "$name every 1000" env DSM_README_TIMELINE_EVERY=1000 "$bin_dir/$name" timeline "$n"
        if [ -x "$bin_dir/${name}_plain" ]; then
            run "$round" "${name}_plain" "$bin_dir/${name}_plain" timeline "$n"
        fi
    done
done
