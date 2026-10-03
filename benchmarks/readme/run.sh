#!/usr/bin/env bash
# Runs the README benchmark binaries and appends one line per map and panel to a raw file.
#
#   benchmarks/readme/run.sh -b <bin dir> -o <raw file> [-n <base>] [-r "<rounds>"] [-w <panels>] [-m <regex>]
#
#   -b  the directory with the dsm_readme_* binaries (<build dir>/benchmarks/readme/bin)
#   -o  the raw file. Lines are appended, so several calls can fill one file.
#   -n  the smallest of the five sizes, default 1000000
#   -r  the rounds to run, default "1 2 3". Odd rounds run the maps in list order, even rounds in
#       reverse order, so that a machine that drifts during the run does not always hit the same maps.
#   -w  the panels, comma separated, default build,buildfree,find,churn,iterate,rss,memory,disk.
#       rss and memory are counted, not timed, and give the same value in every round, so they run
#       in round 1 only. disk is not timed either, but it runs in every round, so that collect.py
#       shows whether the rounds agree.
#   -m  only the binaries whose name matches this regex (grep -E)
#
# Every map and panel runs in its own process. With CORE set, every process runs under
# `taskset -c $CORE`. A line of the raw file is
#   r<round> <variant> <key> <map> <panel> <base> <five values> geomean <value>
# and a binary that fails writes `r<round> FAILED <binary> <panel> exit=<code>` instead. The panels
# memory and disk run the binaries `<name>_memory` and `<name>_disk`, where they exist. disk writes
# one line per quantity (disk, diskpeak, ...).
# DSM_README_METALL_DIR sets where the metall binaries put their datastores.
set -uo pipefail
export LC_ALL=C

bin_dir="" raw="" base=1000000 rounds="1 2 3" panels=build,buildfree,find,churn,iterate,rss,memory,disk filter=""
while getopts "b:o:n:r:w:m:" opt; do
    case $opt in
        b) bin_dir=$OPTARG ;;
        o) raw=$OPTARG ;;
        n) base=$OPTARG ;;
        r) rounds=$OPTARG ;;
        w) panels=$OPTARG ;;
        m) filter=$OPTARG ;;
        *) exit 1 ;;
    esac
done
if [ -z "$bin_dir" ] || [ -z "$raw" ]; then
    echo "usage: $0 -b <bin dir> -o <raw file> [-n base] [-r rounds] [-w panels] [-m regex]" >&2
    exit 1
fi
IFS=, read -r -a panel_list <<<"$panels"
mkdir -p "$(dirname "$raw")"

# the maps, named by their timed binary, also where only the memory or disk binary was built (the
# checks are in the subdirectory check)
mapfile -t binaries < <(find "$bin_dir" -maxdepth 1 -type f -name 'dsm_readme_*' -printf '%f\n' | sed -E 's/_(memory|disk)$//' | sort -u)
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

for round in $rounds; do
    order=("${binaries[@]}")
    if [ $((round % 2)) -eq 0 ]; then
        order=()
        for ((i = ${#binaries[@]} - 1; i >= 0; i--)); do
            order+=("${binaries[$i]}")
        done
    fi
    for panel in "${panel_list[@]}"; do
        if [ "$round" -gt 1 ] && { [ "$panel" = rss ] || [ "$panel" = memory ]; }; then
            continue
        fi
        for name in "${order[@]}"; do
            binary=$bin_dir/$name
            if [ "$panel" = memory ] || [ "$panel" = disk ]; then
                binary=$bin_dir/${name}_$panel
            fi
            [ -x "$binary" ] || continue
            echo "$(date -Is) r$round $name $panel load $(cut -d' ' -f1-3 /proc/loadavg)" >&2
            output=$("${pin[@]}" "$binary" "$panel" "$base")
            code=$?
            if [ $code -eq 0 ] && [[ $output == *geomean* ]]; then
                while IFS= read -r line; do
                    echo "r$round $line" >>"$raw"
                done <<<"$output"
            else
                echo "r$round FAILED $name $panel exit=$code" >>"$raw"
                echo "FAILED $name $panel exit=$code" >&2
            fi
        done
    done
done
