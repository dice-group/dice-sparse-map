#!/usr/bin/env python3
"""Turns the raw file of run.sh into the CSV of the README plots.

    benchmarks/readme/collect.py <raw file>... --csv <csv>

The value of a cell (allocator, key type, panel, base, map) is the median over the rounds of the
geometric mean over the five sizes. The ratio is the value divided by the value of
`dice::sparse_map::sparse_map` (sparsity medium) in the same allocator, key type, panel and base. The CSV has
these columns:

    variant,key,panel,base,map,value,ratio,rounds,spread

`spread` is (largest - smallest) / median over the rounds, 0 for a cell with one round. The
program also prints the noise between the rounds per panel, and every cell that failed.

Standard library only.
"""
import argparse
import csv
import statistics
import sys
from collections import defaultdict

REFERENCE = "sparse-medium"
COLUMNS = ["variant", "key", "panel", "base", "map", "value", "ratio", "rounds", "spread"]


def read_raw(paths):
    values = defaultdict(list)
    failed = []
    for path in paths:
        with open(path) as f:
            for line in f:
                parts = line.split()
                if len(parts) < 2:
                    continue
                if parts[1] == "FAILED":
                    failed.append(line.strip())
                    continue
                # r<round> variant key map panel base v1..v5 geomean g
                if len(parts) != 13 or parts[11] != "geomean":
                    print(f"skipped: {line.strip()}", file=sys.stderr)
                    continue
                variant, key, name, panel, base = parts[1:6]
                values[(variant, key, panel, int(base), name)].append(float(parts[12]))
    return values, failed


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("raw", nargs="+")
    parser.add_argument("--csv", required=True)
    args = parser.parse_args()

    values, failed = read_raw(args.raw)
    rows = []
    for (variant, key, panel, base, name), xs in sorted(values.items()):
        median = statistics.median(xs)
        spread = (max(xs) - min(xs)) / median if len(xs) > 1 and median > 0 else 0.0
        rows.append({"variant": variant, "key": key, "panel": panel, "base": base, "map": name,
                     "value": median, "rounds": len(xs), "spread": spread})
    reference = {(r["variant"], r["key"], r["panel"], r["base"]): r["value"] for r in rows if r["map"] == REFERENCE}
    for r in rows:
        denominator = reference.get((r["variant"], r["key"], r["panel"], r["base"]))
        r["ratio"] = r["value"] / denominator if denominator else float("nan")

    with open(args.csv, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(COLUMNS)
        for r in rows:
            writer.writerow([r["variant"], r["key"], r["panel"], r["base"], r["map"], f"{r['value']:.4f}",
                             f"{r['ratio']:.4f}", r["rounds"], f"{r['spread']:.4f}"])
    print(f"wrote {len(rows)} cells to {args.csv}")

    print("\nnoise between rounds, (largest - smallest) / median of a cell, over the maps:")
    print(f"{'variant':8} {'key':4} {'panel':10} {'cells':>5} {'median':>7} {'largest':>8}  largest cell")
    by_panel = defaultdict(list)
    for r in rows:
        if r["rounds"] > 1:
            by_panel[(r["variant"], r["key"], r["panel"])].append(r)
    for (variant, key, panel), cells in sorted(by_panel.items()):
        spreads = [c["spread"] for c in cells]
        worst = max(cells, key=lambda c: c["spread"])
        print(f"{variant:8} {key:4} {panel:10} {len(cells):5} {statistics.median(spreads):7.1%} "
              f"{max(spreads):8.1%}  {worst['map']}")
    all_spreads = [r["spread"] for r in rows if r["rounds"] > 1]
    if all_spreads:
        print(f"all timed cells: median {statistics.median(all_spreads):.1%}, largest {max(all_spreads):.1%}")
    if failed:
        print("\nfailed:")
        for line in failed:
            print(f"  {line}")


if __name__ == "__main__":
    main()
