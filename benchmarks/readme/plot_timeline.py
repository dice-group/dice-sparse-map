#!/usr/bin/env python3
"""Draws the allocation timeline from the output directory of timeline.sh.

    benchmarks/readme/plot_timeline.py <timeline dir> <out dir>

Reads <timeline dir>/raw.txt and the CSVs of the recordings. Per map it picks the recording with
the median total runtime (of the rounds with every event recorded) and draws it as a step line:
x is the time since the map was constructed, y the bytes allocated from the heap, in MB (10^6
bytes). Writes <out dir>/bench-readme-timeline.svg and <out dir>/bench_readme_timeline.csv (the
plotted runs, columns map, round, seconds, bytes), and prints a table per map: peak MB, MB at the
end of the inserts, the runtime of the three modes (median and spread over the rounds) and the cost
of the recording per event.

The fonts and the text colors are the ones of plot.py. Standard library only.
"""
import argparse
import csv
import os
import statistics
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from plot import FONT, INK, INK_HEAD, INK_MUTED, GRID, BASELINE, SURFACE, esc  # noqa: E402

# CSV name -> (name of the struct in maps.hpp, legend, short label, color, line width, dash). The
# three sparsity levels and the two maps of unordered_dense are full width lines, each in its own
# hue. The other maps are thin, the node maps also dashed. As two sets against white (all pairs),
# the five wide lines have a CVD separation of 6.2 or more (sparse medium and low, also told apart
# by the labels at the line ends) and a normal-vision separation of 15 or more, the four thin lines
# 10.3 and 21.7.
MAPS = {
    "sparse-high": ("sparse_high", "dice::sparse_map::sparse_map, sparsity high", "sparse high", "#4a3aa7", 2.6, None),
    "sparse-medium": ("sparse_medium", "dice::sparse_map::sparse_map, sparsity medium (default)", "sparse medium", "#e34948", 2.6, None),
    "sparse-low": ("sparse_low", "dice::sparse_map::sparse_map, sparsity low", "sparse low", "#c98500", 2.6, None),
    "udm": ("unordered_dense", "ankerl::unordered_dense::map 5.2.0", "unordered_dense", "#008300", 2.6, None),
    "udm-segmented": ("unordered_dense_segmented", "ankerl::unordered_dense::segmented_map 5.2.0", "segmented", "#2a78d6", 2.6, None),
    "boost": ("boost_flat", "boost::unordered_flat_map", "boost flat", "#0891b2", 1.3, None),
    "absl": ("absl_flat", "absl::flat_hash_map", "absl flat", "#db2777", 1.3, None),
    "std": ("std_unordered_map", "std::unordered_map", "std", "#44403c", 1.3, "6 3"),
    "absl-node": ("absl_node", "absl::node_hash_map", "absl node", "#a855f7", 1.3, "6 3"),
}
TITLE = "Inserting 10 million uint64_t -> uint64_t pairs, every allocation counted at its malloc size"

WIDTH, HEIGHT = 1000, 660
LEFT, RIGHT, TOP, BOTTOM = 70, 40, 48, 130


def read_raw(path):
    """(map, mode) -> list of (round, fields). mode is 'full', 'every1000' or 'plain'."""
    runs = {}
    with open(path) as f:
        for line in f:
            parts = line.split()
            if len(parts) < 7 or parts[1] == "FAILED":
                if "FAILED" in line:
                    print("failed run:", line.strip(), file=sys.stderr)
                continue
            rnd = int(parts[0][1:])
            name = parts[3]
            fields = {parts[i]: float(parts[i + 1]) for i in range(6, len(parts) - 1, 2)}
            every = int(fields["every"])
            mode = {1: "full", 1000: "every1000", 0: "plain"}.get(every, f"every{every}")
            runs.setdefault((name, mode), []).append((rnd, fields))
    return runs


def read_timeline(path):
    points = []
    with open(path) as f:
        for r in csv.DictReader(f):
            points.append((float(r["seconds"]), int(r["bytes"])))
    return points


def median_run(runs):
    """the run with the median total runtime (the lower middle one for an even count)"""
    ordered = sorted(runs, key=lambda r: r[1]["total_s"])
    return ordered[(len(ordered) - 1) // 2]


def spread(values):
    """(largest - smallest) / median, in percent"""
    return (max(values) - min(values)) / statistics.median(values) * 100.0


def nice_step(vmax, target=6):
    for step in (0.05, 0.1, 0.2, 0.25, 0.5, 1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000):
        if vmax / step <= target:
            return step
    return vmax


def step_path(points, sx, sy):
    """a step line that holds each value until the next point"""
    x0, y0 = sx(points[0][0]), sy(points[0][1])
    d = [f"M{x0:.1f},{y0:.1f}"]
    for t, b in points[1:]:
        x, y = sx(t), sy(b)
        d.append(f"H{x:.1f}V{y:.1f}")
    return "".join(d)


def draw(out, plotted, peaks, ends):
    """ends: map -> (seconds, bytes) at the end of the inserts, where the label of the line goes"""
    tmax = max(points[-1][0] for points in plotted.values())
    ymax = max(peaks.values()) / 1e6
    xstep, ystep = nice_step(tmax), nice_step(ymax)
    xend = xstep * (int(tmax / xstep) + 1)
    yend = ystep * (int(ymax / ystep) + 1)
    pw, ph = WIDTH - LEFT - RIGHT, HEIGHT - TOP - BOTTOM

    def sx(t):
        return LEFT + t / xend * pw

    def sy(b):
        return TOP + ph - b / 1e6 / yend * ph

    s = (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {WIDTH} {HEIGHT}" width="{WIDTH}" '
         f'height="{HEIGHT}" font-family="{FONT}" font-size="12">\n')
    s += f'  <rect fill="{SURFACE}" width="{WIDTH}" height="{HEIGHT}"/>\n'
    s += (f'  <text x="{LEFT + pw / 2:.0f}" y="24" fill="{INK_HEAD}" font-size="14" font-weight="600" '
          f'text-anchor="middle">{esc(TITLE)}</text>\n')
    i = 0
    while i * ystep <= yend + 1e-9:
        y = sy(i * ystep * 1e6)
        s += f'  <line x1="{LEFT}" y1="{y:.1f}" x2="{LEFT + pw}" y2="{y:.1f}" stroke="{GRID}"/>\n'
        s += (f'  <text x="{LEFT - 8}" y="{y + 4:.1f}" fill="{INK_MUTED}" font-size="11.5" '
              f'text-anchor="end">{i * ystep:g}</text>\n')
        i += 1
    i = 0
    while i * xstep <= xend + 1e-9:
        x = sx(i * xstep)
        s += f'  <line x1="{x:.1f}" y1="{TOP}" x2="{x:.1f}" y2="{TOP + ph}" stroke="{GRID}"/>\n'
        s += (f'  <text x="{x:.1f}" y="{TOP + ph + 18}" fill="{INK_MUTED}" font-size="11.5" '
              f'text-anchor="middle">{i * xstep:g}</text>\n')
        i += 1
    s += (f'  <line x1="{LEFT}" y1="{TOP + ph}" x2="{LEFT + pw}" y2="{TOP + ph}" stroke="{BASELINE}" '
          f'stroke-width="1.2"/>\n')
    s += (f'  <text x="{LEFT + pw / 2:.0f}" y="{TOP + ph + 40}" fill="{INK}" font-size="12.5" '
          f'text-anchor="middle">Runtime [s]</text>\n')
    s += (f'  <text transform="translate(20,{TOP + ph / 2:.0f}) rotate(-90)" fill="{INK}" font-size="12.5" '
          f'text-anchor="middle">Allocated memory [MB]</text>\n')

    # thin lines first, the wide ones on top, dice::sparse_map::sparse_map last
    order = sorted(plotted, key=lambda m: (MAPS[m][4], m.startswith("sparse")))
    for m in order:
        _, legend, _, color, width, dash = MAPS[m]
        dash_attr = f' stroke-dasharray="{dash}"' if dash else ""
        s += (f'  <path d="{step_path(plotted[m], sx, sy)}" fill="none" stroke="{color}" stroke-width="{width}" '
              f'stroke-linejoin="round"{dash_attr}><title>{esc(legend)}: peak {peaks[m] / 1e6:.0f} MB</title></path>\n')

    # a label where the destructor starts, right of the line, pushed down while it overlaps another
    placed = []
    for m in sorted(plotted, key=lambda m: -ends[m][1]):
        short = MAPS[m][2]
        x, y = sx(ends[m][0]) + 6, sy(ends[m][1]) - 4
        w = len(short) * 6.4
        while any(abs(y - py) < 13 and x < px + pw_ and px < x + w for px, py, pw_ in placed):
            y += 13
        placed.append((x, y, w))
        s += (f'  <text x="{x:.1f}" y="{y + 4:.1f}" fill="{INK}" font-size="11.5" stroke="{SURFACE}" '
              f'stroke-width="3" paint-order="stroke">{esc(short)}</text>\n')

    # legend below the plot, three columns in the order of MAPS
    entries = [m for m in MAPS if m in plotted]
    col_w = pw / 3
    for k, m in enumerate(entries):
        _, legend, _, color, width, dash = MAPS[m]
        lx = LEFT + (k // 3) * col_w
        y = TOP + ph + 66 + (k % 3) * 18
        dash_attr = f' stroke-dasharray="{dash}"' if dash else ""
        s += (f'  <line x1="{lx:.1f}" y1="{y:.1f}" x2="{lx + 30:.1f}" y2="{y:.1f}" stroke="{color}" '
              f'stroke-width="{width}"{dash_attr}/>\n')
        weight = ' font-weight="600"' if m == "sparse-medium" else ""
        s += f'  <text x="{lx + 38:.1f}" y="{y + 4:.1f}" fill="{INK}" font-size="12"{weight}>{esc(legend)}</text>\n'
    s += "</svg>\n"
    with open(out, "w") as f:
        f.write(s)
    print(out, file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("timeline_dir")
    parser.add_argument("out_dir")
    args = parser.parse_args()
    runs = read_raw(os.path.join(args.timeline_dir, "raw.txt"))
    os.makedirs(args.out_dir, exist_ok=True)

    plotted, peaks, ends, rows = {}, {}, {}, []
    for m, (struct, legend, *_rest) in MAPS.items():
        full = runs.get((m, "full"), [])
        if not full:
            continue
        rnd, fields = median_run(full)
        plotted[m] = read_timeline(os.path.join(args.timeline_dir, f"{struct}-r{rnd}.csv"))
        peaks[m] = fields["peak_bytes"]
        ends[m] = (fields["insert_s"], fields["inserted_bytes"])
        every1000 = runs.get((m, "every1000"), [])
        plain = runs.get((m, "plain"), [])
        t_full = statistics.median(f["total_s"] for _, f in full)
        t_k = statistics.median(f["total_s"] for _, f in every1000) if every1000 else float("nan")
        t_plain = statistics.median(f["total_s"] for _, f in plain) if plain else float("nan")
        events = fields["events"]
        rows.append({
            "map": m, "legend": legend, "round": rnd,
            "peak_mb": fields["peak_bytes"] / 1e6, "inserted_mb": fields["inserted_bytes"] / 1e6,
            "total_s": fields["total_s"], "insert_s": fields["insert_s"], "destroy_s": fields["destroy_s"],
            "full_median_s": t_full, "full_spread": spread([f["total_s"] for _, f in full]),
            "every1000_s": t_k, "plain_s": t_plain,
            "plain_spread": spread([f["total_s"] for _, f in plain]) if plain else float("nan"),
            "events": events, "ns_per_event": (t_full - t_k) / events * 1e9,
            "peak_agree": len({f["peak_bytes"] for _, f in full}) == 1,
        })

    with open(os.path.join(args.out_dir, "bench_readme_timeline.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["map", "round", "seconds", "bytes"])
        for m, points in plotted.items():
            rnd = next(r["round"] for r in rows if r["map"] == m)
            for t, b in points:
                w.writerow([m, rnd, f"{t:.9f}", b])
    draw(os.path.join(args.out_dir, "bench-readme-timeline.svg"), plotted, peaks, ends)

    print("| map | peak MB | MB after the inserts | runtime s (plotted run) | insert s | destroy s | "
          "median s (spread %) | every 1000th s | no counting s (spread %) | events | ns per event | peaks equal |")
    print("|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|")
    for r in rows:
        # with few events the difference of two runtimes is noise, not a cost per event
        cost = f"{r['ns_per_event']:.1f}" if r["events"] >= 100000 else "-"
        print(f"| {r['legend']} | {r['peak_mb']:.1f} | {r['inserted_mb']:.1f} | {r['total_s']:.3f} | "
              f"{r['insert_s']:.3f} | {r['destroy_s']:.3f} | {r['full_median_s']:.3f} ({r['full_spread']:.1f}) | "
              f"{r['every1000_s']:.3f} | {r['plain_s']:.3f} ({r['plain_spread']:.1f}) | {r['events']:,.0f} | "
              f"{cost} | {'yes' if r['peak_agree'] else 'no'} |")
    for key, label in (("full_median_s", "every event"), ("every1000_s", "every 1000th"), ("plain_s", "no counting")):
        ranking = sorted(rows, key=lambda r: r[key])
        print(f"order by runtime, {label}: " + " < ".join(r["map"] for r in ranking))


if __name__ == "__main__":
    main()
