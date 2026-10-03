#!/usr/bin/env python3
"""Draws the README plots from the CSV of collect.py.

    benchmarks/readme/plot.py <csv> <out dir> [--not-relocatable=std,...]

Writes four SVGs into <out dir>: bench-readme-u64.svg, bench-readme-str.svg (std::allocator) and
bench-readme-u64-metall.svg, bench-readme-str-metall.svg (metall's allocator). Each has five panels
side by side, one bar per map, the value next to the bar, and the rows sorted by the geometric mean
of the five panels. The last panel is the peak resident set (`rss`) with std::allocator and the
peak disk usage of the datastore (`diskpeak`) with metall's allocator. Every value is relative to dice::sparse_map::sparse_map with sparsity medium. A panel stops
at a fixed multiple of the reference, and a bar that runs past it is drawn torn off, with its real
value next to it. `--not-relocatable` names the maps (as in the CSV) that run with metall but keep
raw pointers. They get a star and a note in the metall plots.

The SVGs have a fixed white background and fixed colors, so they read the same on GitHub in light
and in dark mode.

The layout follows the README plots of ankerl::unordered_dense (scripts/ab/mapsplot.py, MIT
license). Standard library only.
"""
import argparse
import csv
import math
import os

# family -> fill. Four hues, checked as a categorical set against white with the palette validator
# of the dataviz skill: adjacent CVD separation 8.5 (deuteranopia), 6.5 (tritanopia), every hue at
# 3:1 or more against the background. The row name next to each bar names the map, so the color is
# never the only way to tell two rows apart.
FAMILY = {
    "sparse": "#b45309",
    "dense": "#0e8f60",
    "flat": "#2563c9",
    "node": "#a53393",
}
OF = {
    "sparse-medium": "sparse", "sparse-high": "sparse", "sparse-low": "sparse",
    "udm": "dense",
    "boost": "flat", "absl": "flat",
    "std": "node", "absl-node": "node",
}
PRETTY = {
    "sparse-medium": "sparse_map medium",
    "sparse-high": "sparse_map high",
    "sparse-low": "sparse_map low",
    "udm": "unordered_dense 5.2.0",
    "boost": "boost flat",
    "absl": "absl flat",
    "absl-node": "absl node",
    "std": "std::unordered_map",
}
PRETTY_METALL = {
    "udm": "unordered_dense 5.2.0, boost vector",
}
REFERENCE = "sparse-medium"
TIMED_PANELS = [
    ("buildfree", "build + destroy"),
    ("find", "find, 50% hits"),
    ("churn", "churn"),
    ("iterate", "iterate"),
]
# the last panel, per allocator, and what its values are
MEMORY_PANEL = {
    "std": (("rss", "peak memory"), "peak resident bytes per entry"),
    "metall": (("diskpeak", "peak disk usage"), "peak bytes of the datastore on disk per entry"),
}
# The axis of a panel ends at this multiple of the reference. iterate gets more room, because the
# node maps are far out there and a short axis would tear all of them off at the same place.
CAP = {"buildfree": 4.0, "find": 4.0, "churn": 4.0, "iterate": 10.0, "rss": 4.0, "diskpeak": 4.0}

FONT = ("Roboto Condensed, Noto Sans Condensed, DejaVu Sans Condensed, Liberation Sans Narrow, "
        "Arial Narrow, Avenir Next Condensed, Inter, system-ui, sans-serif")
INK = "#1f2937"
INK_HEAD = "#111827"
INK_MUTED = "#4b5563"
GRID = "#e5e7eb"
BASELINE = "#9ca3af"
SURFACE = "#ffffff"

PANEL_W = 150
GAP = 16
ROWH = 17
TOP = 104


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def bar(x, y, w, h, r=2.5):
    """A bar anchored at x, with rounded corners at its data end."""
    r = min(r, w / 2 if w > 0 else 0, h / 2)
    if w <= 0:
        return ""
    return (f"M{x:.1f},{y:.1f} L{x + w - r:.1f},{y:.1f} Q{x + w:.1f},{y:.1f} {x + w:.1f},{y + r:.1f} "
            f"L{x + w:.1f},{y + h - r:.1f} Q{x + w:.1f},{y + h:.1f} {x + w - r:.1f},{y + h:.1f} "
            f"L{x:.1f},{y + h:.1f} Z")


def torn(x, y, w, h, teeth=3):
    """A bar whose right edge is a zigzag: it ran past the axis and was cut there."""
    step = h / teeth
    points = [f"M{x:.1f},{y:.1f}", f"L{x + w:.1f},{y:.1f}"]
    for i in range(teeth):
        points.append(f"L{x + w - 7:.1f},{y + step * (i + 0.5):.1f}")
        points.append(f"L{x + w:.1f},{y + step * (i + 1):.1f}")
    points.append(f"L{x:.1f},{y + h:.1f} Z")
    return " ".join(points)


def ticks(vmax):
    for step in (0.25, 0.5, 1, 2, 5, 10, 20, 50, 100):
        if vmax / step <= 5:
            return [i * step for i in range(int(vmax / step) + 2)]
    return [0, vmax]


def read(path):
    rows = []
    with open(path) as f:
        for r in csv.DictReader(f):
            rows.append(r)
    return rows


def geomean(xs):
    return math.exp(sum(math.log(x) for x in xs) / len(xs))


def draw(out, title, lines, notes, order, vals, rank, pretty, panels, memory_unit):
    """vals: (map, panel) -> ratio. order: the maps, top to bottom. panels: (key, heading) pairs."""
    n = len(order)
    # about 6.8 px per glyph at 12.5 px in the plain fallback font, so that names fit without the
    # condensed fonts too
    label = max(len(pretty(m)) for m in order) * 6.8 + 56
    width = int(label + len(panels) * (PANEL_W + GAP) + 8)
    top = TOP + 18 * (len(lines) - 2)
    height = top + n * ROWH + 40 + 18 * len(notes)
    s = (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" width="{width}" '
         f'height="{height}" font-family="{FONT}" font-size="12">\n')
    s += f'  <rect fill="{SURFACE}" width="{width}" height="{height}"/>\n'
    s += f'  <text x="8" y="20" fill="{INK_HEAD}" font-size="13.5" font-weight="600">{esc(title)}</text>\n'
    for i, line in enumerate(lines):
        s += f'  <text x="8" y="{38 + 18 * i}" fill="{INK_MUTED}" font-size="12">{esc(line)}</text>\n'
    for ci, (key, heading) in enumerate(panels):
        x0 = label + ci * (PANEL_W + GAP)
        cap = CAP[key]
        vmax = max(vals[(m, key)] for m in order) * 1.02
        capped = vmax > cap
        if capped:
            vmax = cap
        tk = ticks(vmax)
        if capped:
            tk = [t for t in tk if t <= vmax] or [0, vmax]
            if tk[-1] < vmax:
                tk.append(vmax)
        # room for the widest value label, so that no label is drawn inside its bar
        label_w = max(len(f"{vals[(m, key)]:.2f}") for m in order) * 6.4 + 9
        scale = (PANEL_W - label_w) / max(tk[-1], 1e-9)
        s += (f'  <text x="{x0:.0f}" y="{top - 30}" fill="{INK}" font-size="12.5" '
              f'font-weight="600">{esc(heading)}</text>\n')
        for t in tk:
            x = x0 + t * scale
            s += f'  <line x1="{x:.1f}" y1="{top - 12}" x2="{x:.1f}" y2="{top + n * ROWH}" stroke="{GRID}"/>\n'
            s += (f'  <text x="{x:.1f}" y="{top - 14}" fill="{INK_MUTED}" font-size="11.5" '
                  f'text-anchor="middle">{t:g}</text>\n')
        x = x0 + scale
        s += (f'  <line x1="{x:.1f}" y1="{top - 12}" x2="{x:.1f}" y2="{top + n * ROWH}" stroke="{INK_MUTED}" '
              f'stroke-width="1.2" stroke-dasharray="4 3"/>\n')
        s += (f'  <line x1="{x0:.1f}" y1="{top - 12}" x2="{x0:.1f}" y2="{top + n * ROWH}" stroke="{BASELINE}" '
              f'stroke-width="1.2"/>\n')
        for i, m in enumerate(order):
            v = vals[(m, key)]
            y = top + i * ROWH + 2
            over = v > cap
            w = (cap if over else v) * scale
            shape = torn(x0, y, w, ROWH - 5) if over else bar(x0, y, w, ROWH - 5)
            s += (f'  <path d="{shape}" fill="{FAMILY[OF[m]]}" '
                  f'opacity="{0.95 if m == REFERENCE else 0.8}"/>\n')
            s += (f'  <text x="{x0 + w + 6:.1f}" y="{y + ROWH - 9:.0f}" fill="{INK_MUTED}" font-size="11.5">'
                  f'{v:.2f}</text>\n')
    s += (f'  <text x="{label - 8:.0f}" y="{top - 30}" fill="{INK_MUTED}" font-size="11.5" '
          f'text-anchor="end">geomean</text>\n')
    for i, m in enumerate(order):
        y = top + i * ROWH + ROWH - 7
        weight = ' font-weight="600"' if m == REFERENCE else ""
        s += (f'  <text x="{label - 46:.0f}" y="{y}" fill="{INK}" font-size="12.5" text-anchor="end"{weight}>'
              f'{esc(pretty(m))}</text>\n')
        s += (f'  <text x="{label - 8:.0f}" y="{y}" fill="{INK_MUTED}" font-size="11.5" text-anchor="end">'
              f'{rank[m]:.2f}</text>\n')
    ly = top + n * ROWH + 26
    lx = label
    for fam in [f for f in FAMILY if any(OF[m] == f for m in order)]:
        s += f'  <rect x="{lx:.0f}" y="{ly - 9}" width="11" height="11" fill="{FAMILY[fam]}" opacity="0.85"/>\n'
        s += f'  <text x="{lx + 16:.0f}" y="{ly}" fill="{INK_MUTED}" font-size="11.5">{fam}</text>\n'
        lx += 74
    s += (f'  <text x="{lx + 10:.0f}" y="{ly}" fill="{INK_MUTED}" font-size="11.5">time for the first four '
          f'panels, {esc(memory_unit)} for the last</text>\n')
    for i, note in enumerate(notes):
        s += f'  <text x="8" y="{ly + 22 + 18 * i}" fill="{INK_MUTED}" font-size="11.5">{esc(note)}</text>\n'
    s += "</svg>\n"
    with open(out, "w") as f:
        f.write(s)
    print(out)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("csv")
    parser.add_argument("out_dir")
    parser.add_argument("--not-relocatable", default="")
    args = parser.parse_args()
    not_relocatable = {m for m in args.not_relocatable.split(",") if m}

    rows = read(args.csv)
    for variant in ("std", "metall"):
        memory_panel, memory_unit = MEMORY_PANEL[variant]
        panels = TIMED_PANELS + [memory_panel]
        for key in ("u64", "str"):
            cells = [r for r in rows if r["variant"] == variant and r["key"] == key]
            if not cells:
                continue
            base = max(int(r["base"]) for r in cells)
            vals = {}
            for r in cells:
                if int(r["base"]) == base:
                    vals[(r["map"], r["panel"])] = float(r["ratio"])
            maps = sorted({m for (m, _) in vals if m in OF})
            maps = [m for m in maps if all((m, p) in vals for p, _ in panels)]
            rank = {m: geomean([vals[(m, p)] for p, _ in panels]) for m in maps}
            order = sorted(maps, key=lambda m: rank[m])
            key_name = "uint64_t" if key == "u64" else "std::string"
            lines = [f"geometric mean over one octave from {base:,} entries, mapped type size_t. Lower is better, "
                     f"1.00 is level with dice::sparse_map::sparse_map.",
                     "Rows are sorted by the geomean of all five panels. A bar torn off at the right ran past the axis."]
            notes = []
            if variant == "std":
                title = f"relative to dice::sparse_map::sparse_map (sparsity medium), {key_name} keys, every map in its default configuration"

                def pretty(m):
                    return PRETTY.get(m, m)
            else:
                title = (f"relative to dice::sparse_map::sparse_map (sparsity medium), {key_name} keys, every map with metall's "
                         f"allocator (offset_ptr)")
                lines.insert(1, "The maps are in a metall datastore on a local NVMe disk. Hash and key equality are "
                                "the defaults of each map. Disk usage counts the blocks of the files, not the holes.")
                starred = [m for m in order if m in not_relocatable]

                def pretty(m):
                    name = PRETTY_METALL.get(m, PRETTY.get(m, m))
                    return name + " *" if m in starred else name

                if starred:
                    notes.append("* keeps raw pointers into the datastore: it runs, but the datastore cannot be "
                                 "opened again at another address.")
                if key == "str":
                    notes.append("std::string keeps raw pointers and its characters on the heap, so with string keys "
                                 "no map here can be opened again,")
                    notes.append("and the disk usage counts the table, not the characters of the keys.")
            suffix = "" if variant == "std" else "-metall"
            out = os.path.join(args.out_dir, f"bench-readme-{key}{suffix}.svg")
            draw(out, title, lines, notes, order, vals, rank, pretty, panels, memory_unit)


if __name__ == "__main__":
    main()
