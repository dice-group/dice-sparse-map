# README benchmark

The programs behind the plots in the README and in [doc/benchmarks.md](../../doc/benchmarks.md):
`dice::sparse_map` against `ankerl::unordered_dense::map`, `std::unordered_map`,
`boost::unordered_flat_map`, `absl::flat_hash_map` and `absl::node_hash_map`, with `uint64_t` and
`std::string` keys, with `std::allocator` and with metall's allocator. The workloads, the keys and
the sizes follow the README benchmark of
[ankerl::unordered_dense](https://github.com/martinus/unordered_dense) (MIT license).

## Build

The README benchmark has its own CMake option, because it needs abseil and the other benchmarks do
not. The library itself never depends on abseil. `BUILD_README_BENCHMARKS=ON` sets the conan option
`with_readme_benchmark_deps`, which adds `abseil` to the test dependencies.

```sh
cmake -G Ninja -B build-readme -DCMAKE_BUILD_TYPE=Release -DBUILD_README_BENCHMARKS=ON \
    -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=conan_provider.cmake -DCMAKE_CXX_FLAGS=-march=x86-64-v2
cmake --build build-readme
```

The binaries are in `build-readme/benchmarks/readme/bin`:

| binary | what it is |
|---|---|
| `dsm_readme_<allocator>_<key>_<map>` | the timed panels and the peak resident set of one map |
| `dsm_readme_std_<key>_<map>_memory` | the bytes requested from the heap. It replaces `malloc` and its relatives, which costs time, so the timed binaries do not have it. |
| `dsm_readme_metall_<key>_<map>_disk` | the disk usage of the metall datastore: the blocks of its files after the build and the peak during the build, and metall's own numbers. It replaces `madvise` to take a sample right before every hole that metall punches, so the timed binaries do not have it. |
| `check/dsm_readme_api_<allocator>_<key>_<map>` | uses the rest of the map interface once (`operator[]`, `at`, `operator->` of the iterator, copy, move). Shows whether a map compiles with an allocator. |
| `check/dsm_readme_relocate` | builds every map in a metall datastore, then opens the datastore again at another address in a new process and checks the maps |
| `timeline/dsm_readme_timeline_std_u64_<map>` | the allocation timeline: the bytes allocated from the heap after every allocation and every free, see below |
| `timeline/dsm_readme_timeline_std_u64_<map>_plain` | the workload of the allocation timeline without the replaced `malloc`, for its runtime |

`<allocator>` is `std` or `metall`, `<key>` is `u64` or `str`. One map per binary, because a binary
with many maps has a code layout that moves the numbers by more than the differences between the
maps.

Maps that do not compile with metall's allocator have check targets that are not part of `all`, for
example `cmake --build build-readme --target dsm_readme_api_metall_u64_absl_flat` shows the error.

## Run

```sh
benchmarks/readme/run.sh -b build-readme/benchmarks/readme/bin -o results/raw.txt -r "1 2 3"
benchmarks/readme/collect.py results/raw.txt --csv doc/bench_readme.csv
benchmarks/readme/plot.py doc/bench_readme.csv doc --not-relocatable=std
```

`run.sh` runs every binary once per panel and round, with the rounds outermost, and appends one line
per map and panel to the raw file. Set `CORE` to pin every process with `taskset`, and
`DSM_README_METALL_DIR` to the directory for the metall datastores (a local disk, default
`/tmp/dsm-readme-metall`). Every process removes its datastores at the end.

`collect.py` takes the median over the rounds and writes the CSV. It prints the noise between the
rounds per panel. `plot.py` draws the four SVGs from the CSV. Both need only the Python standard
library. The panel `disk` runs the `_disk` binaries and writes nine lines per map: `disk` (after the
build), `diskpeak` (peak during the build), four checks (`diskflushed`, `diskclear`, `diskfreed`,
`diskempty`) and metall's own numbers (`metallsegment`, `metallchunks`, `metallobjects`), see
`bench_readme.cpp`. The metall plots show `diskpeak` in their last panel. `collect.py --merge` adds
the cells of a new raw file to an existing CSV:

```sh
benchmarks/readme/run.sh -b build-readme/benchmarks/readme/bin -o results/disk.txt -w disk -m metall
benchmarks/readme/collect.py results/disk.txt --csv doc/bench_readme.csv --merge
```

A single binary:

```sh
build-readme/benchmarks/readme/bin/dsm_readme_std_u64_sparse_medium find 1000000
# std u64 sparse-medium find 1000000 <five values, ns per lookup> geomean <value>
```

`DSM_README_OPS=200000` makes the timed panels short, for a smoke test. Its timings mean nothing.

## Allocation timeline

The plot `allocated-memory.svg` follows the plot "allocated memory" of ankerl::unordered_dense: one
process per map makes 10 million random 62 bit `uint64_t` keys, then constructs an empty map,
inserts the keys one by one (nothing reserved) with `std::size_t` values and destroys the map. Every
allocation and every free is one event, and the program records the time since the construction
and the bytes allocated after the event. A block counts with its `malloc_usable_size`, without
glibc's chunk header (the `memory` panel counts the header, 8 bytes more per live block). The maps
are the maps of the README with `std::allocator` and `uint64_t` keys, plus
`ankerl::unordered_dense::segmented_map`, which only the timeline has.

The events go to a buffer from `mmap` that is faulted in before the start, with the time stamp
counter of the CPU as the time. After the run, the program writes the timeline as CSV, reduced to
the first, the last, the smallest and the largest value of each of 2000 time bins, so the peak is
exact. See `alloc_timeline.hpp`.

```sh
CORE=2 benchmarks/readme/timeline.sh -b build-readme/benchmarks/readme/bin/timeline -o results/timeline
benchmarks/readme/plot_timeline.py results/timeline doc
```

`timeline.sh` runs three rounds. Per round and map it runs the recording binary three times in
different modes: every event recorded (writes `<map>-r<round>.csv`), only every 1000th event
recorded (`DSM_README_TIMELINE_EVERY=1000`, the cost of the time stamps and the buffer), and the
`_plain` binary without the replaced `malloc` (the runtime without any counting). It appends one
line per process to `raw.txt`. `plot_timeline.py` draws per map the recording with the median total
runtime, writes `allocated-memory.svg` and `allocated-memory.csv` (the plotted points), and prints a
table: peak MB, MB at the end of the inserts, the runtime in the three modes with the spread over
the rounds, the cost per event, and the order of the maps by runtime in each mode. MB are 10^6 bytes.

A single run:

```sh
build-readme/benchmarks/readme/bin/timeline/dsm_readme_timeline_std_u64_sparse_medium timeline 10000000 sparse.csv
# std u64 sparse-medium timeline 10000000 insert_s <s> destroy_s <s> total_s <s> every 1 events <n> recorded <n> peak_bytes <b> inserted_bytes <b>
```

## Files

| file | content |
|---|---|
| `maps.hpp` | the maps, each in the configuration you get by typing its type name, and the same maps with metall's allocator |
| `bench_readme.cpp` | the panels, the keys and the sizes |
| `max_rss.hpp` | peak resident set of a piece of work, in a forked child |
| `count_alloc.hpp` | bytes requested from the heap, by replacing `malloc` and its relatives |
| `alloc_timeline.hpp` | the allocated bytes after every allocation and every free, for the allocation timeline |
| `disk_usage.hpp` | blocks of the files of a metall datastore, sampled right before every hole punch by replacing `madvise` |
| `api_check.cpp` | the rest of the map interface, to see what compiles with an allocator |
| `relocate.cpp` | opens a metall datastore at another address and checks the maps in it |
| `run.sh`, `collect.py`, `plot.py` | run the binaries, make the CSV, draw the plots |
| `timeline.sh`, `plot_timeline.py` | run the allocation timeline, draw its plot |
