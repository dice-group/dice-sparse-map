# Benchmarks

One binary, `dice_unordered_sparse_benchmarks`, holds every benchmark. Each `bench_<name>.cpp` holds doctest
test cases, and each test case runs one benchmark with [nanobench](https://github.com/martinus/nanobench)
(or with its own clock, see below). Most workloads are ported from the benchmarks of
[ankerl::unordered_dense](https://github.com/martinus/unordered_dense) (MIT license).
`workloads.hpp` keeps the reasons upstream gives for the shape of each workload.

The plots in the README come from a separate set of programs in [readme/](readme/README.md), with
their own CMake option `BUILD_README_BENCHMARKS`. The results are in
[doc/benchmarks.md](../doc/benchmarks.md).

## Build

The benchmarks are built when the project is the top level project and `BUILD_BENCHMARKS` is on.
The dependencies (nanobench, doctest, metall, the boost headers and unordered_dense) come from conan,
as for the tests. `conan_provider.cmake` is the dependency provider of
[cmake-conan](https://github.com/conan-io/cmake-conan); it is not part of the repository.

```sh
cmake -G Ninja -B build-release -DCMAKE_BUILD_TYPE=Release -DBUILD_BENCHMARKS=ON \
    -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=conan_provider.cmake
cmake --build build-release --target dice_unordered_sparse_benchmarks
```

## Run

```sh
build-release/benchmarks/dice_unordered_sparse_benchmarks                       # everything
build-release/benchmarks/dice_unordered_sparse_benchmarks -ltc                  # list the benchmarks
build-release/benchmarks/dice_unordered_sparse_benchmarks -tc="quick_overall*"  # all quick overall scores
build-release/benchmarks/dice_unordered_sparse_benchmarks -tc="*sparse_map high" -tce="*metall*"
build-release/benchmarks/dice_unordered_sparse_benchmarks -d                    # print the time of each test case
```

`-tc` selects test cases by name, `-tce` excludes them. Both take wildcards and comma separated lists.
nanobench prints a markdown table for every benchmark.

For measurements, use a release build on a quiet machine and pin the process to one core, for example
with `taskset -c 2`. The numbers of different runs are only comparable on the same machine.

### Environment variables

| variable                                 | meaning                                                                                                                                                                                                                                                         |
|------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `DICE_UNORDERED_SPARSE_BENCH_QUICK=1`    | Quick mode for smoke tests: smaller sizes, and every nanobench benchmark runs exactly once. The whole binary takes well under a minute. The checks run with checksums for the small sizes. The timings of the quick mode mean nothing.                          |
| `DICE_UNORDERED_SPARSE_BENCH_METALL_DIR` | Directory for the metall datastores, default `/tmp/dice-unordered-sparse-bench-metall`. Every metall test case creates a fresh datastore in the subdirectory `datastore` and removes it at the end. Nothing else in the directory is touched. Use a local disk. |

## Containers

Every benchmark runs `dice::sparse_map` (or `sparse_set`) with each sparsity level
(`sparsity::high`, `medium`, `low`) and two baselines: `ankerl::unordered_dense::map` (or `set`)
5.2.0 and `std::unordered_map` (or `std::unordered_set`). The sparsity sets by how many slots a group
of 64 buckets grows when it is full (2, 4 or 8), so it trades memory for insert speed.

`sparse_map` and `unordered_dense` both use `ankerl::unordered_dense::hash`, so that a comparison of the
two open addressing tables shows the tables and not the hashes. `std::unordered_map` uses `std::hash`.

## What each benchmark measures

The workloads check their results. A checksum depends only on the contents of the map, so every correct
map gives the same one, and a wrong result fails the test case. Two results are not checked: the hits
of `find_all`, whose sum changes from run to run, and `hash_strings`, which measures no map.

| test case | file | what it measures |
|---|---|---|
| `quick_overall <container>` | `bench_quick_overall.cpp` | The quick overall score of upstream: the workloads `iterate` (iterate over the whole map after every insert and after every erase), `insert_erase` (random insert and erase, about 10000 entries live), `build` (200000 inserts into an empty map), `churn` (erase one, insert one at a fixed size of 50000, with lookups in between) and `find_50` (lookups with 50% hits in a map that grows to 50000) for the map types `uint64_t -> size_t`, `std::string -> size_t` and `uint64_t -> big_value` (a 64 byte value). Prints the geometric mean of the median times of all 15 runs. Lower is better. |
| `find_all <container>` | `bench_quick_overall.cpp` | A million lookups in a map of 50000 entries that all hit or all miss, for `uint64_t` and `std::string` keys. The two ends of what the hit rate does to the probe. Not part of the score. |
| `hash_strings` | `bench_quick_overall.cpp` | The string hashes alone, `ankerl::unordered_dense::hash` and `std::hash`, over the string keys of the workloads. |
| `quick_overall metall sparse_map <sparsity>` | `bench_metall.cpp` | The quick overall score with metall's allocator: the maps live in a metall datastore. |
| `quick_overall offset_ptr sparse_map <sparsity>` | `bench_metall.cpp` | The quick overall score with `offset_ptr_allocator` of the tests: fancy pointers (`boost::interprocess::offset_ptr`) with memory from the heap. The difference to `quick_overall sparse_map <sparsity>` is the cost of the fancy pointer, the difference to `quick_overall metall sparse_map <sparsity>` is the cost of metall's allocator. |
| `find_all metall sparse_map <sparsity>`, `find_all offset_ptr sparse_map <sparsity>` | `bench_metall.cpp` | `find_all` with metall's allocator and with `offset_ptr_allocator`. |
| `throwing_move <container>` | `bench_quick_overall.cpp` | The five workloads of the quick overall score for `uint64_t -> throwing_move_value`, a mapped type with a `size_t` payload whose move constructor can throw. A container copies such a value where it would move another one. For `sparse_map medium`, `unordered_dense` and `std::unordered_map`. Not part of the score. |
| `throwing_move metall sparse_map medium` | `bench_metall.cpp` | `throwing_move` with metall's allocator. |
| `drain <container>` | `bench_drain.cpp` | `map.erase(map.begin())` until the map is empty, the pattern of a worklist, for a map of 100000 `uint64_t -> size_t` entries. Only the drain is timed, with `std::chrono::steady_clock`. Prints the median of 5 rounds in seconds. |
| `set <container>` | `bench_set.cpp` | `build`, `find_50` and `churn` for a set of `uint64_t`, with the geometric mean. |
| `find_random <container>` | `bench_find_random.cpp` | Random finds with 0%, 25%, 50%, 75% and 100% success in a `size_t -> size_t` map that grows to 200000 entries, 100 million finds per success rate. Timed with `std::chrono::steady_clock`, one run per success rate. |
| `memory` | `bench_memory.cpp` | Memory per element of maps with 1000, 100000 and 1000000 elements, `uint64_t -> uint64_t` and `std::string -> uint64_t`. A tracking allocator counts what the container requests: bytes per element after the build, peak bytes per element during the build, live allocations (`live allocs`), bucket count, load factor and `sizeof` of the map. The heap memory of `std::string` keys and the overhead of malloc are not counted. |
| `memory after erasing half` | `bench_memory.cpp` | Maps with 1000, 100000 and 1000000 elements, `uint64_t -> size_t` and `uint64_t -> throwing_move_value`, for `sparse_map medium`, `unordered_dense` and `std::unordered_map`. Bytes per element and live allocations after the build, and bytes per live element and live allocations after the elements with an even number were erased. Counted by the tracking allocator, as in `memory`. |
| `small_maps` | `bench_small_maps.cpp` | 100000 maps with 1 to 16 `uint64_t -> uint64_t` entries each (fixed seed), the pattern of the edge maps of a hypertrie in tentris. Times the build, the lookups that hit and that miss (maps in a random order), the iteration over all maps and the destruction, with the median over 11 rounds. Also the heap bytes and allocations per map from a tracking allocator, and `sizeof` of the map. |

`<container>` is one of `sparse_map high`, `sparse_map medium`, `sparse_map low`, `unordered_dense` and
`std::unordered_map` (for sets: `sparse_set ...`, `unordered_dense` and `std::unordered_set`).

There is no metall baseline with `ankerl::unordered_dense::map`. Version 5.2.0 does not compile with
metall's allocator: its `operator[]` calls `operator->` on the iterator of its value vector, a
`std::vector` with metall's allocator, and with libstdc++ that iterator cannot return an `offset_ptr`
as a raw pointer.

## Files

| file | content |
|---|---|
| `benchmark_main.cpp` | doctest's `main` and the nanobench implementation |
| `workloads.hpp` | the workloads, ported from upstream's `workloads.h`, set versions of three of them, and the drain |
| `common.hpp` | quick mode, nanobench settings, geometric mean, the container configurations |
| `map_benchmarks.hpp` | sizes and checksums of the scored workloads, the quick overall score, `throwing_move` and `find_all` of one configuration |
| `tracking_allocator.hpp` | allocator that counts current and peak bytes and allocations |
