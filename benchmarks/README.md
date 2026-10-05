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
| `DICE_UNORDERED_SPARSE_BENCH_METALL_DIR` | Directory for the metall datastores, default `/tmp/dice-unordered-sparse-bench-metall`. Every metall test case creates fresh datastores in subdirectories (`datastore`, and `curves` and `curves-disk` for `map_curves metall`) and removes them at the end. Nothing else in the directory is touched. Use a local disk. |

## Containers

Every benchmark runs `dice::sparse_map` (or `sparse_set`) with each sparsity level
(`sparsity::high`, `medium`, `low`) and two baselines: `ankerl::unordered_dense::map` (or `set`)
5.2.0 and `std::unordered_map` (or `std::unordered_set`). The sparsity sets by how many slots a group
of 64 buckets grows when it is full (2, 4 or 8), so it trades memory for insert speed.

`sparse_map` and `unordered_dense` both use `ankerl::unordered_dense::hash`, so that a comparison of the
two open addressing tables shows the tables and not the hashes. `std::unordered_map` uses `std::hash`.

The macro `DICE_UNORDERED_SPARSE_BENCH_INLINE_CAPACITY` sets the inline capacity of `sparse_map` and
`sparse_set` in every benchmark (see [small maps](../doc/usage.md#small-maps)), for the elements that can be
inline. Without it the capacity is 0.

Before the timed runs of a container type, `quick_overall`, `find_all`, `set` and `find_random` build,
search and destroy three maps of that type with 1, 2 and 3 elements (`use_small_maps` in
`workloads.hpp`). So a container with its own code path for small maps has that path in use in the
same translation unit as the timed workloads on large maps.

## What each benchmark measures

The workloads check their results. A checksum depends only on the contents of the map, so every correct
map gives the same one, and a wrong result fails the test case. Two results are not checked: the hits
of `find_all`, whose sum changes from run to run, and `hash_strings`, which measures no map.

| test case | file | what it measures |
|---|---|---|
| `quick_overall <container>` | `bench_quick_overall.cpp` | The quick overall score of upstream: the workloads `iterate` (iterate over the whole map after every insert and after every erase), `insert_erase` (random insert and erase, about 10000 entries live), `build` (200000 inserts into an empty map), `churn` (erase one, insert one at a fixed size of 50000, with lookups in between) and `find_50` (lookups with 50% hits in a map that grows to 50000) for the map types `uint64_t -> size_t`, `std::string -> size_t` and `uint64_t -> big_value` (a 64 byte value). Prints the geometric mean of the median times of all 15 runs. Lower is better. |
| `find_all <container>` | `bench_quick_overall.cpp` | A million lookups in a map of 50000 entries that all hit or all miss, for `uint64_t` and `std::string` keys. The two ends of what the hit rate does to the probe. Not part of the score. |
| `hash_strings` | `bench_quick_overall.cpp` | The string hashes alone, `ankerl::unordered_dense::hash` and `std::hash`, over the string keys of the workloads. |
| `quick_overall metall sparse_map <sparsity>` | `bench_metall.cpp` | The quick overall score with metall's allocator: the maps live in a metall datastore. `bench_metall.cpp` specializes `allocator_constructs_in_place` for metall's allocator, so the maps copy and shift trivially copyable elements as bytes, without calls of its `construct` and `destroy`. The same holds for all metall rows. |
| `quick_overall offset_ptr sparse_map <sparsity>` | `bench_metall.cpp` | The quick overall score with `offset_ptr_allocator` of the tests: fancy pointers (`boost::interprocess::offset_ptr`) with memory from the heap. The difference to `quick_overall sparse_map <sparsity>` is the cost of the fancy pointer, the difference to `quick_overall metall sparse_map <sparsity>` is the cost of metall's allocator with this opt-in. |
| `find_all metall sparse_map <sparsity>`, `find_all offset_ptr sparse_map <sparsity>` | `bench_metall.cpp` | `find_all` with metall's allocator and with `offset_ptr_allocator`. |
| `iterate <container>` | `bench_iterate.cpp` | One pass over all elements of a map of 1 million and of 10 million entries, `uint64_t -> uint64_t` and `std::string -> uint64_t`, that sums the mapped values. The rows `iterators` loop over the iterators, the rows `for_each` call the member `for_each` of a map that has one. The rows `iterators with break` and `for_each_while` (the member of a map that has one) compare the mapped value of every element with a value: `no stop` uses a value that is not in the map, `stop in the middle` the value of the element in the middle of the order of the iterators, so that the pass stops after half of the elements. These rows report the time per element that the pass visits. The maps are built with random keys before the timed runs. nanobench reports the time per element. For `sparse_map medium`, `unordered_dense` and `std::unordered_map`. |
| `iterate metall sparse_map medium` | `bench_metall.cpp` | `iterate` with metall's allocator. |
| `throwing_move <container>` | `bench_quick_overall.cpp` | The five workloads of the quick overall score for `uint64_t -> throwing_move_value`, a mapped type with a `size_t` payload whose move constructor can throw. A container copies such a value where it would move another one. For `sparse_map medium`, `unordered_dense` and `std::unordered_map`. Not part of the score. |
| `throwing_move metall sparse_map medium` | `bench_metall.cpp` | `throwing_move` with metall's allocator. |
| `drain <container>` | `bench_drain.cpp` | `map.erase(map.begin())` until the map is empty, the pattern of a worklist, for a map of 100000 `uint64_t -> size_t` entries. Only the drain is timed, with `std::chrono::steady_clock`. Prints the median of 5 rounds in seconds. |
| `erase_iterator <container>` | `bench_erase_iterator.cpp` | Erasure by iterator. Copies a map of 100000 `uint64_t -> size_t` entries, and copies it and erases the entries with an odd key with `erase_if`, which erases with `erase(iterator)`. The time of the erasures is the difference of the two rows. In `sparse_map`, `erase(iterator)` finds the bucket of the element from its position in the values of its group. No scored workload erases by iterator. |
| `emplace <container>` | `bench_emplace.cpp` | `emplace(key, value)` with a `key_type` and a `mapped_type`: a million calls with random keys that are in a map of 50000 entries (nothing is inserted), and 200000 calls with new keys into an empty map, for `uint64_t` and `std::string` keys. A map that constructs the element before it looks up the key copies the key on every call, for a `std::string` key longer than the small string buffer with an allocation. |
| `find_each <container>` | `bench_find_each.cpp` | Many lookups in a row, the pattern of a join: a batch of a million keys, all hits or no hits, looked up with `find` in a loop, and with `find_each` for a map that has it. Maps of 50000 entries and of 4 million entries, `uint64_t -> size_t` and `std::string -> size_t`. In `sparse_map`, the maps of 50000 entries have 0.9 MB of groups and elements with `uint64_t` keys, which fit into the L2 cache, and 2.1 MB with `std::string` keys, plus 2.4 MB that the keys own. The maps of 4 million entries have about 68 MB with `uint64_t` keys and 164 MB with `std::string` keys, more than the 36 MB L3 cache of an i9-14900K. In `sparse_map`, `find_each` is a loop of `find` in the maps of 50000 entries and prefetches in the maps of 4 million entries, with `uint64_t` keys only just (68.2 MB, the threshold of the pipeline is 64 MiB, which is 67.1 MB). For `sparse_map medium`, `unordered_dense` and `std::unordered_map`. |
| `set <container>` | `bench_set.cpp` | `build`, `find_50` and `churn` for a set of `uint64_t`, with the geometric mean. |
| `find_random <container>` | `bench_find_random.cpp` | Random finds with 0%, 25%, 50%, 75% and 100% success in a `size_t -> size_t` map that grows to 200000 entries, 100 million finds per success rate. Timed with `std::chrono::steady_clock`, one run per success rate. |
| `memory` | `bench_memory.cpp` | Memory per element of maps with 1000, 100000 and 1000000 elements, `uint64_t -> uint64_t` and `std::string -> uint64_t`. A tracking allocator counts what the container requests: bytes per element after the build, peak bytes per element during the build, live allocations (`live allocs`), bucket count, load factor and `sizeof` of the map. The heap memory of `std::string` keys and the overhead of malloc are not counted. |
| `memory after erasing half` | `bench_memory.cpp` | Maps with 1000, 100000 and 1000000 elements, `uint64_t -> size_t` and `uint64_t -> throwing_move_value`, for `sparse_map medium`, `unordered_dense` and `std::unordered_map`. Bytes per element and live allocations after the build, and bytes per live element and live allocations after the elements with an even number were erased. Counted by the tracking allocator, as in `memory`. |
| `small_maps` | `bench_small_maps.cpp` | 100000 maps with 1 to 16 `uint64_t -> uint64_t` entries each (fixed seed), the pattern of the edge maps of a hypertrie in tentris. Times the build, the lookups that hit and that miss (maps in a random order), the iteration over all maps (with the iterators, and with the member `for_each` of a map that has one, `-` for a map without it) and the destruction, with the median over 11 rounds. For a map with `for_each`, a round times one of the two iteration passes right after the misses, the iterators in the first round and every second round after it, and `for_each` in the others, and runs the other pass untimed after it. These two columns are then the median over 6 and 5 rounds. The columns `iter break` and `for_each_while` are timed in the same way after these two passes: a loop over the iterators with `break`, and the member `for_each_while` of a map that has one (`-` for a map without it). Both compare the mapped value of every element with a value that is in no map, so they stop nothing and show the time per element. Also the heap bytes and allocations per map from a tracking allocator, and `sizeof` of the map. |
| `map_curves`, `map_curves metall` | `bench_map_curves.cpp` | 100000 maps for each element count 0 to 16, 20, 24, 32, 48 and 64: `uint64_t -> uint64_t` maps, `uint64_t` sets and `std::string -> uint64_t` maps (12 byte keys), as `sparse_map medium`, unordered_dense and `std::unordered_map`, and `sparse_map<uint64_t, uint64_t>` with metall. Times the build by insertions in random, ascending and descending key order, the build from a range (`std::from_range` constructor and `insert_range`, random and ascending order, `insert(first, last)` for containers without them), the finds that hit and that miss, the erasure of all elements one by one, the iteration, the destruction, and churn (64 cycles of one erase and one insertion of a new key in each of the first 10000 maps, so that the deleted buckets reach the clean-up threshold), with the median over 3 rounds. Counts the heap bytes and allocations per map with a tracking allocator, the hash calls of the clean-ups during the churn of `sparse_map` and `sparse_set` with a hash function that counts its calls, and for metall the disk bytes per map of a fresh datastore (map objects included). Prints one line `CSV,...` per container and element count. |

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
| `common.hpp` | quick mode, nanobench settings, geometric mean, the container configurations, `HasForEach`, `HasForEachWhile` |
| `map_benchmarks.hpp` | sizes and checksums of the scored workloads, the quick overall score, `throwing_move`, `find_all` and `iterate` of one configuration |
| `tracking_allocator.hpp` | allocator that counts current and peak bytes and allocations |
