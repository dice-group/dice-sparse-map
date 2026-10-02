# Benchmarks

The long version of the plots in the [README](../README.md). `dice::sparse_map::sparse_map` against
`ankerl::unordered_dense::map`, `std::unordered_map`, `boost::unordered_flat_map`,
`absl::flat_hash_map` and `absl::node_hash_map`, with `uint64_t` and `std::string` keys, once with
`std::allocator` and once with metall's allocator. The panels and the way they are measured follow
the README benchmark of [ankerl::unordered_dense](https://github.com/martinus/unordered_dense).

Everything is relative to `dice::sparse_map::sparse_map<Key, std::size_t>`, which has the default sparsity
medium: 1.00 is level with it, 2.00 is twice the cost. Raw numbers are in
[bench_readme.csv](bench_readme.csv). The programs are in [benchmarks/readme](../benchmarks/readme/README.md).

RESULTS_STD

RESULTS_METALL

RESULTS_SPARSITY

## How the numbers were taken

- Every map is in the configuration you get by typing its type name, its own default hash
  included. For `dice::sparse_map::sparse_map` that is
  `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>`. The other benchmarks in
  [benchmarks/](../benchmarks/README.md) give `sparse_map` the hash of `unordered_dense`, so their
  numbers are not the same as the numbers here.
- With metall, every map gets `metall::manager::allocator_type<T>` for its value type and keeps its
  default hash and key equality. `unordered_dense` gets `metall::container::vector` (a
  `boost::container::vector` with metall's allocator) as its value container, see below.
- The mapped type is `std::size_t`. `uint64_t` keys are random 62 bit numbers. `std::string` keys
  are 8 to 135 bytes long, skewed towards short: about a quarter are 16 bytes or less and fit into
  the `std::string` itself, a sixth are longer than 96 bytes. The keys are the ones of the
  `unordered_dense` benchmark.
- One binary per map, allocator and key type, and one process per panel. A binary with many maps
  has a code layout that moves the numbers by more than the differences between the maps, and a
  map in a process with other maps shares their heap.
- Five sizes that span one octave from 1,000,000 entries (1.00, 1.15, 1.32, 1.52 and 1.74 million),
  geometric mean. The load factor of a table is a sawtooth between two doublings, and two maps do
  not double at the same size, so a ratio at one size compares two random points of two cycles.
- About 10 million operations per size and panel, after one run of the same work that is not
  timed. ROUNDS rounds over all maps, the order of the maps reversed in every second round, and the
  median of the rounds. The noise between the rounds is below.
- Peak memory is `VmHWM` of a forked child that builds the map, reset through
  `/proc/self/clear_refs` before the build, minus the resident set before the build and minus what
  the same child reads without a map. The key pools are built before the fork and are not counted.
  The peak is counted once, in the first round: it repeats to the page.
- Like the harness of `unordered_dense`, the benchmark sets glibc's `M_MMAP_THRESHOLD` and
  `M_TRIM_THRESHOLD` to 64 MiB. A freed block below that stays in glibc's heap and stays resident.
  This is a choice of the harness, and it raises the peak of the maps that double one big array.
  The bytes requested from the heap, which do not depend on it, are in the CSV under `memory`
  (`std::allocator` only, see below).
- The metall datastores are on a local NVMe disk (WD_BLACK SN850X, xfs). Every size of a timed panel
  gets a fresh datastore, and every child of the peak memory panel creates its own datastore before
  the baseline is read. All datastores are removed afterwards.
- MACHINE

```sh
cmake -G Ninja -B build-readme -DCMAKE_BUILD_TYPE=Release -DBUILD_README_BENCHMARKS=ON \
    -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=conan_provider.cmake -DCMAKE_CXX_FLAGS=-march=x86-64-v2
cmake --build build-readme
# one container per round, pinned to CPU 2, metall datastores on a local disk
docker run --rm --cpuset-cpus=2 -v $PWD:/work -v /mnt/nvme/metall:/metall -e DSM_README_METALL_DIR=/metall \
    tentris-dev-env bash /work/benchmarks/readme/run.sh -b /work/build-readme/benchmarks/readme/bin -o /work/raw.txt -r 1
benchmarks/readme/collect.py raw.txt --csv doc/bench_readme.csv
benchmarks/readme/plot.py doc/bench_readme.csv doc --not-relocatable=std
```

## What the numbers do not say

NOT_SAY
