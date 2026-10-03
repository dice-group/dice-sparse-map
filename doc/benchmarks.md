# Benchmarks

The long version of the plots in the [README](../README.md). `dice::sparse_map` against
`ankerl::unordered_dense::map`, `std::unordered_map`, `boost::unordered_flat_map`,
`absl::flat_hash_map` and `absl::node_hash_map`, with `uint64_t` and `std::string` keys, once with
`std::allocator` and once with metall's allocator. The panels and the way they are measured follow
the README benchmark of [ankerl::unordered_dense](https://github.com/martinus/unordered_dense).
The [last section](#allocated-memory-over-time) has one more plot, in MB and seconds: the memory
that each map allocates over time while 10 million entries are inserted.

Everything is relative to `dice::sparse_map<Key, std::size_t>` with the default sparsity medium:
1.00 is level with it, 2.00 is twice the cost. Raw numbers are in
[bench_readme.csv](bench_readme.csv). The programs are in
[benchmarks/readme](../benchmarks/readme/README.md).

## With `std::allocator`

![benchmark results, uint64_t keys](bench-readme-u64.svg)

![benchmark results, std::string keys](bench-readme-str.svg)

| panel | one operation is | `sparse_map<uint64_t, size_t>` | `sparse_map<std::string, size_t>` |
|---|---|---|---|
| build + destroy | one insert into an empty map, nothing reserved, plus its share of destroying the map | 63.3 ns | 419 ns |
| find | one lookup, half of them for keys in the map | 38.0 ns | 225 ns |
| churn | one erase plus one insert of a key that is not in the map, at a fixed size | 193 ns | 854 ns |
| iterate | one element of a full pass, summing the mapped value | 1.47 ns | 3.98 ns |
| peak memory | one entry's share of the highest resident set while building, baseline subtracted | 22.2 bytes | 106 bytes |

The `geomean` column is only the sort key. The axis of `iterate` ends at 10 and the other axes at
4. A bar that runs past the axis is drawn torn off, with its real value next to it.

### Memory is what the sparse layout buys, inserts and erases are what it costs

**Peak memory.** With `uint64_t` keys a `sparse_map` entry costs 22.2 bytes of peak resident memory. The flat maps need 2.20x (`unordered_dense`) to 2.64x (`boost`) of that, the node maps 2.06x and 2.17x. Counted as bytes requested from the heap (`memory` in the CSV), it is 19.2 bytes per entry against 40.5 to 48.0, for an entry of 16 bytes. Two things keep it small. A group of 64 buckets stores only its occupied buckets, so the empty buckets cost two bits each. And a rehash moves one old group after the other and frees each old group right after, so the old and the new table are not in memory at the same time. A flat map that doubles holds its old and its new array at the same moment, and with the 64 MiB trim threshold of this harness the old array stays resident after it is freed. With `std::string` keys the keys themselves take about 100 bytes per entry for every map (the `std::string`, the `size_t` and on average 60 bytes of characters on the heap). `sparse_map` needs 106 bytes, the others 1.15x to 1.41x of that.

**Find.** A `uint64_t` lookup takes 38.0 ns, level with `unordered_dense` (1.00). `boost` needs 0.75 of that, `absl` 0.84, `absl node` 1.09 and `std::unordered_map` 1.97. With `std::string` keys `sparse_map` takes 225 ns, and every other map except `std::unordered_map` needs 0.70 to 0.80 of that. Two things are different for strings. A probe of `sparse_map` compares the key of every occupied bucket it visits, while the flat maps first compare a one byte fingerprint, and a string comparison reads the characters on the heap. And every map has its own string hash: `sparse_map` and `std::unordered_map` use `std::hash<std::string>`. The sparsity does not change a lookup.

**Build and churn.** One insert plus its share of the destructor takes 63.3 ns with `uint64_t` keys. The flat maps need 0.34 to 0.39 of that, `absl node` 1.89 and `std::unordered_map` 2.45. With strings it is 419 ns, the flat maps need 0.36 (`unordered_dense`) to 0.70. An insert into a group moves the values behind the new one by one place, and when the values array of the group is full, it allocates a larger one (4 more slots with sparsity medium) and moves all values of the group. A flat map writes one slot. `churn` (one erase plus one insert at a fixed size) costs 193 ns with `uint64_t` keys, and the flat maps need 0.29 to 0.47 of that. An erase moves the values behind the erased one to the front and leaves a deleted bucket. When the elements and the deleted buckets reach 75% of the buckets, the map rehashes to clear the deleted buckets.

**Iteration.** A full pass costs 1.47 ns per element with `uint64_t` keys. `sparse_map` walks the groups and reads the values of each group from one array. `unordered_dense` keeps all values in one array and needs 0.14 of that. `boost` needs 1.25, `absl` 1.76, `absl node` 4.33 and `std::unordered_map` 16.3. With strings the values are 40 bytes instead of 16, and the flat maps need 0.55 and 0.68.

## With metall's allocator

![benchmark results, uint64_t keys, metall allocator](bench-readme-u64-metall.svg)

![benchmark results, std::string keys, metall allocator](bench-readme-str-metall.svg)

metall's allocator has `boost::interprocess::offset_ptr` as its pointer type. A map works with it
only if it allocates through `std::allocator_traits` and stores the pointers it gets. It is
relocatable only if it stores no raw pointer at all, so that the datastore can be opened again at
another address. `check/dsm_readme_api_*` uses the common map interface once with each map, and
`check/dsm_readme_relocate` builds every map with `uint64_t` keys in a datastore, opens the
datastore again in a new process at another address, and checks a filled and an empty map, also
with new inserts.

| map | runs with metall | relocatable | why |
|---|---|---|---|
| `dice::sparse_map`, all three sparsities | yes | yes | stores only the allocator's pointers |
| `ankerl::unordered_dense::map` 5.2.0 with `metall::container::vector` as value container | yes | yes | the fifth template parameter takes a container. The group array is a `std::vector` with the rebound metall allocator, which keeps `offset_ptr` too. |
| `ankerl::unordered_dense::map` 5.2.0 with metall's allocator | partly | not checked | the value container is a `std::vector`. `operator[]` and `operator->` of the iterator do not compile: libstdc++'s vector iterator cannot return an `offset_ptr` as a raw pointer. Not in the plots. |
| `boost::unordered_flat_map` 1.91 | yes | yes | Boost.Unordered supports fancy pointers in its open addressing containers. An empty map is relocatable too. |
| `std::unordered_map` (libstdc++ 14) | yes | no | libstdc++ turns the allocator's pointer into a raw pointer (`std::__to_address`) and keeps raw pointers to its nodes and buckets. It runs in one process. Opened at another address, the check crashes with a segmentation fault. Marked with a star in the plots. |
| `absl::flat_hash_map`, `absl::node_hash_map` 20250814.2 | no | no | `raw_hash_set.h` rejects the allocator: `static_assert` "Allocators with custom pointer types are not supported". Not in the plots. |

With `std::string` keys no map is relocatable. A `std::string` keeps a raw pointer, to its own
buffer or to its characters on the heap. The string plot shows what the maps cost in a datastore,
not a map that can be opened again.

### Results with metall

**Peak disk usage.** With metall the last panel is the disk usage of the datastore: the blocks that its files occupy at the highest point of the build, minus an empty datastore. A hole that metall punched into a file costs nothing. With `uint64_t` keys a `sparse_map` entry takes 31.9 bytes on disk at the peak and 31.8 bytes after the build. `unordered_dense` needs 1.12x of that, `boost` 1.38x and `std::unordered_map` 1.18x. With `uint64_t` keys the peak resident set (`rss` in the CSV) is the same within 0.4 bytes per entry for every map: the resident part of a map in metall is the written pages of the datastore.

So the difference between the maps is much smaller than with `std::allocator` (2.06x to 2.64x), for two reasons. First, the live blocks of `sparse_map` take 18.6 bytes per entry in metall (`metallobjects`), about the 19.2 bytes it requests from glibc, but its datastore holds 31.8. metall serves a small block from a chunk of 2 MiB that holds blocks of one size class only, and it gives a chunk back only when the whole chunk is free. A values array of `sparse_map` grows by 4 slots (64 bytes) at a time, and up to 512 bytes every growth is a new size class. After a rehash the groups have half the elements and start again in the small classes. The chunks of the classes that the groups have left keep their written pages. At 1 million entries metall has 42 MiB of chunks in use for 17.5 MiB of live blocks, and 28.8 MiB of them are on disk. The classes from 192 to 448 bytes alone have 20 MiB of chunks for 4.7 MiB of live arrays. glibc can reuse a freed block for a block of another size, so with `std::allocator` the peak resident set is 22.2 bytes. Second, the arrays of the flat maps are large blocks (more than 1 MiB). metall punches a hole for a freed large block at once, so after a doubling only the new array is on disk, and the peak is the moment of the rehash, when the old and the new array are both there. `boost` peaks at 44.0 bytes per entry, about the 43.8 bytes it requests from glibc, and has 29.4 bytes on disk after the build. With glibc and the 64 MiB thresholds of this harness the old array stays resident, and `boost` has a peak resident set of 58.5 bytes. `std::unordered_map` stores a node in 24 bytes in metall, without the header and the rounding of glibc (32 bytes).

With `std::string` keys the characters of the keys are on the heap, not in the datastore: `std::string` allocates with `std::allocator`. So the disk usage counts the table only. `sparse_map` takes 75.9 bytes per entry at the peak, `unordered_dense` needs 0.97x of that, `std::unordered_map` 0.91x and `boost` 1.39x. The peak resident set counts the characters too, about 60 bytes per key, and is 133 bytes for `sparse_map`. The values of `sparse_map` are 40 bytes, so its arrays grow in steps of 160 bytes, and several of these sizes fall between two size classes of metall: an array of 1120 bytes takes a block of 1280.

**Find.** `sparse_map` costs the same as with `std::allocator`: 38.1 ns with `uint64_t` keys and 225 ns with strings. The `offset_ptr` costs its lookup nothing that shows here. `boost` needs 0.88 (33.4 ns, 1.18x of its time with `std::allocator`). `unordered_dense` with `boost::container::vector` needs 2.20 (83.7 ns, 2.21x of its time with `std::allocator`).

**Build, churn and iterate.** Every insert that allocates pays more with metall. One insert plus its share of the destructor costs `sparse_map` 99.3 ns with `uint64_t` keys (1.57x of `std::allocator`). `boost` needs 0.69 of that, `unordered_dense` 0.82 and `std::unordered_map` 1.37. With `uint64_t` keys `churn` costs 227 ns, `boost` needs 0.37 and `unordered_dense` 0.63. A full pass costs 1.36 ns per element, `unordered_dense` needs 0.31 and `boost` 1.88.

Sorted by the geomean of the five panels, `unordered_dense` with a boost vector and `boost::unordered_flat_map` come before `sparse_map` with metall too, for both key types. Both are relocatable. `sparse_map` keeps the smallest peak disk usage with `uint64_t` keys, and `std::unordered_map` is last by far and cannot be opened again.

## Sparsity

The sparsity changes the memory and the cost of an insertion, not a lookup. Relative to `medium`, with `std::allocator`:

| | `high`, `uint64_t` | `low`, `uint64_t` | `high`, `std::string` | `low`, `std::string` |
|---|---:|---:|---:|---:|
| bytes requested per entry | 0.96 | 1.07 | 0.98 | 1.03 |
| peak memory | 0.95 | 1.09 | 0.99 | 1.04 |
| build + destroy | 1.18 | 0.90 | 1.13 | 0.95 |
| find | 0.99 | 1.00 | 1.00 | 1.00 |
| churn | 0.98 | 1.03 | 1.02 | 1.00 |
| iterate | 0.91 | 1.15 | 1.00 | 1.00 |

With metall the picture is the same, except for the peak memory and the peak disk usage, which are within 3 % for all three.

## How the numbers were taken

- Every map is in the configuration you get by typing its type name, its own default hash
  included. For `dice::sparse_map` that is `std::hash`, which the map mixes itself because
  `std::hash` does not mark itself as avalanching. The other benchmarks in
  [benchmarks/](../benchmarks/README.md) give `sparse_map` the hash of `unordered_dense`, so their
  numbers are not the same as the numbers here.
- With metall, every map gets `metall::manager::allocator_type<T>` for its value type and keeps its
  default hash and key equality. `unordered_dense` gets `metall::container::vector` (a
  `boost::container::vector` with metall's allocator) as its value container. The map object itself
  is on the stack, its memory comes from the datastore.
- The mapped type is `std::size_t`. `uint64_t` keys are random 62 bit numbers. `std::string` keys
  are 8 to 135 bytes long, 50 on average, skewed towards short: a quarter are 15 bytes or less and
  fit into the `std::string` itself, a sixth are longer than 96 bytes. The characters of the other
  keys take 60 bytes per key on the heap on average. The keys are the ones of the `unordered_dense`
  benchmark.
- One binary per map, allocator and key type, and one process per panel. A binary with many maps
  has a code layout that moves the numbers by more than the differences between the maps, and a
  map in a process with other maps shares their heap.
- Five sizes that span one octave from 1,000,000 entries (1.00, 1.15, 1.32, 1.52 and 1.74
  million), geometric mean. The load factor of a table is a sawtooth between two doublings, and two
  maps do not double at the same size, so a ratio at one size compares two random points of two
  cycles.
- About 10 million operations per size and panel, after one run of the same work that is not
  timed. Three rounds, each a new container, with the rounds outermost. The numbers are the medians of the rounds. Between the rounds a cell moved by 0.7 % (median of all timed cells) and by at most 7.2 % (`iterate` of `sparse_map` medium with metall and `uint64_t` keys).
- Peak memory is `VmHWM` of a forked child that builds the map, reset through
  `/proc/self/clear_refs` before the build, minus the resident set before the build and minus what
  the same child reads without a map. The key pools are built before the fork and are not counted.
  The peak is counted in the first round only: it repeats to the page.
- Like the harness of `unordered_dense`, the benchmark sets glibc's `M_MMAP_THRESHOLD` and
  `M_TRIM_THRESHOLD` to 64 MiB. A freed block below that stays in glibc's heap and stays resident.
  This is a choice of the harness, and it raises the peak of the maps that double one big array.
  The bytes requested from the heap do not depend on it. They are in the CSV under `memory`, for
  `std::allocator` only: memory from a metall datastore does not go through `malloc`.
- The metall datastores are on a local NVMe disk (WD_BLACK SN850X, xfs). metall 0.35 creates its
  segment files of 256 MiB with `ftruncate`, maps them shared and allocates in chunks of 2 MiB. A
  chunk holds the blocks of one size class up to 1 MiB, a larger block takes a power of two of
  whole chunks, and its pages that are never written cost no disk space. When a chunk is free,
  metall punches a hole into the file (`MADV_REMOVE`), which frees its blocks on disk. The pages
  of a freed small block stay until its whole chunk is free. Up to 1 MiB of freed small blocks per
  cache stay in metall's object cache (two caches per CPU), and each of them keeps its chunk.
  Every size of a timed panel and of the disk usage gets a fresh datastore, and every child of the
  peak memory panel creates its own datastore before the baseline is read. All datastores are
  removed afterwards.
- Peak disk usage, for metall only, is `st_blocks * 512` summed over the files and directories of
  the datastore, minus the same for the empty datastore (4 KiB, its metadata file). Blocks are only
  freed by a hole punch, so the `_disk` binaries replace `madvise` and take a sample right before
  every `MADV_REMOVE`. The peak is the largest of these samples and the value after the build, so
  it is exact. The binaries also sample every 1/256 of the inserts and after every insert that
  changed `bucket_count()`: 280 to 410 samples per build, 39 to 52 µs each, not timed. None of
  them was above the peak. Without the samples before a hole punch the peak of a flat map would
  miss the old array: 29.4 instead of 44.0 bytes per entry for `boost`. The disk usage ran in three
  rounds, and they agree to the byte.
- Checks of the disk usage, in the CSV: when the map is destroyed and metall's object cache is
  emptied, the disk usage is 0 again for every map and size (`diskempty`). With the map alive and
  the cache emptied, it is never above the chunks that metall has in use (`metallchunks`, from
  `metall::manager::profile`). Before the cache is emptied, a destroyed `sparse_map` or
  `std::unordered_map` keeps most of its disk space (`diskfreed`), because the cached blocks are
  spread over all chunks. On this xfs a write through the mapping allocates the blocks of a
  whole page cache folio, 4 to 64 KiB for one written byte in a probe, so the disk usage can be a
  little larger than the written pages.
- AMD Ryzen Threadripper PRO 7965WX (Zen 4, 24 cores, 32 MiB of L3 for every 6 cores), Ubuntu
  24.04 in a Docker container on Linux 6.8, clang 20.1.8 with libstdc++ 14, `-O3 -DNDEBUG
  -march=x86-64-v2` (CMake `Release`), transparent huge pages on `madvise`, frequency governor
  `ondemand` with boost. Every process pinned to CPU 2 with `docker run --cpuset-cpus=2`, its
  sibling CPU 26 idle, the machine otherwise idle. Run on 2 October 2026. Boost 1.91.0, abseil
  20250814.2, unordered_dense 5.2.0, metall 0.35, all from conan. conan builds abseil's library
  with its default flags, not with `-march=x86-64-v2`.

```sh
cmake -G Ninja -B build-readme -DCMAKE_BUILD_TYPE=Release -DBUILD_README_BENCHMARKS=ON \
    -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=conan_provider.cmake -DCMAKE_CXX_FLAGS=-march=x86-64-v2
cmake --build build-readme
# one container per round, pinned to one core, metall datastores on a local disk
for round in 1 2 3; do
    docker run --rm --cpuset-cpus=2 -v $PWD:/work -v /mnt/nvme/metall:/metall -e DSM_README_METALL_DIR=/metall \
        tentris-dev-env bash /work/benchmarks/readme/run.sh -b /work/build-readme/benchmarks/readme/bin \
        -o /work/raw.txt -r $round
done
benchmarks/readme/collect.py raw.txt --csv doc/bench_readme.csv
benchmarks/readme/plot.py doc/bench_readme.csv doc --not-relocatable=std
```

## What the numbers do not say

- `find` is throughput, not latency. The keys come from a random generator, so several lookups are in flight at the same time. In a chain of lookups where each key depends on the result of the one before, the ranking can be different.
- One million to two million entries. On this CPU six cores share 32 MiB of L3, and a million `uint64_t` entries take 22 MB in `sparse_map` and 49 to 59 MB in the flat maps. At other sizes the ratios can be different, and many small maps are a different workload again (the `small_maps` benchmark in [benchmarks/](../benchmarks/README.md)).
- Peak memory is resident pages, which depends on the allocator and on the thresholds the harness sets for glibc. The bytes requested are in the CSV under `memory`. With metall, the plots show the peak disk usage of the datastore instead. It does not count the heap: metall's own bookkeeping and the characters of the `std::string` keys. The peak resident set, which counts both, is in the CSV under `rss`.
- With metall, a timed panel includes page faults on a file mapping and the work of the kernel to write dirty pages to the disk. That depends on the disk, the file system and the settings of the kernel. Every size starts with an empty datastore.
- With `std::string` keys every map uses another hash. Part of a difference between two maps with string keys is the hash, not the table.
- Every map is in its default configuration. No map got `reserve`, a better hash or a different load factor, which would change the numbers of some maps a lot.
- `iterate` is a full pass over every element, which many programs never do.
- One machine, one compiler, one standard library. A difference of a few percent between two maps is a tie: a cell moves by up to 7 % between rounds.

## Allocated memory over time

![allocated memory over time while 10 million uint64_t pairs are inserted](bench-readme-timeline.svg)

The plot follows the plot "allocated memory" of `ankerl::unordered_dense`. Each map starts empty,
gets 10 million `uint64_t -> uint64_t` pairs inserted one by one (nothing reserved) and is then
destroyed. A line is the sum of the blocks that the map has allocated from the heap, each block
counted at its malloc size, after every allocation and every free. The x axis is the time since
the map was constructed. All maps use `std::allocator`, and the plot has
`ankerl::unordered_dense::segmented_map` in addition to the maps of the panels. The plotted points
are in [bench_readme_timeline.csv](bench_readme_timeline.csv). MB are 10^6 bytes.

| map | peak | after the inserts | runtime of the plotted run | runtime without counting |
|---|---:|---:|---:|---:|
| `dice::sparse_map`, sparsity high | 185.5 MB | 185.5 MB | 2.364 s | 2.026 s |
| `dice::sparse_map`, sparsity medium | 193.7 MB | 193.7 MB | 1.999 s | 1.822 s |
| `dice::sparse_map`, sparsity low | 210.5 MB | 210.5 MB | 1.816 s | 1.730 s |
| `ankerl::unordered_dense::map` | 494.9 MB | 360.7 MB | 0.560 s | 0.559 s |
| `ankerl::unordered_dense::segmented_map` | 253.1 MB | 253.1 MB | 0.465 s | 0.459 s |
| `boost::unordered_flat_map` | 402.7 MB | 268.4 MB | 0.460 s | 0.453 s |
| `absl::flat_hash_map` | 427.8 MB | 285.2 MB | 0.551 s | 0.542 s |
| `std::unordered_map` | 336.9 MB | 336.9 MB | 4.045 s | 3.095 s |
| `absl::node_hash_map` | 402.7 MB | 391.0 MB | 1.815 s | 1.426 s |

The program counts the bytes, and they are the same to the byte in all three rounds. The runtime
of the plotted run includes the cost of the recording. The runtime without counting is the median
of three runs of a binary without the counting.

**`sparse_map` grows in small steps.** The values of a group of 64 buckets are in one array that
holds only the occupied buckets. When the array is full, an insert allocates an array with more
slots (2 more with sparsity high, 4 with medium, 8 with low), moves the values and frees the old
array. So the line climbs in small steps, and the sparsity sets their size: smaller steps cost
time, larger steps cost memory. A rehash first allocates the array of groups of the new table
(16.8 MB at the last rehash). Then it moves one old group after the other and frees the values of
each old group right after. At the end it frees the old array of groups (8.4 MB). So the values are
never in memory twice, and a rehash is a small step in the line. At 10 million entries the peak is
at the end of the inserts: 193.7 MB with sparsity medium, 19.4 bytes per entry for an entry of 16
bytes.

**The flat maps double one array.** `boost::unordered_flat_map` and `absl::flat_hash_map` keep
their elements in one array. When it is full, they allocate an array of twice the size, move the
elements and free the old array. While they move, both arrays are allocated, so the peak is at the
last doubling: 1.50x of what they hold after the inserts. `ankerl::unordered_dense::map` keeps its
values in a `std::vector` and its buckets in a second array, and both double. Its peak is at the
last doubling of the vector, 1.37x of what it holds after the inserts. `segmented_map` keeps its
values in segments that it never moves, so only its bucket array moves to a larger one, and its
peak is at the end of the inserts.

**The node maps allocate one node per element.** `std::unordered_map` and `absl::node_hash_map`
allocate a node for every insert and free every node in the destructor: 20 million allocations and
frees. Their array of buckets doubles too.

**Runtime.** Without counting, `sparse_map` with sparsity medium takes 1.82 s for the 10 million
inserts and the destructor. The flat maps take 0.45 to 0.56 s, `absl::node_hash_map` 1.43 s and
`std::unordered_map` 3.09 s. The x axis of the plot is the runtime of the run that records every
event, and the recording costs time per event. So the lines of `sparse_map` are 5 % (sparsity low)
to 17 % (sparsity high) longer than its runtime without counting, and the lines of the node maps
27 % (`absl::node_hash_map`) and 31 % (`std::unordered_map`). The flat maps make at most 78,198
allocations and frees, and their two runtimes are within 2 %.

How the timeline was taken:

- One process per map and run. The 10 million keys are random 62 bit numbers, made before the map
  is constructed and not counted. The mapped type is `std::size_t`.
- The program replaces `malloc`, `calloc`, `realloc`, `free`, `aligned_alloc`, `posix_memalign`,
  `mmap` and `munmap`. A block counts with `malloc_usable_size`: the size that glibc gives for the
  request, without its chunk header of 8 bytes. The `memory` numbers of the panels count the header
  too. A block that is mapped with `mmap` counts with its length.
- For every allocation and every free, the program records the time stamp counter of the CPU and
  the allocated bytes after the event. The buffer is mapped and faulted in before the start, so the
  recording does not allocate from the heap it counts. The CSV keeps the first, the last, the
  smallest and the largest value of each of 2000 time bins, so the peak in the plot is exact.
- Three rounds, one container per round. Per round and map three processes run: every event
  recorded (the plot), only every 1000th event recorded (the cost of the time stamps and the
  buffer), and a binary without the replaced `malloc` (the runtime without counting). Odd rounds run
  the maps in the order of their names, even rounds in reverse. The plot shows per map the run with
  the median runtime of the three runs that record every event. Between the rounds, the runtime
  without counting moved by 5.7 % for `segmented_map` and by at most 2.1 % for the other maps
  (largest minus smallest, over the median).
- The machine and the build of the panels (see
  [How the numbers were taken](#how-the-numbers-were-taken)): AMD Ryzen Threadripper PRO 7965WX,
  clang 20.1.8 with libstdc++ 14, `-O3 -DNDEBUG -march=x86-64-v2`, `M_MMAP_THRESHOLD` and
  `M_TRIM_THRESHOLD` of glibc at 64 MiB. Every process pinned to CPU 2 with `docker run --cpuset-cpus=2`, load average at most 1.42
  during the runs. Run on 3 October 2026.
- One size. At 10 million entries every map is at another point between two doublings, so the
  ratios change with the size. The panels use five sizes for this reason.
- Bytes allocated, not resident memory. A freed block can stay resident in the heap of glibc, and
  a block that is never written is not resident. The panels show the peak resident set.

```sh
# build-readme as in "How the numbers were taken", then one container per round, pinned to one core
for round in 1 2 3; do
    docker run --rm --cpuset-cpus=2 -v $PWD:/work tentris-dev-env bash /work/benchmarks/readme/timeline.sh \
        -b /work/build-readme/benchmarks/readme/bin/timeline -o /work/results/timeline -r $round
done
benchmarks/readme/plot_timeline.py results/timeline doc
```
