# dice-sparse-map: a hash table (almost) at vector size

`dice::sparse_map::sparse_map` and `dice::sparse_map::sparse_set` are hash tables for C++23 that need little more memory than a `std::vector` of the same elements.

- **Memory efficient:** only about 20 % more memory than a `std::vector` of the same elements (15 to 30 % measured for elements of 16 bytes at 1 to 10 million entries, less for larger ones, see the [benchmarks](#benchmarks)). For elements with a `noexcept` move constructor, the peak during a build is at most about 5 % higher than the memory after it.
- **mmap-able to disk:** supports persistent allocators like [metall](https://github.com/LLNL/metall) through fancy pointers. The map, the set and their elements are standard layout if the hash function, the key equality, the allocator, its pointer type, the key and the mapped type are.
- **The interface of `std::unordered_map` and `std::unordered_set` up to C++26** (see the [differences](doc/usage.md#differences-compared-to-stdunordered_map)).

Compromises:

- **Speed traded for memory:** lookups and insertions are slower than in flat and dense maps like `ankerl::unordered_dense::map`, and iteration is slower than in `unordered_dense`, which keeps all values in one array (see the [benchmarks](#benchmarks)).
- **Needs an avalanching hash function:** the bucket comes from the low bits of the hash as it is. A `static_assert` rejects a hash function that is not marked as avalanching. The default hash function `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>` is marked for every key type it hashes, and a `dice_hash_overload` for your own key type must keep the avalanche (see [hash functions](doc/usage.md#hash-functions)).
- **Allocation failure ends the process:** by default a failed allocation calls `std::abort()`. With `dice::sparse_map::sh::allocation_failure::throwing` the exception of the allocator propagates (see [allocation failure](doc/usage.md#allocation-failure)).
- **Exception safety:** if a rehash of elements with a `noexcept` move constructor throws (the hash function, or the allocator with `sh::allocation_failure::throwing`), the exception reaches the caller and the map is empty, so refill it or drop it. Elements whose move can throw are copied and stay unchanged.

## Usage

The library is header-only and needs C++23 (tested with GCC 14 and Clang 19 and 20) and the headers of [dice-hash](https://github.com/dice-group/dice-hash).

```c++
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <iostream>
#include <ranges>
#include <string>

int main() {
    dice::sparse_map::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    map["c"] = 3;
    map.erase("a");

    // C++23: build the set from a range, the element type is deduced
    dice::sparse_map::sparse_set const wanted(std::from_range, std::views::single(2));

    for (auto const &[key, value] : map) {
        if (wanted.contains(value)) {
            std::cout << key << ' ' << value << '\n';  // b 2
        }
    }
}
```

With CMake, for example from the conan package `dice-sparse-map` of the [DICE conan remote](https://conan.dice-research.org/artifactory/api/conan/tentris), which requires dice-hash:

```cmake
find_package(dice-sparse-map REQUIRED)
target_link_libraries(your_target PRIVATE dice-sparse-map::dice-sparse-map)
```

## Benchmarks

Relative to `dice::sparse_map::sparse_map` (1.00), lower is better. The first two plots use `std::allocator`, the last two the metall allocator. [doc/benchmarks.md](doc/benchmarks.md) has the long version.

![benchmark results, uint64_t keys](doc/bench-readme-u64.svg)

![benchmark results, std::string keys](doc/bench-readme-str.svg)

![benchmark results, uint64_t keys, metall allocator](doc/bench-readme-u64-metall.svg)

![benchmark results, std::string keys, metall allocator](doc/bench-readme-str-metall.svg)

With `uint64_t` keys, `dice::sparse_map::sparse_map` needs less than half the peak memory of every other map in the plots. Its lookups are slower than in the flat maps (they need 0.66 to 0.87 of its time), and its inserts and erases take 1.9 to 3.5 times as long as in the flat maps. With `std::string` keys the other maps need 15 % to 41 % more peak memory. With metall's allocator the last panel is the peak disk usage of the datastore, and the gap is smaller: `unordered_dense` needs 12 % and `boost::unordered_flat_map` 37 % more (`uint64_t` keys).

[doc/benchmarks.md](doc/benchmarks.md#allocated-memory-over-time) also plots the allocated memory over time while 10 million `uint64_t` pairs are inserted. There `dice::sparse_map::sparse_map` peaks at 194 MB, the flat maps of the panels above at 403 to 495 MB, and the node maps at 337 MB (`std::unordered_map`) and 403 MB (`absl::node_hash_map`). This plot does not count the 8 byte header that glibc keeps in front of every block. A node map pays this header once per element, 80 MB at 10 million entries.

## Documentation

- [Usage](doc/usage.md): the interface, the differences to `std::unordered_map`, hash functions, sparsity, exception safety, allocators and metall.
- [Design](doc/design.md): how the table works, and its persisted format.
- [Benchmarks](doc/benchmarks.md): what the plots measure, how they were taken, the raw numbers.
- [Upgrading from 0.3](doc/upgrading-from-0.3.md): what changed since 0.3.

## License

MIT ([LICENSE-dice-sparse-map](LICENSE-dice-sparse-map)).

Based on [tsl::sparse_map](https://github.com/Tessil/sparse-map) by Thibaut Goetghebuer-Planchon (MIT, [LICENSE-tsl-sparse-map](LICENSE-tsl-sparse-map)).

Taken from [ankerl::unordered_dense](https://github.com/martinus/unordered_dense) by Martin Leitner-Ankerl (MIT, [LICENSE-unordered-dense](LICENSE-unordered-dense)): the marker `is_avalanching`, the test fixtures and many tests (`tests/fixtures/`, `tests/tests_ud_*.cpp` and the other files that say so), the benchmarks in `benchmarks/`, and the five panels of the plots above.
