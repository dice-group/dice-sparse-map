## A C++ implementation of a memory efficient hash map and hash set

dice-sparse-map is a C++23 implementation of a memory efficient hash map and hash set, based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). It uses open addressing with sparse quadratic probing. The goal is to use as little memory as possible, also at a low load factor, while keeping good performance. The [article of Stephen Merity](https://smerity.com/articles/2015/google_sparsehash.html) explains the idea behind `google::sparse_hash_map`, which this library follows.

The library provides `dice::sparse_map` and `dice::sparse_set`. Both work with allocators that use fancy pointers, for example the allocators of [metall](https://github.com/LLNL/metall) and `boost::interprocess::offset_ptr`, so they can live in persistent memory.

### Key features

- Header-only library. Add the [include](include/) directory to your include path, or link the CMake target `dice-sparse-map::dice-sparse-map`.
- Memory efficient. The buckets are grouped by 64. A group stores only its occupied buckets, densely, and a 64 bit bitmap says which buckets are occupied. An empty bucket costs about one bit.
- Fancy pointers and persistent memory. The containers are standard layout types if the hash function, the key equality, the allocator and its pointer type are. The elements of a map are stored in a standard layout type, not in `std::pair`.
- Interface of `std::unordered_map` and `std::unordered_set`, including the additions of C++17 to C++29 that fit a sparse table: `try_emplace`, `insert_or_assign`, `merge`, `erase_if`, `insert_range` and the `std::from_range` constructors, deduction guides, allocator-extended copy and move constructors, heterogeneous `find`, `count`, `contains`, `equal_range`, `erase`, `try_emplace`, `insert_or_assign`, `operator[]` and set `insert`, and `lookup` of C++29.
- No sentinel value has to be reserved from the keys.
- A lookup can take a precalculated hash (`precalculated_hash` parameter), so that a key that is looked up in several containers is hashed once.
- The `sparsity` template parameter trades insertion speed for memory. With `sparsity::high` a group grows its storage by 2 elements at a time, with `sparsity::medium` (default) by 4, with `sparsity::low` by 8. High sparsity means less memory and slower insertions. The lookup speed does not depend on it.

### Sparsity

`sparsity::medium` is the default. A higher sparsity needs less memory and builds a map slower. Lookups take the same time at every level.

| sparsity | bytes per element, `uint64_t -> uint64_t` | bytes per element, `std::string -> uint64_t` | build, `uint64_t -> size_t` | build, `std::string -> size_t` |
|---|---:|---:|---:|---:|
| `high` | 17.64 (peak 17.65) | 42.11 (peak 42.12) | 11.8 ms | 40.3 ms |
| `medium` | 18.30 (peak 18.31) | 43.76 (peak 43.77) | 9.5 ms | 33.5 ms |
| `low` | 19.59 (peak 19.60) | 47.05 (peak 47.06) | 8.2 ms | 29.4 ms |

Bytes per element: the memory that a map with 100000 elements requests from its allocator after the build, and the peak during the build (without the heap memory of the strings). A rehash frees the old buckets while it moves the elements, so the peak is close to the memory after the build, see the exception safety below. Build: 200000 insertions into an empty map. Measured with the benchmarks of this repository (see `benchmarks/README.md`) on an i9-14900K, one pinned core, clang 20 Release `-march=x86-64-v2`, median of two runs.

### Requirements

C++23. Tested with GCC 14 and Clang 19 and 20, with libstdc++ 14.

### Differences compared to `std::unordered_map`

- **Iterators return a proxy reference.** `value_type` is `std::pair<Key, T>`. `*it` is `std::pair<Key const &, T &>` (`std::pair<Key const &, T const &>` for a `const_iterator`), and `it->second` is a mutable reference to the mapped value. The key cannot be modified. Bind the elements with `auto &&` or `auto const &`, not with `auto &`:
  ```c++
  dice::sparse_map<int, int> map = {{1, 1}, {2, 1}, {3, 1}};
  for (auto &&[key, value] : map) {
      value = 2;
  }
  map.find(1)->second = 3;
  ```
  The iterators model `std::forward_iterator`. A map iterator meets only the Cpp17InputIterator requirements, because its reference type is not `value_type &`, so its `iterator_category` is `std::input_iterator_tag`. `std::views::keys` and `std::views::values` work.
- **Exception safety.** If an insertion throws, the container holds the same elements as before, except in one case where it is empty. It comes from a rehash, which an insertion, `merge`, `rehash` or `reserve` can do. Calling `reserve` beforehand avoids rehashes. How a rehash transfers the elements depends on their type:
  - Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old buckets is freed while they are moved. If the hash function or the allocator throws while the elements are moved, the container is empty. Any other exception leaves the container unchanged.
  - Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and the new buckets are in memory at the same time. An exception leaves the container unchanged.
- Every operation that modifies the container may invalidate all iterators, references and pointers to elements. `erase` returns an iterator to the next element.
- `emplace` constructs the element first, and inserts it if its key is not in the container yet. `try_emplace` constructs nothing if the key is there.
- There is no bucket interface beyond `bucket_count`, and there are no node handles. `merge` moves the elements, or copies them if their move constructor can throw, instead of transferring nodes.
- Keys and mapped values must be nothrow move constructible or copy constructible.
- Heterogeneous overloads are enabled by `KeyEqual::is_transparent` alone. `Hash` must accept the other key types, too.

Thread safety is the same as for `std::unordered_map`: several readers and no writer.

### Hash functions

The bucket count is 0 or a power of two, and a hash picks its bucket with a mask on its low bits. A hash function is avalanching if every bit of the input changes about half of the bits of the output. Its hash is used as it is. Any other hash function, for example `std::hash<std::uint64_t>`, which is the identity in libstdc++ and libc++, is mixed with one 64 bit multiplication first.

A hash function is marked as avalanching with a member type `is_avalanching`, or with a specialization of `dice::unordered_sparse::hash_is_avalanching`. This is the convention of `ankerl::unordered_dense` and `boost::unordered`:

```c++
struct my_hash {
    using is_avalanching = void;

    std::size_t operator()(std::uint64_t key) const noexcept {
        return some_good_hash(key);
    }
};
```

### Optimization

- Make sure that `Key` and `T` have a `noexcept` move constructor. Without it the containers still work, but insertions copy elements and are much slower.
- The library uses `std::popcount`. Compile with `-mpopcnt` or `-march=native` (x86) so that it becomes one instruction.

### Installation

Add the [include](include/) directory to your include path, or with CMake:

```cmake
find_package(dice-sparse-map REQUIRED)
target_link_libraries(your_target PRIVATE dice-sparse-map::dice-sparse-map)
```

The library is also available as conan package `dice-sparse-map` from the [DICE conan remote](https://conan.dice-research.org/artifactory/api/conan/tentris).

### Tests and benchmarks

The tests use doctest, the benchmarks use nanobench. Their dependencies come from conan through the [cmake-conan](https://github.com/conan-io/cmake-conan) dependency provider:

```bash
cmake -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo -DBUILD_TESTING=ON -DBUILD_BENCHMARKS=ON \
      -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=conan_provider.cmake
cmake --build build
ctest --test-dir build
```

See [benchmarks/README.md](benchmarks/README.md) for the benchmarks.

### Example

```c++
#include <dice/unordered_sparse.hpp>

#include <cstddef>
#include <iostream>
#include <string>

int main() {
    dice::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    map["c"] = 3;
    map["d"] = 4;

    map.insert({"e", 5});
    map.erase("b");

    for (auto &&[key, value] : map) {
        value += 2;
    }

    // {d, 6} {a, 3} {e, 7} {c, 5}, in an unspecified order
    for (auto const &[key, value] : map) {
        std::cout << "{" << key << ", " << value << "}" << std::endl;
    }

    if (map.contains("a")) {
        std::cout << "Found \"a\"." << std::endl;
    }

    // lookup returns a reference to the mapped value, or nothing, and never inserts
    std::cout << map.lookup("z").value_or(0) << std::endl;

    // with the hash of the key, a lookup does not hash the key again
    std::size_t const precalculated_hash = map.hash_function()("a");
    if (map.find("a", precalculated_hash) != map.end()) {
        std::cout << "Found \"a\" with hash " << precalculated_hash << "." << std::endl;
    }

    erase_if(map, [](auto const &element) { return element.second > 6; });

    dice::sparse_set<int> set;
    set.insert({1, 9, 0});
    set.insert({2, -1, 9});

    // {0} {1} {2} {9} {-1}, in an unspecified order
    for (auto const &key : set) {
        std::cout << "{" << key << "}" << std::endl;
    }
}
```

### Heterogeneous lookup

Heterogeneous overloads accept other types than `Key` for lookup, erasure and insertion, as long as these types are hashable and comparable to `Key`. They are enabled if the qualified-id `KeyEqual::is_transparent` is valid, as for [`std::map::find`](https://en.cppreference.com/w/cpp/container/map/find). Use [`std::equal_to<>`](https://en.cppreference.com/w/cpp/utility/functional/equal_to_void) or your own function object. The hash function must accept the other types, too.

```c++
#include <dice/unordered_sparse.hpp>

#include <cstddef>
#include <functional>
#include <iostream>
#include <string>
#include <string_view>

struct string_hash {
    using is_transparent = void;

    std::size_t operator()(std::string_view str) const noexcept {
        return std::hash<std::string_view>{}(str);
    }
};

int main() {
    dice::sparse_map<std::string, int, string_hash, std::equal_to<>> map;

    // the std::string key is constructed only if an element is inserted
    map.try_emplace(std::string_view{"John Doe"}, 2001);
    map[std::string_view{"Jane Doe"}] = 2002;

    // 2001
    std::cout << map.at(std::string_view{"John Doe"}) << std::endl;

    map.erase(std::string_view{"Jane Doe"});
}
```

### Migration from 0.3

- The containers are `dice::sparse_map` and `dice::sparse_set` from `<dice/unordered_sparse.hpp>`, the only header to include. The other names, for example `sparsity` and `hash_is_avalanching`, are in `dice::unordered_sparse`. The namespace `dice::sparse_map::sh` and the headers in `dice/sparse-map/` are gone.
- `iterator::value()` and `iterator::key()` are gone. Use `it->second` and `it->first`.
- `*it` of a map iterator is a `std::pair` of references, not an lvalue. `auto &x = *it` and `for (auto &x : map)` do not compile. Use `auto &&` or `auto const &`.
- The `GrowthPolicy` template parameter, `prime_growth_policy`, `mod_growth_policy`, `power_of_two_growth_policy`, `sparse_pg_map` and `sparse_pg_set` are gone. The bucket count is always a power of two. The template parameters are `sparse_map<Key, T, Hash, KeyEqual, Allocator, sparsity>` and `sparse_set<Key, Hash, KeyEqual, Allocator, sparsity>`.
- A hash function that is not marked as avalanching is mixed. The elements are placed differently, so containers that were persisted with 0.3 cannot be opened (`pobr_version` is 3).
- The `exception_safety` template parameter is gone. See the exception safety above.
- `serialize` and `deserialize` are gone.

### License

The code is licensed under the MIT license, see the LICENSE files ([1](LICENSE-tsl-sparse-map), [2](LICENSE-dice-sparse-map)) for details.
