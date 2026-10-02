## A C++ implementation of a memory efficient hash map and hash set

dice-sparse-map is a C++23 implementation of a memory efficient hash map and hash set, based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). It uses open addressing with sparse quadratic probing. The goal is to use as little memory as possible, also at a low load factor, while keeping good performance. The [article of Stephen Merity](https://smerity.com/articles/2015/google_sparsehash.html) explains the idea behind `google::sparse_hash_map`, which this library follows.

The library provides `dice::sparse_map::sparse_map` and `dice::sparse_map::sparse_set`. Both work with allocators that use fancy pointers, for example the allocators of [metall](https://github.com/LLNL/metall) and `boost::interprocess::offset_ptr`, so they can live in persistent memory.

### Key features

- Header-only library. Add the [include](include/) directory and the headers of [dice-hash](https://github.com/dice-group/dice-hash) to your include path, or link the CMake target `dice-sparse-map::dice-sparse-map`.
- Memory efficient. The buckets are grouped by 64. A group stores only its occupied buckets, densely, and a 64 bit bitmap says which buckets are occupied. An empty bucket costs about one bit.
- Fancy pointers and persistent memory. The containers are standard layout types if the hash function, the key equality, the allocator and its pointer type are. The elements of a map are stored in a standard layout type, not in `std::pair`.
- Interface of `std::unordered_map` and `std::unordered_set`, including the additions of C++17 to C++26 that fit a sparse table: `try_emplace`, `insert_or_assign`, `merge`, `erase_if`, `insert_range` and the `std::from_range` constructors, deduction guides, allocator-extended copy and move constructors, heterogeneous `find`, `count`, `contains`, `equal_range`, `erase`, `try_emplace`, `insert_or_assign`, `operator[]` and set `insert`.
- All member functions are `constexpr`. A map or a set works in a constant expression if its hash function, key equality, allocator and elements do, for example with `std::allocator` and a hash function with a `constexpr` call operator. The default hash function `dice::hash::DiceHash` is not `constexpr`, so a map or a set in a constant expression needs another hash function.
- No sentinel value has to be reserved from the keys.
- A lookup can take a precalculated hash (`precalculated_hash` parameter), so that a key that is looked up in several containers is hashed once.
- The `Sparsity` template parameter trades insertion speed for memory. With `sh::sparsity::high` a group grows its storage by 2 elements at a time, with `sh::sparsity::medium` (default) by 4, with `sh::sparsity::low` by 8. High sparsity means less memory and slower insertions. The lookup speed does not depend on it.

### Template parameters

```c++
sparse_map<Key, T, Hash, KeyEqual, Allocator, GrowthPolicy, ExceptionSafety, Sparsity, AllocationFailure>
sparse_set<Key, Hash, KeyEqual, Allocator, GrowthPolicy, ExceptionSafety, Sparsity, AllocationFailure>
```

The defaults are `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>`, `std::equal_to<Key>`, `std::allocator<std::pair<Key, T>>` (`std::allocator<Key>` for the set), `sh::power_of_two_growth_policy<2>`, `sh::exception_safety::basic`, `sh::sparsity::medium` and `sh::allocation_failure::terminating`. The names with `sh::` are in the namespace `dice::sparse_map::sh`. `GrowthPolicy` and `ExceptionSafety` are placeholders, see [Growth policy](#growth-policy) and the exception safety below.

### Sparsity

`sh::sparsity::medium` is the default. A higher sparsity needs less memory and builds a map slower. Lookups take the same time at every level.

| sparsity | bytes per element, `uint64_t -> uint64_t` | bytes per element, `std::string -> uint64_t` | build, `uint64_t -> size_t` | build, `std::string -> size_t` |
|---|---:|---:|---:|---:|
| `high` | 17.64 (peak 17.65) | 42.11 (peak 42.12) | 11.0 ms | 41.2 ms |
| `medium` | 18.30 (peak 18.31) | 43.76 (peak 43.77) | 8.9 ms | 34.1 ms |
| `low` | 19.59 (peak 19.60) | 47.05 (peak 47.06) | 7.6 ms | 29.3 ms |

Bytes per element: the memory that a map with 100000 elements requests from its allocator after the build, and the peak during the build (without the heap memory of the strings). A rehash frees the old buckets while it moves the elements, so the peak is close to the memory after the build, see the exception safety below. Build: 200000 insertions into an empty map. Measured with the benchmarks of this repository (see `benchmarks/README.md`) on an i9-14900K, one pinned core, clang 20 Release `-march=x86-64-v2`, median of three runs.

### Requirements

C++23. Tested with GCC 14 and Clang 19 and 20, with libstdc++ 14. The default hash function needs the headers of [dice-hash](https://github.com/dice-group/dice-hash).

### Differences compared to `std::unordered_map`

- **Iterators return a proxy reference.** `value_type` is `std::pair<Key, T>`. `*it` is `std::pair<Key const &, T &>` (`std::pair<Key const &, T const &>` for a `const_iterator`), and `it->second` is a mutable reference to the mapped value. The key cannot be modified. Bind the elements with `auto &&` or `auto const &`, not with `auto &`:
  ```c++
  dice::sparse_map::sparse_map<int, int> map = {{1, 1}, {2, 1}, {3, 1}};
  for (auto &&[key, value] : map) {
      value = 2;
  }
  map.find(1)->second = 3;
  ```
  The iterators model `std::forward_iterator`. A map iterator meets only the Cpp17InputIterator requirements, because its reference type is not `value_type &`, so its `iterator_category` is `std::input_iterator_tag`. `std::views::keys` and `std::views::values` work.
- **Allocation failure.** By default the process ends with `std::abort()` when an allocation of the container fails, that is when the `allocate` of the allocator throws. The last template parameter `AllocationFailure` chooses this: `sh::allocation_failure::terminating` (the default) aborts, `sh::allocation_failure::throwing` lets the exception of the allocator propagate (`std::bad_alloc` for `std::allocator`). In both modes a size limit of the container throws `std::length_error`, and an allocation that an element makes itself (like the copy of a `std::string`) is an exception of the element.
- **Exception safety.** The `ExceptionSafety` template parameter is a placeholder: both values of `sh::exception_safety` are accepted and have no effect. The guarantee follows from the type of the elements. If the insertion of one element throws, the container holds the same elements as before, except in one case where it is empty. It comes from a rehash, which an insertion, `merge`, `rehash` or `reserve` can do. Calling `reserve` beforehand avoids rehashes. How a rehash transfers the elements depends on their type:
  - Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old buckets is freed while they are moved. If the hash function throws while the elements are moved, or the allocator with `sh::allocation_failure::throwing`, the container is empty. Any other exception leaves the container unchanged.
  - Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and the new buckets are in memory at the same time. An exception leaves the container unchanged.
- Every operation that modifies the container may invalidate all iterators, references and pointers to elements. `erase` returns an iterator to the next element.
- `emplace` constructs the element first, and inserts it if its key is not in the container yet. `try_emplace` constructs nothing if the key is there.
- There is no bucket interface beyond `bucket_count`, and there are no node handles. `merge` moves the elements, or copies them if their move constructor can throw, instead of transferring nodes.
- Keys and mapped values must be nothrow move constructible or copy constructible.
- Heterogeneous overloads are enabled if `Hash::is_transparent` and `KeyEqual::is_transparent` both exist, as for `std::unordered_map`. `Hash` and `KeyEqual` must accept the other key types.

Thread safety is the same as for `std::unordered_map`: several readers and no writer.

### Growth policy

The number of buckets is 0 or a power of two and doubles when the table grows. A hash picks its bucket with a mask, <code>hash & (2<sup>n</sup> - 1)</code>, not with a modulo. The template parameter `GrowthPolicy` must be `sh::power_of_two_growth_policy<2>`, the default. Other growth policies do not compile.

### Hash functions

A hash picks its bucket with its low bits as it is, without mixing. So the hash function must be avalanching: each bit of the key changes each bit of the hash with a probability of about one half. A `static_assert` checks `dice::sparse_map::sh::hash_is_avalanching<Hash>`. The trait is true if `Hash` declares the member type `is_avalanching`: `using is_avalanching = void;` as in `ankerl::unordered_dense`, or `using is_avalanching = std::true_type;` as in `boost::unordered`. A member type with a `value` that is false, like `std::false_type`, does not count. For a hash function that you cannot change, specialize `dice::sparse_map::sh::hash_is_avalanching`. `std::hash` is not avalanching: for integers, libstdc++ and libc++ return the value itself.

```c++
struct my_hash {
    using is_avalanching = void;

    std::size_t operator()(std::uint64_t key) const noexcept {
        return some_good_hash(key);
    }
};
```

The default hash function is `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>` of [dice-hash](https://github.com/dice-group/dice-hash). It declares `is_avalanching` for integers up to 64 bits, floating point numbers, pointers and strings, and for pairs, tuples, optionals, vectors and other ordered containers of such types. It does not declare it for enums, for unordered containers, and for a type with its own `dice::hash::dice_hash_overload`, unless that overload declares `is_avalanching` (see the README of dice-hash). For such a key type, pass another hash function.

### Optimization

- Make sure that `Key` and `T` have a `noexcept` move constructor. Without it the containers still work, but insertions copy elements and are much slower.
- The library uses `std::popcount`. Compile with `-mpopcnt` or `-march=native` (x86) so that it becomes one instruction.

### Installation

Add the [include](include/) directory and the headers of [dice-hash](https://github.com/dice-group/dice-hash) to your include path, or with CMake:

```cmake
find_package(dice-sparse-map REQUIRED)
target_link_libraries(your_target PRIVATE dice-sparse-map::dice-sparse-map)
```

The library is also available as conan package `dice-sparse-map` from the [DICE conan remote](https://conan.dice-research.org/artifactory/api/conan/tentris). The conan package requires dice-hash.

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
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <cstddef>
#include <iostream>
#include <string>

int main() {
    dice::sparse_map::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
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

    // with the hash of the key, a lookup does not hash the key again
    std::size_t const precalculated_hash = map.hash_function()("a");
    if (map.find("a", precalculated_hash) != map.end()) {
        std::cout << "Found \"a\" with hash " << precalculated_hash << "." << std::endl;
    }

    erase_if(map, [](auto const &element) { return element.second > 6; });

    dice::sparse_map::sparse_set<int> set;
    set.insert({1, 9, 0});
    set.insert({2, -1, 9});

    // {0} {1} {2} {9} {-1}, in an unspecified order
    for (auto const &key : set) {
        std::cout << "{" << key << "}" << std::endl;
    }
}
```

### Heterogeneous lookup

Heterogeneous overloads accept other types than `Key` for lookup, erasure and insertion, as long as these types are hashable and comparable to `Key`. They are enabled if `Hash::is_transparent` and `KeyEqual::is_transparent` are both valid, as for [`std::unordered_map::find`](https://en.cppreference.com/w/cpp/container/unordered_map/find). Use [`std::equal_to<>`](https://en.cppreference.com/w/cpp/utility/functional/equal_to_void) or your own function object as the key equality, and a hash function that accepts the other types and has `is_transparent`. Without both, the argument is converted to `Key` first.

```c++
#include <dice/hash/DiceHash.hpp>
#include <dice/sparse-map/sparse_map.hpp>

#include <cstddef>
#include <functional>
#include <iostream>
#include <string>
#include <string_view>

struct string_hash {
    using is_transparent = void;
    using is_avalanching = void;

    std::size_t operator()(std::string_view str) const noexcept {
        return dice::hash::DiceHash<std::string_view, dice::hash::Policies::wyhash>{}(str);
    }
};

int main() {
    dice::sparse_map::sparse_map<std::string, int, string_hash, std::equal_to<>> map;

    // the std::string key is constructed only if an element is inserted
    map.try_emplace(std::string_view{"John Doe"}, 2001);
    map[std::string_view{"Jane Doe"}] = 2002;

    // 2001
    std::cout << map.at(std::string_view{"John Doe"}) << std::endl;

    map.erase(std::string_view{"Jane Doe"});
}
```

### Migration from 0.3

- The names and the headers are the same: `dice::sparse_map::sparse_map` and `dice::sparse_map::sparse_set` from `<dice/sparse-map/sparse_map.hpp>` and `<dice/sparse-map/sparse_set.hpp>`, the other names in `dice::sparse_map::sh`, and `dice::sparse_map::pobr_version` from `<dice/sparse-map/version.hpp>`.
- The hash function must be avalanching, a `static_assert` checks it, see [Hash functions](#hash-functions). `std::hash` is not. The default hash function is `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>`, so the library needs dice-hash. Key types that the default hash function does not mark as avalanching need another hash function: enums, types with only a `std::hash` specialization, and types with their own `dice::hash::dice_hash_overload` that does not declare `is_avalanching`. Mark your own avalanching hash function with `using is_avalanching = void;`, or specialize `dice::sparse_map::sh::hash_is_avalanching` for it.
- `GrowthPolicy` must be `sh::power_of_two_growth_policy<2>`, the default. `sh::prime_growth_policy`, `sh::mod_growth_policy`, `sparse_pg_map`, `sparse_pg_set` and `sh::probing` are gone.
- `ExceptionSafety` is accepted and has no effect. The guarantee follows from the type of the elements, see the exception safety above.
- The new last template parameter `AllocationFailure` is `sh::allocation_failure::terminating` by default: a failed allocation ends the process with `std::abort()` instead of throwing `std::bad_alloc`. Pass `sh::allocation_failure::throwing` to keep the exception.
- `iterator::value()` and `iterator::key()` are gone. Use `it->second` and `it->first`.
- `*it` of a map iterator is a `std::pair` of references, not an lvalue. `auto &x = *it` and `for (auto &x : map)` do not compile. Use `auto &&` or `auto const &`.
- Heterogeneous overloads need `is_transparent` in the hash function as well as in the key equality.
- The persisted format is different (`pobr_version` is 3): the elements are placed differently and the groups and the table store more. Containers that were persisted with 0.3 cannot be opened.
- `serialize` and `deserialize` are gone.

### License

The code is licensed under the MIT license, see the LICENSE files ([1](LICENSE-tsl-sparse-map), [2](LICENSE-dice-sparse-map)) for details.
