# Usage

[README](../README.md) · **Usage** · [Design](design.md) · [Benchmarks](benchmarks.md) · [Upgrading from 0.3](upgrading-from-0.3.md)

How to use `dice::sparse_map::sparse_map` and `dice::sparse_map::sparse_set`: the interface, where it differs from `std::unordered_map`, the hash functions, the sparsity, allocation failures, the exception safety and the allocators.

## The header and the names

`#include <dice/sparse-map/sparse_map.hpp>` gives `dice::sparse_map::sparse_map`, and `#include <dice/sparse-map/sparse_set.hpp>` gives `dice::sparse_map::sparse_set`. `<dice/sparse-map/version.hpp>` has `dice::sparse_map::pobr_version` and the version of the library. The names that configure the containers are in `dice::sparse_map::sh`: `sh::sparsity`, `sh::allocation_failure`, `sh::hash_is_avalanching`, `sh::power_of_two_growth_policy` and `sh::exception_safety`. The default hash function comes from [dice-hash](https://github.com/dice-group/dice-hash), so the headers need the headers of dice-hash.

The template parameters are:

```c++
sparse_map<Key, T, Hash, KeyEqual, Allocator, GrowthPolicy, ExceptionSafety, Sparsity, AllocationFailure>
sparse_set<Key, Hash, KeyEqual, Allocator, GrowthPolicy, ExceptionSafety, Sparsity, AllocationFailure>
```

The defaults are `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>`, `std::equal_to<Key>`, `std::allocator<std::pair<Key, T>>` (`std::allocator<Key>` for the set), `sh::power_of_two_growth_policy<2>`, `sh::exception_safety::basic`, `sh::sparsity::medium` and `sh::allocation_failure::terminating`.

`GrowthPolicy` and `ExceptionSafety` are placeholders. `GrowthPolicy` must be `sh::power_of_two_growth_policy<2>`: the bucket count is 0 or a power of two and doubles when the table grows. Other growth policies do not compile. Both values of `sh::exception_safety` are accepted and have no effect, the guarantee follows from the type of the elements (see [exception safety](#exception-safety)).

## The interface

The interface is the one of `std::unordered_map` and `std::unordered_set`, including the additions of C++17 to C++26 that fit a sparse table:

- `try_emplace`, `insert_or_assign`, `merge`, `erase_if`, `insert_range` and the `std::from_range` constructors,
- deduction guides,
- allocator-extended copy and move constructors,
- heterogeneous `find`, `count`, `contains`, `equal_range`, `erase`, `try_emplace`, `insert_or_assign`, `operator[]` and set `insert` (see [heterogeneous lookup](#heterogeneous-lookup)).

A lookup can take a precalculated hash (`precalculated_hash` parameter), so that a key that is looked up in several containers is hashed once. The value is `hash_function()(key)`.

No sentinel value has to be reserved from the keys.

Thread safety is the same as for `std::unordered_map`: several readers and no writer.

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

### constexpr

All member functions are `constexpr`. A map or a set works in a constant expression if its hash function, key equality, allocator and elements do, for example with `std::allocator` and a hash function with a `constexpr` call operator that is marked as avalanching. The default hash function `dice::hash::DiceHash` is not `constexpr`, so a map or a set in a constant expression needs another hash function.

## Differences compared to `std::unordered_map`

- **Iterators return a proxy reference.** `value_type` is `std::pair<Key, T>`. `*it` is `std::pair<Key const &, T &>` (`std::pair<Key const &, T const &>` for a `const_iterator`), and `it->second` is a mutable reference to the mapped value. The key cannot be modified. Bind the elements with `auto &&` or `auto const &`, not with `auto &`:
  ```c++
  dice::sparse_map::sparse_map<int, int> map = {{1, 1}, {2, 1}, {3, 1}};
  for (auto &&[key, value] : map) {
      value = 2;
  }
  map.find(1)->second = 3;
  ```
  The iterators model `std::forward_iterator`. A map iterator meets only the Cpp17InputIterator requirements, because its reference type is not `value_type &`, so its `iterator_category` is `std::input_iterator_tag`. `std::views::keys` and `std::views::values` work.
- **Allocation failure.** By default a failed allocation ends the process with `std::abort()`. See [allocation failure](#allocation-failure).
- **Exception safety.** If an insertion throws, the container holds the same elements as before, except in one case where it is empty. See [exception safety](#exception-safety).
- **Iterator invalidation.** Every operation that modifies the container may invalidate all iterators, references and pointers to elements. `erase` returns an iterator to the next element. In detail:
  - `clear`, `operator=`, `reserve`, `rehash`: may invalidate the iterators.
  - `merge`: always invalidates the iterators of both containers.
  - `insert`, `emplace`, `emplace_hint`, `try_emplace`, `insert_or_assign`, `operator[]`: invalidate the iterators if an element is inserted.
  - `erase`: always invalidates the iterators. Use the returned iterator.
- `emplace` constructs the element first, and inserts it if its key is not in the container yet. `try_emplace` constructs nothing if the key is there.
- There is no bucket interface beyond `bucket_count`, and there are no node handles. `merge` moves the elements, or copies them if their move constructor can throw, instead of transferring nodes.
- Keys and mapped values must be nothrow move constructible or copy constructible. The behaviour is undefined if the destructor of a key or a mapped value throws. If they are nothrow move constructible, their move through the allocator (`std::allocator_traits::construct`) must not throw either. With a scoped or a polymorphic allocator, that is the allocator-extended move constructor, which does not throw for equal allocators. `std::vector` makes the same assumption. If they are also nothrow move assignable, an insertion or an erasure moves the elements after it in its group of 64 buckets with their move assignment, as `std::vector::insert` and `std::vector::erase` do.

## Heterogeneous lookup

Heterogeneous overloads accept other types than `Key` for lookup, erasure and insertion, as long as these types are hashable and comparable to `Key`. They are enabled if `Hash::is_transparent` and `KeyEqual::is_transparent` are both valid, as for [`std::unordered_map::find`](https://en.cppreference.com/w/cpp/container/unordered_map/find). Use [`std::equal_to<>`](https://en.cppreference.com/w/cpp/utility/functional/equal_to_void) or your own function object as the key equality, and a hash function that accepts the other types and has `is_transparent`. Without both, the argument is converted to `Key` first: with `int` keys, the default hash function and `std::equal_to<>`, `find(1.5)` finds the element with the key 1.

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

## Hash functions

The bucket count is 0 or a power of two, and a hash picks its bucket with a mask on its low bits, as it is, without mixing. So the hash function must be avalanching: every bit of the input changes about half of the bits of the output. `std::hash` is not avalanching: for integers, libstdc++ and libc++ return the value itself.

A `static_assert` checks `dice::sparse_map::sh::hash_is_avalanching_v<Hash>`. The trait is true if `Hash` declares the member type `is_avalanching`: `using is_avalanching = void;` as in `ankerl::unordered_dense`, or `using is_avalanching = std::true_type;` as in `boost::unordered`. A member type with a `value` that is false, like `std::false_type`, does not count. For a hash function that you cannot change, specialize `dice::sparse_map::sh::hash_is_avalanching`.

```c++
struct my_hash {
    using is_avalanching = void;

    std::size_t operator()(std::uint64_t key) const noexcept {
        return some_good_hash(key);
    }
};
```

The default hash function is `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>` of [dice-hash](https://github.com/dice-group/dice-hash). It declares `is_avalanching` for integers up to 64 bits, floating point numbers, pointers and strings, and for pairs, tuples, optionals, vectors and other ordered containers of such types. It does not declare it for enums, for unordered containers, and for a type with its own `dice::hash::dice_hash_overload`, unless that overload declares `is_avalanching` (see the README of dice-hash). For such a key type, pass another hash function.

## Sparsity

The `Sparsity` template parameter trades insertion speed for memory. With `sh::sparsity::high` a group grows its storage by 2 elements at a time, with `sh::sparsity::medium` (default) by 4, with `sh::sparsity::low` by 8. A higher sparsity needs less memory and builds a map slower. The lookup speed does not depend on it.

| sparsity | bytes per element, `uint64_t -> uint64_t` | bytes per element, `std::string -> uint64_t` | build, `uint64_t -> size_t` | build, `std::string -> size_t` |
|---|---:|---:|---:|---:|
| `high` | 17.64 (peak 17.65) | 42.11 (peak 42.12) | 11.0 ms | 41.2 ms |
| `medium` | 18.30 (peak 18.31) | 43.76 (peak 43.77) | 8.9 ms | 34.1 ms |
| `low` | 19.59 (peak 19.60) | 47.05 (peak 47.06) | 7.6 ms | 29.3 ms |

Bytes per element: the memory that a map with 100000 elements requests from its allocator after the build, and the peak during the build (without the heap memory of the strings). A rehash frees the old buckets while it moves the elements, so the peak is close to the memory after the build, see [exception safety](#exception-safety). Build: 200000 insertions into an empty map. Measured with the benchmarks of this repository (see [benchmarks/README.md](../benchmarks/README.md)) on an i9-14900K, one pinned core, clang 20 Release `-march=x86-64-v2`, median of three runs.

## Allocation failure

The last template parameter `AllocationFailure` says what happens when an allocation of the container fails, that is when the `allocate` of the allocator throws:

- `sh::allocation_failure::terminating` (the default): the process ends with `std::abort()`.
- `sh::allocation_failure::throwing`: the exception of the allocator propagates (`std::bad_alloc` for `std::allocator`), and the guarantees of [exception safety](#exception-safety) hold for it.

In both modes a size limit of the container throws `std::length_error`, and an allocation that an element makes itself (like the copy of a `std::string`) is an exception of the element, not an allocation failure of the container.

## Exception safety

`ExceptionSafety` has no effect, the guarantee follows from the type of the elements. If the insertion of one element throws, the container holds the same elements as before, except in one case where it is empty. It comes from a rehash, which an insertion, `merge`, `rehash` or `reserve` can do. Calling `reserve` beforehand avoids rehashes. How a rehash transfers the elements depends on their type:

- Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old buckets is freed while they are moved. If the hash function throws while the elements are moved, or the allocator with `sh::allocation_failure::throwing`, the container is empty. Any other exception leaves the container unchanged.
- Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and the new buckets are in memory at the same time. An exception leaves the container unchanged.

If `merge` throws, the element that was being merged is still in the source and not in the target. So every element is in exactly one of the two containers.

`erase` does not throw, unless the hash function or the key equality throws. An erased element whose move constructor can throw keeps its memory for a while, see [insertion and erasure in a group](design.md#insertion-and-erasure-in-a-group).

## Allocators and metall

The containers work with allocators that use fancy pointers, for example the allocators of [metall](https://github.com/LLNL/metall) and `boost::interprocess::offset_ptr`, so they can live in persistent memory.

The containers construct, move and destroy their elements through `std::allocator_traits`, so they call `construct` and `destroy` of an allocator that has them. If the elements are trivially copyable and trivially move constructible, and the allocator has no `construct` and no `destroy` of its own, a group copies its elements as bytes, with `std::memcpy`: when its storage grows, when it is copied, and when it is moved into a container with an unequal allocator. The allocator of metall, `metall::stl_allocator`, has its own `construct` and `destroy` up to metall 0.35, so a group in a metall datastore copies its elements one by one. With a metall version whose `stl_allocator` has no `construct` and no `destroy`, a group copies trivially copyable elements as bytes.

The containers are standard layout types if the hash function, the key equality, the allocator and its pointer type are. The elements of a map are stored in a standard layout type, not in `std::pair`. This type supports uses-allocator construction: with a scoped or a polymorphic allocator, the key and the mapped value get the allocator as they would in a `std::pair`.

A map in a metall datastore takes the allocator of the metall manager, as in `tests/tests_metall.cpp`:

```c++
template<typename T>
using metall_allocator = metall::manager::allocator_type<T>;

using persistent_map = dice::sparse_map::sparse_map<std::uint64_t,
                                        std::uint64_t,
                                        dice::hash::DiceHash<std::uint64_t, dice::hash::Policies::wyhash>,
                                        std::equal_to<std::uint64_t>,
                                        metall_allocator<std::pair<std::uint64_t, std::uint64_t>>>;
```

A type that keeps a raw pointer, like `std::string`, cannot live in persistent memory, because the datastore can be mapped at another address when it is opened again.

`dice::sparse_map::pobr_version` is the version of the persisted object binary representation, see [the persisted format](design.md#the-persisted-format).

## Optimization

- Make sure that `Key` and `T` have a `noexcept` move constructor. Without it the containers still work, but insertions copy elements and are much slower.
- Make sure that `Key` and `T` also have a `noexcept` move assignment. Without it, an insertion or an erasure moves the elements of a group by construction and destruction through the allocator, which is slower, most of all with an allocator that has its own `construct` and `destroy`, like the allocator of metall.
- The library uses `std::popcount`. Compile with `-mpopcnt` or `-march=native` (x86) so that it becomes one instruction.
- Define `DICE_SPARSE_MAP_DEBUG` (or `TSL_DEBUG`) to enable internal consistency checks.

## Tests and benchmarks

The tests use doctest, the benchmarks use nanobench. Their dependencies come from conan through the [cmake-conan](https://github.com/conan-io/cmake-conan) dependency provider:

```bash
cmake -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo -DBUILD_TESTING=ON -DBUILD_BENCHMARKS=ON \
      -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=conan_provider.cmake
cmake --build build
ctest --test-dir build
```

See [benchmarks/README.md](../benchmarks/README.md) for the benchmarks.
