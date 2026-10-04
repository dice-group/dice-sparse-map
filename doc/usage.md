# Usage

[README](../README.md) · **Usage** · [Design](design.md) · [Benchmarks](benchmarks.md) · [Upgrading from 0.3](upgrading-from-0.3.md)

How to use `dice::sparse_map` and `dice::sparse_set`: the interface, where it differs from `std::unordered_map`, the hash functions, the sparsity, the exception safety and the allocators.

## The header and the names

`#include <dice/unordered_sparse.hpp>` is the only header to include. It gives `dice::sparse_map` and `dice::sparse_set`. All other names of the library are in `dice::unordered_sparse`, for example `sparsity`, `hash_is_avalanching`, `optional_ref` and `pobr_version`. The headers under `dice/unordered_sparse/detail` are not meant to be included directly.

The template parameters are:

```c++
sparse_map<Key, T, Hash, KeyEqual, Allocator, sparsity, inline_capacity>
sparse_set<Key, Hash, KeyEqual, Allocator, sparsity, inline_capacity>
```

The defaults are `std::hash<Key>`, `std::equal_to<Key>`, `std::allocator<std::pair<Key, T>>` (`std::allocator<Key>` for the set), `sparsity::medium` and the inline capacity 0 (see [small maps](#small-maps)).

## The interface

The interface is the one of `std::unordered_map` and `std::unordered_set`, including the additions of C++17 to C++29 that fit a sparse table:

- `try_emplace`, `insert_or_assign`, `merge`, `erase_if`, `insert_range` and the `std::from_range` constructors,
- deduction guides,
- allocator-extended copy and move constructors,
- heterogeneous `find`, `count`, `contains`, `equal_range`, `erase`, `try_emplace`, `insert_or_assign`, `operator[]` and set `insert` (see [heterogeneous lookup](#heterogeneous-lookup)),
- `lookup` of C++29.

`lookup` returns a reference to the mapped value, or nothing, and never inserts. Its result type is `dice::unordered_sparse::optional_ref<T>`. With a standard library that provides `std::optional<T &>`, this is `std::optional<T &>`. Otherwise it is a type of the library with the same interface.

A lookup can take a precalculated hash (`precalculated_hash` parameter), so that a key that is looked up in several containers is hashed once. The value is `hash_function()(key)`.

No sentinel value has to be reserved from the keys.

Thread safety is the same as for `std::unordered_map`: several readers and no writer.

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

### constexpr

All members are `constexpr`, so a map and a set can be used in a constant expression. `std::hash` is not `constexpr`, so a constant expression needs another hash function.

## Differences compared to `std::unordered_map`

- **Iterators return a proxy reference.** `value_type` is `std::pair<Key, T>`. `*it` is `std::pair<Key const &, T &>` (`std::pair<Key const &, T const &>` for a `const_iterator`), and `it->second` is a mutable reference to the mapped value. The key cannot be modified. Bind the elements with `auto &&` or `auto const &`, not with `auto &`:
  ```c++
  dice::sparse_map<int, int> map = {{1, 1}, {2, 1}, {3, 1}};
  for (auto &&[key, value] : map) {
      value = 2;
  }
  map.find(1)->second = 3;
  ```
  The iterators model `std::forward_iterator`. A map iterator meets only the Cpp17InputIterator requirements, because its reference type is not `value_type &`, so its `iterator_category` is `std::input_iterator_tag`. `std::views::keys` and `std::views::values` work.
- **Exception safety.** If an insertion throws, the container holds the same elements as before, except in one case where it is empty. See [exception safety](#exception-safety).
- **Iterator invalidation.** Every operation that modifies the container may invalidate all iterators, references and pointers to elements. `erase` returns an iterator to the next element. In detail:
  - `clear`, `operator=`, `reserve`, `rehash`: may invalidate the iterators.
  - `merge`: always invalidates the iterators of both containers.
  - `insert`, `emplace`, `emplace_hint`, `try_emplace`, `insert_or_assign`, `operator[]`: invalidate the iterators if an element is inserted.
  - `erase`: always invalidates the iterators. Use the returned iterator.
  - With an [inline capacity](#small-maps): the move constructor, the move assignment and `swap` invalidate the iterators, references and pointers to inline elements.
- `emplace` constructs the element first, and inserts it if its key is not in the container yet. `try_emplace` constructs nothing if the key is there.
- There is no bucket interface beyond `bucket_count`, and there are no node handles. `merge` moves the elements, or copies them if their move constructor can throw, instead of transferring nodes.
- Keys and mapped values must be nothrow move constructible or copy constructible. The behaviour is undefined if the destructor of a key or a mapped value throws. If they are nothrow move constructible, their move through the allocator (`std::allocator_traits::construct`) must not throw either. With a scoped or a polymorphic allocator, that is the allocator-extended move constructor, which does not throw for equal allocators. `std::vector` makes the same assumption. If they are also nothrow move assignable, an insertion or an erasure moves the elements after it in its group of 64 buckets with their move assignment, as `std::vector::insert` and `std::vector::erase` do.

## Heterogeneous lookup

Heterogeneous overloads accept other types than `Key` for lookup, erasure and insertion, as long as these types are hashable and comparable to `Key`. They are enabled if `Hash::is_transparent` and `KeyEqual::is_transparent` are both valid, as for [`std::unordered_map::find`](https://en.cppreference.com/w/cpp/container/unordered_map/find). Use [`std::equal_to<>`](https://en.cppreference.com/w/cpp/utility/functional/equal_to_void) or your own function object as the key equality, and a hash function that accepts the other types and has `is_transparent`. Without both, the argument is converted to `Key` first: with `int` keys, `std::hash<int>` and `std::equal_to<>`, `find(1.5)` finds the element with the key 1.

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

## Small maps

The template parameter `inline_capacity` (default 0) is the number of elements that a container keeps in the container object itself, without an allocation. Many small maps, for example the maps of the nodes of a tree, then need no heap memory and no pointer chase:

```c++
// up to 4 elements in the map object, sizeof is 96 with std::allocator on 64 bit targets
using small_map = dice::sparse_map<std::uint64_t, std::uint64_t, std::hash<std::uint64_t>, std::equal_to<std::uint64_t>,
                                   std::allocator<std::pair<std::uint64_t, std::uint64_t>>,
                                   dice::unordered_sparse::sparsity::medium, 4>;
```

The container object then holds group 0 of a table of 64 buckets: a bitmap of the buckets that hold an element, a bitmap of the deleted buckets, and room for `inline_capacity` elements, densely in bucket order. A lookup is the one of a table of 64 buckets. When an insertion finds the inline group full, the elements move into an allocated group of 64 buckets, in the same buckets and in the same order, without a rehash. A lower maximum load factor lowers the limit to the load threshold of 64 buckets. An erase of an inline element moves the elements after it, in the container object, and marks its bucket as deleted only while an element can be outside the bucket of its hash. So it allocates nothing, does not throw unless the hash function or the key equality throws, and keeps the order of the other elements. When the elements and the deleted buckets reach twice the inline capacity (at most the clean-up threshold of 64 buckets), an insertion rebuilds the inline group, as a table rebuilds its buckets.

The inline capacity is at most 32. Inline storage makes the container object larger, also for a container that holds more elements than fit inline. With `std::allocator` on 64 bit targets:

| type | inline capacity 0 | inline capacity 4 |
|---|---:|---:|
| `sparse_map<std::uint64_t, std::uint64_t>` | 72 | 96 |
| `sparse_set<std::uint64_t>` | 72 | 72 |
| `sparse_map<std::string, std::uint64_t>` (libstdc++) | 72 | 192 |

The inline group takes the place of the state of the buckets (56 bytes), so the first inline elements cost nothing: with 16 bytes of bitmaps, 40 bytes are left, 2 `uint64_t -> uint64_t` elements or 5 `uint64_t` keys. With metall's allocator every size is 8 bytes larger.

An inline capacity other than 0 does not compile for:

- elements whose move constructor can throw (a `static_assert`). The inline elements move one by one in a move, a swap and the move into an allocated group, and `noexcept` of the move constructor and of `swap` does not depend on the element type. Such elements keep the holes of their groups, see [exception safety](#exception-safety).
- elements with an alignment larger than the alignment of the allocator's pointer type and `size_type`, 8 bytes with `std::allocator` on 64 bit targets (a `static_assert`).
- a key or a mapped type that is incomplete where the container type is instantiated, for example a map that is a member of its own mapped type. The container object holds the elements, so the compiler needs their size, and it reports the incomplete type, for example "field has incomplete type" (clang) or "has incomplete type" (gcc) for a map, and "incomplete type used in type trait expression" (clang) or "invalid use of incomplete type" (gcc) for a set. The inline capacity 0 allows incomplete types.

Move construction, move assignment and `swap` move the inline elements one by one. So, unlike with `std::unordered_map`, they invalidate the iterators, references and pointers to inline elements, and a byte copy of a container object is not a move. In a constant expression the inline group holds no element: the first insertion allocates the group of 64 buckets. `bucket_count()` of a container with inline elements is 64, of an empty inline container 0. `reserve(n)` keeps an empty container inline if `n` elements fit, and a bucket count `n` from 1 to 64, in the constructor or in `rehash(n)`, gives an empty container an allocated group instead of the inline group.

The inline capacity and the layout of the inline group are part of the [persisted format](design.md#the-persisted-format).

## Hash functions

The bucket count is 0 or a power of two of at least 64, and a hash picks its bucket with a mask on its low bits. A hash function is avalanching if every bit of the input changes about half of the bits of the output. Its hash is used as it is. Any other hash function, for example `std::hash<std::uint64_t>`, which is the identity in libstdc++ and libc++, is mixed with one 64 bit multiplication first.

A hash function is marked as avalanching with a member type `is_avalanching`, or with a specialization of `dice::unordered_sparse::hash_is_avalanching`. This is the convention of `ankerl::unordered_dense` and `boost::unordered`:

```c++
struct my_hash {
    using is_avalanching = void;

    std::size_t operator()(std::uint64_t key) const noexcept {
        return some_good_hash(key);
    }
};
```

## Sparsity

The `sparsity` template parameter trades insertion speed for memory. With `sparsity::high` a group grows its storage by 2 elements at a time, with `sparsity::medium` (default) by 4, with `sparsity::low` by 8. A higher sparsity needs less memory and builds a map slower. The lookup speed does not depend on it.

| sparsity | bytes per element, `uint64_t -> uint64_t` | bytes per element, `std::string -> uint64_t` | build, `uint64_t -> size_t` | build, `std::string -> size_t` |
|---|---:|---:|---:|---:|
| `high` | 17.64 (peak 17.65) | 42.11 (peak 42.12) | 11.0 ms | 41.2 ms |
| `medium` | 18.30 (peak 18.31) | 43.76 (peak 43.77) | 8.9 ms | 34.1 ms |
| `low` | 19.59 (peak 19.60) | 47.05 (peak 47.06) | 7.6 ms | 29.3 ms |

Bytes per element: the memory that a map with 100000 elements requests from its allocator after the build, and the peak during the build (without the heap memory of the strings). A rehash frees the old buckets while it moves the elements, so the peak is close to the memory after the build, see [exception safety](#exception-safety). Build: 200000 insertions into an empty map. Measured with the benchmarks of this repository (see [benchmarks/README.md](../benchmarks/README.md)) on an i9-14900K, one pinned core, clang 20 Release `-march=x86-64-v2`, median of three runs.

## Exception safety

If an insertion throws, the container holds the same elements as before, except in one case where it is empty. It comes from a rehash, which an insertion, `merge`, `rehash` or `reserve` can do. Calling `reserve` beforehand avoids rehashes. How a rehash transfers the elements depends on their type:

- Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old buckets is freed while they are moved. If the hash function or the allocator throws while the elements are moved, the container is empty. Any other exception leaves the container unchanged.
- Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and the new buckets are in memory at the same time. An exception leaves the container unchanged.

If `merge` throws, the element that was being merged is still in the source and not in the target. So every element is in exactly one of the two containers.

`erase` does not throw, unless the hash function or the key equality throws. An erased element whose move constructor can throw keeps its memory for a while, see [insertion and erasure in a group](design.md#insertion-and-erasure-in-a-group).

## Allocators and metall

The containers work with allocators that use fancy pointers, for example the allocators of [metall](https://github.com/LLNL/metall) and `boost::interprocess::offset_ptr`, so they can live in persistent memory.

The containers construct, move and destroy their elements through `std::allocator_traits`, so they call `construct` and `destroy` of an allocator that has them. If the elements are trivially copyable and nothrow move constructible, and the allocator has no `construct` and no `destroy` of its own, a group copies its elements as bytes, with `std::memcpy`: when its storage grows, when it is copied, and when it is moved into a container with an unequal allocator.

The allocator of metall has a `construct` and a `destroy`, which construct and destroy in place (placement new, and a destructor call after a check for null). An allocator like that can opt in to the copies as bytes with a specialization of `dice::unordered_sparse::allocator_constructs_in_place`:

```c++
template<typename T, typename Kernel>
struct dice::unordered_sparse::allocator_constructs_in_place<metall::stl_allocator<T, Kernel>> : std::true_type {};
```

Then a group copies trivially copyable elements as bytes also with this allocator, and an insertion or an erasure in the middle of a group moves the elements after it with `std::memmove`. The calls of `construct` and `destroy` for these copies are left out. Specialize it only for an allocator whose `construct` and `destroy` do nothing else with the object (a check for null, as in metall, is fine), and declare the specialization before the first use of a container with this allocator, in every translation unit that uses one. The containers look the trait up for their allocator rebound to the type of the stored elements, so specialize it for every value type of the allocator, as above. A specialization only for `metall::stl_allocator<std::pair<Key, T>, Kernel>` has no effect. The persisted format does not change.

The containers are standard layout types if the hash function, the key equality, the allocator and its pointer type are. The elements of a map are stored in a standard layout type, not in `std::pair`. This type supports uses-allocator construction: with a scoped or a polymorphic allocator, the key and the mapped value get the allocator as they would in a `std::pair`.

A map in a metall datastore takes the allocator of the metall manager, as in `tests/tests_metall.cpp`:

```c++
template<typename T>
using metall_allocator = metall::manager::allocator_type<T>;

using persistent_map = dice::sparse_map<std::uint64_t,
                                        std::uint64_t,
                                        std::hash<std::uint64_t>,
                                        std::equal_to<std::uint64_t>,
                                        metall_allocator<std::pair<std::uint64_t, std::uint64_t>>>;
```

A type that keeps a raw pointer, like `std::string`, cannot live in persistent memory, because the datastore can be mapped at another address when it is opened again.

`dice::unordered_sparse::pobr_version` is the version of the persisted object binary representation, see [the persisted format](design.md#the-persisted-format).

## Optimization

- Make sure that `Key` and `T` have a `noexcept` move constructor. Without it the containers still work, but insertions copy elements and are much slower.
- Make sure that `Key` and `T` also have a `noexcept` move assignment. Without it, an insertion or an erasure moves the elements of a group by construction and destruction through the allocator, which is slower, most of all with an allocator that has its own `construct` and `destroy`, like the allocator of metall.
- The library uses `std::popcount`. Compile with `-mpopcnt` or `-march=native` (x86) so that it becomes one instruction.
- Define `DICE_UNORDERED_SPARSE_DEBUG` (or `TSL_DEBUG`) to enable internal consistency checks.

## Tests and benchmarks

The tests use doctest, the benchmarks use nanobench. Their dependencies come from conan through the [cmake-conan](https://github.com/conan-io/cmake-conan) dependency provider:

```bash
cmake -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo -DBUILD_TESTING=ON -DBUILD_BENCHMARKS=ON \
      -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=conan_provider.cmake
cmake --build build
ctest --test-dir build
```

See [benchmarks/README.md](../benchmarks/README.md) for the benchmarks.
