# Design

[README](../README.md) · [Usage](usage.md) · **Design** · [Benchmarks](benchmarks.md) · [Upgrading from 0.3](upgrading-from-0.3.md)

How the table works: the groups of buckets and their bitmaps, the probing, the growth, the rehash, the deleted buckets and the persisted format.

The library is based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). It uses open addressing with sparse quadratic probing. The goal is to use as little memory as possible, also at a low load factor, while keeping good performance. The [article of Stephen Merity](https://smerity.com/articles/2015/google_sparsehash.html) explains the idea behind `google::sparse_hash_map`, which this library follows.

## Groups and bitmaps

The buckets are grouped by 64. A group stores only its occupied buckets, densely, in bucket order, in a values array. A group has:

- a pointer to its values array,
- a 64 bit bitmap that says which buckets are occupied,
- a 64 bit bitmap that says which buckets are deleted (their value was erased),
- the number of values and the capacity of the values array, 8 bits each,
- a flag that marks the last group of the table.

So each bucket has one bit in each bitmap, and the pointer and the counters are shared by the 64 buckets of a group. A group does not store an allocator, so that groups do not grow with a stateful allocator. Every function of a group that allocates or frees takes the allocator as an argument.

The position of the value of a bucket in the values array is the number of occupied buckets before it in the group. The library counts them with `std::popcount` on the occupancy bitmap.

The table holds an array of groups. Bucket `i` is in group `i / 64`, at position `i % 64`.

## Insertion and erasure in a group

If the elements are nothrow move constructible, the values array grows by 2, 4 or 8 slots when it is full, depending on the [sparsity](usage.md#sparsity). The values after the position of the new value are moved one place to the back. An erasure destroys the value and moves the values after it one place to the front. When the last value of a group is erased, the capacity stays.

If the move constructor of the elements can throw, every insertion allocates a new values array for one more value, copies the values into it and constructs the new value there, and every erasure copies all other values into a new array. This is slower, but it keeps the strong exception guarantee.

## Hashing and probing

The bucket count is 0 or a power of two. A hash picks its bucket with a mask on its low bits. The hash of a hash function that is not marked as avalanching (see [hash functions](usage.md#hash-functions)) is mixed first: it is multiplied with the golden ratio constant `0x9E3779B97F4A7C15` to 128 bits, and the two halves are combined with xor.

Collisions are resolved with quadratic probing. Probe number `i` goes `i` buckets further than probe number `i - 1`.

- A lookup and an erasure follow the probe sequence until they find the key, or reach an empty bucket that is not deleted, or the number of probes reaches the bucket count.
- An insertion follows the probe sequence like a lookup. A deleted bucket on the way is not enough to know that the key is absent, so the search goes on until an empty bucket. It remembers the first deleted bucket, and the new element goes there. If there is none, the new element goes into the empty bucket.

## Growth and rehash

A new container has no buckets. The default maximum load factor is 0.5. `max_load_factor(float)` clamps its argument to [0.1, 0.8]. When the number of elements has reached the maximum load factor times the bucket count, the next insertion doubles the bucket count.

A rehash builds a new table and inserts every element into it, which calls the hash function once per element. How the elements get there depends on their type:

- Elements whose move constructor cannot throw are moved, one old group after the other. Each old group is freed right after its elements are moved. So the peak memory of a rehash is close to the memory after it. If the hash function or the allocator throws while the elements are moved, the table is cleared, so it is empty afterwards. An exception of the allocator before, while the new table allocates its groups, leaves the table unchanged.
- Elements whose move constructor can throw are copied. The old groups are freed at the end, so the old and the new groups are in memory at the same time. The table stays untouched until all copies are made, so an exception leaves it unchanged.

The table takes the new groups with a swap that cannot throw.

An insertion that needs a rehash constructs the new element first, because its key and its arguments may refer to elements that the rehash moves.

## Deleted buckets

An erasure marks the bucket as deleted (a tombstone), so that a later lookup goes on past it. An insertion can reuse a deleted bucket. The deleted buckets are removed with a rehash to the same bucket count. It happens when the number of elements plus the number of deleted buckets reaches `max_load_factor + 0.5 * (1 - max_load_factor)` of the bucket count, which is 0.75 with the default maximum load factor.

## Iterators

An iterator holds plain pointers, also with an allocator that uses fancy pointers: the group, the element and the end of the elements of the group. To go to the next element, it moves to the next value in the values array, or to the first value of the next group that is not empty. An insertion or an erasure can invalidate it.

## Standard layout

The hash function, the key equality and the allocator are members, not base classes, and the table has no base class. So the table, and with it `sparse_map` and `sparse_set`, is a standard layout type whenever `Hash`, `KeyEqual`, the allocator and the allocator's pointer type are standard layout. Empty members take no space.

The elements of a map are stored as `map_slot<Key, T>`, not as `std::pair<Key, T>`. `map_slot` is standard layout when `Key` and `T` are, unlike `std::pair`, whose layout the standard does not define. The public `value_type` is still `std::pair<Key, T>`.

## The persisted format

A container in persistent memory, for example in a metall datastore, keeps the binary representation of its objects. `dice::unordered_sparse::pobr_version` is the version of this persisted object binary representation (POBR). It is 3. CMake writes it into `include/dice/unordered_sparse/version.hpp` from `POBR_VERSION` in `CMakeLists.txt`.

The GitHub Actions workflow `Detect POBR diff` runs on pull requests to `develop` and `main`. It checks the headers `sparse_hash.hpp`, `sparse_map.hpp` and `sparse_set.hpp` under `include/dice/unordered_sparse/detail` for changes of the binary representation against the base branch, with `pobr_version` as the version constant.

Containers that were persisted with 0.3 cannot be opened. 0.4 mixes the hash of a hash function that is not marked as avalanching, so the elements are placed differently than in 0.3. See [upgrading from 0.3](upgrading-from-0.3.md).
