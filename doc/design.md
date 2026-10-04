# Design

[README](../README.md) · [Usage](usage.md) · **Design** · [Benchmarks](benchmarks.md) · [Upgrading from 0.3](upgrading-from-0.3.md)

How the table works: the groups of buckets and their bitmaps, the probing, the growth, the rehash, the deleted buckets, the inline group and the persisted format.

The library is based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). It uses open addressing with sparse quadratic probing. The goal is to use as little memory as possible, also at a low load factor, while keeping good performance. The [article of Stephen Merity](https://smerity.com/articles/2015/google_sparsehash.html) explains the idea behind `google::sparse_hash_map`, which this library follows.

## Groups and bitmaps

The buckets are grouped by 64. A group stores only its occupied buckets, densely, in bucket order, in a values array. A group has:

- a pointer to its values array,
- a 64 bit bitmap that says which buckets are occupied,
- a 64 bit bitmap that says which buckets are deleted (their value was erased),
- the number of values and the capacity of the values array, 8 bits each,
- a flag that marks the last group of the table.

So each bucket has one bit in each bitmap, and the pointer and the counters are shared by the 64 buckets of a group. A group does not store an allocator, so that groups do not grow with a stateful allocator. Every function of a group that allocates or frees takes the allocator as an argument.

The position of the value of a bucket in the values array is the number of occupied buckets before it in the group. The library counts them with `std::popcount` on the occupancy bitmap. The other way, from a position in the values array to its bucket, as `erase(iterator)` needs it, is a broadword select on the occupancy bitmap, without branches. The first position is the lowest set bit (`std::countr_zero`), without the select, so `erase(begin())` stays cheap.

A bucket with both bits set is a hole. Holes exist only for elements whose move constructor can throw (see below). The slot of a hole in the values array holds no value, but it counts as occupied for the position of the values after it.

The table holds an array of groups. Bucket `i` is in group `i / 64`, at position `i % 64`. The table also keeps the index of its first group with a value, so `begin()` takes constant time. `begin()` only reads this index, so it also works on a read-only mapping. An insertion can lower the index. An erasure that empties the group at the index looks for the next group with a value. So erasing in the order of iteration, `while (!map.empty()) map.erase(map.begin());`, reads each group once in total.

## Insertion and erasure in a group

If the elements are nothrow move constructible, the values array grows by 2, 4 or 8 slots when it is full, depending on the [sparsity](usage.md#sparsity). The values after the position of the new value are moved one place to the back. An erasure removes the value and moves the values after it one place to the front. When the last value of a group is erased, the capacity stays.

How the values move depends on the move assignment of the elements:

- Nothrow move assignable elements move by move assignment, as in `std::vector::insert` and `std::vector::erase`. An insertion moves the last value into a new slot behind it, moves the others one place to the back by assignment and assigns the new value into its place. An erasure moves the values after it one place to the front by assignment and destroys the last value. So, besides the new value in a temporary, only one value is constructed or destroyed through the allocator. For a trivially copyable element, libstdc++ does the assignments with one `memmove`.
- Other elements are constructed at their new place and destroyed at their old place through the allocator, one by one.

When the values array is full, an insertion allocates one that is 2, 4 or 8 slots larger and moves the values into it, one by one through the allocator. If the elements are trivially copyable and the allocator has no `construct` and no `destroy` of its own, the group copies them as bytes with `std::memcpy` instead, also when the group is copied or moved into a new values array. An allocator whose `construct` and `destroy` only construct and destroy in place, like the allocator of metall, can opt in with `allocator_constructs_in_place` (see [allocators and metall](usage.md#allocators-and-metall)). Then the group also copies the elements as bytes, and an insertion or an erasure moves the values after it with `std::memmove`, without calls of `construct` and `destroy`.

If the move constructor of the elements can throw, no value is moved inside the values array:

- An erasure destroys the value and leaves a hole. When the last value of a group is erased, the group frees its values array.
- An insertion into a hole constructs the new value in its slot.
- Any other insertion allocates a new values array, copies the values into it and constructs the new value there. The holes of the group become deleted buckets.

So an erasure never throws, and an insertion keeps the strong exception guarantee. A hole keeps its memory until an insertion of the last kind or a rehash. A copy of the container has no holes.

## Hashing and probing

The bucket count is 0 or a power of two of at least 64, one group. A table with fewer buckets would still have one group, so it would take the same memory and grow five more times before it holds 32 elements. A hash picks its bucket with a mask on its low bits. The hash of a hash function that is not marked as avalanching (see [hash functions](usage.md#hash-functions)) is mixed first: it is multiplied with the golden ratio constant `0x9E3779B97F4A7C15` to 128 bits, and the two halves are combined with xor.

Collisions are resolved with quadratic probing. Probe number `i` goes `i` buckets further than probe number `i - 1`.

- A lookup and an erasure follow the probe sequence until they find the key, or reach an empty bucket that is not deleted, or the number of probes reaches the bucket count.
- An insertion follows the probe sequence like a lookup. A deleted bucket on the way is not enough to know that the key is absent, so the search goes on until an empty bucket. It remembers the first deleted bucket, and the new element goes there. If there is none, the new element goes into the empty bucket.

The lookups of the containers (`find`, `contains`, `count`, `at`, `equal_range` and `lookup`) and the functions of the lookup below them, down to the probe (`find_impl`), are always inlined into their caller with Clang and GCC, at every inline capacity. So every lookup by key gets the probe inlined, also the lookup of `merge`, and the code of a lookup does not depend on the inlining heuristics of the compiler. Measured with the benchmark binary of the inline capacity 0 (instructions on an AMD EPYC 9684X): without the attribute on `find_impl` of a container without an inline group (the inline capacity 0), Clang 20 kept the lookup of a `std::string` key out of line in 7 places, and GCC 14 the lookups of all key types in 33 places. With it, Clang 20 runs 0.78 to 0.94 of the instructions for a lookup of a `std::string` key that is not in the map and 0.97 for one that is, and no benchmark row needs more than 1.01 times the instructions. With GCC 14 the rows of `find_all` take 0.66 to 0.97 of the instructions, and other rows 0.69 to 1.20: up to 1.07 for rows that look up keys of which half are in the map, up to 1.20 for rows that insert and erase. GCC 14 also inlines other code of the benchmarks differently: rows of the other containers take 0.80 to 1.00 of the instructions. The erasure by key is left to the inlining heuristics. Always inlined into the loop of a benchmark that iterates over a map while it adds and removes `std::string` keys (metall's allocator), the probe of the erasure made Clang 20 keep a variable of the loop on the stack instead of in a register, and the row took 1.35 to 1.52 times as long (times on a Xeon Platinum 8468V). GCC 14 kept the variable in a register.

## Growth and rehash

A new container has no buckets. The first insertion gives it one group of 64 buckets, and `reserve` and `rehash` round a bucket count that is not 0 up to 64. The default maximum load factor is 0.5. `max_load_factor(float)` clamps its argument to [0.1, 0.8]. When the number of elements has reached the maximum load factor times the bucket count, the next insertion doubles the bucket count.

A rehash builds a new table and inserts every element into it, which calls the hash function once per element. How the elements get there depends on their type:

- Elements whose move constructor cannot throw are moved, one old group after the other. Each old group is freed right after its elements are moved. So the peak memory of a rehash is close to the memory after it. If the hash function or the allocator throws while the elements are moved, the table is cleared, so it is empty afterwards. An exception of the allocator before, while the new table allocates its groups, leaves the table unchanged.
- Elements whose move constructor can throw are copied. The old groups are freed at the end, so the old and the new groups are in memory at the same time. The table stays untouched until all copies are made, so an exception leaves it unchanged.

The table takes the new groups with a swap that cannot throw.

An insertion that needs a rehash constructs the new element first, because its key and its arguments may refer to elements that the rehash moves.

## Deleted buckets

An erasure marks the bucket as deleted (a tombstone), so that a later lookup goes on past it. A hole counts as a deleted bucket. An insertion can reuse a deleted bucket. The deleted buckets are removed with a rehash to the same bucket count. It happens when the number of elements plus the number of deleted buckets reaches `max_load_factor + 0.5 * (1 - max_load_factor)` of the bucket count, which is 0.75 with the default maximum load factor.

## The inline group

A container with an inline capacity (`inline_capacity`, see [small maps](usage.md#small-maps)) keeps the elements of a small container in the container object. The container object then holds a union of the state of the buckets (the pointer to the groups, the number of groups, the bucket count, the first group with a value, the number of deleted buckets and the two load thresholds, 56 bytes) and the inline group, and a flag that tells which of the two is in use. A container with an inline group has buckets if and only if it is not inline. The inline capacity 0 has no union and no flag, the container object holds the state of the buckets directly.

The inline group is group 0 of a table of 64 buckets:

- a 64 bit bitmap of the buckets that hold an element,
- a 64 bit bitmap of the deleted buckets,
- room for `inline_capacity` elements, aligned like the state of the buckets, which holds the elements of the occupied buckets densely in bucket order.

The number of elements is outside the union, and so is a byte that counts the elements outside their home bucket (the bucket of their hash), in the padding after the flag. The inline group puts every element into the bucket of group 0 of a table of 64 buckets: a hash picks its bucket with the mask 63, collisions are resolved with quadratic probing, a lookup goes on past deleted buckets, an insertion takes the first deleted bucket on the probe sequence or the empty bucket at its end, and an erasure moves the elements after it one place to the front. An erasure finds the bucket of an inline element from its position with a loop that clears the lowest set bit of the occupancy bitmap once per position, at every inline capacity, not with the select. The loop needs fewer instructions than the select. With 2 to 4 elements the select took 0.97 to 1.16 times as long, with 8 elements the two were within 10 % of each other, and from 16 elements on the select took less time.

The inline group keeps fewer deleted buckets than a group of a table. A lookup needs a deleted bucket only on the probe sequence of an element, before the bucket of that element. If every element is in its home bucket, no probe sequence passes another bucket before it reaches its element, so no deleted bucket is needed. The count of the elements outside their home bucket tells this: an insertion into another bucket than the home bucket adds one, an erasure by key of such an element subtracts one, and a clean-up counts the elements again. An erasure by iterator does not know the home bucket of its element, so it only keeps the count at most at the number of elements. So the count is at least the number of elements outside their home bucket. An erasure marks its bucket as deleted only if the count is not 0 afterwards, otherwise it removes all deleted buckets of the inline group. The bitmap of the deleted buckets is 0 whenever the count is 0. Without deleted buckets, a lookup tests the bitmap of the deleted buckets once and ends at the first empty bucket.

When the elements plus the deleted buckets reach twice the inline capacity, at most `max_load_factor + 0.5 * (1 - max_load_factor)` of 64 buckets (48), an insertion rebuilds the inline group: it inserts the elements again in bucket order into a group without deleted buckets, as a rehash to 64 buckets does. It computes all hashes first, so a hash function that throws leaves the group unchanged, and then moves the elements through storage on the stack. This storage has the size of the inline elements, `inline_capacity * sizeof(element)` bytes (32 KiB for 32 elements of 1 KiB). A swap of two inline groups of elements that are copied as bytes uses storage of the same size on the stack.

The inline group holds at most `inline_capacity` elements, and at most the load threshold of 64 buckets (32 at the default maximum load factor). After a lower maximum load factor it can hold more. Then the next insertion, `reserve` or `rehash` moves the elements into an allocated table that is large enough, as a table with more elements than its load threshold grows then. An insertion into a full inline group allocates an array of one group and a values array, moves the elements into it with both bitmaps, and continues as an insertion into that table. The elements keep their buckets, their order and their deleted buckets, so they are in the buckets where a table of 64 buckets puts them, and no element is hashed. If an allocation throws, the container is unchanged. A rehash to more than 64 buckets moves the inline elements into a new table, with the rules of the rehash.

The lookups are always inlined into their caller (see [hashing and probing](#hashing-and-probing)). So a lookup in a container with an inline group is inlined with both of its parts, the lookup in the inline group and the lookup in the buckets. Then the compiler can test the flag once for a loop of lookups. An erasure in the inline group is a function that is never inlined, so that an erasure in the buckets is inlined as without an inline group.

The inline elements move one by one, with the allocator of the container, in a move, a swap and the move into an allocated group. Trivially copyable elements that the groups copy as bytes are copied as bytes there too. So only elements whose move constructor cannot throw can be inline, and only elements whose alignment fits the alignment of the state of the buckets. In a constant expression the elements cannot be constructed one by one in the storage of a union, so the inline group holds no element there: the first insertion allocates the group of 64 buckets.

## Iterators

An iterator holds plain pointers, also with an allocator that uses fancy pointers: the group, the element and the end of the elements of the group. An iterator to an inline element has no group. To go to the next element, it moves to the next value in the values array, or to the first value of the next group that is not empty. For elements whose move constructor can throw, it holds the run of its element instead of the end: the number of elements in the slots right after it, up to the next hole, and the index after which the next run starts. It moves to the next slot while the run has elements, and otherwise asks the group for the next run, so it skips the holes. A group without holes is one run. An insertion or an erasure can invalidate it.

## Standard layout

The hash function, the key equality and the allocator are members, not base classes, and the table has no base class. So the table, and with it `sparse_map` and `sparse_set`, is a standard layout type whenever `Hash`, `KeyEqual`, the allocator and the allocator's pointer type are standard layout. Empty members take no space.

The elements of a map are stored as `map_slot<Key, T>`, not as `std::pair<Key, T>`. `map_slot` is standard layout when `Key` and `T` are, unlike `std::pair`, whose layout the standard does not define. The public `value_type` is still `std::pair<Key, T>`.

## The persisted format

A container in persistent memory, for example in a metall datastore, keeps the binary representation of its objects. `dice::unordered_sparse::pobr_version` is the version of this persisted object binary representation (POBR). It is 3. CMake writes it into `include/dice/unordered_sparse/version.hpp` from `POBR_VERSION` in `CMakeLists.txt`.

The GitHub Actions workflow `Detect POBR diff` runs on pull requests to `develop` and `main`. It checks the headers `sparse_hash.hpp`, `sparse_map.hpp` and `sparse_set.hpp` under `include/dice/unordered_sparse/detail` for changes of the binary representation against the base branch, with `pobr_version` as the version constant.

The binary representation includes the members of the table (with `std::allocator`, `sizeof(sparse_map<int, int>)` is 72 bytes), the group headers and their bitmaps, and the holes of groups whose elements have a move constructor that can throw.

With an inline capacity, the binary representation also includes the inline capacity and the layout of the inline group: the union of the state of the buckets and the inline group, the flag, the count of the elements outside their home bucket and its meaning (at least that number and at most the number of elements, 0 only without deleted buckets, 0 when the container is not inline), the header of the inline group (the bitmap of the buckets with an element and the bitmap of the deleted buckets), the order of the inline elements and the deleted buckets in it. A change of the inline capacity of a container type, or of the header of the inline group, is a change of the persisted format of that type. So is a change of the rule which elements can be inline (a move constructor that cannot throw, the alignment), of the limit of 32 elements, of the rule when the inline group moves into an allocated group, of the rule when an erasure leaves a deleted bucket, and of the threshold at which an insertion cleans up the inline group.

Containers that were persisted with 0.3 cannot be opened. 0.4 mixes the hash of a hash function that is not marked as avalanching, so the elements are placed differently than in 0.3. See [upgrading from 0.3](upgrading-from-0.3.md).
