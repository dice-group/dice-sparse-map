#include "fixtures/allocators.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <new>
#include <ranges>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * Holes. A group of elements whose move constructor can throw never moves its elements: an erase destroys the
 * element in place and keeps its slot as a hole. The elements are `copied_obj`, a `counter::obj` whose move
 * constructor can throw. `counter::obj` aborts the program when a destroyed object is compared, copied or destroyed
 * again, so a test that takes a hole for an element ends there.
 *
 * `tests::bucket_hash` puts key `k` into bucket `k` of a table with more than `k` buckets, so a test knows which keys share
 * a group: the keys 0 to 63 are in group 0, 64 to 127 in group 1 and so on. `leak_checking_allocator` counts the
 * bytes of the groups and of their storage, so a test sees how many slots the groups hold. The containers are
 * `tests::throwing_map` and a `sparse_set` with `sh::allocation_failure::throwing`, so an allocation that `bomb_after`
 * makes fail throws `std::bad_alloc`.
 */
namespace {
    using namespace dice::sparse_map;
    using namespace dice::sparse_map::tests;

    using map_t = throwing_map<
        copied_obj,
        copied_obj,
        bucket_hash,
        std::equal_to<counter::obj>,
        leak_checking_allocator<std::pair<copied_obj, copied_obj>>
    >;
    using set_t = sparse_set<
        copied_obj,
        bucket_hash,
        std::equal_to<counter::obj>,
        leak_checking_allocator<copied_obj>,
        sh::sparsity::medium,
        sh::allocation_failure::throwing
    >;
    using slot_t = detail_sparse_hash::map_slot<copied_obj, copied_obj>;
    using group_t = detail_sparse_hash::sparse_array<
        slot_t,
        leak_checking_allocator<slot_t>,
        sh::sparsity::medium,
        sh::allocation_failure::throwing
    >;

    static_assert(!std::is_nothrow_move_constructible_v<slot_t>);

    /// number of `counter::obj` that were constructed and not destroyed yet
    std::size_t alive(counter const &counts) {
        return counts.ctor()
            + counts.default_ctor()
            + counter::static_ctor
            + counts.copy_ctor()
            + counts.move_ctor()
            - counts.dtor()
            - counter::static_dtor;
    }

    copied_obj key(counter &counts, std::size_t n) {
        return copied_obj{n, counts};
    }

    /// inserts the key `n` mapped to `n`
    template<typename Map>
    void insert(Map &map, counter &counts, std::size_t n) {
        map.try_emplace(typename Map::key_type{n, counts}, typename Map::mapped_type{n, counts});
    }

    /// a map with room for `n` elements, so that no insertion rehashes it
    template<typename Map = map_t>
    Map reserved_map(std::size_t n) {
        auto map = Map{};
        map.reserve(n);
        return map;
    }

    /// the keys that iteration visits, in its order
    template<typename Container>
    std::vector<std::size_t> iterated_keys(Container const &container) {
        std::vector<std::size_t> keys;
        for (auto it = container.begin(); it != container.end(); ++it) {
            if constexpr (requires { it->first; }) {
                keys.push_back(it->first.get());
            } else {
                keys.push_back(it->get());
            }
        }
        return keys;
    }

    /// the keys of `map` that `find` reaches, among the keys below `searched_up_to`
    template<typename Map>
    std::vector<std::size_t> found_keys(Map const &map, counter &counts, std::size_t searched_up_to) {
        std::vector<std::size_t> keys;
        for (std::size_t n = 0; n < searched_up_to; ++n) {
            auto const it = map.find(typename Map::key_type{n, counts});
            if (it != map.end()) {
                keys.push_back(n);
            }
        }
        return keys;
    }

    /**
     * Checks that every element of `map` is mapped to its key, and that `size`, `find` and iteration agree on the
     * elements among the keys below `searched_up_to`.
     */
    template<typename Map>
    void check_consistent(Map const &map, counter &counts, std::size_t searched_up_to) {
        bool values_right = true;
        for (auto const &entry : map) {
            values_right = values_right && entry.second.get() == entry.first.get();
        }
        auto iterated = iterated_keys(map);
        std::ranges::sort(iterated);
        CHECK(values_right);
        CHECK(iterated.size() == map.size());
        CHECK(found_keys(map, counts, searched_up_to) == iterated);
    }

    /// the bytes of the groups of `map` and of `nb_slots` slots of storage
    std::size_t bytes_of(map_t const &map, std::size_t nb_slots) {
        return map.bucket_count() / group_t::nb_buckets * sizeof(group_t) + nb_slots * sizeof(slot_t);
    }

    std::vector<std::size_t> range(std::size_t first, std::size_t last) {
        std::vector<std::size_t> numbers;
        for (std::size_t n = first; n < last; ++n) {
            numbers.push_back(n);
        }
        return numbers;
    }

    std::vector<std::size_t> concat(std::vector<std::vector<std::size_t>> const &parts) {
        std::vector<std::size_t> all;
        for (auto const &part : parts) {
            all.insert(all.end(), part.begin(), part.end());
        }
        return all;
    }

} // namespace

TEST_CASE("an erase in the middle of a group moves, copies and allocates nothing") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (std::size_t n = 0; n < 10; ++n) {
            insert(map, counts, n);
        }

        auto const copies = counts.copy_ctor();
        auto const moves = counts.move_ctor();
        auto const allocations = nb_allocations;
        CHECK(map.erase(key(counts, 5)) == 1);
        auto const next = map.erase(map.find(key(counts, 2)));
        CHECK(counts.copy_ctor() == copies);
        CHECK(counts.move_ctor() == moves);
        CHECK(nb_allocations == allocations);

        REQUIRE(next != map.end());
        CHECK(next->first.get() == 3);
        CHECK(iterated_keys(map) == std::vector<std::size_t>{0, 1, 3, 4, 6, 7, 8, 9});
        check_consistent(map, counts, 64);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("an insertion into a hole allocates nothing") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (std::size_t n = 0; n < 10; ++n) {
            insert(map, counts, n);
        }
        CHECK(map.erase(key(counts, 5)) == 1);

        auto const bytes = live_bytes;
        auto const allocations = nb_allocations;
        {
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(insert(map, counts, 5));
        }
        CHECK(live_bytes == bytes);
        CHECK(nb_allocations == allocations);
        CHECK(map.size() == 10);
        CHECK(iterated_keys(map) == range(0, 10));
        check_consistent(map, counts, 64);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("iteration, find and size skip the holes") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        REQUIRE(map.bucket_count() == 256);
        for (auto const n : concat({range(0, 10), range(64, 74), range(128, 138)})) {
            insert(map, counts, n);
        }

        // holes at the first, at a middle and at the last slot of group 1
        CHECK(map.erase(key(counts, 64)) == 1);
        CHECK(map.erase(key(counts, 68)) == 1);
        CHECK(map.erase(key(counts, 73)) == 1);
        auto expected = concat({range(0, 10), {65, 66, 67, 69, 70, 71, 72}, range(128, 138)});
        CHECK(iterated_keys(map) == expected);
        CHECK(map.size() == expected.size());
        CHECK(found_keys(map, counts, 256) == expected);

        // `erase` returns the next element, past a hole and into the next group
        auto it = map.erase(map.find(key(counts, 67)));
        REQUIRE(it != map.end());
        CHECK(it->first.get() == 69);
        it = map.erase(map.find(key(counts, 72)));
        REQUIRE(it != map.end());
        CHECK(it->first.get() == 128);

        // a group whose elements are all erased is skipped
        for (std::size_t n = 0; n < 10; ++n) {
            CHECK(map.erase(key(counts, n)) == 1);
        }
        REQUIRE(map.begin() != map.end());
        CHECK(map.begin()->first.get() == 65);
        CHECK(std::as_const(map).begin()->first.get() == 65);
        expected = concat({{65, 66, 69, 70, 71}, range(128, 138)});
        CHECK(iterated_keys(map) == expected);
        CHECK(found_keys(map, counts, 256) == expected);
        check_consistent(map, counts, 256);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("iteration and find of a set skip the holes") {
    auto counts = counter{};
    {
        auto set = set_t{};
        set.reserve(100);
        for (std::size_t n = 0; n < 10; ++n) {
            set.insert(key(counts, n));
        }
        CHECK(set.erase(key(counts, 0)) == 1);
        CHECK(set.erase(key(counts, 4)) == 1);
        CHECK(set.erase(key(counts, 9)) == 1);
        CHECK(iterated_keys(set) == std::vector<std::size_t>{1, 2, 3, 5, 6, 7, 8});
        CHECK(set.size() == 7);
        CHECK_FALSE(set.contains(key(counts, 4)));
        CHECK(set.contains(key(counts, 5)));
        set.insert(key(counts, 4));
        CHECK(iterated_keys(set) == std::vector<std::size_t>{1, 2, 3, 4, 5, 6, 7, 8});
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

/*
 * The iterator steps through the values in consecutive slots of a group (a run) like through a group without holes,
 * and asks the group for the next run at a hole. A group without holes is one run. An iterator that `find` returns
 * knows only the index of its element, so its first `++` asks the group.
 */

TEST_CASE("iteration and find over groups with and without holes") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        REQUIRE(map.bucket_count() == 256);
        for (auto const n : concat({range(0, 10), range(64, 76), range(128, 138), {192, 193}})) {
            insert(map, counts, n);
        }
        // group 0 and group 2 have no holes. Group 1 has holes at its first slot, at two neighbouring slots and at its
        // last slot. Group 3 has a hole after its only element.
        for (std::size_t const n : {64, 67, 68, 75, 193}) {
            CHECK(map.erase(key(counts, n)) == 1);
        }
        auto const expected = concat({range(0, 10), {65, 66, 69, 70, 71, 72, 73, 74}, range(128, 138), {192}});

        CHECK(iterated_keys(map) == expected);
        CHECK(found_keys(map, counts, 256) == expected);
        CHECK(static_cast<std::size_t>(std::distance(map.begin(), map.end())) == expected.size());

        // from every element that `find` returns, `++` reaches the element that iteration visits next, and the walk
        // from there visits the rest
        bool next_right = true;
        bool rest_right = true;
        for (std::size_t i = 0; i < expected.size(); ++i) {
            auto it = map.find(key(counts, expected[i]));
            auto const previous = it++;
            next_right = next_right && previous->first.get() == expected[i];
            if (i + 1 == expected.size()) {
                next_right = next_right && it == map.end();
                continue;
            }
            next_right = next_right && it != map.end() && it->first.get() == expected[i + 1];
            std::vector<std::size_t> rest;
            for (; it != map.end(); ++it) {
                rest.push_back(it->first.get());
            }
            rest_right = rest_right
                && std::ranges::equal(
                    rest,
                    std::ranges::subrange(expected.begin() + static_cast<std::ptrdiff_t>(i) + 1, expected.end())
                );
        }
        CHECK(next_right);
        CHECK(rest_right);
        check_consistent(map, counts, 256);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("an erase while iterating gives a group without holes its first hole") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (auto const n : concat({range(0, 10), range(64, 74)})) {
            insert(map, counts, n);
        }

        // `erase` returns an iterator that knows the index of its element. An iterator that `++` reached gets the
        // index of its element from its slot.
        std::vector<std::size_t> visited;
        for (auto it = map.begin(); it != map.end();) {
            auto const n = it->first.get();
            visited.push_back(n);
            if (n % 3 == 1) {
                it = map.erase(it);
            } else {
                ++it;
            }
        }
        CHECK(visited == concat({range(0, 10), range(64, 74)}));
        std::vector<std::size_t> kept;
        std::ranges::copy_if(visited, std::back_inserter(kept), [](std::size_t n) { return n % 3 != 1; });
        CHECK(iterated_keys(map) == kept);

        // through a `const_iterator` that `++` reached, in a group with holes
        auto it = std::as_const(map).begin();
        ++it;
        ++it;
        REQUIRE(it->first.get() == 3);
        auto const next = map.erase(it);
        REQUIRE(next != map.end());
        CHECK(next->first.get() == 5);
        kept.erase(std::ranges::find(kept, 3));
        CHECK(iterated_keys(map) == kept);
        check_consistent(map, counts, 128);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("an insertion into a group without holes copies its slots in order into storage of the exact size") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (std::size_t n = 1; n < 10; ++n) {
            if (n != 5) {
                insert(map, counts, n);
            }
        }
        REQUIRE(live_bytes == bytes_of(map, 8));

        // into the middle, to the front and to the end of the group: each insertion copies the elements of the group,
        // key and mapped value, and constructs the new element from its arguments
        std::size_t nb_slots = 8;
        for (std::size_t const n : {5, 0, 10}) {
            auto const copies = counts.copy_ctor();
            insert(map, counts, n);
            CHECK(counts.copy_ctor() - copies == 2 * nb_slots + 2);
            ++nb_slots;
            CHECK(live_bytes == bytes_of(map, nb_slots));
        }
        CHECK(iterated_keys(map) == range(0, 11));
        check_consistent(map, counts, 64);

        // an insertion that throws leaves the group as it was
        {
            auto const bomb = bomb_after{0};
            CHECK_THROWS_AS(insert(map, counts, 20), std::bad_alloc);
        }
        CHECK(live_bytes == bytes_of(map, 11));
        CHECK(iterated_keys(map) == range(0, 11));

        // with a hole, an insertion into another bucket compacts the group: 10 elements and the new one
        CHECK(map.erase(key(counts, 4)) == 1);
        CHECK(live_bytes == bytes_of(map, 11));
        auto const copies = counts.copy_ctor();
        insert(map, counts, 20);
        CHECK(counts.copy_ctor() - copies == 2 * 10 + 2);
        CHECK(live_bytes == bytes_of(map, 11));
        CHECK(iterated_keys(map) == concat({range(0, 4), range(5, 11), {20}}));
        check_consistent(map, counts, 64);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("an insertion into a copied group allocates storage of the exact size") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (std::size_t n = 0; n < 10; ++n) {
            insert(map, counts, n);
        }
        for (std::size_t const n : {2, 5, 7}) {
            CHECK(map.erase(key(counts, n)) == 1);
        }
        REQUIRE(live_bytes == bytes_of(map, 10));

        // the copy has no holes and storage for its 7 elements only
        auto copy = map_t{map};
        REQUIRE(copy.bucket_count() == map.bucket_count());
        CHECK(live_bytes == bytes_of(map, 10) + bytes_of(copy, 7));

        // an insertion into another bucket allocates storage for the 7 elements and the new one
        insert(copy, counts, 10);
        CHECK(live_bytes == bytes_of(map, 10) + bytes_of(copy, 8));
        CHECK(iterated_keys(copy) == std::vector<std::size_t>{0, 1, 3, 4, 6, 8, 9, 10});
        check_consistent(copy, counts, 64);

        // the source keeps its holes
        {
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(insert(map, counts, 5));
        }
        CHECK(live_bytes == bytes_of(map, 10) + bytes_of(copy, 8));
        check_consistent(map, counts, 64);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("a rehash copies the groups with and without holes") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (auto const n : concat({range(0, 20), range(64, 84), range(128, 148)})) {
            insert(map, counts, n);
        }
        // holes in group 1 only
        for (std::size_t n = 64; n < 84; n += 3) {
            CHECK(map.erase(key(counts, n)) == 1);
        }
        auto const keys = iterated_keys(map);
        auto const bucket_count = map.bucket_count();
        REQUIRE(keys.size() == 53);

        map.rehash(bucket_count);
        CHECK(map.bucket_count() == bucket_count);
        CHECK(live_bytes == bytes_of(map, keys.size()));
        CHECK(iterated_keys(map) == keys);
        check_consistent(map, counts, 256);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("an insertion into a bucket without a slot compacts the group") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (std::size_t n = 0; n < 10; ++n) {
            insert(map, counts, n);
        }
        REQUIRE(live_bytes == bytes_of(map, 10));

        // two holes, they keep their slots
        CHECK(map.erase(key(counts, 3)) == 1);
        CHECK(map.erase(key(counts, 6)) == 1);
        CHECK(live_bytes == bytes_of(map, 10));
        auto const keys = iterated_keys(map);

        // an insertion into an empty bucket of the group that throws leaves the group as it was, holes included
        {
            auto const bomb = bomb_after{0};
            CHECK_THROWS_AS(insert(map, counts, 20), std::bad_alloc);
        }
        CHECK(live_bytes == bytes_of(map, 10));
        CHECK(iterated_keys(map) == keys);
        {
            // the hole of key 3 is still there, its insertion allocates nothing
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(insert(map, counts, 3));
        }
        CHECK(map.erase(key(counts, 3)) == 1);

        // the insertion copies the 8 elements and the new one into storage of 9 slots, without the holes
        insert(map, counts, 20);
        CHECK(live_bytes == bytes_of(map, 9));
        CHECK(iterated_keys(map) == std::vector<std::size_t>{0, 1, 2, 4, 5, 7, 8, 9, 20});

        // the holes are deleted buckets without a slot now, so their insertion allocates
        {
            auto const bomb = bomb_after{0};
            CHECK_THROWS_AS(insert(map, counts, 3), std::bad_alloc);
        }
        insert(map, counts, 3);
        CHECK(live_bytes == bytes_of(map, 10));
        CHECK(map.size() == 10);
        check_consistent(map, counts, 64);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("a group frees its storage when its last element is erased") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (std::size_t n = 0; n < 3; ++n) {
            insert(map, counts, n);
        }
        insert(map, counts, 64);
        REQUIRE(live_bytes == bytes_of(map, 4));

        CHECK(map.erase(key(counts, 1)) == 1);
        CHECK(map.erase(key(counts, 0)) == 1);
        CHECK(live_bytes == bytes_of(map, 4));
        CHECK(map.erase(key(counts, 2)) == 1);
        CHECK(live_bytes == bytes_of(map, 1));
        CHECK(iterated_keys(map) == std::vector<std::size_t>{64});

        // the holes of the group are deleted buckets without a slot now, so an insertion allocates
        {
            auto const bomb = bomb_after{0};
            CHECK_THROWS_AS(insert(map, counts, 1), std::bad_alloc);
        }
        insert(map, counts, 1);
        CHECK(live_bytes == bytes_of(map, 2));
        CHECK(iterated_keys(map) == std::vector<std::size_t>{1, 64});
        check_consistent(map, counts, 128);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("a rehash for growth drops the holes") {
    auto counts = counter{};
    {
        auto map = map_t{};
        for (std::size_t n = 0; n < 200; ++n) {
            insert(map, counts, n);
        }
        REQUIRE(map.bucket_count() == 512);
        // holes in the groups 0 to 3, which hold the keys
        for (std::size_t n = 0; n < 200; n += 3) {
            CHECK(map.erase(key(counts, n)) == 1);
        }
        CHECK(live_bytes == bytes_of(map, 200));

        // 133 elements reach the growth threshold of 128 at this load factor, so the next insertion grows the table
        map.max_load_factor(0.25f);
        insert(map, counts, 1000);
        CHECK(map.bucket_count() == 1024);
        // every group holds exactly its elements
        CHECK(live_bytes == bytes_of(map, map.size()));
        check_consistent(map, counts, 1001);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("a rehash to the same bucket count drops the holes") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (std::size_t n = 0; n < 100; ++n) {
            insert(map, counts, n);
        }
        for (std::size_t n = 0; n < 100; n += 3) {
            CHECK(map.erase(key(counts, n)) == 1);
        }
        auto const bucket_count = map.bucket_count();
        CHECK(live_bytes == bytes_of(map, 100));

        map.rehash(bucket_count);
        CHECK(map.bucket_count() == bucket_count);
        CHECK(live_bytes == bytes_of(map, map.size()));
        check_consistent(map, counts, 100);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("the clean-up rehash of an insertion drops the holes") {
    auto counts = counter{};
    {
        // 128 buckets, at max load factor 0.8 the table takes 102 elements without growing
        auto map = map_t{};
        map.rehash(128);
        map.max_load_factor(0.8f);
        for (std::size_t n = 0; n < 80; ++n) {
            insert(map, counts, n);
        }
        REQUIRE(map.bucket_count() == 128);

        // 11 elements and 69 deleted buckets: 53 holes in group 0, and group 1, which freed its storage. Below the
        // growth threshold of 12 and above the clean-up threshold of 70, so the next insertion of a new key removes
        // the holes with a rehash.
        for (std::size_t n = 11; n < 80; ++n) {
            CHECK(map.erase(key(counts, n)) == 1);
        }
        CHECK(live_bytes == bytes_of(map, 64));
        map.max_load_factor(0.1f);

        // into group 1, so that only the rehash compacts group 0
        insert(map, counts, 100);
        CHECK(map.bucket_count() == 128);
        CHECK(live_bytes == bytes_of(map, 12));
        CHECK(iterated_keys(map) == concat({range(0, 11), {100}}));
        check_consistent(map, counts, 128);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

// A rehash copies elements whose move constructor can throw, and keeps the old groups until all copies are made. So
// an allocation that fails leaves the map as it was, holes included.
TEST_CASE("a rehash that throws keeps the holes") {
    auto counts = counter{};
    std::size_t failures = 0;
    bool completed = false;
    for (int budget = 0; budget < 100000 && !completed; ++budget) {
        {
            auto map = reserved_map(100);
            for (std::size_t n = 0; n < 100; ++n) {
                insert(map, counts, n);
            }
            for (std::size_t n = 0; n < 100; n += 3) {
                CHECK(map.erase(key(counts, n)) == 1);
            }
            auto const bucket_count = map.bucket_count();
            auto const bytes = live_bytes;
            auto const keys = iterated_keys(map);
            try {
                auto const bomb = bomb_after{budget};
                map.rehash(bucket_count * 2);
                completed = true;
            } catch (std::bad_alloc const &) {
                ++failures;
                CHECK(map.bucket_count() == bucket_count);
                CHECK(live_bytes == bytes);
                CHECK(iterated_keys(map) == keys);
                // the hole of key 3 is still there, its insertion allocates nothing
                auto const bomb = bomb_after{0};
                CHECK_NOTHROW(insert(map, counts, 3));
            }
            if (completed) {
                CHECK(map.bucket_count() == bucket_count * 2);
                CHECK(live_bytes == bytes_of(map, map.size()));
                CHECK(iterated_keys(map) == keys);
            }
            check_consistent(map, counts, 100);
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(failures > 0);
    CHECK(completed);
}

TEST_CASE("copies, swap, erase_if and clear of a map with holes") {
    auto counts = counter{};
    {
        auto map = reserved_map(200);
        for (std::size_t n = 0; n < 200; ++n) {
            insert(map, counts, n);
        }
        for (std::size_t n = 0; n < 200; n += 3) {
            CHECK(map.erase(key(counts, n)) == 1);
        }
        auto const keys = iterated_keys(map);
        REQUIRE(keys.size() == 133);

        SUBCASE("copy construction") {
            auto copy = map_t{map};
            CHECK(iterated_keys(copy) == keys);
            check_consistent(copy, counts, 200);
            // the holes of the copy are deleted buckets without a slot
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(insert(map, counts, 3));
            CHECK_THROWS_AS(insert(copy, counts, 3), std::bad_alloc);
        }
        SUBCASE("copy assignment") {
            auto target = reserved_map(10);
            for (std::size_t n = 1000; n < 1010; ++n) {
                insert(target, counts, n);
            }
            target = map;
            CHECK(iterated_keys(target) == keys);
            check_consistent(target, counts, 1010);
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(insert(map, counts, 3));
            CHECK_THROWS_AS(insert(target, counts, 3), std::bad_alloc);
        }
        SUBCASE("swap") {
            auto other = reserved_map(100);
            for (std::size_t n = 1000; n < 1010; ++n) {
                insert(other, counts, n);
            }
            CHECK(other.erase(key(counts, 1005)) == 1);
            auto const other_keys = iterated_keys(other);
            map.swap(other);
            CHECK(iterated_keys(map) == other_keys);
            CHECK(iterated_keys(other) == keys);
            check_consistent(map, counts, 1010);
            check_consistent(other, counts, 1010);
            // the holes travel with the storage
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(insert(map, counts, 1005));
            CHECK_NOTHROW(insert(other, counts, 3));
        }
        SUBCASE("erase_if of every second element") {
            auto const nb_even = static_cast<std::size_t>(std::ranges::count_if(keys, [](std::size_t n) {
                return n % 2 == 0;
            }));
            std::size_t erased = 0;
            {
                auto const bomb = bomb_after{0};
                CHECK_NOTHROW(erased = erase_if(map, [](auto const &entry) { return entry.first.get() % 2 == 0; }));
            }
            CHECK(erased == nb_even);
            CHECK(map.size() == keys.size() - nb_even);
            CHECK(std::ranges::all_of(iterated_keys(map), [](std::size_t n) { return n % 2 == 1; }));
            check_consistent(map, counts, 200);
        }
        SUBCASE("clear") {
            map.clear();
            CHECK(map.empty());
            CHECK(map.begin() == map.end());
            // the groups hold no storage, also not for their holes
            CHECK(live_bytes == bytes_of(map, 0));
            insert(map, counts, 3);
            check_consistent(map, counts, 200);
        }
    }
    // the destructor of a map with holes destroys every element once
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("a move with an unequal allocator copies a map with holes and leaves the holes behind") {
    using allocator_t = id_allocator<std::pair<copied_obj, copied_obj>>;
    using id_map_t = sparse_map<copied_obj, copied_obj, bucket_hash, std::equal_to<counter::obj>, allocator_t>;
    auto counts = counter{};
    auto counts_1 = alloc_counts{};
    auto counts_2 = alloc_counts{};
    {
        auto source = id_map_t{allocator_t{1, &counts_1}};
        source.reserve(200);
        for (std::size_t n = 0; n < 200; ++n) {
            source.try_emplace(key(counts, n), key(counts, n));
        }
        for (std::size_t n = 0; n < 200; n += 3) {
            CHECK(source.erase(key(counts, n)) == 1);
        }
        auto const keys = iterated_keys(source);

        auto moved = id_map_t{std::move(source), allocator_t{2, &counts_2}};
        CHECK(moved.get_allocator().id == 2);
        CHECK(iterated_keys(moved) == keys);
        CHECK(source.empty()); // NOLINT(bugprone-use-after-move)
        bool all_found = true;
        for (auto const n : keys) {
            auto const it = moved.find(key(counts, n));
            all_found = all_found && it != moved.end() && it->second.get() == n;
        }
        CHECK(all_found);
        CHECK(counts_2.allocations > 0);

        // the holes stayed behind: the bucket of the erased key 3 is a deleted bucket without a slot in `moved`, so
        // its insertion allocates
        auto const allocations = counts_2.allocations;
        moved.try_emplace(key(counts, 3), key(counts, 3));
        CHECK(counts_2.allocations == allocations + 1);
    }
    CHECK(counts_1.allocations == counts_1.deallocations);
    CHECK(counts_2.allocations == counts_2.deallocations);
    CHECK(alive(counts) == 0);
}

namespace {
    /// copies and moves of `fragile_copied_obj` that are left before one throws, -1 never throws
    int transfers_until_throw = -1; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * `copied_obj` whose copy constructor throws `std::runtime_error` when `transfers_until_throw` reaches 0. Its move
     * constructor copies and counts the same way.
     */
    struct fragile_copied_obj : copied_obj {
        fragile_copied_obj(std::size_t n, counter &counts) : copied_obj(n, counts) {}

        fragile_copied_obj(fragile_copied_obj const &other) : copied_obj(count_down(other)) {}

        fragile_copied_obj(fragile_copied_obj &&other) noexcept(false) // NOLINT(performance-noexcept-move-constructor)
            :
            copied_obj(count_down(other)) {}

        fragile_copied_obj &operator=(fragile_copied_obj const &) = default;
        fragile_copied_obj &operator=(fragile_copied_obj &&) noexcept(false) = default;
        ~fragile_copied_obj() = default;

    private:
        static copied_obj const &count_down(fragile_copied_obj const &other) {
            if (transfers_until_throw >= 0 && 0 == transfers_until_throw--) {
                throw std::runtime_error("fragile_copied_obj");
            }
            return other;
        }
    };

    using fragile_map_t = throwing_map<
        fragile_copied_obj,
        fragile_copied_obj,
        bucket_hash,
        std::equal_to<counter::obj>,
        leak_checking_allocator<std::pair<fragile_copied_obj, fragile_copied_obj>>
    >;

    /// inserts the key `n` mapped to `n` with the lvalues `k`, so that the copies into the map are the only transfers
    void insert_from_lvalue(fragile_map_t &map, fragile_copied_obj const &k) {
        map.try_emplace(k, k);
    }
} // namespace

// The bits of a bucket change after its value is constructed, and the new storage of a copying insertion or of a copy
// is complete before the old one is touched. So a copy that throws leaves everything as it was. Each copy and move of
// `fragile_copied_obj` counts, so the loops make every copy of the operation throw once.
TEST_CASE("a copy that throws leaves a hole, a group with holes and the source of a copy as they were") {
    auto counts = counter{};
    {
        auto map = reserved_map<fragile_map_t>(100);
        for (std::size_t n = 0; n < 10; ++n) {
            insert(map, counts, n);
        }
        CHECK(map.erase(fragile_copied_obj{3, counts}) == 1);
        auto const keys = iterated_keys(map);
        auto const bytes = live_bytes;
        auto const blocks = live_blocks;

        SUBCASE("into a hole: the key, then the mapped value") {
            auto const k = fragile_copied_obj{3, counts};
            auto const alive_before = alive(counts);
            for (int const transfers : {0, 1}) {
                transfers_until_throw = transfers;
                CHECK_THROWS_AS(insert_from_lvalue(map, k), std::runtime_error);
                transfers_until_throw = -1;
                CHECK(iterated_keys(map) == keys);
                CHECK(live_bytes == bytes);
                CHECK(alive(counts) == alive_before);
            }
            // the hole is still there, its insertion allocates nothing
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(insert_from_lvalue(map, k));
            CHECK(iterated_keys(map) == range(0, 10));
            check_consistent(map, counts, 64);
        }
        SUBCASE("compaction: the 9 elements, then the new key and its mapped value") {
            auto const k = fragile_copied_obj{20, counts};
            auto const alive_before = alive(counts);
            int transfers = 0;
            for (;; ++transfers) {
                REQUIRE(transfers <= 20);
                transfers_until_throw = transfers;
                try {
                    insert_from_lvalue(map, k);
                    transfers_until_throw = -1;
                    break;
                } catch (std::runtime_error const &) {}
                transfers_until_throw = -1;
                CHECK(iterated_keys(map) == keys);
                CHECK(live_bytes == bytes);
                CHECK(live_blocks == blocks);
                CHECK(alive(counts) == alive_before);
            }
            CHECK(transfers == 20);
            CHECK(iterated_keys(map) == concat({range(0, 3), range(4, 10), {20}}));
            check_consistent(map, counts, 64);
        }
        SUBCASE("a copy of the map: the 9 elements") {
            auto const alive_before = alive(counts);
            int transfers = 0;
            for (;; ++transfers) {
                REQUIRE(transfers <= 18);
                transfers_until_throw = transfers;
                try {
                    auto const copy = fragile_map_t{map};
                    transfers_until_throw = -1;
                    CHECK(iterated_keys(copy) == keys);
                    break;
                } catch (std::runtime_error const &) {}
                transfers_until_throw = -1;
                CHECK(alive(counts) == alive_before);
                CHECK(live_blocks == blocks);
                CHECK(live_bytes == bytes);
            }
            CHECK(transfers == 18);
            CHECK(iterated_keys(map) == keys);
            check_consistent(map, counts, 64);
        }
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE("merge between maps with holes") {
    auto counts = counter{};
    {
        auto target = reserved_map(100);
        for (auto const n : concat({range(0, 10), range(64, 74)})) {
            insert(target, counts, n);
        }
        // holes at the buckets 3 and 67 of the target
        CHECK(target.erase(key(counts, 3)) == 1);
        CHECK(target.erase(key(counts, 67)) == 1);
        auto source = reserved_map(100);
        for (std::size_t const n : {3, 4, 5, 6, 67, 70, 100}) {
            insert(source, counts, n);
        }
        // holes at the buckets 4 and 6 of the source, between elements that stay
        CHECK(source.erase(key(counts, 4)) == 1);
        CHECK(source.erase(key(counts, 6)) == 1);

        // 3 and 67 fill the holes of the target without an allocation, 100 needs one, 5 and 70 stay in the source
        auto const allocations = nb_allocations;
        {
            auto const bomb = bomb_after{1};
            CHECK_NOTHROW(target.merge(source));
        }
        CHECK(nb_allocations == allocations + 1);
        CHECK(iterated_keys(target) == concat({range(0, 10), range(64, 74), {100}}));
        CHECK(iterated_keys(source) == std::vector<std::size_t>{5, 70});
        check_consistent(target, counts, 128);
        check_consistent(source, counts, 128);

        // the other direction: the first element of each group (0 and 64) compacts the holes of the source, the other
        // elements go into empty buckets and into the former holes (3, 4, 6, 67 and 100) with copying insertions
        source.merge(target);
        CHECK(iterated_keys(source) == concat({range(0, 10), range(64, 74), {100}}));
        CHECK(iterated_keys(target) == std::vector<std::size_t>{5, 70});
        check_consistent(source, counts, 128);
        check_consistent(target, counts, 128);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

// `nb_deleted_buckets_` decides when the clean-up rehash runs. `rehash(bucket_count())` returns at once if and only
// if it is 0, so a rehash under `bomb_after{0}` shows that an insertion took the deleted mark of its bucket with it.
TEST_CASE("an insertion into a hole or a former hole removes its deleted mark") {
    auto counts = counter{};
    {
        auto map = reserved_map(100);
        for (std::size_t n = 0; n < 10; ++n) {
            insert(map, counts, n);
        }
        auto const nb_buckets = map.bucket_count();

        // a hole, refilled in place
        CHECK(map.erase(key(counts, 3)) == 1);
        insert(map, counts, 3);
        {
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(map.rehash(nb_buckets));
        }

        // a hole that a compaction turns into a deleted bucket without a slot, refilled by a copying insertion
        CHECK(map.erase(key(counts, 3)) == 1);
        insert(map, counts, 20);
        insert(map, counts, 3);
        {
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(map.rehash(nb_buckets));
        }

        // a group that freed its storage (group 1 with its only element erased), refilled
        insert(map, counts, 64);
        CHECK(map.erase(key(counts, 64)) == 1);
        insert(map, counts, 64);
        {
            auto const bomb = bomb_after{0};
            CHECK_NOTHROW(map.rehash(nb_buckets));
        }
        check_consistent(map, counts, 128);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}
