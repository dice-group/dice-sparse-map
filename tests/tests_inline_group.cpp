#include "fixtures/allocators.hpp"
#include "fixtures/test_types.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <new>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

/**
 * A map with an inline capacity keeps its first elements in the map object, as group 0 of a table of 64 buckets: a
 * bitmap of the buckets with an element, a bitmap of the deleted buckets, and the elements of the occupied buckets in
 * bucket order. These tests use a hash that puts key `k` into bucket `k % 64`, so that they know the buckets, the probe
 * chains and the order of the elements. They check the lookup with probing, the erasure, which leaves a deleted bucket
 * while an element can be outside its home bucket, the reuse of deleted buckets, the clean-up, the move of the elements
 * into an allocated group, copies, moves and swaps, `merge`, and the limits.
 */
namespace {
    using namespace dice::sparse_map;
    using namespace dice::sparse_map::tests;

    /// calls of `counting_bucket_hash` since the last reset
    std::size_t hash_calls = 0; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// calls of `counting_bucket_hash` that are left before it throws, -1 never throws
    int hash_calls_until_throw = -1; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * Arms the countdown of `counting_bucket_hash` while it is in scope.
     */
    struct hash_bomb_after {
        explicit hash_bomb_after(int n) noexcept {
            hash_calls_until_throw = n;
        }
        hash_bomb_after(hash_bomb_after const &) = delete;
        hash_bomb_after(hash_bomb_after &&) = delete;
        hash_bomb_after &operator=(hash_bomb_after const &) = delete;
        hash_bomb_after &operator=(hash_bomb_after &&) = delete;
        ~hash_bomb_after() {
            hash_calls_until_throw = -1;
        }
    };

    /// puts key `k` into bucket `k % 64` of a table of 64 buckets
    struct counting_bucket_hash {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(int key) const {
            ++hash_calls;
            if (hash_calls_until_throw >= 0 && 0 == hash_calls_until_throw--) {
                throw std::runtime_error{"counting_bucket_hash"};
            }
            return static_cast<std::size_t>(key);
        }
    };

    /// eight `int -> int` elements inline. A failed allocation throws, so that a test sees it.
    template<typename Allocator = std::allocator<std::pair<int, int>>>
    using group_map = sparse_map<
        int,
        int,
        counting_bucket_hash,
        std::equal_to<int>,
        Allocator,
        sh::sparsity::medium,
        sh::allocation_failure::throwing,
        8
    >;

    using counted_group_map = group_map<counting_allocator<std::pair<int, int>>>;

    template<typename Map>
    std::vector<int> keys_of(Map const &map) {
        std::vector<int> keys;
        for (auto const &[key, value] : map) {
            keys.push_back(key);
        }
        return keys;
    }

    /// a map with `keys`, inserted in this order, each mapped to `key * 10`
    template<typename Map>
    Map make_map(std::initializer_list<int> keys) {
        Map map;
        for (int const key : keys) {
            map.try_emplace(key, key * 10);
        }
        return map;
    }

    /// true if `map` holds exactly `keys`, each found and mapped to `key * 10`
    template<typename Map>
    bool holds(Map const &map, std::vector<int> const &keys) {
        bool all_found = map.size() == keys.size();
        for (int const key : keys) {
            auto const it = map.find(key);
            all_found = all_found && it != map.end() && it->first == key && it->second == key * 10;
        }
        return all_found;
    }

    /// `1` in bucket 1, `65` and `129` with home bucket 1 in buckets 2 and 4 (1 + 3), `3` in bucket 3, `5` in bucket 5
    std::initializer_list<int> const colliding{5, 1, 65, 129, 3};

    /**
     * Inserts and erases the keys `first` to `last - 1` one after the other. If an element of `map` is outside its home
     * bucket, each erase leaves a deleted bucket in the bucket of its key, if no deleted bucket on the probe sequence of
     * the key was reused.
     */
    template<typename Map>
    void insert_and_erase(Map &map, int first, int last) {
        for (int key = first; key < last; ++key) {
            map.try_emplace(key);
            REQUIRE(map.erase(key) == 1);
        }
    }
} // namespace

TEST_CASE("the inline elements are in bucket order and are found through the bitmap and probing") {
    auto const before = num_allocations;
    auto map = make_map<counted_group_map>(colliding);
    CHECK(num_allocations == before);
    CHECK(map.bucket_count() == 64);
    CHECK(keys_of(map) == std::vector<int>{1, 65, 3, 129, 5});
    CHECK(holds(map, {1, 3, 5, 65, 129}));

    // misses: 193 probes the buckets 1, 2, 4 and 7, 2 probes 2, 3, 5 and 8, 0 finds bucket 0 empty
    CHECK_FALSE(map.contains(193));
    CHECK_FALSE(map.contains(2));
    CHECK_FALSE(map.contains(0));

    auto const [it, inserted] = map.try_emplace(129, -1);
    CHECK_FALSE(inserted);
    CHECK(it->second == 1290);
    CHECK(map.size() == 5);
}

TEST_CASE("an erasure by key leaves a deleted bucket, so the elements behind it are still found") {
    auto const before = num_allocations;
    auto map = make_map<counted_group_map>(colliding);
    CHECK(map.erase(1) == 1);
    // 65 and 129 have the home bucket 1, which is deleted now
    CHECK(holds(map, {3, 5, 65, 129}));
    CHECK(keys_of(map) == std::vector<int>{65, 3, 129, 5});
    CHECK(map.erase(1) == 0);
    CHECK(map.erase(65) == 1);
    CHECK(holds(map, {3, 5, 129}));
    CHECK(keys_of(map) == std::vector<int>{3, 129, 5});
    CHECK(num_allocations == before);
    CHECK(map.bucket_count() == 64);
}

TEST_CASE("an erasure by iterator keeps the order of the other inline elements and allocates nothing") {
    auto const before = num_allocations;
    auto map = make_map<counted_group_map>({1, 65, 129, 3});
    REQUIRE(keys_of(map) == std::vector<int>{1, 65, 3, 129});
    auto next = map.erase(map.find(65));
    REQUIRE(next != map.end());
    CHECK(next->first == 3);
    CHECK(keys_of(map) == std::vector<int>{1, 3, 129});
    CHECK(holds(map, {1, 3, 129}));

    next = map.erase(map.find(129));
    CHECK(next == map.end());
    next = map.erase(map.begin());
    REQUIRE(next != map.end());
    CHECK(next->first == 3);
    CHECK(holds(map, {3}));
    CHECK(num_allocations == before);
    CHECK(map.bucket_count() == 64);
}

TEST_CASE("an erasure of an inline element allocates nothing, also with an allocator that would throw") {
    using bombing_map = group_map<bombing_allocator<std::pair<int, int>>>;
    auto map = make_map<bombing_map>(colliding);
    {
        bomb_after const bomb(0);
        CHECK(map.erase(65) == 1);
        CHECK(map.erase(map.find(1)) != map.end());
        auto const removed = erase_if(map, [](auto const &element) { return element.first == 5; });
        CHECK(removed == 1);
    }
    CHECK(holds(map, {3, 129}));
}

TEST_CASE("an insertion reuses the first deleted bucket on its probe sequence") {
    auto map = make_map<group_map<>>(colliding);
    REQUIRE(map.erase(65) == 1); // bucket 2 is deleted
    // 193 has the home bucket 1 and probes 1, 2 (deleted), 4 and 7 (empty), so it goes into bucket 2
    map.try_emplace(193, 1930);
    CHECK(keys_of(map) == std::vector<int>{1, 193, 3, 129, 5});
    CHECK(holds(map, {1, 3, 5, 129, 193}));
    // 129 is behind the reused bucket and still found, 65 is not there
    CHECK_FALSE(map.contains(65));
}

TEST_CASE("an insertion of a key behind a deleted bucket finds the element and inserts nothing") {
    auto map = make_map<group_map<>>(colliding);
    REQUIRE(map.erase(1) == 1); // 65 and 129 are behind the deleted bucket 1
    auto const [it, inserted] = map.try_emplace(129, -1);
    CHECK_FALSE(inserted);
    CHECK(it->second == 1290);
    CHECK(map.size() == 4);
    CHECK(holds(map, {3, 5, 65, 129}));
}

// No probe sequence passes another bucket before it reaches its element if every element is in its home bucket. Then
// an erasure leaves no deleted bucket, so the inline group never reaches its clean-up threshold, which would hash the
// elements.
TEST_CASE("an erasure leaves no deleted bucket while every inline element is in its home bucket") {
    auto at_home = make_map<group_map<>>({1});
    hash_calls = 0;
    insert_and_erase(at_home, 10, 60);
    CHECK(hash_calls == 100); // one per insertion and erasure, no clean-up
    CHECK(holds(at_home, {1}));

    // 65 is outside its home bucket 1, so the erasures leave deleted buckets, and the inline group cleans up
    auto displaced = make_map<group_map<>>({1, 65});
    hash_calls = 0;
    insert_and_erase(displaced, 10, 60);
    CHECK(hash_calls > 100);
    CHECK(holds(displaced, {1, 65}));
}

TEST_CASE("an erasure by key of the last element outside its home bucket removes the deleted buckets") {
    auto map = make_map<group_map<>>({1, 65, 5}); // 65 in bucket 2
    REQUIRE(map.erase(1) == 1); // bucket 1 is deleted, 65 is behind it
    CHECK(holds(map, {5, 65}));
    REQUIRE(map.erase(65) == 1); // no element is outside its home bucket now, and 5 is left
    hash_calls = 0;
    insert_and_erase(map, 10, 60);
    CHECK(hash_calls == 100); // no deleted bucket, so no clean-up
    CHECK(holds(map, {5}));
}

// An erasure by iterator does not know the home bucket of the element, so it keeps the deleted buckets while an element
// can be outside its home bucket.
TEST_CASE("an erasure by iterator keeps the deleted buckets while an element can be outside its home bucket") {
    auto map = make_map<group_map<>>({1, 65, 129}); // 65 in bucket 2, 129 in bucket 4
    map.erase(map.find(1));
    CHECK(holds(map, {65, 129}));
    map.erase(map.find(65)); // 129 is behind the deleted buckets 1 and 2
    CHECK(holds(map, {129}));
    CHECK(map.erase(129) == 1);
    CHECK(map.empty());
}

// Each insertion of 129 into bucket 4 counts an element outside its home bucket, and the erasure by iterator cannot
// count it out. The counter stays at most at the number of elements, so it never wraps to 0 while 65 is outside its
// home bucket (255 such insertions would wrap a counter of 8 bits from 1 to 0).
TEST_CASE("the counter of elements outside their home bucket stays at most at the number of elements") {
    auto map = make_map<group_map<>>({1, 65}); // 65 in bucket 2
    for (int i = 0; i < 255; ++i) {
        map.try_emplace(129, 1290); // home bucket 1, in bucket 4
        map.erase(map.find(129));
    }
    REQUIRE(map.erase(1) == 1); // 65 is behind the deleted bucket 1
    CHECK(holds(map, {65}));
}

// A moved-from map whose elements were in buckets is an empty inline map. Its counter of elements outside their home
// bucket starts at 0, so its erasures leave no deleted bucket while every element is in its home bucket.
TEST_CASE("a map that enters its inline group again counts no element outside its home bucket") {
    auto map = make_map<group_map<>>({1, 65, 2, 3, 4, 5, 6, 7, 8}); // 9 elements, so they are in buckets
    auto const moved = std::move(map);
    CHECK(holds(moved, {1, 2, 3, 4, 5, 6, 7, 8, 65}));
    REQUIRE(map.empty()); // NOLINT(bugprone-use-after-move)
    map.try_emplace(1, 10);
    hash_calls = 0;
    insert_and_erase(map, 10, 60);
    CHECK(hash_calls == 100); // one per insertion and erasure, no clean-up
    CHECK(holds(map, {1}));
}

// The clean-up threshold of the inline group is twice its inline capacity (16 for 8), at most the threshold of 64
// buckets. At the threshold, the next insertion of a new element hashes the elements for the clean-up.
TEST_CASE("an inline group cleans up when its elements and deleted buckets reach twice its capacity") {
    auto map = make_map<group_map<>>({1, 65}); // 65 is outside its home bucket, so the erasures leave deleted buckets
    hash_calls = 0;
    insert_and_erase(map, 10, 24); // 14 deleted buckets, and two elements
    CHECK(hash_calls == 28);
    map.try_emplace(30, 300);
    CHECK(hash_calls == 28 + 1 + 2); // the new key, then the clean-up hashes 1 and 65
    CHECK(holds(map, {1, 30, 65}));
}

TEST_CASE("erase_if and the erasure of a range visit each inline element once") {
    for (int const odd : {0, 1}) {
        CAPTURE(odd);
        // 2 has home bucket 2 and goes to bucket 8 (2 + 6)
        auto map = make_map<group_map<>>({1, 65, 129, 3, 5, 2, 7, 9});
        REQUIRE(map.size() == 8);
        std::vector<int> visited;
        auto const erased = erase_if(map, [&visited, odd](auto const &element) {
            visited.push_back(element.first);
            return element.first % 2 == odd;
        });
        std::ranges::sort(visited);
        CHECK(visited == std::vector<int>{1, 2, 3, 5, 7, 9, 65, 129});
        if (odd == 1) {
            CHECK(erased == 7);
            CHECK(holds(map, {2}));
        } else {
            CHECK(erased == 1);
            CHECK(holds(map, {1, 3, 5, 7, 9, 65, 129}));
        }
    }

    auto map = make_map<group_map<>>({1, 65, 129, 3, 5});
    REQUIRE(keys_of(map) == std::vector<int>{1, 65, 3, 129, 5});
    auto const it = map.erase(std::next(map.cbegin()), std::next(map.cbegin(), 3));
    CHECK(it->first == 129);
    CHECK(keys_of(map) == std::vector<int>{1, 129, 5});
    CHECK(holds(map, {1, 5, 129}));
}

TEST_CASE("a full inline group moves into an allocated group without a rehash") {
    auto map = make_map<counted_group_map>({1, 65, 129, 3, 5, 2, 7, 9});
    REQUIRE(map.size() == 8);
    auto const order = keys_of(map);
    auto const before = num_allocations;
    hash_calls = 0;

    map.try_emplace(20, 200);

    CHECK(hash_calls == 1); // only the new key
    CHECK(num_allocations == before + 2); // the bucket array and the values array
    CHECK(map.bucket_count() == 64);
    auto keys = keys_of(map);
    std::erase(keys, 20);
    CHECK(keys == order);
    CHECK(holds(map, {1, 2, 3, 5, 7, 9, 20, 65, 129}));
    CHECK(map.begin()->first == 1);

    // the table grows as usual: 32 elements in 64 buckets, then 128 buckets
    for (int key = 21; map.size() < 32; ++key) {
        map.try_emplace(key, key * 10);
    }
    CHECK(map.bucket_count() == 64);
    map.try_emplace(1000, 10000);
    CHECK(map.bucket_count() == 128);
    CHECK(map.contains(129));
}

TEST_CASE("the move into an allocated group keeps the deleted buckets, so the elements behind them are found") {
    auto map = make_map<group_map<>>(colliding);
    REQUIRE(map.erase(1) == 1); // 65 and 129 are behind the deleted bucket 1
    for (int const key : {10, 11, 12, 13}) {
        map.try_emplace(key, key * 10);
    }
    REQUIRE(map.size() == 8);
    auto const order = keys_of(map);

    map.try_emplace(20, 200);
    REQUIRE(map.size() == 9);
    auto keys = keys_of(map);
    std::erase(keys, 20);
    CHECK(keys == order);
    CHECK(holds(map, {3, 5, 10, 11, 12, 13, 20, 65, 129}));

    // a key behind the deleted bucket is found, so it is not inserted a second time
    auto const [it, inserted] = map.try_emplace(65, -1);
    CHECK_FALSE(inserted);
    CHECK(it->second == 650);
    CHECK(map.size() == 9);
    CHECK(map.erase(129) == 1);
    CHECK_FALSE(map.contains(129));
}

TEST_CASE("an inline group without elements moves into an empty allocated group") {
    auto map = make_map<group_map<>>(colliding);
    for (int const key : colliding) {
        REQUIRE(map.erase(key) == 1);
    }
    REQUIRE(map.empty());
    CHECK(map.begin() == map.end());
    map.rehash(10); // an empty map gets the 64 buckets it asks for
    CHECK(map.bucket_count() == 64);
    CHECK(map.begin() == map.end());
    map.try_emplace(65, 650);
    CHECK(map.begin()->first == 65);
    CHECK(holds(map, {65}));
}

TEST_CASE("copies, moves and swaps keep the deleted buckets of the inline group") {
    auto original = make_map<group_map<>>(colliding);
    REQUIRE(original.erase(1) == 1); // 65 and 129 are behind the deleted bucket 1
    REQUIRE(holds(original, {3, 5, 65, 129}));

    auto copy = original;
    CHECK(keys_of(copy) == keys_of(original));
    CHECK(holds(copy, {3, 5, 65, 129}));

    auto moved = std::move(copy);
    CHECK(holds(moved, {3, 5, 65, 129}));
    CHECK(copy.empty()); // NOLINT(bugprone-use-after-move)
    CHECK(moved.erase(65) == 1);
    CHECK(holds(moved, {3, 5, 129}));

    auto assigned = make_map<group_map<>>({7});
    assigned = original;
    CHECK(holds(assigned, {3, 5, 65, 129}));

    auto move_assigned = make_map<group_map<>>({7});
    move_assigned = std::move(assigned);
    CHECK(holds(move_assigned, {3, 5, 65, 129}));

    // `second` has no deleted bucket, `first` has one
    auto first = original;
    auto second = make_map<group_map<>>({2, 6});
    first.swap(second);
    CHECK(holds(first, {2, 6}));
    CHECK(holds(second, {3, 5, 65, 129}));
    CHECK(second.erase(129) == 1);
    CHECK(holds(second, {3, 5, 65}));
    CHECK(first.erase(2) == 1);
    CHECK(holds(first, {6}));
}

TEST_CASE("a move to an unequal allocator keeps the deleted buckets and the counter of the inline group") {
    using unequal_group_map = group_map<pmr_like_allocator<std::pair<int, int>>>;
    using allocator_type = unequal_group_map::allocator_type;
    // 65 and 129 are behind the deleted bucket 1, 3 is in its home bucket
    auto const make_source = [](int id) {
        unequal_group_map map(allocator_type{id});
        for (int const key : colliding) {
            map.try_emplace(key, key * 10);
        }
        REQUIRE(map.erase(1) == 1);
        return map;
    };
    // the erasure of 3 must keep the deleted bucket 1, the erasure of 65 must keep it for 129
    auto const check_erasures = [](unequal_group_map &map) {
        CHECK(holds(map, {3, 5, 65, 129}));
        CHECK(map.erase(3) == 1);
        CHECK(holds(map, {5, 65, 129}));
        CHECK(map.erase(65) == 1);
        CHECK(holds(map, {5, 129}));
    };

    auto source = make_source(1);
    unequal_group_map assigned(allocator_type{2});
    assigned = std::move(source);
    REQUIRE(assigned.get_allocator().id == 2);
    check_erasures(assigned);

    auto constructed = unequal_group_map(make_source(3), allocator_type{4});
    REQUIRE(constructed.get_allocator().id == 4);
    check_erasures(constructed);
}

// An erase of an inline element allocates nothing, so `merge` out of an inline source with elements behind a deleted
// bucket needs no allocation of the source. The target has room inline, so nothing allocates at all.
TEST_CASE("a merge out of an inline source allocates nothing in the source") {
    using bombing_map = group_map<bombing_allocator<std::pair<int, int>>>;
    auto source = make_map<bombing_map>(colliding);
    REQUIRE(source.erase(3) == 1); // a deleted bucket in the source
    auto target = bombing_map{};
    {
        bomb_after const bomb(0);
        target.merge(source);
    }
    CHECK(source.empty());
    CHECK(holds(target, {1, 5, 65, 129}));
}

TEST_CASE("merge fills an inline group and moves it into an allocated group") {
    auto target = make_map<group_map<>>(colliding);
    auto source = make_map<group_map<>>({65, 193, 4, 6, 66});
    target.merge(source);
    CHECK(holds(target, {1, 3, 4, 5, 6, 65, 66, 129, 193}));
    CHECK(holds(source, {65}));
    CHECK(target.bucket_count() == 64);

    auto back = group_map<>{};
    back.merge(target);
    CHECK(holds(back, {1, 3, 4, 5, 6, 65, 66, 129, 193}));
    CHECK(target.empty());
}

namespace {
    using string_group_map = sparse_map<
        int,
        std::string,
        counting_bucket_hash,
        std::equal_to<int>,
        std::allocator<std::pair<int, std::string>>,
        sh::sparsity::medium,
        sh::allocation_failure::terminating,
        8
    >;
} // namespace

// 65 is outside its home bucket 1, so the erasures leave deleted buckets. The elements and the deleted buckets of the
// target reach the clean-up threshold of the inline group (16, twice the inline capacity 8), so the next insertion
// cleans up the inline group, which hashes the elements of the target. `merge` cleans up before it moves the element
// out of the source, so a hash function that throws there leaves the element in the source.
TEST_CASE("a merge into an inline group at the clean-up threshold keeps the element in the source if the hash throws") {
    auto const value = std::string(40, 'x');
    string_group_map target;
    target.try_emplace(1, "one");
    target.try_emplace(65, "sixty-five");
    insert_and_erase(target, 10, 24); // 14 deleted buckets, and two elements
    REQUIRE(target.size() == 2);

    string_group_map source;
    source.try_emplace(100, value);

    bool threw = false;
    {
        // the first call hashes the key of the source element, the second one the element of the target
        hash_bomb_after const bomb(1);
        try {
            target.merge(source);
        } catch (std::runtime_error const &) {
            threw = true;
        }
    }
    CHECK(threw);
    CHECK(source.size() == 1);
    CHECK(source.at(100) == value);
    CHECK(target.size() == 2);
    CHECK(target.at(1) == "one");

    target.merge(source);
    CHECK(source.empty());
    CHECK(target.at(100) == value);
    CHECK(target.at(1) == "one");
    CHECK(target.at(65) == "sixty-five");
}

// 65 has the home bucket 1 and lies in bucket 11, behind the deleted buckets 1, 2, 4 and 7. A clean-up inserts the
// elements again in bucket order, into a group without deleted buckets: 10 goes to bucket 10, 65 to bucket 1. So the
// order changes from 10, 65 to 65, 10. The clean-up threshold of the inline group is 16, twice the inline capacity 8.
TEST_CASE("a clean-up rebuilds the inline group in bucket order, and a hash that throws leaves the group unchanged") {
    for (int budget = 0; budget < 5; ++budget) {
        CAPTURE(budget);
        auto map = make_map<group_map<>>({1, 2, 4, 7, 10, 65});
        REQUIRE(keys_of(map) == std::vector<int>{1, 2, 4, 7, 10, 65});
        for (int const key : {1, 2, 4, 7}) {
            REQUIRE(map.erase(key) == 1);
        }
        insert_and_erase(map, 20, 30); // 14 deleted buckets, and two elements
        REQUIRE(keys_of(map) == std::vector<int>{10, 65});

        bool threw = false;
        {
            // the key 3, then the two elements of the clean-up
            hash_bomb_after const bomb(budget);
            try {
                map.try_emplace(3, 30);
            } catch (std::runtime_error const &) {
                threw = true;
            }
        }
        if (budget < 3) {
            CHECK(threw);
            CHECK(keys_of(map) == std::vector<int>{10, 65});
            CHECK(holds(map, {10, 65}));
        } else {
            CHECK_FALSE(threw);
            CHECK(keys_of(map) == std::vector<int>{65, 3, 10});
            CHECK(holds(map, {3, 10, 65}));
        }
    }
}

TEST_CASE("a lower maximum load factor moves the inline elements out earlier") {
    using set_t = sparse_set<
        std::uint16_t,
        test_hash<std::uint16_t>,
        std::equal_to<std::uint16_t>,
        counting_allocator<std::uint16_t>,
        sh::sparsity::medium,
        sh::allocation_failure::terminating,
        32
    >;
    auto set = set_t{};
    set.max_load_factor(0.1F);
    auto const before = num_allocations;
    for (std::uint16_t key = 0; key < 6; ++key) {
        set.insert(key);
        CHECK(set.load_factor() <= set.max_load_factor());
    }
    CHECK(num_allocations == before);
    set.insert(std::uint16_t{6});
    CHECK(set.load_factor() <= set.max_load_factor());
    CHECK(num_allocations > before);
    CHECK(set.size() == 7);
}

namespace {
    /// the state of a set after `reserve(1)`
    struct reserved {
        std::size_t bucket_count = 0;
        bool load_right = false;
        std::size_t allocations = 0;
        std::vector<std::uint16_t> keys;
    };

    /// a set of 32 elements with the maximum load factor 0.1, after `reserve(1)`
    template<std::size_t inline_capacity>
    reserved reserve_one_after_lower_load_factor() {
        using set_t = sparse_set<
            std::uint16_t,
            test_hash<std::uint16_t>,
            std::equal_to<std::uint16_t>,
            counting_allocator<std::uint16_t>,
            sh::sparsity::medium,
            sh::allocation_failure::terminating,
            inline_capacity
        >;
        auto set = set_t{};
        for (std::uint16_t key = 0; key < 32; ++key) {
            set.insert(key);
        }
        set.max_load_factor(0.1F);
        auto const before = num_allocations;
        set.reserve(1);
        reserved result;
        result.bucket_count = set.bucket_count();
        result.load_right = set.load_factor() <= set.max_load_factor();
        result.allocations = num_allocations - before;
        result.keys.assign(set.begin(), set.end());
        return result;
    }
} // namespace

// The inline group holds 32 elements at the default maximum load factor. After a lower maximum load factor it holds
// more than its limit, and `reserve` makes room for them as the same set type without an inline group does.
TEST_CASE("reserve after a lower maximum load factor makes room for the elements, inline or not") {
    auto const in_table = reserve_one_after_lower_load_factor<0>();
    auto const in_group = reserve_one_after_lower_load_factor<32>();
    CHECK(in_table.bucket_count == 512);
    CHECK(in_table.load_right);
    CHECK(in_group.bucket_count == 512);
    CHECK(in_group.load_right);
    CHECK(in_group.allocations == in_table.allocations);
    CHECK(in_group.keys == in_table.keys);
}

TEST_CASE("reserve keeps an empty map inline if the elements fit, and moves it out otherwise") {
    auto map = counted_group_map{};
    auto const before = num_allocations;
    map.reserve(8);
    CHECK(num_allocations == before);
    CHECK(map.bucket_count() == 0);
    for (int key = 0; key < 8; ++key) {
        map.try_emplace(key, key * 10);
    }
    CHECK(num_allocations == before);

    auto bigger = counted_group_map{};
    bigger.reserve(9);
    CHECK(num_allocations == before + 1); // the bucket array
    CHECK(bigger.bucket_count() == 64);
    hash_calls = 0;
    for (int key = 0; key < 32; ++key) {
        bigger.try_emplace(key, key * 10);
    }
    CHECK(hash_calls == 32); // no rehash
    CHECK(bigger.bucket_count() == 64);
}

TEST_CASE("reserve and rehash of an inline group with elements and deleted buckets keep it inline and clean it up") {
    auto map = make_map<counted_group_map>({1, 2, 4, 7, 10, 65});
    for (int const key : {1, 2, 4, 7}) {
        REQUIRE(map.erase(key) == 1);
    }
    REQUIRE(keys_of(map) == std::vector<int>{10, 65});
    auto const before = num_allocations;
    map.reserve(3);
    CHECK(keys_of(map) == std::vector<int>{65, 10});
    CHECK(num_allocations == before);

    auto rehashed = make_map<counted_group_map>({1, 2, 4, 7, 10, 65});
    for (int const key : {1, 2, 4, 7}) {
        REQUIRE(rehashed.erase(key) == 1);
    }
    rehashed.rehash(0);
    CHECK(keys_of(rehashed) == std::vector<int>{65, 10});
    CHECK(rehashed.bucket_count() == 64);
    CHECK(num_allocations == before);

    rehashed.rehash(200);
    CHECK(rehashed.bucket_count() == 256);
    CHECK(holds(rehashed, {10, 65}));
}
