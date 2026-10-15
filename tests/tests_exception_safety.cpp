#include "fixtures/allocators.hpp"
#include "fixtures/checksum.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"
#include "fixtures/throwing_hash.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <memory>
#include <new>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * What a map holds after an allocation or the hash function failed in the middle of an operation. The allocations
 * fail through `tests::bombing_allocator`, the hash function through `throwing_test_hash`. The elements are
 * `counter::obj`, so a test sees every element that is not destroyed, or destroyed twice. The maps that allocate
 * through `tests::bombing_allocator` are `tests::throwing_map`, so a failed allocation throws `std::bad_alloc`.
 *
 * A rehash moves the elements and frees each old group right after its elements are moved, so the map is empty if
 * it throws while it moves. `moving_map` holds `counter::obj` and `trivial_map` holds `std::size_t`.
 *
 * Partly ported from the growth and assignment exception safety tests of ankerl::unordered_dense
 * (MIT license).
 */
namespace {
    using namespace dice::sparse_map;
    using namespace dice::sparse_map::tests;

    /**
     * `test_hash` of the key. The overload for `counter::obj` also takes the types derived from it, which
     * `std::hash` is not specialized for.
     */
    struct inner_test_hash {
        [[nodiscard]] std::size_t operator()(counter::obj const &key) const noexcept {
            return test_hash<counter::obj>{}(key);
        }

        [[nodiscard]] std::size_t operator()(std::size_t key) const noexcept {
            return test_hash<std::size_t>{}(key);
        }
    };

    /**
     * Hash function that throws `std::runtime_error` on the n-th call after a `hash_bomb_after(n)`. Otherwise it
     * returns the hash of `test_hash`.
     */
    using throwing_test_hash = throwing_hash<inner_test_hash>;
    using hash_bomb_after = throwing_test_hash::bomb_after;

    template<sh::sparsity Sparsity, typename Hash = test_hash<counter::obj>>
    using moving_map = throwing_map<
        counter::obj,
        counter::obj,
        Hash,
        std::equal_to<counter::obj>,
        leak_checking_allocator<std::pair<counter::obj, counter::obj>>,
        Sparsity
    >;

    template<typename Hash = test_hash<std::size_t>>
    using trivial_map = throwing_map<
        std::size_t,
        std::size_t,
        Hash,
        std::equal_to<std::size_t>,
        leak_checking_allocator<std::pair<std::size_t, std::size_t>>
    >;

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

    /// the key or mapped value `n` of the type `T`
    template<typename T>
    T make(counter &counts, std::size_t n) {
        if constexpr (std::is_same_v<T, std::size_t>) {
            return n;
        } else {
            return T{n, counts};
        }
    }

    template<typename T>
    std::size_t number(T const &value) {
        if constexpr (std::is_same_v<T, std::size_t>) {
            return value;
        } else {
            return value.get();
        }
    }

    /// inserts `key` mapped to itself
    template<typename Map>
    void insert_key(Map &map, counter &counts, std::size_t key) {
        map.try_emplace(make<typename Map::key_type>(counts, key), make<typename Map::mapped_type>(counts, key));
    }

    template<typename Map>
    bool contains_key(Map const &map, counter &counts, std::size_t key) {
        return map.contains(make<typename Map::key_type>(counts, key));
    }

    /// true if the next insert of a new key rehashes `map`
    template<typename Map>
    bool next_insert_rehashes(Map const &map) {
        return map.size() >= static_cast<std::size_t>(static_cast<float>(map.bucket_count()) * map.max_load_factor());
    }

    /// inserts the keys 0, 1, 2 and so on, until `map` holds at least `min_size` elements and the next insert of a new key rehashes it
    template<typename Map>
    void fill_until_next_insert_rehashes(Map &map, counter &counts, std::size_t min_size) {
        std::size_t key = 0;
        while (map.size() < min_size || !next_insert_rehashes(map)) {
            insert_key(map, counts, key);
            ++key;
        }
    }

    /**
     * Checks that `map` agrees with itself: each key below `searched_up_to` that `find` reaches is
     * mapped to itself, and `size` counts exactly the elements that `find` reaches and that iteration
     * visits.
     */
    template<typename Map>
    void check_consistent(Map const &map, counter &counts, std::size_t searched_up_to) {
        std::size_t found = 0;
        bool values_right = true;
        for (std::size_t key = 0; key < searched_up_to; ++key) {
            auto const it = map.find(make<typename Map::key_type>(counts, key));
            if (it != map.end()) {
                values_right = values_right && number(it->second) == key;
                ++found;
            }
        }
        std::size_t iterated = 0;
        bool iterated_found = true;
        for (auto const &entry : map) {
            iterated_found = iterated_found && map.find(entry.first) != map.end();
            ++iterated;
        }
        CHECK(values_right);
        CHECK(iterated_found);
        CHECK(found == map.size());
        CHECK(iterated == map.size());
    }

    /// inserts a key that `map` does not hold, and checks that the map holds it afterwards
    template<typename Map>
    void check_insert_works(Map &map, counter &counts, std::size_t new_key) {
        auto const size_before = map.size();
        insert_key(map, counts, new_key);
        CHECK(map.size() == size_before + 1);
        CHECK(contains_key(map, counts, new_key));
    }

    /// upper bound for the allocation budgets a test walks through, only a guard against an endless loop
    constexpr int max_budget = 100000;

} // namespace

TYPE_TO_STRING_AS("moving, high", moving_map<sh::sparsity::high>);
TYPE_TO_STRING_AS("moving, medium", moving_map<sh::sparsity::medium>);
TYPE_TO_STRING_AS("moving, low", moving_map<sh::sparsity::low>);
TYPE_TO_STRING_AS("trivial", trivial_map<>);
TYPE_TO_STRING_AS("moving, throwing hash", moving_map<sh::sparsity::medium, throwing_test_hash>);
TYPE_TO_STRING_AS("trivial, throwing hash", trivial_map<throwing_test_hash>);

// A rehash that moves frees each old group right after its elements are moved, so it leaves the map empty if an
// allocation fails while it moves. Any other failed allocation leaves the contents unchanged.
TEST_CASE_TEMPLATE(
    "an insert that rehashes and throws leaves the contents unchanged or empty",
    map_t,
    moving_map<sh::sparsity::high>,
    moving_map<sh::sparsity::medium>,
    moving_map<sh::sparsity::low>,
    trivial_map<>
) {
    auto counts = counter{};
    std::size_t unchanged = 0;
    std::size_t emptied = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        {
            auto map = map_t{};
            fill_until_next_insert_rehashes(map, counts, 200);
            auto const new_key = map.size();
            auto const size_before = map.size();
            auto const checksum_before = checksum::map(map);
            auto const bucket_count_before = map.bucket_count();
            try {
                auto const bomb = bomb_after{budget};
                insert_key(map, counts, new_key);
                completed = true;
            } catch (std::bad_alloc const &) {
                CHECK_FALSE(contains_key(map, counts, new_key));
                if (map.size() == size_before && checksum::map(map) == checksum_before) {
                    ++unchanged;
                } else {
                    CHECK(map.empty());
                    ++emptied;
                }
            }
            if (completed) {
                CHECK(map.bucket_count() > bucket_count_before);
                CHECK(map.size() == size_before + 1);
            }
            check_consistent(map, counts, new_key + 2);
            check_insert_works(map, counts, new_key + 1);
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(unchanged > 0);
    CHECK(emptied > 0);
    CHECK(completed);
}

TEST_CASE_TEMPLATE(
    "a reserve that throws leaves the contents unchanged or empty",
    map_t,
    moving_map<sh::sparsity::high>,
    moving_map<sh::sparsity::medium>,
    moving_map<sh::sparsity::low>,
    trivial_map<>
) {
    auto counts = counter{};
    std::size_t unchanged = 0;
    std::size_t emptied = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        {
            auto map = map_t{};
            for (std::size_t key = 0; key < 200; ++key) {
                insert_key(map, counts, key);
            }
            auto const checksum_before = checksum::map(map);
            auto const bucket_count_before = map.bucket_count();
            try {
                auto const bomb = bomb_after{budget};
                map.reserve(5000);
                completed = true;
            } catch (std::bad_alloc const &) {
                CHECK(map.bucket_count() == bucket_count_before);
                if (map.size() == 200 && checksum::map(map) == checksum_before) {
                    ++unchanged;
                } else {
                    CHECK(map.empty());
                    ++emptied;
                }
            }
            if (completed) {
                CHECK(map.bucket_count() > bucket_count_before);
                CHECK(map.size() == 200);
                CHECK(checksum::map(map) == checksum_before);
            }
            check_consistent(map, counts, 202);
            check_insert_works(map, counts, 200);
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(unchanged > 0);
    CHECK(emptied > 0);
    CHECK(completed);
}

// A rehash hashes the elements while it moves them, so it leaves the map empty whenever the hash function throws.
TEST_CASE_TEMPLATE(
    "a rehash whose hash function throws leaves the contents unchanged or empty",
    map_t,
    moving_map<sh::sparsity::medium, throwing_test_hash>,
    trivial_map<throwing_test_hash>
) {
    auto counts = counter{};
    std::size_t unchanged = 0;
    std::size_t emptied = 0;
    bool completed = false;
    for (int calls = 0; calls < max_budget && !completed; ++calls) {
        {
            auto map = map_t{};
            for (std::size_t key = 0; key < 200; ++key) {
                insert_key(map, counts, key);
            }
            auto const checksum_before = checksum::map(map);
            auto const bucket_count_before = map.bucket_count();
            try {
                auto const bomb = hash_bomb_after{calls};
                map.rehash(bucket_count_before * 2);
                completed = true;
                CHECK(map.size() == 200);
                CHECK(checksum::map(map) == checksum_before);
            } catch (std::runtime_error const &) {
                if (map.size() == 200 && checksum::map(map) == checksum_before) {
                    ++unchanged;
                } else {
                    CHECK(map.empty());
                    ++emptied;
                }
            }
            check_consistent(map, counts, 202);
            check_insert_works(map, counts, 200);
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(completed);
    CHECK(unchanged == 0);
    CHECK(emptied > 0);
}

// A rehash frees each old group right after its elements are moved, so it needs less memory than the new table on
// top of the old one.
TEST_CASE_TEMPLATE("a rehash frees the old groups while it moves", map_t, moving_map<sh::sparsity::medium>, trivial_map<>) {
    auto counts = counter{};
    {
        auto map = map_t{};
        for (std::size_t key = 0; key < 10000; ++key) {
            insert_key(map, counts, key);
        }
        auto const bytes_before = live_bytes;
        peak_bytes = live_bytes;
        map.rehash(map.bucket_count() * 2);
        auto const bytes_after = live_bytes;
        auto const extra_bytes = peak_bytes - bytes_before;
        CAPTURE(bytes_before);
        CAPTURE(bytes_after);
        CAPTURE(extra_bytes);
        CHECK(extra_bytes < bytes_after / 2);
        CHECK(map.size() == 10000);
        check_consistent(map, counts, 10000);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE_TEMPLATE(
    "an insert that throws without a rehash leaves the other elements alone",
    map_t,
    moving_map<sh::sparsity::high>,
    moving_map<sh::sparsity::medium>,
    moving_map<sh::sparsity::low>
) {
    auto counts = counter{};
    std::size_t failures = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        {
            auto map = map_t{};
            map.reserve(1000);
            auto const bucket_count = map.bucket_count();
            for (std::size_t key = 0; key < 100; ++key) {
                insert_key(map, counts, key);
            }
            std::size_t inserted = 0;
            try {
                auto const bomb = bomb_after{budget};
                for (std::size_t key = 100; key < 300; ++key) {
                    insert_key(map, counts, key);
                    ++inserted;
                }
                completed = true;
            } catch (std::bad_alloc const &) {
                ++failures;
                CHECK_FALSE(contains_key(map, counts, 100 + inserted));
            }
            CHECK(map.bucket_count() == bucket_count);
            CHECK(map.size() == 100 + inserted);
            bool all_found = true;
            for (std::size_t key = 0; key < 100 + inserted; ++key) {
                all_found = all_found && contains_key(map, counts, key);
            }
            CHECK(all_found);
            check_consistent(map, counts, 302);
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(failures > 0);
    CHECK(completed);
}

TEST_CASE_TEMPLATE(
    "a copy assignment that throws leaks nothing",
    map_t,
    moving_map<sh::sparsity::high>,
    moving_map<sh::sparsity::medium>,
    moving_map<sh::sparsity::low>
) {
    auto counts = counter{};
    {
        auto source = map_t{};
        for (std::size_t key = 0; key < 200; ++key) {
            insert_key(source, counts, key);
        }
        auto const source_checksum = checksum::map(source);
        auto const source_blocks = live_blocks;
        auto const source_alive = alive(counts);

        // targets with fewer, as many and no bucket groups as the source
        for (std::size_t const target_size : {std::size_t{50}, std::size_t{200}, std::size_t{0}}) {
            CAPTURE(target_size);
            std::size_t failures = 0;
            bool completed = false;
            for (int budget = 0; budget < max_budget && !completed; ++budget) {
                {
                    auto target = map_t{};
                    for (std::size_t key = 1000; key < 1000 + target_size; ++key) {
                        insert_key(target, counts, key);
                    }
                    try {
                        auto const bomb = bomb_after{budget};
                        target = source;
                        completed = true;
                        CHECK(target.size() == source.size());
                    } catch (std::bad_alloc const &) {
                        ++failures;
                    }
                }
                CHECK(live_blocks == source_blocks);
                CHECK(alive(counts) == source_alive);
            }
            CHECK(failures > 0);
            CHECK(completed);
        }
        CHECK(checksum::map(source) == source_checksum);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE_TEMPLATE(
    "a copy assignment that throws leaves a usable map",
    map_t,
    moving_map<sh::sparsity::high>,
    moving_map<sh::sparsity::medium>,
    moving_map<sh::sparsity::low>
) {
    auto counts = counter{};
    {
        auto source = map_t{};
        for (std::size_t key = 0; key < 200; ++key) {
            insert_key(source, counts, key);
        }

        for (std::size_t const target_size : {std::size_t{50}, std::size_t{200}, std::size_t{0}}) {
            CAPTURE(target_size);
            std::size_t failures = 0;
            bool completed = false;
            for (int budget = 0; budget < max_budget && !completed; ++budget) {
                auto target = map_t{};
                for (std::size_t key = 1000; key < 1000 + target_size; ++key) {
                    insert_key(target, counts, key);
                }
                try {
                    auto const bomb = bomb_after{budget};
                    target = source;
                    completed = true;
                } catch (std::bad_alloc const &) {
                    ++failures;
                }
                check_consistent(target, counts, 1000 + target_size);
                check_insert_works(target, counts, 5000);
                check_consistent(target, counts, 5001);
            }
            CHECK(failures > 0);
            CHECK(completed);
        }
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

namespace {

    /**
     * A hash whose move assignment throws while `fragile_test_hash::moves_throw` is true. `std::swap` of two of
     * them move assigns, so it throws too. A call cannot throw and returns the hash of `test_hash`.
     */
    using fragile_test_hash = throwing_hash<inner_test_hash, false>;

    /// `map` holds the keys `first` to `first + count - 1`, each mapped to itself, and nothing else
    template<typename Map>
    void check_holds(Map const &map, std::size_t first, std::size_t count) {
        CHECK(map.size() == count);
        std::size_t found = 0;
        for (std::size_t key = first; key < first + count; ++key) {
            auto const it = map.find(key);
            found += (it != map.end() && it->second == key) ? 1 : 0;
        }
        CHECK(found == count);
        CHECK(static_cast<std::size_t>(std::distance(map.begin(), map.end())) == count);
    }
} // namespace

TEST_CASE("a rehash does not swap the hash function") {
    using map_t = sparse_map<std::size_t, std::string, fragile_test_hash>;
    auto map = map_t{};
    for (std::size_t key = 0; key < 100; ++key) {
        map.try_emplace(key, std::to_string(key));
    }

    fragile_test_hash::moves_throw = true;
    CHECK_NOTHROW(map.rehash(map.bucket_count() * 2));
    fragile_test_hash::moves_throw = false;

    CHECK(map.size() == 100);
    bool all_found = true;
    for (std::size_t key = 0; key < 100; ++key) {
        auto const it = map.find(key);
        all_found = all_found && it != map.end() && it->second == std::to_string(key);
    }
    CHECK(all_found);
}

TEST_CASE("a move assignment whose hash throws leaves both maps usable") {
    using map_t = sparse_map<std::size_t, std::size_t, fragile_test_hash>;
    auto source = map_t{};
    for (std::size_t key = 0; key < 100; ++key) {
        source[key] = key;
    }
    auto target = map_t{};
    for (std::size_t key = 1000; key < 1010; ++key) {
        target[key] = key;
    }

    fragile_test_hash::moves_throw = true;
    CHECK_THROWS_AS(target = std::move(source), std::runtime_error);
    fragile_test_hash::moves_throw = false;

    check_holds(target, 0, 0);
    check_holds(source, 0, 100); // NOLINT(bugprone-use-after-move)
    target[5000] = 5000;
    source[5000] = 5000; // NOLINT(bugprone-use-after-move)
    check_holds(target, 5000, 1);
    CHECK(source.size() == 101);
}

TEST_CASE("a swap whose hash throws swaps neither the elements nor the allocators") {
    using allocator_t = pocs_allocator<std::pair<std::size_t, std::size_t>>;
    using map_t = sparse_map<std::size_t, std::size_t, fragile_test_hash, std::equal_to<std::size_t>, allocator_t>;
    auto counts_1 = alloc_counts{};
    auto counts_2 = alloc_counts{};
    {
        auto map_1 = map_t{allocator_t{1, &counts_1}};
        auto map_2 = map_t{allocator_t{2, &counts_2}};
        for (std::size_t key = 0; key < 100; ++key) {
            map_1[key] = key;
            map_2[key + 1000] = key + 1000;
        }

        fragile_test_hash::moves_throw = true;
        CHECK_THROWS_AS(map_1.swap(map_2), std::runtime_error);
        fragile_test_hash::moves_throw = false;

        CHECK(map_1.get_allocator().id == 1);
        CHECK(map_2.get_allocator().id == 2);
        check_holds(map_1, 0, 100);
        check_holds(map_2, 1000, 100);
    }
    // each allocator got back exactly the blocks it handed out
    CHECK(counts_1.allocations == counts_1.deallocations);
    CHECK(counts_2.allocations == counts_2.deallocations);
}

namespace {
    /// a string that does not fit the small string buffer, so that a moved-from copy is empty
    std::string long_value(std::size_t key) {
        return std::string(40, 'x') + std::to_string(key);
    }

    template<sh::sparsity Sparsity>
    using bombing_string_map = throwing_map<
        std::size_t,
        std::string,
        test_hash<std::size_t>,
        std::equal_to<std::size_t>,
        leak_checking_allocator<std::pair<std::size_t, std::string>>,
        Sparsity
    >;

    /// the next insertion of a new key into `map` rehashes it
    template<typename Map>
    void fill_strings_until_next_insert_rehashes(Map &map, std::size_t min_size) {
        std::size_t key = 0;
        while (map.size() < min_size || !next_insert_rehashes(map)) {
            map.try_emplace(key, long_value(key));
            ++key;
        }
    }

} // namespace

// The rehash of the target moves the strings, so an allocation that fails while it moves leaves the target empty.
// A failed allocation of the new buckets, or of the new element after the rehash, leaves the target unchanged. The
// element stays in the source in both cases.
TEST_CASE_TEMPLATE(
    "a merge that throws in the rehash of the target leaves the element in the source",
    map_t,
    bombing_string_map<sh::sparsity::high>,
    bombing_string_map<sh::sparsity::medium>,
    bombing_string_map<sh::sparsity::low>
) {
    std::size_t unchanged = 0;
    std::size_t emptied = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        {
            auto target = map_t{};
            fill_strings_until_next_insert_rehashes(target, 200);
            auto const new_key = target.size();
            auto const target_size = target.size();
            auto source = map_t{};
            source.try_emplace(new_key, long_value(new_key));
            try {
                auto const bomb = bomb_after{budget};
                target.merge(source);
                completed = true;
            } catch (std::bad_alloc const &) {
                CHECK(static_cast<std::size_t>(std::distance(target.begin(), target.end())) == target.size());
                CHECK_FALSE(target.contains(new_key));
                if (target.size() == target_size) {
                    ++unchanged;
                } else {
                    CHECK(target.empty());
                    ++emptied;
                }
            }
            if (completed) {
                CHECK(source.empty());
                CHECK(target.at(new_key) == long_value(new_key));
            } else {
                REQUIRE(source.size() == 1);
                CHECK(source.at(new_key) == long_value(new_key));
            }
        }
        CHECK(live_blocks == 0);
    }
    CHECK(unchanged > 0);
    CHECK(emptied > 0);
    CHECK(completed);
}

// The clean-up rehash of the target moves the strings, so an allocation that fails while it moves leaves the target
// empty. A failed allocation of the new buckets leaves the target unchanged. The element stays in the source in
// both cases.
TEST_CASE_TEMPLATE(
    "a merge that throws in the clean-up rehash of the target leaves the element in the source",
    map_t,
    bombing_string_map<sh::sparsity::high>,
    bombing_string_map<sh::sparsity::medium>,
    bombing_string_map<sh::sparsity::low>
) {
    std::size_t unchanged = 0;
    std::size_t emptied = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        {
            // 64 buckets, at max load factor 0.8 the table takes 51 elements without growing
            auto target = map_t{};
            target.rehash(64);
            target.max_load_factor(0.8f);
            for (std::size_t key = 0; key < 51; ++key) {
                target.try_emplace(key, long_value(key));
            }
            REQUIRE(target.bucket_count() == 64);
            // 5 elements and 46 deleted buckets: below the growth threshold of 6, above the clean-up threshold of
            // 35, so the next insertion of a new key removes the deleted buckets with a rehash
            for (std::size_t key = 5; key < 51; ++key) {
                target.erase(key);
            }
            target.max_load_factor(0.1f);

            auto source = map_t{};
            source.try_emplace(1000, long_value(1000));
            try {
                auto const bomb = bomb_after{budget};
                target.merge(source);
                completed = true;
            } catch (std::bad_alloc const &) {
                CHECK_FALSE(target.contains(1000));
                if (target.size() == 5) {
                    ++unchanged;
                } else {
                    CHECK(target.empty());
                    ++emptied;
                }
            }
            if (completed) {
                CHECK(source.empty());
                CHECK(target.at(1000) == long_value(1000));
                CHECK(target.bucket_count() == 64);
            } else {
                REQUIRE(source.size() == 1);
                CHECK(source.at(1000) == long_value(1000));
            }
        }
        CHECK(live_blocks == 0);
    }
    CHECK(unchanged > 0);
    CHECK(emptied > 0);
    CHECK(completed);
}

namespace {
    /**
     * Allocator with an id, like `id_allocator`, that also throws `std::bad_alloc` on the n-th allocation after a
     * `bomb_after(n)` and counts the blocks it hands out. Instances with different ids compare unequal and nothing
     * propagates, so a move between two of them moves the elements one by one.
     */
    template<typename T>
    struct bombing_id_allocator {
        using value_type = T;
        using propagate_on_container_copy_assignment = std::false_type;
        using propagate_on_container_move_assignment = std::false_type;
        using propagate_on_container_swap = std::false_type;
        using is_always_equal = std::false_type;

        int id = 0;
        alloc_counts *counts = nullptr;

        bombing_id_allocator() noexcept = default;

        explicit bombing_id_allocator(int id, alloc_counts *counts = nullptr) noexcept : id(id), counts(counts) {}

        template<typename U>
        bombing_id_allocator(bombing_id_allocator<U> const &other) noexcept // NOLINT(google-explicit-constructor)
            :
            id(other.id),
            counts(other.counts) {}

        template<typename U>
        struct rebind {
            using other = bombing_id_allocator<U>;
        };

        T *allocate(std::size_t n) {
            T *const p = bombing_allocator<T>{}.allocate(n);
            if (counts != nullptr && n != 0) {
                ++counts->allocations;
            }
            return p;
        }

        void deallocate(T *p, std::size_t n) noexcept {
            if (counts != nullptr && n != 0) {
                ++counts->deallocations;
            }
            bombing_allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(bombing_id_allocator const &lhs, bombing_id_allocator const &rhs) noexcept {
            return lhs.id == rhs.id;
        }
    };

    using unequal_alloc = bombing_id_allocator<std::pair<std::size_t, std::size_t>>;

    /// a map whose allocator has an id, so that two of them move the elements one by one
    using unequal_map = throwing_map<std::size_t, std::size_t, test_hash<std::size_t>, std::equal_to<std::size_t>, unequal_alloc>;

    /// the same with a hash whose move assignment throws
    using fragile_unequal_map = throwing_map<std::size_t, std::size_t, fragile_test_hash, std::equal_to<std::size_t>, unequal_alloc>;

    /// the copy assignment of `copy_fragile_hash` throws while this is true
    bool hash_copies_throw = false; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * A hash with state whose copy assignment throws while `hash_copies_throw` is true. The state goes into the
     * hash value, so a moved-from one hashes differently and no longer finds the elements of its table.
     */
    struct copy_fragile_hash {
        using is_avalanching = void;

        std::vector<std::size_t> seed = std::vector<std::size_t>(3);

        copy_fragile_hash() = default;
        copy_fragile_hash(copy_fragile_hash const &) = default;
        copy_fragile_hash(copy_fragile_hash &&) = default;
        copy_fragile_hash &operator=(copy_fragile_hash &&) = default;
        ~copy_fragile_hash() = default;

        copy_fragile_hash &operator=(copy_fragile_hash const & /*other*/) {
            if (hash_copies_throw) {
                throw std::runtime_error("copy_fragile_hash copy assignment");
            }
            return *this;
        }

        std::size_t operator()(std::size_t key) const noexcept {
            return test_hash<std::size_t>{}(key + seed.size());
        }
    };

    using copy_fragile_map = throwing_map<std::size_t, std::size_t, copy_fragile_hash, std::equal_to<std::size_t>, unequal_alloc>;
} // namespace

// The move assignment to an unequal allocator moves the elements one by one into new groups. An allocation that
// fails while they move leaves the target empty and the source with all of its elements, because the target takes
// the groups of the source only after every element was moved.
TEST_CASE("a move assignment to an unequal allocator that fails to allocate leaves the target empty") {
    auto counts = alloc_counts{};
    std::size_t emptied = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        CAPTURE(budget);
        {
            auto source = unequal_map{unequal_alloc{1, &counts}};
            for (std::size_t key = 0; key < 200; ++key) {
                source[key] = key;
            }
            auto target = unequal_map{unequal_alloc{2, &counts}};
            target[5000] = 5000;

            try {
                auto const bomb = bomb_after{budget};
                target = std::move(source);
                completed = true;
            } catch (std::bad_alloc const &) {
                CHECK(target.empty());
                check_holds(source, 0, 200); // NOLINT(bugprone-use-after-move)
                ++emptied;
            }
            if (completed) {
                check_holds(target, 0, 200);
                CHECK(source.empty()); // NOLINT(bugprone-use-after-move)
            }
        }
        CHECK(counts.allocations == counts.deallocations);
    }
    CHECK(emptied > 0);
    CHECK(completed);
}

// The move assignment moves the functors after the elements, so that the source keeps its own while they move. If a
// functor move throws, the target gives the groups it built back and stays empty.
TEST_CASE("a move assignment to an unequal allocator whose hash throws leaves the target empty") {
    auto counts = alloc_counts{};
    {
        auto source = fragile_unequal_map{unequal_alloc{1, &counts}};
        for (std::size_t key = 0; key < 200; ++key) {
            source[key] = key;
        }
        auto target = fragile_unequal_map{unequal_alloc{2, &counts}};
        target[5000] = 5000;

        {
            auto const fragile = fragile_test_hash::fragile_moves{};
            CHECK_THROWS_AS(target = std::move(source), std::runtime_error);
        }

        CHECK(target.empty());
        check_holds(source, 0, 200); // NOLINT(bugprone-use-after-move)
    }
    CHECK(counts.allocations == counts.deallocations);
}

// The allocator-extended move constructor moves the elements one by one into new groups as well. The source keeps
// its own functors, so it still finds its elements if an allocation fails while they move.
TEST_CASE("a move construction with an unequal allocator that fails to allocate leaves the source usable") {
    auto counts = alloc_counts{};
    std::size_t failures = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        CAPTURE(budget);
        {
            auto source = unequal_map{unequal_alloc{1, &counts}};
            for (std::size_t key = 0; key < 200; ++key) {
                source[key] = key;
            }

            try {
                auto const bomb = bomb_after{budget};
                auto const moved = unequal_map{std::move(source), unequal_alloc{2, &counts}};
                completed = true;
                check_holds(moved, 0, 200);
                CHECK(source.empty()); // NOLINT(bugprone-use-after-move)
            } catch (std::bad_alloc const &) {
                check_holds(source, 0, 200); // NOLINT(bugprone-use-after-move)
                ++failures;
            }
        }
        CHECK(counts.allocations == counts.deallocations);
    }
    CHECK(failures > 0);
    CHECK(completed);
}

// The constructor copies the functors of the source on the path with the unequal allocator instead of moving them
// out and giving copies back, so it never copy assigns a functor of the source.
TEST_CASE("a move construction with an unequal allocator does not copy assign the functors of the source") {
    auto counts = alloc_counts{};
    {
        auto source = copy_fragile_map{unequal_alloc{1, &counts}};
        for (std::size_t key = 0; key < 100; ++key) {
            source[key] = key;
        }

        hash_copies_throw = true;
        auto const moved = copy_fragile_map{std::move(source), unequal_alloc{2, &counts}};
        hash_copies_throw = false;

        check_holds(moved, 0, 100);
        CHECK(source.empty()); // NOLINT(bugprone-use-after-move)
    }
    CHECK(counts.allocations == counts.deallocations);
}
