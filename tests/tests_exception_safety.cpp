#include "fixtures/allocators.hpp"
#include "fixtures/checksum.hpp"
#include "fixtures/counter.hpp"

#include <dice/sparse-map/sparse_map.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <functional>
#include <new>
#include <utility>

/**
 * What a map holds after an allocation failed in the middle of an operation. The allocations fail
 * through `tests::bombing_allocator`. The elements are `counter::obj`, so a test sees every element
 * that is not destroyed, or destroyed twice.
 *
 * Partly ported from the growth and assignment exception safety tests of ankerl::unordered_dense
 * (MIT license).
 */
namespace {
    using namespace dice::sparse_map;
    using namespace dice::sparse_map::tests;

    /// blocks that `leak_checking_allocator` handed out and did not get back yet
    std::ptrdiff_t live_blocks = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * `bombing_allocator` that also counts the blocks it hands out and gets back, so that a test can
     * check that a failed operation leaks no memory. All instances compare equal.
     */
    template<typename T>
    struct leak_checking_allocator {
        using value_type = T;

        leak_checking_allocator() noexcept = default;

        template<typename U>
        leak_checking_allocator(leak_checking_allocator<U> const & /*other*/) noexcept {  // NOLINT(google-explicit-constructor)
        }

        T *allocate(std::size_t n) {
            T *p = bombing_allocator<T>{}.allocate(n);
            ++live_blocks;
            return p;
        }

        void deallocate(T *p, std::size_t n) noexcept {
            --live_blocks;
            bombing_allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(leak_checking_allocator const & /*lhs*/, leak_checking_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

    template<sh::exception_safety ExceptionSafety, sh::sparsity Sparsity>
    using bombing_map = sparse_map<counter::obj,
                                   counter::obj,
                                   std::hash<counter::obj>,
                                   std::equal_to<counter::obj>,
                                   leak_checking_allocator<std::pair<counter::obj, counter::obj>>,
                                   sh::power_of_two_growth_policy<2>,
                                   ExceptionSafety,
                                   Sparsity>;

    constexpr auto basic = sh::exception_safety::basic;
    constexpr auto strong = sh::exception_safety::strong;

    /// number of `counter::obj` that were constructed and not destroyed yet
    std::size_t alive(counter const &counts) {
        return counts.ctor() + counts.default_ctor() + counter::static_default_ctor + counts.copy_ctor() + counts.move_ctor()
               - counts.dtor() - counter::static_dtor;
    }

    /// inserts `key` mapped to itself
    template<typename Map>
    void insert_key(Map &map, counter &counts, std::size_t key) {
        map.try_emplace(counter::obj{key, counts}, key, counts);
    }

    template<typename Map>
    bool contains_key(Map const &map, counter &counts, std::size_t key) {
        return map.contains(counter::obj{key, counts});
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
            auto const it = map.find(counter::obj{key, counts});
            if (it != map.end()) {
                values_right = values_right && it->second.get() == key;
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

}  // namespace

TYPE_TO_STRING_AS("basic, high", bombing_map<basic, sh::sparsity::high>);
TYPE_TO_STRING_AS("basic, medium", bombing_map<basic, sh::sparsity::medium>);
TYPE_TO_STRING_AS("basic, low", bombing_map<basic, sh::sparsity::low>);
TYPE_TO_STRING_AS("strong, high", bombing_map<strong, sh::sparsity::high>);
TYPE_TO_STRING_AS("strong, medium", bombing_map<strong, sh::sparsity::medium>);
TYPE_TO_STRING_AS("strong, low", bombing_map<strong, sh::sparsity::low>);

// skipped: with `exception_safety::basic`, `sparse_hash::rehash_impl` moves the elements group by
// group into the new table and clears each old group right after it. When an allocation fails, the
// new table is destroyed with the elements it got so far, but `m_nb_elements` keeps its value:
// `size()` counts elements that `find` and iteration no longer reach.
TEST_CASE_TEMPLATE("basic: an insert that rehashes and throws leaves a valid map" * doctest::skip(),
                   map_t,
                   bombing_map<basic, sh::sparsity::high>,
                   bombing_map<basic, sh::sparsity::medium>,
                   bombing_map<basic, sh::sparsity::low>) {
    auto counts = counter{};
    std::size_t failures = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        {
            auto map = map_t{};
            fill_until_next_insert_rehashes(map, counts, 200);
            auto const new_key = map.size();
            auto const bucket_count_before = map.bucket_count();
            try {
                auto const bomb = bomb_after{budget};
                insert_key(map, counts, new_key);
                completed = true;
            } catch (std::bad_alloc const &) {
                ++failures;
            }
            if (completed) {
                CHECK(map.bucket_count() > bucket_count_before);
            }
            check_consistent(map, counts, new_key + 2);
            check_insert_works(map, counts, new_key + 1);
            check_consistent(map, counts, new_key + 2);
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(failures > 0);
    CHECK(completed);
}

TEST_CASE_TEMPLATE("strong: an insert that rehashes and throws leaves the contents unchanged",
                   map_t,
                   bombing_map<strong, sh::sparsity::high>,
                   bombing_map<strong, sh::sparsity::medium>,
                   bombing_map<strong, sh::sparsity::low>) {
    auto counts = counter{};
    std::size_t failures = 0;
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
                ++failures;
                CHECK(map.size() == size_before);
                CHECK(checksum::map(map) == checksum_before);
                CHECK_FALSE(contains_key(map, counts, new_key));
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
    CHECK(failures > 0);
    CHECK(completed);
}

TEST_CASE_TEMPLATE("strong: a reserve that throws leaves the contents unchanged",
                   map_t,
                   bombing_map<strong, sh::sparsity::high>,
                   bombing_map<strong, sh::sparsity::medium>,
                   bombing_map<strong, sh::sparsity::low>) {
    auto counts = counter{};
    std::size_t failures = 0;
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
                ++failures;
                CHECK(map.bucket_count() == bucket_count_before);
            }
            if (completed) {
                CHECK(map.bucket_count() > bucket_count_before);
            }
            CHECK(map.size() == 200);
            CHECK(checksum::map(map) == checksum_before);
            check_consistent(map, counts, 202);
            check_insert_works(map, counts, 200);
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(failures > 0);
    CHECK(completed);
}

TEST_CASE_TEMPLATE("an insert that throws without a rehash leaves the other elements alone",
                   map_t,
                   bombing_map<basic, sh::sparsity::high>,
                   bombing_map<basic, sh::sparsity::medium>,
                   bombing_map<basic, sh::sparsity::low>,
                   bombing_map<strong, sh::sparsity::high>,
                   bombing_map<strong, sh::sparsity::medium>,
                   bombing_map<strong, sh::sparsity::low>) {
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

TEST_CASE_TEMPLATE("a copy assignment that throws leaks nothing",
                   map_t,
                   bombing_map<basic, sh::sparsity::high>,
                   bombing_map<basic, sh::sparsity::medium>,
                   bombing_map<basic, sh::sparsity::low>,
                   bombing_map<strong, sh::sparsity::high>,
                   bombing_map<strong, sh::sparsity::medium>,
                   bombing_map<strong, sh::sparsity::low>) {
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

// skipped: when `sparse_hash::operator=(sparse_hash const &)` throws, the target keeps the growth
// policy of the source, but only the bucket groups it copied so far, and `sparse_buckets_` still
// points to the memory of the old bucket vector. `find` then asserts in `bucket_for_hash`, or reads
// freed memory, and `insert` writes there.
TEST_CASE_TEMPLATE("a copy assignment that throws leaves a usable map" * doctest::skip(),
                   map_t,
                   bombing_map<basic, sh::sparsity::high>,
                   bombing_map<basic, sh::sparsity::medium>,
                   bombing_map<basic, sh::sparsity::low>,
                   bombing_map<strong, sh::sparsity::high>,
                   bombing_map<strong, sh::sparsity::medium>,
                   bombing_map<strong, sh::sparsity::low>) {
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
