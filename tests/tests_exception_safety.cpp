#include "fixtures/allocators.hpp"
#include "fixtures/checksum.hpp"
#include "fixtures/counter.hpp"

#include <dice/sparse-map/sparse_map.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <functional>
#include <iterator>
#include <new>
#include <stdexcept>
#include <string>
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
                                   ExceptionSafety,
                                   Sparsity>;

    constexpr auto basic = sh::exception_safety::basic;
    constexpr auto strong = sh::exception_safety::strong;

    /// number of `counter::obj` that were constructed and not destroyed yet
    std::size_t alive(counter const &counts) {
        return counts.ctor() + counts.default_ctor() + counter::static_ctor + counts.copy_ctor() + counts.move_ctor()
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

TEST_CASE_TEMPLATE("basic: an insert that rehashes and throws leaves a valid map",
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

TEST_CASE_TEMPLATE("a copy construction that throws leaks nothing",
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

        std::size_t failures = 0;
        bool completed = false;
        for (int budget = 0; budget < max_budget && !completed; ++budget) {
            try {
                auto const bomb = bomb_after{budget};
                auto const copy = map_t{source};
                completed = true;
                CHECK(copy.size() == source.size());
            } catch (std::bad_alloc const &) {
                ++failures;
            }
            CHECK(live_blocks == source_blocks);
            CHECK(alive(counts) == source_alive);
        }
        CHECK(failures > 0);
        CHECK(completed);
        CHECK(checksum::map(source) == source_checksum);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
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

TEST_CASE_TEMPLATE("a copy assignment that throws leaves a usable map",
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

namespace {
    /// a string that does not fit the small string buffer, so that a moved-from copy is empty
    std::string long_value(std::size_t key) {
        return std::string(40, 'x') + std::to_string(key);
    }

    template<sh::exception_safety ExceptionSafety, sh::sparsity Sparsity>
    using bombing_string_map = sparse_map<std::size_t,
                                          std::string,
                                          std::hash<std::size_t>,
                                          std::equal_to<std::size_t>,
                                          leak_checking_allocator<std::pair<std::size_t, std::string>>,
                                          ExceptionSafety,
                                          Sparsity>;

    /// the next insertion of a new key into `map` rehashes it
    template<typename Map>
    void fill_strings_until_next_insert_rehashes(Map &map, std::size_t min_size) {
        std::size_t key = 0;
        while (map.size() < min_size || !next_insert_rehashes(map)) {
            map.try_emplace(key, long_value(key));
            ++key;
        }
    }

    /// number of copies that are left before `fragile_string` throws, -1 never throws
    int copies_until_throw = -1;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * A string whose move constructor is not `noexcept`, and whose copy constructor throws when
     * `copies_until_throw` reaches 0. A container copies it where a move could throw.
     */
    struct fragile_string {
        std::string value;

        explicit fragile_string(std::string v)
            : value(std::move(v)) {
        }

        fragile_string(fragile_string const &other)
            : value(other.value) {
            if (copies_until_throw >= 0 && 0 == copies_until_throw--) {
                throw std::runtime_error("fragile_string copy");
            }
        }

        fragile_string(fragile_string &&other) noexcept(false)
            : value(std::move(other.value)) {
        }

        fragile_string &operator=(fragile_string const &) = default;
        fragile_string &operator=(fragile_string &&) noexcept(false) = default;
        ~fragile_string() = default;
    };

    /// the move assignment of `fragile_hash` throws while this is true
    bool hash_moves_throw = false;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * A hash whose move assignment throws while `hash_moves_throw` is true. `std::swap` of two of them
     * move assigns, so it throws, too.
     */
    struct fragile_hash {
        fragile_hash() = default;
        fragile_hash(fragile_hash const &) = default;
        fragile_hash(fragile_hash &&) = default;
        fragile_hash &operator=(fragile_hash const &) = default;
        ~fragile_hash() = default;

        fragile_hash &operator=(fragile_hash && /*other*/) noexcept(false) {
            if (hash_moves_throw) {
                throw std::runtime_error("fragile_hash move assignment");
            }
            return *this;
        }

        std::size_t operator()(std::size_t key) const noexcept {
            return std::hash<std::size_t>{}(key);
        }
    };

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
}  // namespace

TEST_CASE_TEMPLATE("a merge that throws in the rehash of the target leaves the element in the source",
                   map_t,
                   bombing_string_map<basic, sh::sparsity::high>,
                   bombing_string_map<basic, sh::sparsity::medium>,
                   bombing_string_map<basic, sh::sparsity::low>,
                   bombing_string_map<strong, sh::sparsity::high>,
                   bombing_string_map<strong, sh::sparsity::medium>,
                   bombing_string_map<strong, sh::sparsity::low>) {
    std::size_t failures = 0;
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
                ++failures;
                // with exception_safety::basic a failed rehash can lose elements of the target
                CHECK(target.size() <= target_size);
                CHECK(static_cast<std::size_t>(std::distance(target.begin(), target.end())) == target.size());
                CHECK_FALSE(target.contains(new_key));
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
    CHECK(failures > 0);
    CHECK(completed);
}

TEST_CASE_TEMPLATE("a merge that throws in the clean-up rehash of the target leaves the element in the source",
                   map_t,
                   bombing_string_map<basic, sh::sparsity::high>,
                   bombing_string_map<basic, sh::sparsity::medium>,
                   bombing_string_map<basic, sh::sparsity::low>,
                   bombing_string_map<strong, sh::sparsity::high>,
                   bombing_string_map<strong, sh::sparsity::medium>,
                   bombing_string_map<strong, sh::sparsity::low>) {
    std::size_t failures = 0;
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
                ++failures;
                CHECK(target.size() <= 5);
                CHECK_FALSE(target.contains(1000));
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
    CHECK(failures > 0);
    CHECK(completed);
}

TEST_CASE("a merge copies an element whose move constructor can throw, so that the element stays in the source on an exception") {
    using map_t = sparse_map<std::size_t, fragile_string, std::hash<std::size_t>, std::equal_to<std::size_t>, std::allocator<std::pair<std::size_t, fragile_string>>, sh::exception_safety::basic, sh::sparsity::high>;
    auto target = map_t{};
    target.rehash(64);
    // one group; with sparsity high a group of 10 elements has no free capacity, so the next insertion
    // copies the group into a new array: the elements before the new one, the new one, the elements after it
    for (std::size_t key = 0; key < 10; ++key) {
        target.try_emplace(key, std::to_string(key));
    }
    REQUIRE(target.bucket_count() == 64);

    // a key whose element does not land behind all elements of the group, so that copies follow the new element
    std::size_t new_key = 100;
    for (;; ++new_key) {
        auto probe = target;
        probe.try_emplace(new_key, "");
        auto const position = static_cast<std::size_t>(std::distance(probe.begin(), probe.find(new_key)));
        if (position + 1 < probe.size()) {
            break;
        }
    }

    bool completed = false;
    for (int copies = 0; copies < 20 && !completed; ++copies) {
        CAPTURE(copies);
        auto source = map_t{};
        source.try_emplace(new_key, long_value(new_key));
        copies_until_throw = copies;
        try {
            target.merge(source);
            completed = true;
        } catch (std::runtime_error const &) {
        }
        copies_until_throw = -1;
        if (completed) {
            CHECK(source.empty());
            CHECK(target.at(new_key).value == long_value(new_key));
        } else {
            CHECK(target.size() == 10);
            CHECK_FALSE(target.contains(new_key));
            REQUIRE(source.size() == 1);
            CHECK(source.at(new_key).value == long_value(new_key));
        }
    }
    CHECK(completed);
}

TEST_CASE("a move assignment whose hash throws leaves both maps usable") {
    using map_t = sparse_map<std::size_t, std::size_t, fragile_hash>;
    auto source = map_t{};
    for (std::size_t key = 0; key < 100; ++key) {
        source[key] = key;
    }
    auto target = map_t{};
    for (std::size_t key = 1000; key < 1010; ++key) {
        target[key] = key;
    }

    hash_moves_throw = true;
    CHECK_THROWS_AS(target = std::move(source), std::runtime_error);
    hash_moves_throw = false;

    check_holds(target, 0, 0);
    check_holds(source, 0, 100);  // NOLINT(bugprone-use-after-move)
    target[5000] = 5000;
    source[5000] = 5000;  // NOLINT(bugprone-use-after-move)
    check_holds(target, 5000, 1);
    CHECK(source.size() == 101);
}

TEST_CASE("a swap whose hash throws swaps neither the elements nor the allocators") {
    using allocator_t = pocs_allocator<std::pair<std::size_t, std::size_t>>;
    using map_t = sparse_map<std::size_t, std::size_t, fragile_hash, std::equal_to<std::size_t>, allocator_t>;
    auto counts_1 = alloc_counts{};
    auto counts_2 = alloc_counts{};
    {
        auto map_1 = map_t{allocator_t{1, &counts_1}};
        auto map_2 = map_t{allocator_t{2, &counts_2}};
        for (std::size_t key = 0; key < 100; ++key) {
            map_1[key] = key;
            map_2[key + 1000] = key + 1000;
        }

        hash_moves_throw = true;
        CHECK_THROWS_AS(map_1.swap(map_2), std::runtime_error);
        hash_moves_throw = false;

        CHECK(map_1.get_allocator().id == 1);
        CHECK(map_2.get_allocator().id == 2);
        check_holds(map_1, 0, 100);
        check_holds(map_2, 1000, 100);
    }
    // each allocator got back exactly the blocks it handed out
    CHECK(counts_1.allocations == counts_1.deallocations);
    CHECK(counts_2.allocations == counts_2.deallocations);
}
