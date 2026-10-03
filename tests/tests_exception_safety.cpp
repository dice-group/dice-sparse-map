#include "fixtures/allocators.hpp"
#include "fixtures/checksum.hpp"
#include "fixtures/counter.hpp"

#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <new>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * What a map holds after an allocation or the hash function failed in the middle of an operation. The allocations
 * fail through `tests::bombing_allocator`, the hash function through `throwing_hash`. The elements are
 * `counter::obj`, so a test sees every element that is not destroyed, or destroyed twice.
 *
 * A rehash moves an element whose move constructor cannot throw and frees each old group right after its elements
 * are moved, so the map is empty if it throws while it moves. It copies an element whose move constructor can throw
 * and keeps the old groups until the end. The maps here cover both cases: `moving_map` holds `counter::obj` and
 * `trivial_map` holds `std::size_t`, which are moved, and `copying_map` holds `copied_obj`, whose move constructor
 * can throw.
 *
 * Partly ported from the growth and assignment exception safety tests of ankerl::unordered_dense
 * (MIT license).
 */
namespace {
    using namespace dice::unordered_sparse;
    using namespace dice::unordered_sparse::tests;

    /// calls of `throwing_hash` that are left before it throws, -1 never throws
    int hash_calls_until_throw = -1;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * Arms the countdown of `throwing_hash` while it is in scope.
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

    /**
     * Hash function that throws `std::runtime_error` on the n-th call after a `hash_bomb_after(n)`.
     */
    struct throwing_hash {
        [[nodiscard]] std::size_t operator()(counter::obj const &key) const {
            count_down();
            return std::hash<counter::obj>{}(key);
        }

        [[nodiscard]] std::size_t operator()(std::size_t key) const {
            count_down();
            return std::hash<std::size_t>{}(key);
        }

    private:
        static void count_down() {
            if (hash_calls_until_throw >= 0 && 0 == hash_calls_until_throw--) {
                throw std::runtime_error{"throwing_hash"};
            }
        }
    };

    template<sparsity Sparsity, typename Hash = std::hash<counter::obj>>
    using moving_map = sparse_map<counter::obj,
                                  counter::obj,
                                  Hash,
                                  std::equal_to<counter::obj>,
                                  leak_checking_allocator<std::pair<counter::obj, counter::obj>>,
                                  Sparsity>;

    template<typename Hash = std::hash<counter::obj>>
    using copying_map = sparse_map<copied_obj,
                                   copied_obj,
                                   Hash,
                                   std::equal_to<counter::obj>,
                                   leak_checking_allocator<std::pair<copied_obj, copied_obj>>>;

    template<typename Hash = std::hash<std::size_t>>
    using trivial_map = sparse_map<std::size_t,
                                   std::size_t,
                                   Hash,
                                   std::equal_to<std::size_t>,
                                   leak_checking_allocator<std::pair<std::size_t, std::size_t>>>;

    /// the type in which `Map` stores its elements
    template<typename Map>
    using stored_t = detail::map_slot<typename Map::key_type, typename Map::mapped_type>;

    /// true if a rehash of `Map` moves its elements and frees each old group right after its elements are moved
    template<typename Map>
    constexpr bool moves_on_rehash = std::is_nothrow_move_constructible_v<stored_t<Map>>;

    /// true if a rehash of `Map` copies its elements and frees the old groups at the end
    template<typename Map>
    constexpr bool copies_on_rehash = !moves_on_rehash<Map>;

    static_assert(moves_on_rehash<moving_map<sparsity::medium>>);
    static_assert(moves_on_rehash<trivial_map<>>);
    static_assert(copies_on_rehash<copying_map<>>);

    /// number of `counter::obj` that were constructed and not destroyed yet
    std::size_t alive(counter const &counts) {
        return counts.ctor() + counts.default_ctor() + counter::static_ctor + counts.copy_ctor() + counts.move_ctor()
               - counts.dtor() - counter::static_dtor;
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

}  // namespace

TYPE_TO_STRING_AS("moving, high", moving_map<sparsity::high>);
TYPE_TO_STRING_AS("moving, medium", moving_map<sparsity::medium>);
TYPE_TO_STRING_AS("moving, low", moving_map<sparsity::low>);
TYPE_TO_STRING_AS("copying", copying_map<>);
TYPE_TO_STRING_AS("trivial", trivial_map<>);
TYPE_TO_STRING_AS("moving, throwing hash", moving_map<sparsity::medium, throwing_hash>);
TYPE_TO_STRING_AS("copying, throwing hash", copying_map<throwing_hash>);
TYPE_TO_STRING_AS("trivial, throwing hash", trivial_map<throwing_hash>);

// A rehash that moves frees each old group right after its elements are moved, so it leaves the map empty if an
// allocation fails while it moves. Any other failed allocation leaves the contents unchanged.
TEST_CASE_TEMPLATE("an insert that rehashes and throws leaves the contents unchanged or empty",
                   map_t,
                   moving_map<sparsity::high>,
                   moving_map<sparsity::medium>,
                   moving_map<sparsity::low>,
                   copying_map<>,
                   trivial_map<>) {
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
                    CHECK(moves_on_rehash<map_t>);
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
    CHECK((emptied > 0) == moves_on_rehash<map_t>);
    CHECK(completed);
}

// As above: only a rehash that moves can leave the map empty.
TEST_CASE_TEMPLATE("a reserve that throws leaves the contents unchanged or empty",
                   map_t,
                   moving_map<sparsity::high>,
                   moving_map<sparsity::medium>,
                   moving_map<sparsity::low>,
                   copying_map<>,
                   trivial_map<>) {
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
                    CHECK(moves_on_rehash<map_t>);
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
    CHECK((emptied > 0) == moves_on_rehash<map_t>);
    CHECK(completed);
}

// A rehash that moves hashes the elements while it moves them, so it leaves the map empty whenever the hash
// function throws. A rehash that copies leaves it unchanged.
TEST_CASE_TEMPLATE("a rehash whose hash function throws leaves the contents unchanged or empty",
                   map_t,
                   moving_map<sparsity::medium, throwing_hash>,
                   copying_map<throwing_hash>,
                   trivial_map<throwing_hash>) {
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
                    CHECK(moves_on_rehash<map_t>);
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
    CHECK((unchanged > 0) == copies_on_rehash<map_t>);
    CHECK((emptied > 0) == moves_on_rehash<map_t>);
}

// A rehash that moves frees each old group right after its elements are moved, so it needs less memory than the new
// table on top of the old one. A rehash that copies frees the old groups at the end, so both tables are in memory at
// once.
TEST_CASE_TEMPLATE("a rehash frees the old groups while it moves and keeps them while it copies",
                   map_t,
                   moving_map<sparsity::medium>,
                   copying_map<>,
                   trivial_map<>) {
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
        if constexpr (moves_on_rehash<map_t>) {
            CHECK(extra_bytes < bytes_after / 2);
        } else {
            CHECK(extra_bytes >= bytes_after);
        }
        CHECK(map.size() == 10000);
        check_consistent(map, counts, 10000);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

TEST_CASE_TEMPLATE("an insert that throws without a rehash leaves the other elements alone",
                   map_t,
                   moving_map<sparsity::high>,
                   moving_map<sparsity::medium>,
                   moving_map<sparsity::low>,
                   copying_map<>) {
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
                   moving_map<sparsity::high>,
                   moving_map<sparsity::medium>,
                   moving_map<sparsity::low>) {
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
                   moving_map<sparsity::high>,
                   moving_map<sparsity::medium>,
                   moving_map<sparsity::low>) {
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
                   moving_map<sparsity::high>,
                   moving_map<sparsity::medium>,
                   moving_map<sparsity::low>) {
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

// No erase function of an unordered container throws, unless the hash function or the key equality throws
// ([unord.req.except] of the standard). An element whose move constructor can throw is erased in place, so the erase
// allocates nothing and copies nothing.
TEST_CASE("erase of an element whose move constructor can throw does not throw") {
    auto counts = counter{};
    {
        auto map = copying_map<>{};
        for (std::size_t key = 0; key < 100; ++key) {
            insert_key(map, counts, key);
        }

        // the first element of its group, so that other elements follow it in the group
        auto const bomb = bomb_after{0};
        CHECK_NOTHROW(map.erase(map.begin()));
        CHECK(map.size() == 99);
        check_consistent(map, counts, 100);

        // by key, a range and a predicate
        auto const key = map.begin()->first.get();
        std::size_t erased = 0;
        CHECK_NOTHROW(erased = map.erase(make<copied_obj>(counts, key)));
        CHECK(erased == 1);
        CHECK_NOTHROW(map.erase(std::next(map.cbegin(), 10), std::next(map.cbegin(), 20)));
        CHECK(map.size() == 88);
        CHECK_NOTHROW(erased = erase_if(map, [](auto const &entry) {
                          return entry.first.get() % 3 == 0;
                      }));
        CHECK(erased > 0);
        CHECK(map.size() == 88 - erased);
        check_consistent(map, counts, 100);
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

namespace {

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

TEST_CASE("a rehash does not swap the hash function") {
    using map_t = sparse_map<std::size_t, std::string, fragile_hash>;
    auto map = map_t{};
    for (std::size_t key = 0; key < 100; ++key) {
        map.try_emplace(key, std::to_string(key));
    }

    hash_moves_throw = true;
    CHECK_NOTHROW(map.rehash(map.bucket_count() * 2));
    hash_moves_throw = false;

    CHECK(map.size() == 100);
    bool all_found = true;
    for (std::size_t key = 0; key < 100; ++key) {
        auto const it = map.find(key);
        all_found = all_found && it != map.end() && it->second == std::to_string(key);
    }
    CHECK(all_found);
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

namespace {
    /**
     * A hash with a seed, which it adds to the key. Two maps with different seeds place the same key in different
     * buckets.
     */
    struct seeded_hash {
        std::size_t seed = 0;

        std::size_t operator()(std::size_t key) const noexcept {
            return key + seed;
        }
    };

    /// the move assignment of `fragile_key_equal` throws while this is true
    bool key_equal_moves_throw = false;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * A key equality whose move assignment throws while `key_equal_moves_throw` is true. `std::swap` of two of
     * them move assigns, so it throws, too.
     */
    struct fragile_key_equal {
        fragile_key_equal() = default;
        fragile_key_equal(fragile_key_equal const &) = default;
        fragile_key_equal(fragile_key_equal &&) = default;
        fragile_key_equal &operator=(fragile_key_equal const &) = default;
        ~fragile_key_equal() = default;

        fragile_key_equal &operator=(fragile_key_equal && /*other*/) noexcept(false) {
            if (key_equal_moves_throw) {
                throw std::runtime_error("fragile_key_equal move assignment");
            }
            return *this;
        }

        bool operator()(std::size_t lhs, std::size_t rhs) const noexcept {
            return lhs == rhs;
        }
    };
}  // namespace

TEST_CASE("a swap whose key equality throws leaves both maps able to find their elements") {
    using map_t = sparse_map<std::size_t, std::size_t, seeded_hash, fragile_key_equal>;
    auto map_1 = map_t{0, seeded_hash{1}};
    auto map_2 = map_t{0, seeded_hash{1000}};
    for (std::size_t key = 0; key < 100; ++key) {
        map_1[key] = key;
        map_2[key + 1000] = key + 1000;
    }
    static_assert(!noexcept(map_1.swap(map_2)));

    // the hash functions are swapped first, then the key equality throws
    key_equal_moves_throw = true;
    CHECK_THROWS_AS(map_1.swap(map_2), std::runtime_error);
    key_equal_moves_throw = false;

    check_holds(map_1, 0, 100);
    check_holds(map_2, 1000, 100);
}

namespace {
    /// moves and copies of `countdown_value` that are left before one throws, -1 never throws
    int value_transfers_until_throw = -1;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * A value whose move constructor can throw. Each move and copy counts `value_transfers_until_throw`
     * down and throws when it reaches 0. A move sets its source to -1, so a test sees the moved-from values.
     */
    struct countdown_value {
        int value = 0;

        explicit countdown_value(int v) noexcept
            : value(v) {
        }

        countdown_value(countdown_value const &other)
            : value(other.value) {
            count_down();
        }

        countdown_value(countdown_value &&other) noexcept(false)  // NOLINT(performance-noexcept-move-constructor)
            : value(other.value) {
            count_down();
            other.value = -1;
        }

        countdown_value &operator=(countdown_value const &) = default;
        countdown_value &operator=(countdown_value &&) = default;
        ~countdown_value() = default;

    private:
        static void count_down() {
            if (value_transfers_until_throw >= 0 && 0 == value_transfers_until_throw--) {
                throw std::runtime_error("countdown_value");
            }
        }
    };
}  // namespace

TEST_CASE("a move assignment to an unequal allocator that throws keeps the values of the source") {
    using allocator_t = id_allocator<std::pair<std::size_t, countdown_value>>;
    using map_t = sparse_map<std::size_t, countdown_value, std::hash<std::size_t>, std::equal_to<std::size_t>, allocator_t>;
    auto source = map_t{allocator_t{1}};
    for (std::size_t key = 0; key < 100; ++key) {
        source.try_emplace(key, static_cast<int>(key));
    }
    auto target = map_t{allocator_t{2}};
    target.try_emplace(1000, 1000);
    static_assert(!noexcept(target = std::move(source)));

    // the 51st move or copy of a value throws, after 50 values reached the target
    value_transfers_until_throw = 50;
    CHECK_THROWS_AS(target = std::move(source), std::runtime_error);
    value_transfers_until_throw = -1;

    CHECK(target.empty());
    CHECK(target.get_allocator().id == 2);
    CHECK(source.size() == 100);  // NOLINT(bugprone-use-after-move)
    std::size_t kept = 0;
    for (std::size_t key = 0; key < 100; ++key) {
        auto const it = source.find(key);  // NOLINT(bugprone-use-after-move)
        kept += (it != source.end() && it->second.value == static_cast<int>(key)) ? 1 : 0;
    }
    CHECK(kept == 100);
}

namespace {
    /**
     * A hash with state. It adds the size of `seed` to the key, and a move leaves `seed` empty, so a moved-from
     * `move_sensitive_hash` hashes differently.
     */
    struct move_sensitive_hash {
        std::vector<std::size_t> seed = std::vector<std::size_t>(3);

        std::size_t operator()(std::size_t key) const noexcept {
            return key + seed.size();
        }
    };

    using seeded_allocator_t = id_allocator<std::pair<std::size_t, countdown_value>>;
    using seeded_map_t = sparse_map<std::size_t, countdown_value, move_sensitive_hash, std::equal_to<std::size_t>, seeded_allocator_t>;

    /// number of the keys 0 to 99 that `map` finds with their value
    std::size_t nb_found_with_value(seeded_map_t const &map) {
        std::size_t found = 0;
        for (std::size_t key = 0; key < 100; ++key) {
            auto const it = map.find(key);
            found += (it != map.end() && it->second.value == static_cast<int>(key)) ? 1 : 0;
        }
        return found;
    }
}  // namespace

TEST_CASE("a move assignment to an unequal allocator that throws leaves the source able to find its values") {
    auto source = seeded_map_t{seeded_allocator_t{1}};
    for (std::size_t key = 0; key < 100; ++key) {
        source.try_emplace(key, static_cast<int>(key));
    }
    auto target = seeded_map_t{seeded_allocator_t{2}};

    // the 51st move or copy of a value throws, after 50 values reached the target
    value_transfers_until_throw = 50;
    CHECK_THROWS_AS(target = std::move(source), std::runtime_error);
    value_transfers_until_throw = -1;

    CHECK(target.empty());
    CHECK(source.size() == 100);                // NOLINT(bugprone-use-after-move)
    CHECK(nb_found_with_value(source) == 100);  // NOLINT(bugprone-use-after-move)
}

TEST_CASE("a move construction with an unequal allocator that throws leaves the source able to find its values") {
    auto source = seeded_map_t{seeded_allocator_t{1}};
    for (std::size_t key = 0; key < 100; ++key) {
        source.try_emplace(key, static_cast<int>(key));
    }

    // the 51st move or copy of a value throws, after 50 values reached the new map
    value_transfers_until_throw = 50;
    CHECK_THROWS_AS((seeded_map_t{std::move(source), seeded_allocator_t{2}}), std::runtime_error);
    value_transfers_until_throw = -1;

    CHECK(source.size() == 100);                // NOLINT(bugprone-use-after-move)
    CHECK(nb_found_with_value(source) == 100);  // NOLINT(bugprone-use-after-move)
}

namespace {
    /// a string that does not fit the small string buffer, so that a moved-from copy is empty
    std::string long_value(std::size_t key) {
        return std::string(40, 'x') + std::to_string(key);
    }

    template<sparsity Sparsity>
    using bombing_string_map = sparse_map<std::size_t,
                                          std::string,
                                          std::hash<std::size_t>,
                                          std::equal_to<std::size_t>,
                                          leak_checking_allocator<std::pair<std::size_t, std::string>>,
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
}  // namespace

// The rehash of the target moves the strings, so an allocation that fails while it moves leaves the target empty.
// A failed allocation of the new buckets, or of the new element after the rehash, leaves the target unchanged. The
// element stays in the source in both cases.
TEST_CASE_TEMPLATE("a merge that throws in the rehash of the target leaves the element in the source",
                   map_t,
                   bombing_string_map<sparsity::high>,
                   bombing_string_map<sparsity::medium>,
                   bombing_string_map<sparsity::low>) {
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
TEST_CASE_TEMPLATE("a merge that throws in the clean-up rehash of the target leaves the element in the source",
                   map_t,
                   bombing_string_map<sparsity::high>,
                   bombing_string_map<sparsity::medium>,
                   bombing_string_map<sparsity::low>) {
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

TEST_CASE("a merge copies an element whose move constructor can throw, so that the element stays in the source on an exception") {
    using map_t = sparse_map<std::size_t, fragile_string, std::hash<std::size_t>, std::equal_to<std::size_t>, std::allocator<std::pair<std::size_t, fragile_string>>, sparsity::high>;
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

namespace {
    /// the hash of a key is its number, and the table uses it without mixing, so key `k` lands in bucket `k`
    struct bucket_hash {
        using is_avalanching = void;

        std::size_t operator()(counter::obj const &key) const {
            return key.get_for_hash();
        }
    };
}  // namespace

// `merge` erases an element from the source after it inserted it into the target, and that erase does not throw. So
// an exception, here from the insertion of the next element, leaves every element in exactly one of the two maps.
TEST_CASE("a merge that throws leaves every element whose move constructor can throw in exactly one map") {
    using map_t = copying_map<bucket_hash>;
    auto counts = counter{};
    std::size_t failures = 0;
    bool completed = false;
    for (int budget = 0; budget < max_budget && !completed; ++budget) {
        CAPTURE(budget);
        {
            // no rehash in either map, and the keys 1 and 2 share the first group in both maps
            auto target = map_t{};
            target.reserve(100);
            auto source = map_t{};
            source.reserve(100);
            insert_key(source, counts, 1);
            insert_key(source, counts, 2);
            try {
                auto const bomb = bomb_after{budget};
                target.merge(source);
                completed = true;
            } catch (std::bad_alloc const &) {
                ++failures;
            }
            CHECK(contains_key(target, counts, 1) != contains_key(source, counts, 1));
            CHECK(contains_key(target, counts, 2) != contains_key(source, counts, 2));
            CHECK(target.size() + source.size() == 2);
            check_consistent(target, counts, 3);
            check_consistent(source, counts, 3);
            if (completed) {
                CHECK(target.size() == 2);
            }
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(failures > 0);
    CHECK(completed);
}
