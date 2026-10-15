#include "fixtures/allocators.hpp"
#include "fixtures/checksum.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

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

/**
 * What a map holds after an allocation or the hash function failed in the middle of an operation. The allocations
 * fail through `tests::bombing_allocator`, the hash function through `throwing_hash`. The elements are
 * `counter::obj`, so a test sees every element that is not destroyed, or destroyed twice. The maps that allocate
 * through `tests::bombing_allocator` are `tests::throwing_map`, so a failed allocation throws `std::bad_alloc`.
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
    using namespace dice::sparse_map;
    using namespace dice::sparse_map::tests;

    /// blocks that `leak_checking_allocator` handed out and did not get back yet
    std::ptrdiff_t live_blocks = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// bytes that `leak_checking_allocator` handed out and did not get back yet
    std::size_t live_bytes = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// the maximum of `live_bytes` since a test set it
    std::size_t peak_bytes = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * `bombing_allocator` that also counts the blocks and the bytes it hands out and gets back, so that a
     * test can check that a failed operation leaks no memory, and how much memory an operation needs at
     * most. All instances compare equal.
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
            live_bytes += n * sizeof(T);
            peak_bytes = std::max(peak_bytes, live_bytes);
            return p;
        }

        void deallocate(T *p, std::size_t n) noexcept {
            --live_blocks;
            live_bytes -= n * sizeof(T);
            bombing_allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(leak_checking_allocator const & /*lhs*/, leak_checking_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

    /**
     * `counter::obj` whose move constructor can throw, so that a rehash copies it. Its move operations copy.
     */
    struct copied_obj : counter::obj {
        using counter::obj::obj;

        copied_obj(copied_obj const &other) = default;

        copied_obj(copied_obj &&other) noexcept(false)  // NOLINT(performance-noexcept-move-constructor)
            : counter::obj(static_cast<counter::obj const &>(other)) {
        }

        copied_obj &operator=(copied_obj const &other) = default;

        copied_obj &operator=(copied_obj &&other) noexcept(false) {  // NOLINT(performance-noexcept-move-constructor)
            counter::obj::operator=(static_cast<counter::obj const &>(other));
            return *this;
        }

        ~copied_obj() = default;
    };

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
     * Hash function that throws `std::runtime_error` on the n-th call after a `hash_bomb_after(n)`. Otherwise it
     * returns the hash of `test_hash`.
     */
    struct throwing_hash {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(counter::obj const &key) const {
            count_down();
            return test_hash<counter::obj>{}(key);
        }

        [[nodiscard]] std::size_t operator()(std::size_t key) const {
            count_down();
            return test_hash<std::size_t>{}(key);
        }

    private:
        static void count_down() {
            if (hash_calls_until_throw >= 0 && 0 == hash_calls_until_throw--) {
                throw std::runtime_error{"throwing_hash"};
            }
        }
    };

    template<sh::sparsity Sparsity, typename Hash = test_hash<counter::obj>>
    using moving_map = throwing_map<counter::obj,
                                    counter::obj,
                                    Hash,
                                    std::equal_to<counter::obj>,
                                    leak_checking_allocator<std::pair<counter::obj, counter::obj>>,
                                    Sparsity>;

    template<typename Hash = test_hash<counter::obj>>
    using copying_map = throwing_map<copied_obj,
                                     copied_obj,
                                     Hash,
                                     std::equal_to<counter::obj>,
                                     leak_checking_allocator<std::pair<copied_obj, copied_obj>>>;

    template<typename Hash = test_hash<std::size_t>>
    using trivial_map = throwing_map<std::size_t,
                                     std::size_t,
                                     Hash,
                                     std::equal_to<std::size_t>,
                                     leak_checking_allocator<std::pair<std::size_t, std::size_t>>>;

    /// the type in which `Map` stores its elements
    template<typename Map>
    using stored_t = typename Map::value_type;

    /// true if a rehash of `Map` moves its elements and frees each old group right after its elements are moved
    template<typename Map>
    constexpr bool moves_on_rehash = std::is_nothrow_move_constructible_v<stored_t<Map>>;

    /// true if a rehash of `Map` copies its elements and frees the old groups at the end
    template<typename Map>
    constexpr bool copies_on_rehash = !moves_on_rehash<Map>;

    static_assert(moves_on_rehash<moving_map<sh::sparsity::medium>>);
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

TYPE_TO_STRING_AS("moving, high", moving_map<sh::sparsity::high>);
TYPE_TO_STRING_AS("moving, medium", moving_map<sh::sparsity::medium>);
TYPE_TO_STRING_AS("moving, low", moving_map<sh::sparsity::low>);
TYPE_TO_STRING_AS("copying", copying_map<>);
TYPE_TO_STRING_AS("trivial", trivial_map<>);
TYPE_TO_STRING_AS("moving, throwing hash", moving_map<sh::sparsity::medium, throwing_hash>);
TYPE_TO_STRING_AS("copying, throwing hash", copying_map<throwing_hash>);
TYPE_TO_STRING_AS("trivial, throwing hash", trivial_map<throwing_hash>);

// A rehash that moves frees each old group right after its elements are moved, so it leaves the map empty if an
// allocation fails while it moves. Any other failed allocation leaves the contents unchanged.
TEST_CASE_TEMPLATE("an insert that rehashes and throws leaves the contents unchanged or empty",
                   map_t,
                   moving_map<sh::sparsity::high>,
                   moving_map<sh::sparsity::medium>,
                   moving_map<sh::sparsity::low>,
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
                   moving_map<sh::sparsity::high>,
                   moving_map<sh::sparsity::medium>,
                   moving_map<sh::sparsity::low>,
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
                   moving_map<sh::sparsity::medium, throwing_hash>,
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
                   moving_map<sh::sparsity::medium>,
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
                   moving_map<sh::sparsity::high>,
                   moving_map<sh::sparsity::medium>,
                   moving_map<sh::sparsity::low>,
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

TEST_CASE_TEMPLATE("a copy assignment that throws leaks nothing",
                   map_t,
                   moving_map<sh::sparsity::high>,
                   moving_map<sh::sparsity::medium>,
                   moving_map<sh::sparsity::low>) {
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
                   moving_map<sh::sparsity::high>,
                   moving_map<sh::sparsity::medium>,
                   moving_map<sh::sparsity::low>) {
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

    /// the move assignment of `fragile_hash` throws while this is true
    bool hash_moves_throw = false;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * A hash whose move assignment throws while `hash_moves_throw` is true. `std::swap` of two of them
     * move assigns, so it throws, too. Otherwise it returns the hash of `test_hash`.
     */
    struct fragile_hash {
        using is_avalanching = void;

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
            return test_hash<std::size_t>{}(key);
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
    using map_t = sparse_map<std::size_t, countdown_value, test_hash<std::size_t>, std::equal_to<std::size_t>, allocator_t>;
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
