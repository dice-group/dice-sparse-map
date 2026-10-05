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
#include <memory>
#include <new>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

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
                                    sh::exception_safety::basic,
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
        return counts.ctor() + counts.default_ctor() + counter::static_default_ctor + counts.copy_ctor() + counts.move_ctor()
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

// skipped: when `sparse_hash::operator=(sparse_hash const &)` throws, the target keeps the mask of the
// source, but only the bucket groups it copied so far, and `sparse_buckets_` still points to the memory
// of the old bucket vector. `find` then reads freed memory, and `insert` writes there.
TEST_CASE_TEMPLATE("a copy assignment that throws leaves a usable map" * doctest::skip(),
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

namespace {

    /// `moving_map<sh::sparsity::medium>` with `sh::exception_safety::strong`
    using strong_moving_map = throwing_map<counter::obj,
                                           counter::obj,
                                           test_hash<counter::obj>,
                                           std::equal_to<counter::obj>,
                                           leak_checking_allocator<std::pair<counter::obj, counter::obj>>,
                                           sh::exception_safety::strong,
                                           sh::sparsity::medium>;

    /// what an insert that rehashes did with one allocation budget
    enum class insert_outcome {
        completed,
        unchanged,
        emptied
    };

    /**
     * Fills a new `Map` until the next insert rehashes it, then inserts a new key with the allocation budgets 0, 1, 2
     * and so on, until the insert completes.
     * @return the outcome for each budget
     */
    template<typename Map>
    std::vector<insert_outcome> insert_outcomes(counter &counts) {
        std::vector<insert_outcome> outcomes;
        for (int budget = 0; budget < max_budget; ++budget) {
            auto map = Map{};
            fill_until_next_insert_rehashes(map, counts, 200);
            auto const size_before = map.size();
            auto const checksum_before = checksum::map(map);
            try {
                auto const bomb = bomb_after{budget};
                insert_key(map, counts, size_before);
                outcomes.push_back(insert_outcome::completed);
                break;
            } catch (std::bad_alloc const &) {
                bool const unchanged = map.size() == size_before && checksum::map(map) == checksum_before;
                outcomes.push_back(unchanged ? insert_outcome::unchanged : insert_outcome::emptied);
            }
        }
        return outcomes;
    }

    /// the bytes that a rehash of `map` to twice its bucket count needs on top of the memory that was in use before
    template<typename Map>
    std::size_t rehash_extra_bytes(Map &map) {
        auto const bytes_before = live_bytes;
        peak_bytes = live_bytes;
        map.rehash(map.bucket_count() * 2);
        return peak_bytes - bytes_before;
    }

}  // namespace

// `ExceptionSafety` is a placeholder. With `exception_safety::strong` a rehash moves elements whose move constructor
// cannot throw and frees each old group while it moves, as with `exception_safety::basic`. So both need the same
// memory, a failed allocation has the same effect on both, and a mapped type that cannot be copied works.
TEST_CASE("exception_safety::strong is accepted and behaves like basic") {
    using basic_moving_map = moving_map<sh::sparsity::medium>;
    auto counts = counter{};
    {
        auto basic = basic_moving_map{};
        auto strong = strong_moving_map{};
        for (std::size_t key = 0; key < 10000; ++key) {
            insert_key(basic, counts, key);
            insert_key(strong, counts, key);
        }
        auto const basic_extra = rehash_extra_bytes(basic);
        auto const strong_extra = rehash_extra_bytes(strong);
        CHECK(strong_extra == basic_extra);
        CHECK(checksum::map(strong) == checksum::map(basic));
    }

    auto const basic_outcomes = insert_outcomes<basic_moving_map>(counts);
    auto const strong_outcomes = insert_outcomes<strong_moving_map>(counts);
    CHECK(strong_outcomes == basic_outcomes);
    CHECK(std::ranges::count(strong_outcomes, insert_outcome::emptied) > 0);
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);

    using unique_value = std::unique_ptr<std::size_t>;
    auto move_only = sparse_map<std::size_t,
                                unique_value,
                                test_hash<std::size_t>,
                                std::equal_to<std::size_t>,
                                std::allocator<std::pair<std::size_t, unique_value>>,
                                sh::exception_safety::strong>{};
    auto set = sparse_set<std::size_t,
                          test_hash<std::size_t>,
                          std::equal_to<std::size_t>,
                          std::allocator<std::size_t>,
                          sh::exception_safety::strong>{};
    for (std::size_t key = 0; key < 100; ++key) {
        move_only.try_emplace(key, std::make_unique<std::size_t>(key));
        set.insert(key);
    }
    move_only.rehash(move_only.bucket_count() * 2);
    set.rehash(set.bucket_count() * 2);
    bool all_found = true;
    for (std::size_t key = 0; key < 100; ++key) {
        auto const it = move_only.find(key);
        all_found = all_found && it != move_only.end() && *it->second == key && set.contains(key);
    }
    CHECK(all_found);
}
