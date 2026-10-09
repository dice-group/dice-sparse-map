#include <dice/sparse-map/sparse_map.hpp>

#include <doctest/doctest.h>
#include <nanobench.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

/**
 * The inline group puts every element into the bucket where group 0 of a table of 64 buckets would put it, and the move
 * into an allocated group keeps the buckets and the order. These tests compare a map with an inline capacity and the
 * same map type with the inline capacity 0 with a model of their buckets (`bucket_model`), after every operation of
 * random runs of insertions, erasures by key and by iterator, lookups, copies, moves and `clear`. Each map holds its
 * elements in the order of its model, and both maps hold the same elements.
 *
 * The model has no deleted buckets. An insertion takes the first bucket of its probe sequence that holds no element. In
 * a table and in the inline group that is the first deleted or empty bucket of the probe sequence, whichever buckets are
 * deleted. So the deleted buckets matter only for the time of a rebuild of the buckets (a clean-up of the deleted
 * buckets or a growth). The inline group keeps fewer deleted buckets than a table and has a lower clean-up threshold,
 * so it rebuilds at other times. A rebuild hashes every element, so the tests see it from the calls of the hash
 * function and rebuild the model of that map too.
 */
namespace {
    using namespace dice::sparse_map;

    /// calls of `oracle_hash` since the last reset
    std::size_t hash_calls = 0; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * Puts a key into one of `nb_homes` home buckets, so that the probe sequences cross deleted buckets. Marked as
     * avalanching, so that the table uses the hash as it is.
     */
    template<std::uint64_t nb_homes>
    struct oracle_hash {
        using is_avalanching = void;

        [[nodiscard]] static std::size_t hash_of(std::uint64_t key) noexcept {
            return static_cast<std::size_t>((key * UINT64_C(0x9E3779B97F4A7C15)) >> 40U) % nb_homes;
        }

        [[nodiscard]] std::size_t operator()(std::uint64_t key) const noexcept {
            ++hash_calls;
            return hash_of(key);
        }
    };

    template<std::uint64_t nb_homes, std::size_t inline_capacity>
    using oracle_map = sparse_map<
        std::uint64_t,
        std::uint64_t,
        oracle_hash<nb_homes>,
        std::equal_to<std::uint64_t>,
        std::allocator<std::pair<std::uint64_t, std::uint64_t>>,
        sh::sparsity::medium,
        sh::allocation_failure::terminating,
        inline_capacity
    >;

    template<typename Map>
    std::vector<std::uint64_t> keys_of(Map const &map) {
        std::vector<std::uint64_t> keys;
        keys.reserve(map.size());
        for (auto const &[key, value] : map) {
            keys.push_back(key);
        }
        return keys;
    }

    template<typename Map>
    std::vector<std::uint64_t> sorted_keys_of(Map const &map) {
        auto keys = keys_of(map);
        std::ranges::sort(keys);
        return keys;
    }

    /**
     * The buckets of a table with `buckets.size()` buckets, and which key is in which bucket. A key goes into the first
     * bucket of its probe sequence (its hash masked with the bucket count minus one, then quadratic probing) that holds
     * no key. A rebuild inserts the keys again, in bucket order, into buckets without keys.
     */
    template<std::uint64_t nb_homes>
    struct bucket_model {
        std::vector<std::optional<std::uint64_t>> buckets = std::vector<std::optional<std::uint64_t>>(64);

        void insert(std::uint64_t key) {
            std::size_t const mask = buckets.size() - 1;
            std::size_t ibucket = oracle_hash<nb_homes>::hash_of(key) & mask;
            for (std::size_t probe = 0; buckets[ibucket].has_value();) {
                ++probe;
                ibucket = (ibucket + probe) & mask;
            }
            buckets[ibucket] = key;
        }

        void erase(std::uint64_t key) {
            auto const it = std::ranges::find(buckets, std::optional<std::uint64_t>(key));
            REQUIRE(it != buckets.end());
            it->reset();
        }

        void clear() {
            std::ranges::fill(buckets, std::nullopt);
        }

        void rebuild(std::size_t nb_buckets) {
            auto const old_keys = keys();
            buckets.assign(std::max<std::size_t>(nb_buckets, 64), std::nullopt);
            for (auto const key : old_keys) {
                insert(key);
            }
        }

        [[nodiscard]] std::vector<std::uint64_t> keys() const {
            std::vector<std::uint64_t> result;
            for (auto const &bucket : buckets) {
                if (bucket.has_value()) {
                    result.push_back(*bucket);
                }
            }
            return result;
        }

        /// the key after `key` in bucket order, if there is one
        [[nodiscard]] std::optional<std::uint64_t> next_after(std::uint64_t key) const {
            auto it = std::ranges::find(buckets, std::optional<std::uint64_t>(key));
            for (++it; it != buckets.end(); ++it) {
                if (it->has_value()) {
                    return *it;
                }
            }
            return std::nullopt;
        }
    };

    /**
     * A map and the model of its buckets. `rebuilds` counts the rebuilds that the hash calls showed. `in_order` is
     * false once the map did not follow its model.
     */
    template<std::uint64_t nb_homes, std::size_t inline_capacity>
    struct modeled_map {
        oracle_map<nb_homes, inline_capacity> map;
        bucket_model<nb_homes> model;
        std::size_t rebuilds = 0;
        bool in_order = true;

        /**
         * Inserts `key`, or finds it. The insertion of a new key hashes it once, and the elements before it if the
         * buckets are rebuilt first.
         * @return true if `key` was inserted
         */
        bool insert(std::uint64_t key) {
            std::size_t const size_before = map.size();
            hash_calls = 0;
            auto const [it, inserted] = map.try_emplace(key, key * 10);
            in_order = in_order && it->first == key && it->second == key * 10;
            if (inserted && size_before > 0 && hash_calls == 1 + size_before) {
                ++rebuilds;
                model.rebuild(map.bucket_count());
            } else {
                in_order = in_order && hash_calls == 1;
            }
            if (inserted) {
                model.insert(key);
            }
            return inserted;
        }

        std::size_t erase(std::uint64_t key) {
            std::size_t const erased = map.erase(key);
            if (erased == 1) {
                model.erase(key);
            }
            return erased;
        }

        /// erases the element at `position` of the iteration, and checks the iterator that `erase` returns
        void erase_at(std::size_t position) {
            auto it = map.begin();
            std::advance(it, static_cast<std::ptrdiff_t>(position));
            std::uint64_t const key = it->first;
            auto const expected_next = model.next_after(key);
            auto const next = map.erase(it);
            model.erase(key);
            in_order = in_order && (next == map.end()) == !expected_next.has_value();
            in_order = in_order && (next == map.end() || next->first == *expected_next);
        }

        void clear() {
            map.clear();
            model.clear();
        }

        /**
         * True if the map holds the keys of its model in the order of the model and finds each of them (only for up to
         * 64 elements, to keep the tests fast).
         */
        [[nodiscard]] bool follows_model() {
            auto const keys = model.keys();
            in_order = in_order && keys_of(map) == keys;
            if (keys.size() <= 64) {
                for (auto const key : keys) {
                    auto const it = map.find(key);
                    in_order = in_order && it != map.end() && it->second == key * 10;
                }
            }
            return in_order;
        }
    };

    /**
     * Runs `steps` random operations on a map with the inline capacity `inline_capacity` and on the same map type with
     * the inline capacity 0, and checks after every operation that each map follows its model and that both hold the
     * same elements. The size moves around `target_size` and never exceeds `max_size`.
     * @return the number of rebuilds of the map with the inline capacity, so that a test can check that clean-ups
     * happened
     */
    template<std::uint64_t nb_homes, std::size_t inline_capacity>
    std::size_t compare_runs(std::uint64_t seed, std::size_t steps, std::size_t target_size, std::size_t max_size) {
        modeled_map<nb_homes, inline_capacity> small;
        modeled_map<nb_homes, 0> table;
        ankerl::nanobench::Rng rng(seed);
        std::uint64_t next_key = 1;

        bool all_equal = true;
        std::size_t first_difference = steps;
        for (std::size_t step = 0; step < steps && all_equal; ++step) {
            auto const roll = rng.bounded(100);
            bool const grows = small.map.size() < max_size && (small.map.size() < target_size ? roll < 65 : roll < 35);
            auto const operation = grows ? 0 : 45 + rng.bounded(55);
            if (operation < 45) {
                // a new key, or a key that may be there
                std::uint64_t const key = rng.bounded(4) == 0 && next_key > 1
                    ? 1 + rng.bounded(static_cast<std::uint32_t>(next_key - 1))
                    : next_key++;
                all_equal = all_equal && small.insert(key) == table.insert(key);
            } else if (operation < 75 && !small.map.empty()) {
                // erase an element by key, or a key that is not there
                std::uint64_t key = 0;
                if (rng.bounded(5) == 0) {
                    key = next_key + 1000;
                } else {
                    auto it = small.map.begin();
                    std::advance(it, rng.bounded(static_cast<std::uint32_t>(small.map.size())));
                    key = it->first;
                }
                all_equal = all_equal && small.erase(key) == table.erase(key);
            } else if (operation < 95 && !small.map.empty()) {
                // erase the element at a position of the iteration, and the same element of the other map
                auto const position = rng.bounded(static_cast<std::uint32_t>(small.map.size()));
                auto it = small.map.begin();
                std::advance(it, position);
                auto const table_it = table.map.find(it->first);
                all_equal = all_equal && table_it != table.map.end();
                if (all_equal) {
                    auto const table_position = static_cast<std::size_t>(std::distance(table.map.begin(), table_it));
                    small.erase_at(position);
                    table.erase_at(table_position);
                }
            } else if (operation < 97) {
                // a copy, a move and a move assignment of both
                auto copy = small.map;
                auto table_copy = table.map;
                auto moved = std::move(copy);
                auto table_moved = std::move(table_copy);
                small.map = std::move(moved);
                table.map = std::move(table_moved);
            } else if (operation < 98 && small.map.size() < 3) {
                small.clear();
                table.clear();
            }

            all_equal = all_equal
                && small.follows_model()
                && table.follows_model()
                && sorted_keys_of(small.map) == sorted_keys_of(table.map);
            if (!all_equal) {
                first_difference = step;
            }
        }
        CAPTURE(first_difference);
        CHECK(small.in_order);
        CHECK(table.in_order);
        CHECK(all_equal);

        // both maps find the same keys, and every erased key in neither
        bool all_found = small.map.size() == table.map.size();
        for (std::uint64_t key = 1; key < next_key; ++key) {
            all_found = all_found && small.map.contains(key) == table.map.contains(key);
        }
        CHECK(all_found);
        return small.rebuilds;
    }

    /**
     * Runs `cycles` cycles on a map with the inline capacity `inline_capacity` and on the same map type with the inline
     * capacity 0, with the checks of `compare_runs`. A cycle churns at the inline capacity, so that the inline group
     * collects deleted buckets, grows to `inline_capacity + extra` elements, so that the inline group moves into an
     * allocated group with its deleted buckets, looks up and inserts keys that are there, erases all elements, and
     * rehashes to 0 buckets, so that the map is inline again.
     */
    template<std::uint64_t nb_homes, std::size_t inline_capacity>
    void compare_cycles(std::uint64_t seed, std::size_t cycles, std::size_t churn_steps, std::size_t extra) {
        modeled_map<nb_homes, inline_capacity> small;
        modeled_map<nb_homes, 0> table;
        ankerl::nanobench::Rng rng(seed);
        std::uint64_t next_key = 1;
        bool all_equal = true;
        std::size_t first_difference_cycle = cycles;

        auto const check = [&] {
            all_equal = all_equal
                && small.follows_model()
                && table.follows_model()
                && sorted_keys_of(small.map) == sorted_keys_of(table.map);
        };
        auto const insert = [&](std::uint64_t key) {
            all_equal = all_equal && small.insert(key) == table.insert(key);
            check();
        };
        auto const erase_random = [&] {
            auto it = small.map.begin();
            std::advance(it, rng.bounded(static_cast<std::uint32_t>(small.map.size())));
            std::uint64_t const key = it->first;
            all_equal = all_equal && small.erase(key) == table.erase(key);
            check();
        };

        for (std::size_t cycle = 0; cycle < cycles && all_equal; ++cycle) {
            while (small.map.size() < inline_capacity) {
                insert(next_key++);
            }
            for (std::size_t step = 0; step < churn_steps; ++step) {
                erase_random();
                insert(next_key++);
            }
            for (std::size_t i = 0; i < extra; ++i) {
                insert(next_key++);
            }
            // keys that are there: no insertion, the same element
            for (auto const key : table.model.keys()) {
                all_equal = all_equal && !small.insert(key) && small.map.at(key) == key * 10;
            }
            check();
            while (!small.map.empty() && all_equal) {
                erase_random();
            }
            all_equal = all_equal && table.map.empty();
            small.map.rehash(0);
            table.map.rehash(0);
            small.model.rebuild(0);
            table.model.rebuild(0);
            all_equal = all_equal && small.map.bucket_count() == 0 && table.map.bucket_count() == 0;
            if (!all_equal) {
                first_difference_cycle = cycle;
            }
        }
        CAPTURE(first_difference_cycle);
        CHECK(small.in_order);
        CHECK(table.in_order);
        CHECK(all_equal);
    }
} // namespace

TEST_CASE("an inline map puts its elements into the buckets of group 0 of a table of 64 buckets, small sizes") {
    // the size stays within the inline capacity, so the inline group collects deleted buckets and cleans up
    for (std::uint64_t seed = 1; seed <= 4; ++seed) {
        CAPTURE(seed);
        static_cast<void>(compare_runs<64, 1>(seed, 4000, 1, 1));
        static_cast<void>(compare_runs<8, 8>(seed, 4000, 6, 8));
        // With 3 home buckets an insertion almost always reuses a deleted bucket, so the inline group rarely cleans up.
        static_cast<void>(compare_runs<3, 32>(seed, 4000, 24, 32));
        // Here the erasures leave deleted buckets while an element is outside its home bucket, at different places, and
        // the inline group reaches its clean-up threshold. Each clean-up is a rebuild.
        CHECK(compare_runs<64, 4>(seed, 4000, 3, 4) > 0);
        CHECK(compare_runs<64, 8>(seed, 4000, 6, 8) > 0);
        CHECK(compare_runs<64, 32>(seed, 4000, 24, 32) > 0);
        CHECK(compare_runs<8, 4>(seed, 4000, 3, 4) > 0);
        CHECK(compare_runs<16, 8>(seed, 4000, 6, 8) > 0);
    }
}

TEST_CASE("an inline map moves into an allocated group with its deleted buckets, in cycles") {
    for (std::uint64_t seed = 21; seed <= 24; ++seed) {
        CAPTURE(seed);
        compare_cycles<8, 4>(seed, 200, 20, 6);
        compare_cycles<16, 8>(seed, 200, 30, 10);
        compare_cycles<64, 4>(seed, 200, 60, 4);
        compare_cycles<64, 32>(seed, 50, 40, 20);
    }
}

TEST_CASE("an inline map puts its elements into the buckets of a table, across the move out") {
    // the size moves around the inline capacity and far beyond it, so the inline group moves out with deleted buckets
    for (std::uint64_t seed = 11; seed <= 14; ++seed) {
        CAPTURE(seed);
        static_cast<void>(compare_runs<16, 4>(seed, 6000, 6, 12));
        static_cast<void>(compare_runs<64, 8>(seed, 6000, 9, 40));
        static_cast<void>(compare_runs<1024, 32>(seed, 6000, 300, 400));
    }
}
