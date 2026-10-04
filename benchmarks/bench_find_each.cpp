#include "common.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <vector>

using namespace dice::unordered_sparse::bench;

/*
 * Many lookups in a row, the pattern of a join: a batch of keys looked up one by one with `find`, and, for a map
 * that has it, with `find_each`. The map has 50000 entries or 4 million entries. With 50000 entries, `sparse_map` has
 * 0.9 MB of groups and elements with `uint64_t` keys, which fit into the L2 cache, and 2.1 MB with `std::string` keys,
 * plus 2.4 MB that the keys own. With 4 million entries it has about 68 MB with `uint64_t` keys and 164 MB with
 * `std::string` keys, more than the 36 MB L3 cache of an i9-14900K.
 */

namespace {

    struct find_each_full {
        static constexpr std::size_t small_table = 50000;
        static constexpr std::size_t large_table = 4000000;
        static constexpr std::size_t lookups = 1000000;
    };

    struct find_each_quick {
        static constexpr std::size_t small_table = 5000;
        static constexpr std::size_t large_table = 100000;
        static constexpr std::size_t lookups = 100000;
    };

    template<typename Map>
    concept HasFindEach = requires (Map const &map, std::vector<typename Map::key_type> const &keys) {
        map.find_each(keys, [](auto const &, auto) {
        });
    };

    /**
     * `lookups` random keys that are in the map (`hits`) or that cannot be in it.
     */
    template<typename Map>
    [[nodiscard]] std::vector<typename Map::key_type> batch_keys(lookup_table<Map> &t, bool hits, std::size_t lookups) {
        std::vector<typename Map::key_type> keys;
        keys.reserve(lookups);
        for (std::size_t i = 0; i < lookups; ++i) {
            auto const r = t.rng();
            keys.push_back(key_for<Map>(hits ? t.keys[((r >> 32U) * t.keys.size()) >> 32U] : (r | never_inserted)));
        }
        return keys;
    }

    template<bool use_find_each, typename Map>
    [[nodiscard]] std::size_t find_batch(Map const &map, std::vector<typename Map::key_type> const &keys) {
        std::size_t checksum = 0;
        if constexpr (use_find_each) {
            map.find_each(keys, [&checksum, end = map.end()](auto const &, auto it) {
                if (it != end) {
                    checksum += it->second;
                }
            });
        } else {
            auto const end = map.end();
            for (auto const &key : keys) {
                auto const it = map.find(key);
                if (it != end) {
                    checksum += it->second;
                }
            }
        }
        return checksum;
    }

    template<typename Map>
    void find_each_rows(ankerl::nanobench::Bench &bench, std::string_view name, std::size_t table_size, std::size_t lookups) {
        lookup_table<Map> table(table_size);
        for (bool const hits : {true, false}) {
            auto const keys = batch_keys(table, hits, lookups);
            auto const expected = find_batch<false>(table.map, keys);
            if (!hits) {
                CHECK(expected == 0);
            }
            auto const what = hits ? "all hits" : "no hits";
            bench.run(std::format("{} {} entries {}, find", name, table_size, what), [&] {
                CHECK(find_batch<false>(table.map, keys) == expected);
            });
            if constexpr (HasFindEach<Map>) {
                bench.run(std::format("{} {} entries {}, find_each", name, table_size, what), [&] {
                    CHECK(find_batch<true>(table.map, keys) == expected);
                });
            }
        }
    }

    template<typename Family>
    void find_each_benchmarks(std::string_view container) {
        with_sizes<find_each_full, find_each_quick>([&]<typename Sizes>() {
            using int_map = typename Family::template map<std::uint64_t, std::size_t>;
            using str_map = typename Family::template map<std::string, std::size_t>;
            ankerl::nanobench::Bench bench;
            bench.title(std::format("find_each {}", container));
            configure(bench);
            find_each_rows<int_map>(bench, std::format("{} uint64_t -> size_t", container), Sizes::small_table, Sizes::lookups);
            find_each_rows<int_map>(bench, std::format("{} uint64_t -> size_t", container), Sizes::large_table, Sizes::lookups);
            find_each_rows<str_map>(bench, std::format("{} std::string -> size_t", container), Sizes::small_table, Sizes::lookups);
            find_each_rows<str_map>(bench, std::format("{} std::string -> size_t", container), Sizes::large_table, Sizes::lookups);
        });
    }

}  // namespace

TEST_CASE("find_each sparse_map medium") {
    find_each_benchmarks<sparse_medium>("sparse_map medium");
}

TEST_CASE("find_each unordered_dense") {
    find_each_benchmarks<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("find_each std::unordered_map") {
    find_each_benchmarks<std_family<>>("std::unordered_map");
}
