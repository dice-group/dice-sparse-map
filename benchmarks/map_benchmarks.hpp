#ifndef DICE_SPARSE_MAP_BENCHMARKS_MAP_BENCHMARKS_HPP
#define DICE_SPARSE_MAP_BENCHMARKS_MAP_BENCHMARKS_HPP

#include "common.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <string>
#include <string_view>

/**
 * The map benchmarks that run for many container configurations, with the plain allocator and
 * with fancy pointers: the quick overall score and the lookups that all hit or all miss.
 *
 * Ported from ankerl::unordered_dense (test/bench/quick_overall_map.cpp, MIT license).
 */
namespace dice::sparse_map::bench {

    /**
     * The sizes of the scored workloads and the checksums for them, as upstream has them.
     */
    struct overall_full {
        static constexpr std::size_t insert_erase_range = 20000;
        static constexpr std::size_t insert_erase_erased = 1994641;
        static constexpr std::size_t insert_erase_size = 9987;
        static constexpr std::size_t iterate_elements = 5000;
        static constexpr std::size_t iterate_checksum = 62282755409U;
        static constexpr std::size_t build_elements = 200000;
        static constexpr std::size_t churn_elements = 50000;
        static constexpr std::size_t churn_checksum = 13688565634U;
        static constexpr std::size_t find_50_steps = 100000;
        static constexpr std::size_t find_50_checksum = 124865472559U;
    };

    /**
     * The sizes for the quick mode, about a tenth of the full ones, and the checksums every correct
     * map gives for them. `iterate_elements` is large enough that some keys repeat, so that the
     * checksum also covers the overwrite of a mapped value. After a change of a size, take the new
     * checksum from a run with `std::unordered_map`.
     */
    struct overall_quick {
        static constexpr std::size_t insert_erase_range = 2000;
        static constexpr std::size_t insert_erase_erased = 199357;
        static constexpr std::size_t insert_erase_size = 1013;
        static constexpr std::size_t iterate_elements = 2000;
        static constexpr std::size_t iterate_checksum = 3996077775U;
        static constexpr std::size_t build_elements = 20000;
        static constexpr std::size_t churn_elements = 5000;
        static constexpr std::size_t churn_checksum = 136240046;
        static constexpr std::size_t find_50_steps = 10000;
        static constexpr std::size_t find_50_checksum = 1247109603;
    };

    /**
     * Runs the five scored workloads for `Map` on `bench` and checks their results. `source` makes
     * the empty maps (see `default_source`).
     */
    template<typename Map, typename Sizes, typename Source>
    void bench_all(ankerl::nanobench::Bench &bench, std::string_view name, Source const &source) {
        auto const make_map = [&source] {
            return source.template make<Map>();
        };
        bench.run(std::format("{} iterate while adding then removing", name), [&] {
            CHECK(iterate<Map, Sizes::iterate_elements>(make_map) == Sizes::iterate_checksum);
        });
        bench.run(std::format("{} random insert erase", name), [&] {
            auto const r = insert_erase<Map, Sizes::insert_erase_range>(make_map);
            CHECK(r.erased == Sizes::insert_erase_erased);
            CHECK(r.size == Sizes::insert_erase_size);
        });
        bench.run(std::format("{} build from empty", name), [&] {
            CHECK(build<Map, Sizes::build_elements>(make_map) == Sizes::build_elements);
        });
        bench.run(std::format("{} churn at a fixed size", name), [&] {
            CHECK(churn<Map, Sizes::churn_elements>(make_map) == Sizes::churn_checksum);
        });
        bench.run(std::format("{} 50% probability to find", name), [&] {
            CHECK(find_50<Map, Sizes::find_50_steps>(make_map) == Sizes::find_50_checksum);
        });
    }

    /**
     * The quick overall score of one container configuration: the scored workloads for its three
     * map types `uint64_t -> size_t`, `std::string -> size_t` and `uint64_t -> big_value` on one
     * `Bench`, then the geometric mean over all of them. `Family::map<Key, T>` is the map type,
     * `source` makes the empty maps.
     */
    template<typename Family, typename Source = default_source>
    void quick_overall(std::string_view container, Source const &source = Source{}) {
        with_sizes<overall_full, overall_quick>([&]<typename Sizes>() {
            ankerl::nanobench::Bench bench;
            bench.title(std::format("quick_overall {}", container));
            configure(bench);
            bench_all<typename Family::template map<std::uint64_t, std::size_t>, Sizes>(
                bench,
                std::format("{} uint64_t -> size_t", container),
                source);
            bench_all<typename Family::template map<std::string, std::size_t>, Sizes>(
                bench,
                std::format("{} std::string -> size_t", container),
                source);
            // A 64 byte mapped value. See `big_value` for what the other two cannot show.
            bench_all<typename Family::template map<std::uint64_t, big_value>, Sizes>(
                bench,
                std::format("{} uint64_t -> big_value", container),
                source);
            std::cout << std::format("quick_overall {}: geomean {:.4f} ms\n", container, geomean_elapsed(bench) * 1e3);
        });
    }

    struct find_all_full {
        static constexpr std::size_t table_elements = 50000;
        static constexpr std::size_t lookups = 1000000;
    };

    struct find_all_quick {
        static constexpr std::size_t table_elements = 5000;
        static constexpr std::size_t lookups = 100000;
    };

    /**
     * The two ends of what the hit rate does to the probe: lookups that all hit and lookups that
     * all miss, in a map of a fixed size, for `uint64_t` and `std::string` keys. Not part of the
     * score. The maps are built before the timed runs, so that the runs time the lookups and not a
     * fill (see `lookup_table`).
     */
    template<typename Family, typename Source = default_source>
    void find_all_hits_or_misses(std::string_view container, Source const &source = Source{}) {
        with_sizes<find_all_full, find_all_quick>([&]<typename Sizes>() {
            using int_map = typename Family::template map<std::uint64_t, std::size_t>;
            using str_map = typename Family::template map<std::string, std::size_t>;

            ankerl::nanobench::Bench bench;
            bench.title(std::format("find_all {}", container));
            configure(bench);
            auto ints = lookup_table<int_map>(Sizes::table_elements, [&source] {
                return source.template make<int_map>();
            });
            auto strings = lookup_table<str_map>(Sizes::table_elements, [&source] {
                return source.template make<str_map>();
            });
            bench.run(std::format("{} uint64_t -> size_t all hits", container), [&] {
                ankerl::nanobench::doNotOptimizeAway(find_all<true, Sizes::lookups>(&ints));
            });
            bench.run(std::format("{} uint64_t -> size_t no hits", container), [&] {
                CHECK(find_all<false, Sizes::lookups>(&ints) == 0);
            });
            bench.run(std::format("{} std::string -> size_t all hits", container), [&] {
                ankerl::nanobench::doNotOptimizeAway(find_all<true, Sizes::lookups>(&strings));
            });
            bench.run(std::format("{} std::string -> size_t no hits", container), [&] {
                CHECK(find_all<false, Sizes::lookups>(&strings) == 0);
            });
        });
    }

}  // namespace dice::sparse_map::bench

#endif  // DICE_SPARSE_MAP_BENCHMARKS_MAP_BENCHMARKS_HPP
