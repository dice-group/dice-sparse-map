#ifndef DICE_UNORDERED_SPARSE_BENCHMARKS_MAP_BENCHMARKS_HPP
#define DICE_UNORDERED_SPARSE_BENCHMARKS_MAP_BENCHMARKS_HPP

#include "common.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <string>
#include <string_view>

/**
 * The map benchmarks that run for many container configurations, with the plain allocator and
 * with fancy pointers: the quick overall score, the lookups that all hit or all miss, and the passes
 * over all elements of a large map.
 *
 * The quick overall score and the lookups are ported from ankerl::unordered_dense
 * (test/bench/quick_overall_map.cpp, MIT license).
 */
namespace dice::unordered_sparse::bench {

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
        CHECK(use_small_maps<Map>(make_map) == 6);
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

    /**
     * The scored workloads for `uint64_t -> throwing_move_value`, a mapped type whose move constructor can throw.
     * Not part of the score. `Family::map<Key, T>` is the map type, `source` makes the empty maps.
     */
    template<typename Family, typename Source = default_source>
    void throwing_move(std::string_view container, Source const &source = Source{}) {
        with_sizes<overall_full, overall_quick>([&]<typename Sizes>() {
            ankerl::nanobench::Bench bench;
            bench.title(std::format("throwing_move {}", container));
            configure(bench);
            bench_all<typename Family::template map<std::uint64_t, throwing_move_value>, Sizes>(
                bench,
                std::format("{} uint64_t -> throwing_move_value", container),
                source);
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

            CHECK(use_small_maps<int_map>([&source] {
                      return source.template make<int_map>();
                  })
                  == 6);
            CHECK(use_small_maps<str_map>([&source] {
                      return source.template make<str_map>();
                  })
                  == 6);

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

    struct iterate_large_full {
        static constexpr std::array<std::size_t, 2> sizes{1000000, 10000000};
    };

    struct iterate_large_quick {
        static constexpr std::array<std::size_t, 2> sizes{1000, 20000};
    };

    /**
     * The sum of the mapped values of `map`, with `map.for_each` if `use_for_each`, and otherwise with a loop over
     * the iterators.
     */
    template<bool use_for_each, typename Map>
    [[nodiscard]] std::uint64_t sum_mapped(Map const &map) {
        std::uint64_t sum = 0;
        if constexpr (use_for_each) {
            map.for_each([&sum](auto const &element) {
                sum += element.second;
            });
        } else {
            for (auto const &element : map) {
                sum += element.second;
            }
        }
        return sum;
    }

    /**
     * A map from `make_map` with `num_elements` random keys. The value of a key is the number of keys inserted
     * before it, so the values are 0 to `num_elements - 1`.
     */
    template<typename Map, typename MakeMap>
    [[nodiscard]] Map filled_map(std::size_t num_elements, MakeMap const &make_map) {
        tame_allocator();
        ankerl::nanobench::Rng rng(271828);
        Map map = make_map();
        while (map.size() < num_elements) {
            map.try_emplace(key_for<Map>(rng() & ~never_inserted), map.size());
        }
        return map;
    }

    /**
     * Builds a map of `num_elements` entries, then times one pass over all its elements with the iterators and, if
     * the map has it, with `for_each`. Each pass sums the mapped values.
     */
    template<typename Map, typename Source>
    void iterate_rows(ankerl::nanobench::Bench &bench, std::string_view name, std::size_t num_elements, Source const &source) {
        auto const map = filled_map<Map>(num_elements, [&source] {
            return source.template make<Map>();
        });
        auto const expected = static_cast<std::uint64_t>(num_elements) * (num_elements - 1) / 2;
        bench.batch(num_elements);
        bench.run(std::format("{} {} entries, iterators", name, num_elements), [&] {
            CHECK(sum_mapped<false>(map) == expected);
        });
        if constexpr (HasForEach<Map>) {
            bench.run(std::format("{} {} entries, for_each", name, num_elements), [&] {
                CHECK(sum_mapped<true>(map) == expected);
            });
        }
    }

    /**
     * A pass over all elements of a map of 1 million and of 10 million entries, `uint64_t -> uint64_t` and
     * `std::string -> uint64_t`, with a loop over the iterators and, if the map has it, with `for_each`. The maps are
     * built before the timed runs, so that the runs time the pass and not a fill. nanobench reports the time per
     * element. `Family::map<Key, T>` is the map type, `source` makes the empty maps.
     */
    template<typename Family, typename Source = default_source>
    void iterate_large(std::string_view container, Source const &source = Source{}) {
        with_sizes<iterate_large_full, iterate_large_quick>([&]<typename Sizes>() {
            ankerl::nanobench::Bench bench;
            bench.title(std::format("iterate {}", container)).unit("element");
            configure(bench);
            for (auto const num_elements : Sizes::sizes) {
                iterate_rows<typename Family::template map<std::uint64_t, std::uint64_t>>(
                    bench,
                    std::format("{} uint64_t -> uint64_t", container),
                    num_elements,
                    source);
                iterate_rows<typename Family::template map<std::string, std::uint64_t>>(
                    bench,
                    std::format("{} std::string -> uint64_t", container),
                    num_elements,
                    source);
            }
        });
    }

}  // namespace dice::unordered_sparse::bench

#endif  // DICE_UNORDERED_SPARSE_BENCHMARKS_MAP_BENCHMARKS_HPP
