#ifndef DICE_UNORDERED_SPARSE_BENCHMARKS_COMMON_HPP
#define DICE_UNORDERED_SPARSE_BENCHMARKS_COMMON_HPP

#include <dice/unordered_sparse.hpp>

#include <ankerl/unordered_dense.h>
#include <nanobench.h>

#include <chrono>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <memory>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>

/**
 * What the benchmarks share: the quick mode, the nanobench settings, the geometric mean and the
 * container configurations.
 */
namespace dice::unordered_sparse::bench {

    /**
     * True if the environment variable `DICE_SPARSE_MAP_BENCH_QUICK` is set to anything but empty
     * or `0`. The quick mode shrinks sizes and runs every benchmark once, so that the whole binary
     * finishes in well under a minute. It is a smoke test: its timings mean nothing.
     */
    [[nodiscard]] inline bool quick_mode() {
        static bool const quick = [] {
            char const *value = std::getenv("DICE_SPARSE_MAP_BENCH_QUICK");
            return value != nullptr && *value != '\0' && std::string_view{value} != "0";
        }();
        return quick;
    }

    /**
     * Calls `f.template operator()<Quick>()` in the quick mode and `f.template operator()<Full>()`
     * otherwise. `Full` and `Quick` hold the sizes of a benchmark and the checksums for them.
     */
    template<typename Full, typename Quick, typename F>
    void with_sizes(F &&f) {
        if (quick_mode()) {
            std::forward<F>(f).template operator()<Quick>();
        } else {
            std::forward<F>(f).template operator()<Full>();
        }
    }

    /**
     * Sets how long nanobench measures. In the quick mode every benchmark runs exactly once.
     * Otherwise an epoch lasts at least 100 ms, as in the benchmarks of ankerl::unordered_dense.
     */
    inline void configure(ankerl::nanobench::Bench &bench) {
        if (quick_mode()) {
            bench.epochs(1).epochIterations(1).warmup(0);
        } else {
            bench.minEpochTime(std::chrono::milliseconds{100});
        }
    }

    /**
     * Geometric mean of the median times of all benchmarks run on `bench`, in seconds.
     *
     * Ported from ankerl::unordered_dense (test/app/geomean.h, MIT license).
     */
    [[nodiscard]] inline double geomean_elapsed(ankerl::nanobench::Bench const &bench) {
        double sum = 0.0;
        for (auto const &result : bench.results()) {
            sum += std::log(result.median(ankerl::nanobench::Result::Measure::elapsed));
        }
        return std::exp(sum / static_cast<double>(bench.results().size()));
    }

    /**
     * The hash of `sparse_map`, `sparse_set` and the `ankerl::unordered_dense` containers. With the
     * same hash, a comparison of the two open addressing tables shows the tables and not the
     * hashes. It mixes well, like `dice::hash`, which tentris uses with `sparse_map`.
     * `std::unordered_map` and `std::unordered_set` keep `std::hash`.
     */
    template<typename Key>
    using table_hash = ankerl::unordered_dense::hash<Key>;

    /**
     * `sparse_map` and `sparse_set` with the given sparsity and an allocator made from the template
     * `Alloc`.
     */
    template<sparsity Sparsity, template<typename> typename Alloc = std::allocator>
    struct sparse_family {
        template<typename Key, typename T>
        using map = ::dice::sparse_map<Key,
                                       T,
                                       table_hash<Key>,
                                       std::equal_to<Key>,
                                       Alloc<std::pair<Key, T>>,
                                       Sparsity>;

        template<typename Key>
        using set = ::dice::sparse_set<Key,
                                       table_hash<Key>,
                                       std::equal_to<Key>,
                                       Alloc<Key>,
                                       Sparsity>;
    };

    template<template<typename> typename Alloc = std::allocator>
    struct unordered_dense_family {
        template<typename Key, typename T>
        using map = ankerl::unordered_dense::map<Key, T, table_hash<Key>, std::equal_to<Key>, Alloc<std::pair<Key, T>>>;

        template<typename Key>
        using set = ankerl::unordered_dense::set<Key, table_hash<Key>, std::equal_to<Key>, Alloc<Key>>;
    };

    template<template<typename> typename Alloc = std::allocator>
    struct std_family {
        template<typename Key, typename T>
        using map = std::unordered_map<Key, T, std::hash<Key>, std::equal_to<Key>, Alloc<std::pair<Key const, T>>>;

        template<typename Key>
        using set = std::unordered_set<Key, std::hash<Key>, std::equal_to<Key>, Alloc<Key>>;
    };

    using sparse_high = sparse_family<sparsity::high>;
    using sparse_medium = sparse_family<sparsity::medium>;
    using sparse_low = sparse_family<sparsity::low>;

    /**
     * Makes every map with `Map{}`. A source of maps has a member template `make<Map>()`.
     */
    struct default_source {
        template<typename Map>
        [[nodiscard]] Map make() const {
            return Map{};
        }
    };

}  // namespace dice::unordered_sparse::bench

#endif  // DICE_UNORDERED_SPARSE_BENCHMARKS_COMMON_HPP
