#include "common.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <string_view>

using namespace dice::unordered_sparse::bench;

/*
 * The set versions of build, find_50 and churn for `uint64_t` keys.
 */

namespace {

    struct set_full {
        static constexpr std::size_t build_elements = 200000;
        static constexpr std::size_t find_50_steps = 100000;
        static constexpr std::size_t find_50_checksum = 17249707481313650169U;
        static constexpr std::size_t churn_elements = 50000;
        static constexpr std::size_t churn_checksum = 3852085512878285291U;
    };

    struct set_quick {
        static constexpr std::size_t build_elements = 20000;
        static constexpr std::size_t find_50_steps = 10000;
        static constexpr std::size_t find_50_checksum = 10982421180883016863U;
        static constexpr std::size_t churn_elements = 5000;
        static constexpr std::size_t churn_checksum = 13697434006604101416U;
    };

    template<typename Family>
    void set_workloads(std::string_view container) {
        with_sizes<set_full, set_quick>([&]<typename Sizes>() {
            using set_t = typename Family::template set<std::uint64_t>;

            ankerl::nanobench::Bench bench;
            bench.title(std::format("set {}", container));
            configure(bench);
            bench.run(std::format("{} uint64_t build from empty", container), [&] {
                CHECK(build_set<set_t, Sizes::build_elements>() == Sizes::build_elements);
            });
            bench.run(std::format("{} uint64_t churn at a fixed size", container), [&] {
                CHECK(churn_set<set_t, Sizes::churn_elements>() == Sizes::churn_checksum);
            });
            bench.run(std::format("{} uint64_t 50% probability to find", container), [&] {
                CHECK(find_50_set<set_t, Sizes::find_50_steps>() == Sizes::find_50_checksum);
            });
            std::cout << std::format("set {}: geomean {:.4f} ms\n", container, geomean_elapsed(bench) * 1e3);
        });
    }

}  // namespace

TEST_CASE("set sparse_set high") {
    set_workloads<sparse_high>("sparse_set high");
}

TEST_CASE("set sparse_set medium") {
    set_workloads<sparse_medium>("sparse_set medium");
}

TEST_CASE("set sparse_set low") {
    set_workloads<sparse_low>("sparse_set low");
}

TEST_CASE("set unordered_dense") {
    set_workloads<unordered_dense_family<>>("unordered_dense::set");
}

TEST_CASE("set std::unordered_set") {
    set_workloads<std_family<>>("std::unordered_set");
}
