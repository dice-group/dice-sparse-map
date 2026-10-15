#include "common.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <string_view>
#include <vector>

using namespace dice::sparse_map::bench;

/*
 * Draining a map with `map.erase(map.begin())` until it is empty (see `drain`). Every round builds a map of
 * `uint64_t -> size_t` entries, and only the drain is timed, with `std::chrono::steady_clock`. The line
 * `<seconds>s <container> uint64_t -> size_t drain` gives the median over the rounds, in the form of the
 * `find_random` lines.
 */

namespace {

    struct drain_full {
        static constexpr std::size_t num_elements = 100000;
        static constexpr std::size_t rounds = 5;
    };

    struct drain_quick {
        static constexpr std::size_t num_elements = 10000;
        static constexpr std::size_t rounds = 1;
    };

    template<typename Family>
    void drain_rounds(std::string_view container) {
        with_sizes<drain_full, drain_quick>([&]<typename Sizes>() {
            using map_t = typename Family::template map<std::uint64_t, std::size_t>;
            tame_allocator();

            std::vector<std::chrono::steady_clock::duration> times;
            for (std::size_t round = 0; round < Sizes::rounds; ++round) {
                map_t map;
                for (std::size_t i = 0; i < Sizes::num_elements; ++i) {
                    map[key_for<map_t>(i)] = i;
                }
                REQUIRE(map.size() == Sizes::num_elements);

                auto const before = std::chrono::steady_clock::now();
                auto const sum = drain(map);
                auto const after = std::chrono::steady_clock::now();
                times.push_back(after - before);
                CHECK(sum == Sizes::num_elements * (Sizes::num_elements - 1) / 2);
            }
            std::ranges::sort(times);
            std::cout << std::format(
                "{:.6f}s {} uint64_t -> size_t drain\n",
                std::chrono::duration<double>(times[times.size() / 2]).count(),
                container
            );
        });
    }

} // namespace

TEST_CASE("drain sparse_map high") {
    drain_rounds<sparse_high>("sparse_map high");
}

TEST_CASE("drain sparse_map medium") {
    drain_rounds<sparse_medium>("sparse_map medium");
}

TEST_CASE("drain sparse_map low") {
    drain_rounds<sparse_low>("sparse_map low");
}

TEST_CASE("drain unordered_dense") {
    drain_rounds<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("drain std::unordered_map") {
    drain_rounds<std_family<>>("std::unordered_map");
}
