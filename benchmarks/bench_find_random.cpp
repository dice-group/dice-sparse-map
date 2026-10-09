#include "common.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <array>
#include <chrono>
#include <cstddef>
#include <format>
#include <iostream>
#include <string_view>

using namespace dice::sparse_map::bench;

/*
 * Random finds with a success rate of 0%, 25%, 50%, 75% and 100% in a map that grows while it is
 * searched. Every iteration inserts four entries, some of them random and some from a second rng,
 * then does 2000 finds that replay the second rng from its start. Which of the four inserts are
 * random is shuffled for every iteration.
 *
 * Ported from ankerl::unordered_dense (test/bench/find_random.cpp, MIT license).
 */

namespace {

    /**
     * The sizes of upstream. Upstream's checksums for 25% and 50% are one lower: they come from
     * nanobench 4.3.7, whose `Rng::shuffle` draws other positions than the one of nanobench 4.3.11.
     */
    struct find_random_full {
        static constexpr std::size_t num_inserts = 200000;
        static constexpr std::size_t num_finds_per_insert = 500;
        static constexpr std::array<std::size_t, 5> checksums{200000, 25198621, 50197241, 75195862, 100194482};
    };

    struct find_random_quick {
        static constexpr std::size_t num_inserts = 20000;
        static constexpr std::size_t num_finds_per_insert = 50;
        static constexpr std::array<std::size_t, 5> checksums{20000, 269892, 519782, 769675, 1019566};
    };

    template<typename Map, typename Sizes>
    void bench_find(std::string_view container) {
        static constexpr std::size_t num_total = 4;
        static constexpr std::size_t num_finds_per_iter = Sizes::num_finds_per_insert * num_total;

        CHECK(use_small_maps<Map>() == 6);

        auto total = std::chrono::steady_clock::duration();
        for (std::size_t num_found = 0; num_found < 5; ++num_found) {
            auto rng = ankerl::nanobench::Rng(123);
            std::size_t checksum = 0;

            auto insert_random = std::array<bool, num_total>();
            insert_random.fill(true);
            for (std::size_t i = 0; i < num_found; ++i) {
                insert_random[i] = false;
            }

            auto another_unrelated_rng = ankerl::nanobench::Rng(987654321);
            auto const another_unrelated_rng_initial_state = another_unrelated_rng.state();
            auto find_rng = ankerl::nanobench::Rng(another_unrelated_rng_initial_state);

            {
                Map map;
                std::size_t i = 0;
                std::size_t find_count = 0;
                auto const before = std::chrono::steady_clock::now();
                do {
                    // insert num_total entries: some random, some from another_unrelated_rng
                    rng.shuffle(insert_random);
                    for (bool const is_random_to_insert : insert_random) {
                        auto const val = another_unrelated_rng();
                        if (is_random_to_insert) {
                            map[static_cast<std::size_t>(rng())] = static_cast<std::size_t>(1);
                        } else {
                            map[static_cast<std::size_t>(val)] = static_cast<std::size_t>(1);
                        }
                        ++i;
                    }

                    // the actual benchmark code which should be as fast as possible
                    for (std::size_t j = 0; j < num_finds_per_iter; ++j) {
                        if (++find_count > i) {
                            find_count = 0;
                            find_rng = ankerl::nanobench::Rng(another_unrelated_rng_initial_state);
                        }
                        auto it = map.find(static_cast<std::size_t>(find_rng()));
                        if (it != map.end()) {
                            checksum += it->second;
                        }
                    }
                } while (i < Sizes::num_inserts);
                checksum += map.size();
                auto const after = std::chrono::steady_clock::now();
                total += after - before;
                std::cout << std::format(
                    "{:.6f}s random find {}% success {}\n",
                    std::chrono::duration<double>(after - before).count(),
                    num_found * 100 / num_total,
                    container
                );
            }
            REQUIRE(checksum == Sizes::checksums[num_found]);
        }
        std::cout << std::format("{:.6f}s total {}\n", std::chrono::duration<double>(total).count(), container);
    }

    template<typename Family>
    void find_random(std::string_view container) {
        with_sizes<find_random_full, find_random_quick>([&]<typename Sizes>() {
            bench_find<typename Family::template map<std::size_t, std::size_t>, Sizes>(container);
        });
    }

} // namespace

TEST_CASE("find_random sparse_map high") {
    find_random<sparse_high>("sparse_map high");
}

TEST_CASE("find_random sparse_map medium") {
    find_random<sparse_medium>("sparse_map medium");
}

TEST_CASE("find_random sparse_map low") {
    find_random<sparse_low>("sparse_map low");
}

TEST_CASE("find_random unordered_dense") {
    find_random<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("find_random std::unordered_map") {
    find_random<std_family<>>("std::unordered_map");
}
