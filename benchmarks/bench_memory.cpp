#include "common.hpp"
#include "tracking_allocator.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <string>
#include <string_view>

using namespace dice::sparse_map::bench;
namespace sh = dice::sparse_map::sh;

/*
 * Memory per element. A sparse map exists to save memory, so this table matters as much as the
 * speed. Every map is built from empty with distinct keys and a `tracking_allocator`, which sees
 * everything the container allocates itself.
 */

namespace {

    struct memory_full {
        static constexpr std::array<std::size_t, 3> sizes{1000, 100000, 1000000};
    };

    struct memory_quick {
        static constexpr std::array<std::size_t, 3> sizes{1000, 10000, 100000};
    };

    void print_header(std::string_view types) {
        std::cout
            << std::format("\nmemory per element, {}\n\n", types)
            << std::format(
                   "| {:<18} | {:>8} | {:>10} | {:>15} | {:>11} | {:>12} | {:>11} | {:>11} |\n",
                   "container",
                   "N",
                   "bytes/elem",
                   "peak bytes/elem",
                   "live allocs",
                   "bucket_count",
                   "load_factor",
                   "sizeof(map)"
               )
            << std::format(
                   "|{:-<20}|{:->9}:|{:->11}:|{:->16}:|{:->12}:|{:->13}:|{:->12}:|{:->12}:|\n",
                   "",
                   "",
                   "",
                   "",
                   "",
                   "",
                   "",
                   ""
               );
    }

    /**
     * Builds a map of `n` elements and prints one row: the bytes per element after the build, the
     * peak bytes per element during the build, the allocations that are alive after the build, the
     * bucket count and the load factor.
     */
    template<typename Map>
    void measure(std::string_view container, std::size_t n) {
        allocation_stats stats;
        {
            auto map = Map(typename Map::allocator_type(&stats));
            for (std::size_t i = 0; i < n; ++i) {
                map[key_for<Map>(i)] = i;
            }
            REQUIRE(map.size() == n);
            auto const per_element = [n](std::size_t bytes) {
                return static_cast<double>(bytes) / static_cast<double>(n);
            };
            std::cout << std::format(
                "| {:<18} | {:>8} | {:>10.2f} | {:>15.2f} | {:>11} | {:>12} | {:>11.3f} | {:>11} |\n",
                container,
                n,
                per_element(stats.current),
                per_element(stats.peak),
                stats.live_allocations,
                map.bucket_count(),
                map.load_factor(),
                sizeof(Map)
            );
        }
        CHECK(stats.current == 0);
        CHECK(stats.live_allocations == 0);
    }

    template<typename Key>
    void measure_all(std::size_t n) {
        measure<sparse_family<sh::sparsity::high, tracking_allocator>::map<Key, std::uint64_t>>("sparse_map high", n);
        measure<sparse_family<sh::sparsity::medium, tracking_allocator>::map<Key, std::uint64_t>>(
            "sparse_map medium",
            n
        );
        measure<sparse_family<sh::sparsity::low, tracking_allocator>::map<Key, std::uint64_t>>("sparse_map low", n);
        measure<unordered_dense_family<tracking_allocator>::map<Key, std::uint64_t>>("unordered_dense", n);
        measure<std_family<tracking_allocator>::map<Key, std::uint64_t>>("std::unordered_map", n);
    }

    void print_erase_header(std::string_view types) {
        std::cout
            << std::format("\nmemory per live element after erasing every second element, {}\n\n", types)
            << std::format(
                   "| {:<18} | {:>8} | {:>16} | {:>17} | {:>17} | {:>18} |\n",
                   "container",
                   "N",
                   "built bytes/elem",
                   "built live allocs",
                   "erased bytes/elem",
                   "erased live allocs"
               )
            << std::format("|{:-<20}|{:->9}:|{:->17}:|{:->18}:|{:->18}:|{:->19}:|\n", "", "", "", "", "", "");
    }

    /**
     * Builds a map of `n` elements, then erases the elements with an even number, and prints one row: the bytes per
     * element and the live allocations after the build, and the bytes per live element and the live allocations
     * after the erase.
     */
    template<typename Map>
    void measure_erase_half(std::string_view container, std::size_t n) {
        allocation_stats stats;
        {
            auto map = Map(typename Map::allocator_type(&stats));
            for (std::size_t i = 0; i < n; ++i) {
                map[key_for<Map>(i)] = i;
            }
            REQUIRE(map.size() == n);
            auto const built_bytes = stats.current;
            auto const built_allocations = stats.live_allocations;

            for (std::size_t i = 0; i < n; i += 2) {
                map.erase(key_for<Map>(i));
            }
            REQUIRE(map.size() == n / 2);
            std::cout << std::format(
                "| {:<18} | {:>8} | {:>16.2f} | {:>17} | {:>17.2f} | {:>18} |\n",
                container,
                n,
                static_cast<double>(built_bytes) / static_cast<double>(n),
                built_allocations,
                static_cast<double>(stats.current) / static_cast<double>(map.size()),
                stats.live_allocations
            );
        }
        CHECK(stats.current == 0);
        CHECK(stats.live_allocations == 0);
    }

    template<typename T>
    void measure_erase_half_all(std::size_t n) {
        measure_erase_half<sparse_family<sh::sparsity::medium, tracking_allocator>::map<std::uint64_t, T>>(
            "sparse_map medium",
            n
        );
        measure_erase_half<unordered_dense_family<tracking_allocator>::map<std::uint64_t, T>>("unordered_dense", n);
        measure_erase_half<std_family<tracking_allocator>::map<std::uint64_t, T>>("std::unordered_map", n);
    }

} // namespace

TEST_CASE("memory") {
    tame_allocator();
    with_sizes<memory_full, memory_quick>([]<typename Sizes>() {
        print_header("uint64_t -> uint64_t");
        for (auto const n : Sizes::sizes) {
            measure_all<std::uint64_t>(n);
        }

        print_header("std::string -> uint64_t");
        for (auto const n : Sizes::sizes) {
            measure_all<std::string>(n);
        }
        std::cout << "\nThe bytes count what the container allocates through its allocator. The heap memory of a\n"
                     "std::string key (keys longer than its small string buffer) is not counted. Neither is the\n"
                     "overhead of malloc for each allocation.\n\n";
    });
}

/*
 * Memory after an erase: a map keeps memory for erased elements until something gives it back. `throwing_move_value`
 * is a mapped type whose move constructor can throw, which a container copies instead of moving.
 */
TEST_CASE("memory after erasing half") {
    tame_allocator();
    with_sizes<memory_full, memory_quick>([]<typename Sizes>() {
        print_erase_header("uint64_t -> size_t");
        for (auto const n : Sizes::sizes) {
            measure_erase_half_all<std::size_t>(n);
        }

        print_erase_header("uint64_t -> throwing_move_value");
        for (auto const n : Sizes::sizes) {
            measure_erase_half_all<throwing_move_value>(n);
        }
        std::cout << "\nThe bytes count what the container allocates through its allocator, without the overhead of\n"
                     "malloc.\n\n";
    });
}
