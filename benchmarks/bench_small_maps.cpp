#include "common.hpp"
#include "tracking_allocator.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <numeric>
#include <string_view>
#include <vector>

using namespace dice::sparse_map::bench;
namespace sh = dice::sparse_map::sh;

/*
 * Many small maps, as in a tree whose nodes each hold one small map from a key part to a child.
 * The maps hold 1 to 16 `uint64_t -> uint64_t` entries.
 */

namespace {

    struct small_maps_full {
        static constexpr std::size_t num_maps = 100000;
        static constexpr std::size_t rounds = 11;
    };

    struct small_maps_quick {
        static constexpr std::size_t num_maps = 10000;
        static constexpr std::size_t rounds = 1;
    };

    /**
     * The keys of all maps, the same for every container. Map `i` holds the keys
     * `keys[offsets[i]]` to `keys[offsets[i + 1] - 1]`, distinct within the map, and the value of a
     * key is its index in `keys`.
     */
    struct small_maps_input {
        std::vector<std::size_t> offsets;
        std::vector<std::uint64_t> keys;
        std::vector<std::size_t> lookup_order;  ///< the maps in a random order
        std::uint64_t value_sum = 0;            ///< the sum of all values

        explicit small_maps_input(std::size_t num_maps) {
            ankerl::nanobench::Rng rng(4242);
            offsets.reserve(num_maps + 1);
            offsets.push_back(0);
            for (std::size_t i = 0; i < num_maps; ++i) {
                auto const size = 1 + rng.bounded(16);
                auto const begin = keys.size();
                while (keys.size() - begin < size) {
                    auto const key = rng() & ~never_inserted;
                    if (std::find(keys.begin() + static_cast<std::ptrdiff_t>(begin), keys.end(), key) == keys.end()) {
                        keys.push_back(key);
                    }
                }
                offsets.push_back(keys.size());
            }
            lookup_order.resize(num_maps);
            std::iota(lookup_order.begin(), lookup_order.end(), std::size_t{0});
            rng.shuffle(lookup_order);
            for (std::size_t k = 0; k < keys.size(); ++k) {
                value_sum += k;
            }
        }

        [[nodiscard]] std::size_t num_maps() const noexcept {
            return offsets.size() - 1;
        }
    };

    /**
     * Median of the times of all rounds, in nanoseconds per unit of work.
     */
    [[nodiscard]] double median_ns_per(std::vector<std::chrono::nanoseconds> times, std::size_t units) {
        std::ranges::sort(times);
        auto const median = times[times.size() / 2];
        return static_cast<double>(median.count()) / static_cast<double>(units);
    }

    void print_header(std::size_t num_maps, std::size_t num_elements) {
        std::cout << std::format("\nsmall maps: {} maps with 1 to 16 elements, {} elements in total, uint64_t -> uint64_t\n\n",
                                 num_maps,
                                 num_elements)
                  << std::format("| {:<18} | {:>12} | {:>11} | {:>12} | {:>12} | {:>14} | {:>10} | {:>12} | {:>11} |\n",
                                 "container",
                                 "build ns/map",
                                 "hit ns/find",
                                 "miss ns/find",
                                 "iter ns/elem",
                                 "destroy ns/map",
                                 "heap B/map",
                                 "allocs/map",
                                 "sizeof(map)")
                  << std::format("|{:-<20}|{:->13}:|{:->12}:|{:->13}:|{:->13}:|{:->15}:|{:->11}:|{:->13}:|{:->12}:|\n", "", "", "", "", "", "", "", "", "");
    }

    /**
     * Builds all maps, looks up every key (hits) and every key with the top bit set (misses), with
     * the maps in a random order as a trie lookup jumps between nodes, iterates over all maps in
     * the order they were built, and destroys them. Each phase is timed on its own, and the table
     * shows the median over all rounds.
     */
    template<typename Family>
    void small_maps(std::string_view container, small_maps_input const &input, std::size_t rounds) {
        using map_t = typename Family::template map<std::uint64_t, std::uint64_t>;
        using clock = std::chrono::steady_clock;

        auto const num_maps = input.num_maps();
        auto const num_elements = input.keys.size();
        allocation_stats stats;
        std::vector<std::chrono::nanoseconds> build_times;
        std::vector<std::chrono::nanoseconds> hit_times;
        std::vector<std::chrono::nanoseconds> miss_times;
        std::vector<std::chrono::nanoseconds> iterate_times;
        std::vector<std::chrono::nanoseconds> destroy_times;
        std::size_t heap_bytes = 0;
        std::size_t live_allocations = 0;

        for (std::size_t round = 0; round < rounds; ++round) {
            stats.reset();
            std::vector<map_t> maps;
            maps.reserve(num_maps);

            auto const start = clock::now();
            for (std::size_t i = 0; i < num_maps; ++i) {
                auto &map = maps.emplace_back(typename map_t::allocator_type(&stats));
                for (auto k = input.offsets[i]; k < input.offsets[i + 1]; ++k) {
                    map[input.keys[k]] = k;
                }
            }
            auto const built = clock::now();

            std::uint64_t hit_sum = 0;
            for (auto const i : input.lookup_order) {
                auto const &map = maps[i];
                for (auto k = input.offsets[i]; k < input.offsets[i + 1]; ++k) {
                    auto it = map.find(input.keys[k]);
                    if (it != map.end()) {
                        hit_sum += it->second;
                    }
                }
            }
            auto const hit = clock::now();

            std::size_t false_hits = 0;
            for (auto const i : input.lookup_order) {
                auto const &map = maps[i];
                for (auto k = input.offsets[i]; k < input.offsets[i + 1]; ++k) {
                    if (map.find(input.keys[k] | never_inserted) != map.end()) {
                        ++false_hits;
                    }
                }
            }
            auto const missed = clock::now();

            std::uint64_t iterate_sum = 0;
            for (auto const &map : maps) {
                for (auto const &key_value : map) {
                    iterate_sum += key_value.second;
                }
            }
            auto const iterated = clock::now();

            heap_bytes = stats.current;
            live_allocations = stats.live_allocations;
            maps.clear();
            auto const destroyed = clock::now();

            CHECK(hit_sum == input.value_sum);
            CHECK(false_hits == 0);
            CHECK(iterate_sum == input.value_sum);
            CHECK(stats.current == 0);
            CHECK(stats.live_allocations == 0);

            build_times.push_back(built - start);
            hit_times.push_back(hit - built);
            miss_times.push_back(missed - hit);
            iterate_times.push_back(iterated - missed);
            destroy_times.push_back(destroyed - iterated);
        }

        std::cout << std::format("| {:<18} | {:>12.1f} | {:>11.1f} | {:>12.1f} | {:>12.2f} | {:>14.1f} | {:>10.1f} | {:>12.2f} | {:>11} |\n",
                                 container,
                                 median_ns_per(build_times, num_maps),
                                 median_ns_per(hit_times, num_elements),
                                 median_ns_per(miss_times, num_elements),
                                 median_ns_per(iterate_times, num_elements),
                                 median_ns_per(destroy_times, num_maps),
                                 static_cast<double>(heap_bytes) / static_cast<double>(num_maps),
                                 static_cast<double>(live_allocations) / static_cast<double>(num_maps),
                                 sizeof(map_t));
    }

}  // namespace

TEST_CASE("small_maps") {
    tame_allocator();
    with_sizes<small_maps_full, small_maps_quick>([]<typename Sizes>() {
        small_maps_input const input(Sizes::num_maps);
        print_header(input.num_maps(), input.keys.size());
        small_maps<sparse_family<sh::sparsity::high, tracking_allocator>>("sparse_map high", input, Sizes::rounds);
        small_maps<sparse_family<sh::sparsity::medium, tracking_allocator>>("sparse_map medium", input, Sizes::rounds);
        small_maps<sparse_family<sh::sparsity::low, tracking_allocator>>("sparse_map low", input, Sizes::rounds);
        small_maps<unordered_dense_family<tracking_allocator>>("unordered_dense", input, Sizes::rounds);
        small_maps<std_family<tracking_allocator>>("std::unordered_map", input, Sizes::rounds);
        std::cout << "\nheap B/map and allocs/map count what the maps allocate through their allocator, without the\n"
                     "overhead of malloc. sizeof(map) comes on top, wherever the map object lives.\n\n";
    });
}
