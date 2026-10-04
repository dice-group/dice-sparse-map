#include "common.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <cstddef>
#include <cstdint>
#include <format>
#include <string_view>
#include <unordered_map>

using namespace dice::unordered_sparse::bench;

/*
 * Erase by iterator. `erase_if` erases with `erase(iterator)`, and in `sparse_map` that call finds the bucket of
 * the element from its position in the values of its group (`offset_to_index`). No scored workload erases by
 * iterator, they erase by key.
 */

namespace {

    struct erase_iterator_full {
        static constexpr std::size_t elements = 100000;
    };

    struct erase_iterator_quick {
        static constexpr std::size_t elements = 10000;
    };

    /**
     * Copies a map of `elements` entries, and copies it and erases the half of the entries with an odd key with
     * `erase_if`. The time of the erasure is the difference of the two rows.
     */
    template<typename Family>
    void erase_iterator(std::string_view container) {
        with_sizes<erase_iterator_full, erase_iterator_quick>([&]<typename Sizes>() {
            using map_t = typename Family::template map<std::uint64_t, std::size_t>;
            ankerl::nanobench::Bench bench;
            bench.title(std::format("erase_iterator {}", container));
            configure(bench);
            lookup_table<map_t> const table(Sizes::elements);
            bench.run(std::format("{} uint64_t -> size_t copy", container), [&] {
                map_t copy = table.map;
                ankerl::nanobench::doNotOptimizeAway(copy.size());
            });
            bench.run(std::format("{} uint64_t -> size_t copy and erase_if half", container), [&] {
                map_t copy = table.map;
                using std::erase_if;
                auto const erased = erase_if(copy, [](auto const &key_value) {
                    return (key_value.first & 1U) != 0;
                });
                CHECK(erased + copy.size() == Sizes::elements);
                ankerl::nanobench::doNotOptimizeAway(copy.size());
            });
        });
    }

}  // namespace

TEST_CASE("erase_iterator sparse_map high") {
    erase_iterator<sparse_high>("sparse_map high");
}

TEST_CASE("erase_iterator sparse_map medium") {
    erase_iterator<sparse_medium>("sparse_map medium");
}

TEST_CASE("erase_iterator sparse_map low") {
    erase_iterator<sparse_low>("sparse_map low");
}

TEST_CASE("erase_iterator unordered_dense") {
    erase_iterator<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("erase_iterator std::unordered_map") {
    erase_iterator<std_family<>>("std::unordered_map");
}
