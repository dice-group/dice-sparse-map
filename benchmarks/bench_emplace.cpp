#include "common.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>

using namespace dice::unordered_sparse::bench;

/*
 * `emplace` with a key and a mapped value. With a key that is in the map, `emplace` inserts nothing, and a map
 * that constructs the element before it looks up the key does work for nothing: for a `std::string` key that is
 * longer than the small string buffer, an allocation.
 */

namespace {

    struct emplace_full {
        static constexpr std::size_t table_elements = 50000;
        static constexpr std::size_t calls = 1000000;
        static constexpr std::size_t new_elements = 200000;
    };

    struct emplace_quick {
        static constexpr std::size_t table_elements = 5000;
        static constexpr std::size_t calls = 100000;
        static constexpr std::size_t new_elements = 20000;
    };

    /**
     * `calls` calls of `emplace(key, value)` with random keys that are in the map.
     */
    template<std::size_t calls, typename Map>
    std::size_t emplace_existing(lookup_table<Map> *t) {
        auto rng = t->rng.copy();
        auto const *keys = t->keys.data();
        auto const num_keys = t->keys.size();
        std::size_t inserted = 0;
        for (std::size_t i = 0; i < calls; ++i) {
            auto const r = rng();
            inserted += t->map.emplace(key_for<Map>(keys[((r >> 32U) * num_keys) >> 32U]), i).second ? 1U : 0U;
        }
        t->rng = std::move(rng);
        return inserted;
    }

    /**
     * `elements` calls of `emplace(key, value)` with new keys, into an empty map.
     */
    template<typename Map, std::size_t elements>
    std::size_t emplace_new() {
        tame_allocator();
        ankerl::nanobench::Rng rng(777);
        Map map;
        for (std::size_t i = 0; i < elements; ++i) {
            map.emplace(key_for<Map>(rng() & ~never_inserted), i);
        }
        return map.size();
    }

    template<typename Family>
    void emplace_benchmarks(std::string_view container) {
        with_sizes<emplace_full, emplace_quick>([&]<typename Sizes>() {
            using int_map = typename Family::template map<std::uint64_t, std::size_t>;
            using str_map = typename Family::template map<std::string, std::size_t>;

            ankerl::nanobench::Bench bench;
            bench.title(std::format("emplace {}", container));
            configure(bench);
            lookup_table<int_map> ints(Sizes::table_elements);
            lookup_table<str_map> strings(Sizes::table_elements);
            bench.run(std::format("{} uint64_t -> size_t emplace existing keys", container), [&] {
                CHECK(emplace_existing<Sizes::calls>(&ints) == 0);
            });
            bench.run(std::format("{} std::string -> size_t emplace existing keys", container), [&] {
                CHECK(emplace_existing<Sizes::calls>(&strings) == 0);
            });
            bench.run(std::format("{} uint64_t -> size_t emplace new keys", container), [&] {
                CHECK(emplace_new<int_map, Sizes::new_elements>() == Sizes::new_elements);
            });
            bench.run(std::format("{} std::string -> size_t emplace new keys", container), [&] {
                CHECK(emplace_new<str_map, Sizes::new_elements>() == Sizes::new_elements);
            });
        });
    }

}  // namespace

TEST_CASE("emplace sparse_map high") {
    emplace_benchmarks<sparse_high>("sparse_map high");
}

TEST_CASE("emplace sparse_map medium") {
    emplace_benchmarks<sparse_medium>("sparse_map medium");
}

TEST_CASE("emplace sparse_map low") {
    emplace_benchmarks<sparse_low>("sparse_map low");
}

TEST_CASE("emplace unordered_dense") {
    emplace_benchmarks<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("emplace std::unordered_map") {
    emplace_benchmarks<std_family<>>("std::unordered_map");
}
