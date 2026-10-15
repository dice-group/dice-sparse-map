#include "fixtures/allocators.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <dice/sparse-map/sparse_map.hpp>

#include <doctest/doctest.h>

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <memory_resource>
#include <random>
#include <type_traits>
#include <unordered_map>
#include <utility>

using namespace dice::sparse_map::tests;

/*
 * A group copies trivially copyable values as bytes (`sparse_array::copies_bytes`) when the allocator has no
 * `construct` and no `destroy` of its own: when it grows its storage, and when it is copied or moved into new
 * storage. The static assertions check which groups do that. The test cases run these paths against
 * `std::unordered_map`.
 */

namespace {

    template<typename Key, typename T, typename Allocator>
    using array_of = dice::sparse_map::detail_sparse_hash::sparse_array<
        dice::sparse_map::detail_sparse_hash::map_slot<Key, T>,
        Allocator,
        dice::sparse_map::sh::sparsity::medium,
        dice::sparse_map::sh::allocation_failure::terminating
    >;

    using slot_u64 = dice::sparse_map::detail_sparse_hash::map_slot<std::uint64_t, std::uint64_t>;
    using slot_obj = dice::sparse_map::detail_sparse_hash::map_slot<counter::obj, counter::obj>;

    static_assert(array_of<std::uint64_t, std::uint64_t, std::allocator<slot_u64>>::copies_bytes);
    static_assert(array_of<std::uint64_t, std::uint64_t, offset_ptr_allocator<slot_u64>>::copies_bytes);
    static_assert(array_of<std::uint64_t, std::uint64_t, pmr_like_allocator<slot_u64>>::copies_bytes);
    // an allocator with its own `construct` and `destroy` constructs and destroys every value
    static_assert(!array_of<std::uint64_t, std::uint64_t, std::pmr::polymorphic_allocator<slot_u64>>::copies_bytes);
    static_assert(!array_of<std::uint64_t, std::uint64_t, construct_counting_allocator<slot_u64>>::copies_bytes);
    // a value that is not trivially copyable is copied one by one
    static_assert(!array_of<counter::obj, counter::obj, std::allocator<slot_obj>>::copies_bytes);

    /**
     * A trivially copyable mapped value of 40 bytes, so that a copy moves more than one word per value.
     */
    struct wide_value {
        std::array<std::uint64_t, 5> words{};

        friend bool operator==(wide_value const &, wide_value const &) = default;
    };
    static_assert(std::is_trivially_copyable_v<wide_value>);

    [[nodiscard]] wide_value wide(std::uint64_t v) {
        return wide_value{{v, v + 1, v + 2, v + 3, v + 4}};
    }

    template<typename Map, typename Reference>
    [[nodiscard]] bool same_contents(Map const &map, Reference const &reference) {
        if (map.size() != reference.size()) {
            return false;
        }
        for (auto const &[key, value] : reference) {
            auto const it = map.find(key);
            if (it == map.end() || !(it->second == value)) {
                return false;
            }
        }
        std::size_t visited = 0;
        for (auto it = map.begin(); it != map.end(); ++it) {
            ++visited;
        }
        return visited == reference.size();
    }

    /**
     * Random insertions and erasures, by key and by iterator, and copies, in a table of up to a few hundred
     * elements. The keys come from a small range, so most groups hold many values, and their storage grows often.
     */
    template<typename Map, typename Make>
    void run_random_operations(Map &map, Make const &make, std::uint64_t seed) {
        using mapped = typename Map::mapped_type;
        std::unordered_map<std::uint64_t, mapped> reference;
        std::mt19937_64 rng(seed);
        for (int step = 0; step < 20000; ++step) {
            auto const key = rng() % 600;
            switch (rng() % 6) {
                case 0:
                case 1: {
                    auto const value = make(rng());
                    REQUIRE(map.try_emplace(key, value).second == reference.try_emplace(key, value).second);
                    break;
                }
                case 2: {
                    auto const value = make(rng());
                    map.insert_or_assign(key, value);
                    reference.insert_or_assign(key, value);
                    break;
                }
                case 3: {
                    REQUIRE(map.erase(key) == reference.erase(key));
                    break;
                }
                case 4: {
                    auto it = map.find(key);
                    if (it != map.end()) {
                        map.erase(it);
                        reference.erase(key);
                    }
                    break;
                }
                default: {
                    if (step % 97 == 0) {
                        auto copy = map;
                        REQUIRE(same_contents(copy, reference));
                    }
                    break;
                }
            }
            if (step % 1000 == 0) {
                REQUIRE(same_contents(map, reference));
            }
        }
        REQUIRE(same_contents(map, reference));
    }

} // namespace

TEST_CASE_MAP("trivially copyable values: random insertions and erasures", std::uint64_t, std::uint64_t) {
    for (std::uint64_t seed = 0; seed < 4; ++seed) {
        auto map = map_t();
        run_random_operations(map, [](std::uint64_t v) { return v; }, seed);
    }
}

TEST_CASE_MAP("trivially copyable wide values: random insertions and erasures", std::uint64_t, wide_value) {
    for (std::uint64_t seed = 0; seed < 4; ++seed) {
        auto map = map_t();
        run_random_operations(map, [](std::uint64_t v) { return wide(v); }, seed);
    }
}

TEST_CASE_MAP("trivially copyable values: high load factor", std::uint64_t, std::uint64_t) {
    auto map = map_t();
    map.max_load_factor(0.8f);
    run_random_operations(map, [](std::uint64_t v) { return v; }, 42);
}

TEST_CASE("trivially copyable values: copy and move into a table with an unequal allocator") {
    using alloc_t = pmr_like_allocator<std::pair<std::uint64_t, std::uint64_t>>;
    using map_t =
        dice::sparse_map::sparse_map<std::uint64_t, std::uint64_t, test_hash<std::uint64_t>, std::equal_to<std::uint64_t>, alloc_t>;
    auto source = map_t(alloc_t(1));
    std::unordered_map<std::uint64_t, std::uint64_t> reference;
    for (std::uint64_t key = 0; key < 1000; ++key) {
        source.try_emplace(key * 7, key);
        reference.try_emplace(key * 7, key);
    }
    auto copy = map_t(source, alloc_t(2));
    CHECK(same_contents(copy, reference));
    auto moved = map_t(std::move(source), alloc_t(3));
    CHECK(same_contents(moved, reference));
    CHECK(source.empty()); // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
}
