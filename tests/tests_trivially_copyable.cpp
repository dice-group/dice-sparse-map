#include "fixtures/allocators.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <array>
#include <concepts>
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
 * storage. A group with holes copies one by one. The static assertions check which groups do that. The test cases
 * run these paths against `std::unordered_map`, and copy and move a set whose group has a hole.
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
     * A trivially copyable key whose move can throw. An rvalue takes the constructor template, which is not a move
     * constructor, so the type stays trivially copyable. A group of such keys has holes.
     */
    struct throwing_move_key {
        std::size_t v = 0;

        explicit throwing_move_key(std::size_t x) noexcept : v(x) {}

        throwing_move_key(throwing_move_key const &other) = default;

        template<typename U>
        requires std::same_as<U, throwing_move_key>
        throwing_move_key(U &&other) noexcept(false) // NOLINT(bugprone-forwarding-reference-overload)
            :
            v(other.v) {}

        throwing_move_key &operator=(throwing_move_key const &other) = default;
        ~throwing_move_key() = default;

        friend bool operator==(throwing_move_key const &, throwing_move_key const &) = default;
    };

    /**
     * The hash of a key is its number, so key `k` lands in bucket `k`. It is not avalanching, but it is marked as
     * avalanching, so that the containers accept it.
     */
    struct identity_key_hash {
        using is_avalanching = void;

        std::size_t operator()(throwing_move_key const &key) const noexcept {
            return key.v;
        }
    };

    using hole_group = dice::sparse_map::detail_sparse_hash::sparse_array<
        throwing_move_key,
        std::allocator<throwing_move_key>,
        dice::sparse_map::sh::sparsity::medium,
        dice::sparse_map::sh::allocation_failure::terminating
    >;
    static_assert(std::is_trivially_copyable_v<throwing_move_key>);
    static_assert(!std::is_nothrow_move_constructible_v<throwing_move_key>);
    static_assert(hole_group::has_holes);
    // the slots of a group with holes include the holes, so the group copies its values one by one
    static_assert(!hole_group::copies_bytes);

    /**
     * Inserts the keys 1 to 4 into `set` (buckets 1 to 4 of its first group) and erases key 1. That leaves a hole
     * in the first slot of the group.
     */
    template<typename Set>
    void make_hole(Set &set) {
        set.reserve(32);
        REQUIRE(set.bucket_count() == 64);
        for (std::size_t k = 1; k <= 4; ++k) {
            throwing_move_key const key{k};
            set.insert(key);
        }
        REQUIRE(set.erase(throwing_move_key{1}) == 1);
    }

    /// `set` holds exactly the keys 2, 3 and 4
    template<typename Set>
    void check_keys_after_hole(Set const &set) {
        std::size_t sum = 0;
        for (auto const &key : set) {
            sum += key.v;
        }
        CHECK(set.size() == 3);
        CHECK(sum == 9);
        CHECK(set.contains(throwing_move_key{2}));
        CHECK(set.contains(throwing_move_key{3}));
        CHECK(set.contains(throwing_move_key{4}));
        CHECK(!set.contains(throwing_move_key{1}));
    }

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
    using map_t = dice::sparse_map::sparse_map<
        std::uint64_t,
        std::uint64_t,
        test_hash<std::uint64_t>,
        std::equal_to<std::uint64_t>,
        alloc_t
    >;
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

TEST_CASE("a trivially copyable key whose move can throw: a copy of a set with a hole keeps its keys") {
    using set_t = dice::sparse_map::sparse_set<throwing_move_key, identity_key_hash>;
    auto set = set_t{};
    make_hole(set);
    check_keys_after_hole(set);

    auto const copy = set;
    check_keys_after_hole(copy);
}

TEST_CASE(
    "a trivially copyable key whose move can throw: copy and move of a set with a hole into a table with an unequal allocator"
) {
    using alloc_t = pmr_like_allocator<throwing_move_key>;
    using set_t =
        dice::sparse_map::sparse_set<throwing_move_key, identity_key_hash, std::equal_to<throwing_move_key>, alloc_t>;
    auto set = set_t(alloc_t(1));
    make_hole(set);

    auto const copy = set_t(set, alloc_t(2));
    check_keys_after_hole(copy);
    auto const moved = set_t(std::move(set), alloc_t(3));
    check_keys_after_hole(moved);
}
