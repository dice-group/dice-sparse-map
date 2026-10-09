#include "fixtures/test_types.hpp"
#include "fixtures/utils.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <array>
#include <cstddef>
#include <functional>
#include <memory>
#include <ranges>
#include <utility>

/**
 * The allocator requirements allow an `explicit` converting constructor: `A a(b)` must work, `A a = b` need not. The
 * containers rebind their allocator, so they must convert it explicitly.
 */
namespace {
    using namespace dice::sparse_map;

    /**
     * `std::allocator` with an id and an `explicit` converting constructor. Instances with different ids compare
     * unequal.
     */
    template<typename T>
    struct explicit_allocator {
        using value_type = T;

        int id = 0;

        explicit_allocator() noexcept = default;

        explicit explicit_allocator(int new_id) noexcept
            : id(new_id) {
        }

        template<typename U>
        explicit explicit_allocator(explicit_allocator<U> const &other) noexcept
            : id(other.id) {
        }

        T *allocate(std::size_t n) {
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, std::size_t n) noexcept {
            std::allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(explicit_allocator const &lhs, explicit_allocator const &rhs) noexcept {
            return lhs.id == rhs.id;
        }
    };

    using map_t = sparse_map<int, int, tests::test_hash<int>, std::equal_to<int>, explicit_allocator<std::pair<int, int>>>;
    using set_t = sparse_set<int, tests::test_hash<int>, std::equal_to<int>, explicit_allocator<int>>;

    /// the elements 0 to 9 as `value_type`
    template<typename Container>
    auto elements() {
        auto result = std::array<typename Container::value_type, 10>{};
        for (int i = 0; i < 10; ++i) {
            if constexpr (requires { typename Container::mapped_type; }) {
                result[static_cast<std::size_t>(i)] = {i, i};
            } else {
                result[static_cast<std::size_t>(i)] = i;
            }
        }
        return result;
    }
}  // namespace

TYPE_TO_STRING_AS("map", map_t);
TYPE_TO_STRING_AS("set", set_t);

TEST_CASE_TEMPLATE("a container works with an allocator whose converting constructor is explicit", container_t, map_t, set_t) {
    using allocator_type = typename container_t::allocator_type;
    auto const alloc_1 = allocator_type{1};
    auto const alloc_2 = allocator_type{2};
    auto const values = elements<container_t>();

    auto container = container_t{alloc_1};
    for (int key = 0; key < 100; ++key) {
        tests::insert_one(container, key);
    }
    container.erase(container.begin());
    container.rehash(1024);
    container.reserve(10);
    CHECK(container.size() == 99);
    CHECK(container.get_allocator() == alloc_1);

    // `emplace`, `emplace_hint` and the map's `insert` of a pair-like value construct the element with the allocator,
    // converted to an allocator of `value_type`
    auto emplaced = container_t{alloc_1};
    if constexpr (requires { typename container_t::mapped_type; }) {
        emplaced.emplace(200, 200);
        emplaced.emplace_hint(emplaced.end(), 201, 201);
        emplaced.insert(std::pair<int, long>{202, 202});
        CHECK(emplaced.size() == 3);
        CHECK(emplaced.at(202) == 202);
    } else {
        emplaced.emplace(200);
        emplaced.emplace_hint(emplaced.end(), 201);
        CHECK(emplaced.size() == 2);
    }
    CHECK(emplaced.contains(200));
    CHECK(emplaced.contains(201));

    CHECK(container_t(16, alloc_2).get_allocator() == alloc_2);
    CHECK(container_t(16, tests::test_hash<int>{}, alloc_2).get_allocator() == alloc_2);
    CHECK(container_t(16, tests::test_hash<int>{}, std::equal_to<int>{}, alloc_2).get_allocator() == alloc_2);
    CHECK(container_t(values.begin(), values.end(), alloc_2).size() == 10);
    CHECK(container_t(values.begin(), values.end(), 16, alloc_2).size() == 10);
    CHECK(container_t(std::from_range, values, alloc_2).size() == 10);
    CHECK(container_t(std::from_range, values, 16, alloc_2).size() == 10);
    CHECK(container_t({values[0], values[1]}, alloc_2).size() == 2);

    auto copy = container_t(container, alloc_2);
    CHECK(copy.size() == 99);
    CHECK(copy.get_allocator() == alloc_2);

    auto moved = container_t(std::move(copy), alloc_1);
    CHECK(moved.size() == 99);
    CHECK(moved.get_allocator() == alloc_1);

    auto assigned = container_t{alloc_1};
    assigned = moved;
    CHECK(assigned == container);
    assigned = std::move(moved);
    CHECK(assigned == container);
    assigned.swap(container);
    CHECK(assigned.size() == 99);
}
