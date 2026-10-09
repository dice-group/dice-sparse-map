// Ported from the unit tests assignment_combinations, assign_to_move, move_to_moved, copy_and_assign_maps, copyassignment and swap of ankerl::unordered_dense (MIT license).
#include "fixtures/allocators.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstdint>
#include <functional>
#include <string>
#include <utility>
#include <vector>

using namespace dice::sparse_map::tests;

// assignment_combinations

TEST_CASE_MAP("assignment_combinations_1", std::uint64_t, std::uint64_t) {
    map_t a;
    map_t b;
    b = a;
    REQUIRE(b == a);
}

TEST_CASE_MAP("assignment_combinations_2", std::uint64_t, std::uint64_t) {
    map_t a;
    map_t const &a_const = a;
    map_t b;
    a[123] = 321;
    b = a;

    REQUIRE(a.find(123)->second == 321);
    REQUIRE(a_const.find(123)->second == 321);

    REQUIRE(b.find(123)->second == 321);
    a[123] = 111;
    REQUIRE(a.find(123)->second == 111);
    REQUIRE(a_const.find(123)->second == 111);
    REQUIRE(b.find(123)->second == 321);
    b[123] = 222;
    REQUIRE(a.find(123)->second == 111);
    REQUIRE(a_const.find(123)->second == 111);
    REQUIRE(b.find(123)->second == 222);
}

TEST_CASE_MAP("assignment_combinations_3", std::uint64_t, std::uint64_t) {
    map_t a;
    map_t b;
    a[123] = 321;
    a.clear();
    b = a;

    REQUIRE(a.size() == 0);
    REQUIRE(b.size() == 0);
}

TEST_CASE_MAP("assignment_combinations_4", std::uint64_t, std::uint64_t) {
    map_t a;
    map_t b;
    b[123] = 321;
    b = a;

    REQUIRE(a.size() == 0);
    REQUIRE(b.size() == 0);
}

TEST_CASE_MAP("assignment_combinations_5", std::uint64_t, std::uint64_t) {
    map_t a;
    map_t b;
    b[123] = 321;
    b.clear();
    b = a;

    REQUIRE(a.size() == 0);
    REQUIRE(b.size() == 0);
}

TEST_CASE_MAP("assignment_combinations_6", std::uint64_t, std::uint64_t) {
    map_t a;
    a[1] = 2;
    map_t b;
    b[3] = 4;
    b = a;

    REQUIRE(a.size() == 1);
    REQUIRE(b.size() == 1);
    REQUIRE(b.find(1)->second == 2);
    a[1] = 123;
    REQUIRE(a.size() == 1);
    REQUIRE(b.size() == 1);
    REQUIRE(b.find(1)->second == 2);
}

TEST_CASE_MAP("assignment_combinations_7", std::uint64_t, std::uint64_t) {
    map_t a;
    a[1] = 2;
    a.clear();
    map_t b;
    REQUIRE(a == b);
    b[3] = 4;
    REQUIRE(a != b);
    b = a;
    REQUIRE(a == b);
}

TEST_CASE_MAP("assignment_combinations_7b", std::uint64_t, std::uint64_t) {
    map_t a;
    a[1] = 2;
    map_t b;
    REQUIRE(a != b);
    b[3] = 4;
    b.clear();
    REQUIRE(a != b);
    b = a;
    REQUIRE(a == b);
}

TEST_CASE_MAP("assignment_combinations_8", std::uint64_t, std::uint64_t) {
    map_t a;
    a[1] = 2;
    a.clear();
    map_t b;
    b[3] = 4;
    REQUIRE(a != b);
    b.clear();
    REQUIRE(a == b);
    b = a;
    REQUIRE(a == b);
}

TEST_CASE_MAP("assignment_combinations_9", std::uint64_t, std::uint64_t) {
    map_t a;
    a[1] = 2;

    // self assignment works too
    map_t *b = &a;
    a = *b;
    REQUIRE(a == a);
    REQUIRE(a.size() == 1);
    REQUIRE(a.find(1) != a.end());
}

TEST_CASE_MAP("assignment_combinations_10", std::uint64_t, std::uint64_t) {
    map_t a;
    a[1] = 2;
    map_t b;
    b[2] = 1;

    // the maps have the same number of elements, but are not equal
    REQUIRE(!(a == b));
    REQUIRE(a != b);
    REQUIRE(b != a);
    REQUIRE(!(a == b));
    REQUIRE(!(b == a));

    map_t c;
    c[1] = 3;
    REQUIRE(a != c);
    REQUIRE(c != a);
    REQUIRE(!(a == c));
    REQUIRE(!(c == a));

    b.clear();
    REQUIRE(a != b);
    REQUIRE(b != a);
    REQUIRE(!(a == b));
    REQUIRE(!(b == a));

    map_t empty;
    REQUIRE(b == empty);
    REQUIRE(empty == b);
    REQUIRE(!(b != empty));
    REQUIRE(!(empty != b));
}

// assign_to_move and move_to_moved

TEST_CASE_MAP("assign_to_moved", int, int) {
    auto a = map_t();
    a[1] = 2;
    auto moved = std::move(a);
    REQUIRE(moved.size() == 1U);

    auto c = map_t();
    c[3] = 4;

    // assign to a moved from map
    a = c; // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
    REQUIRE(a.size() == 1U);
    REQUIRE(a.at(3) == 4);
    REQUIRE(a == c);
}

TEST_CASE_MAP("move_to_moved", int, int) {
    auto a = map_t();
    a[1] = 2;
    auto moved = std::move(a);

    auto c = map_t();
    c[3] = 4;

    // move assign to a moved from map
    a = std::move(c); // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)

    a[5] = 6;
    moved[6] = 7;
    REQUIRE(moved[6] == 7);
    REQUIRE(moved.size() == 2U);
    REQUIRE(a.size() == 2U);
    REQUIRE(a.at(3) == 4);
    REQUIRE(a.at(5) == 6);
}

// copy_and_assign_maps

namespace {

    /**
     * Creates a map with `num_elements` elements.
     */
    template<typename Map>
    [[nodiscard]] Map create_map(int num_elements) {
        Map m;
        for (int i = 0; i < num_elements; ++i) {
            m[static_cast<typename Map::key_type>((i + 123) * 7)] = static_cast<typename Map::mapped_type>(i);
        }
        return m;
    }

} // namespace

TEST_CASE_MAP("copy_and_assign_maps_1", int, int) {
    auto a = create_map<map_t>(15);
    REQUIRE(a.size() == 15U);
}

TEST_CASE_MAP("copy_and_assign_maps_2", int, int) {
    auto a = create_map<map_t>(100);
    REQUIRE(a.size() == 100U);
}

TEST_CASE_MAP("copy_and_assign_maps_3", int, int) {
    auto a = create_map<map_t>(1);
    // NOLINTNEXTLINE(performance-unnecessary-copy-initialization)
    auto b = a;
    REQUIRE(a == b);
}

TEST_CASE_MAP("copy_and_assign_maps_4", int, int) {
    map_t a;
    REQUIRE(a.empty());
    a.clear();
    REQUIRE(a.empty());
}

TEST_CASE_MAP("copy_and_assign_maps_5", int, int) {
    auto a = create_map<map_t>(100);
    // NOLINTNEXTLINE(performance-unnecessary-copy-initialization)
    auto b = a;
    REQUIRE(b == a);
}

TEST_CASE_MAP("copy_and_assign_maps_6", int, int) {
    map_t a;
    a[123] = 321;
    a.clear();
    auto const maps = std::vector<map_t>(10, a);

    for (auto const &map : maps) {
        REQUIRE(map.empty());
    }
}

TEST_CASE_MAP("copy_and_assign_maps_7", int, int) {
    auto const maps = std::vector<map_t>(10);
    REQUIRE(maps.size() == 10U);
}

TEST_CASE_MAP("copy_and_assign_maps_8", int, int) {
    map_t a;
    auto const maps = std::vector<map_t>(12, a);
    REQUIRE(maps.size() == 12U);
}

TEST_CASE_MAP("copy_and_assign_maps_9", int, int) {
    map_t a;
    a[123] = 321;
    auto const maps = std::vector<map_t>(10, a);
    a[123] = 1;

    for (auto const &map : maps) {
        REQUIRE(map.size() == 1);
        REQUIRE(map.find(123)->second == 321);
    }
}

// copyassignment

TEST_CASE_MAP("copyassignment", std::string, std::string) {
    auto map = map_t();
    auto tmp = map_t();

    map.emplace("a", "b");
    map = tmp;
    map.emplace("c", "d");

    REQUIRE(map.size() == 1);
    REQUIRE(map["c"] == "d");
    REQUIRE(map.size() == 1);

    REQUIRE(tmp.size() == 0);

    map["e"] = "f";
    REQUIRE(map.size() == 2);
    REQUIRE(tmp.size() == 0);

    tmp["g"] = "h";
    REQUIRE(map.size() == 2);
    REQUIRE(tmp.size() == 1);
}

// swap

TEST_CASE_MAP("swap", int, int) {
    {
        auto b = map_t();
        {
            auto a = map_t();
            b[1] = 2;

            a.swap(b);
            REQUIRE(a.end() != a.find(1));
            REQUIRE(b.end() == b.find(1));
        }
        REQUIRE(b.end() == b.find(1));
        b[2] = 3;
        REQUIRE(b.end() != b.find(2));
        REQUIRE(b.size() == 1);
    }

    {
        auto a = map_t();
        {
            auto b = map_t();
            b[1] = 2;

            a.swap(b);
            REQUIRE(a.end() != a.find(1));
            REQUIRE(b.end() == b.find(1));
        }
        REQUIRE(a.end() != a.find(1));
        a[2] = 3;
        REQUIRE(a.end() != a.find(2));
        REQUIRE(a.size() == 2);
    }

    {
        auto a = map_t();
        {
            auto b = map_t();
            a.swap(b);
            REQUIRE(a.end() == a.find(1));
            REQUIRE(b.end() == b.find(1));
        }
        REQUIRE(a.end() == a.find(1));
    }
}

// Exchanging what two maps own needs no allocation, through the member and through the swap() that generic code
// finds with `using std::swap`.
TEST_CASE("swap_does_not_allocate") {
    using map_t = dice::sparse_map::sparse_map<
        int,
        int,
        test_hash<int>,
        std::equal_to<int>,
        counting_allocator<std::pair<int, int>>
    >;

    auto a = map_t();
    auto b = map_t();
    for (int i = 0; i < 1000; ++i) {
        a[i] = i;
    }
    for (int i = 0; i < 10; ++i) {
        b[i] = -i;
    }

    num_allocations = 0;
    a.swap(b);
    REQUIRE(num_allocations == 0);

    REQUIRE(a.size() == 10U);
    REQUIRE(b.size() == 1000U);
    REQUIRE(a.at(5) == -5);
    REQUIRE(b.at(5) == 5);

    using std::swap;
    swap(a, b);
    REQUIRE(num_allocations == 0);
    REQUIRE(a.size() == 1000U);
    REQUIRE(b.size() == 10U);
    REQUIRE(a.at(999) == 999);
}
