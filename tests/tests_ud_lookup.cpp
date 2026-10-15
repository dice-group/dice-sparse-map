// Ported from the unit tests at, count, contains, empty and equal_range of ankerl::unordered_dense (MIT license).
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

using namespace dice::sparse_map::tests;
using namespace std::literals;

// at

TEST_CASE_MAP("at", int, int) {
    map_t map;
    map_t const &cmap = map;

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(map.at(123), std::out_of_range);

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(static_cast<void>(map.at(0)), std::out_of_range);

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(static_cast<void>(cmap.at(123)), std::out_of_range);

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(static_cast<void>(cmap.at(0)), std::out_of_range);

    map[123] = 333;
    REQUIRE(map.at(123) == 333);
    REQUIRE(cmap.at(123) == 333);

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(static_cast<void>(map.at(0)), std::out_of_range);

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(static_cast<void>(cmap.at(0)), std::out_of_range);
}

// count

TEST_CASE_MAP("count", int, int) {
    auto map = map_t();
    REQUIRE(map.count(123) == 0);
    REQUIRE(map.count(0) == 0);
    map[123];
    REQUIRE(map.count(123) == 1);
    REQUIRE(map.count(0) == 0);
}

// contains

TEST_CASE_MAP("contains", std::uint64_t, std::uint64_t) {
    static_assert(std::is_same_v<bool, decltype(map_t{}.contains(1))>);

    auto map = map_t();

    REQUIRE(!map.contains(0));
    REQUIRE(!map.contains(123));
    map[123];
    REQUIRE(!map.contains(0));
    REQUIRE(map.contains(123));
    map.clear();
    REQUIRE(!map.contains(0));
    REQUIRE(!map.contains(123));
}

// empty

TEST_CASE_MAP("empty_map_operations", int, int) {
    map_t m;

    REQUIRE(m.end() == m.find(123));
    REQUIRE(m.end() == m.begin());
    m[32];
    REQUIRE(m.end() != m.begin());
    REQUIRE(m.end() == m.find(123));
    REQUIRE(m.end() != m.find(32));

    m = map_t();
    REQUIRE(m.end() == m.begin());
    REQUIRE(m.end() == m.find(123));
    REQUIRE(m.end() == m.find(32));

    map_t m2(m);
    REQUIRE(m2.end() == m2.begin());
    REQUIRE(m2.end() == m2.find(123));
    REQUIRE(m2.end() == m2.find(32));
    m2[32];
    REQUIRE(m2.end() != m2.begin());
    REQUIRE(m2.end() == m2.find(123));
    REQUIRE(m2.end() != m2.find(32));

    map_t empty;
    map_t m3(empty);
    REQUIRE(m3.end() == m3.begin());
    REQUIRE(m3.end() == m3.find(123));
    REQUIRE(m3.end() == m3.find(32));
    m3[32];
    REQUIRE(m3.end() != m3.begin());
    REQUIRE(m3.end() == m3.find(123));
    REQUIRE(m3.end() != m3.find(32));

    map_t m4(std::move(empty));
    REQUIRE(m4.count(123) == 0);
    REQUIRE(m4.end() == m4.begin());
    REQUIRE(m4.end() == m4.find(123));
    REQUIRE(m4.end() == m4.find(32));
}

// equal_range

TEST_CASE_MAP("equal_range", int, int) {
    auto map = map_t();

    auto range = map.equal_range(123);
    REQUIRE(range.first == map.end());
    REQUIRE(range.second == map.end());

    map.try_emplace(1, 1);
    range = map.equal_range(123);
    REQUIRE(range.first == map.end());
    REQUIRE(range.second == map.end());

    int const x = 1;
    auto const_range = std::as_const(map).equal_range(x);
    REQUIRE(const_range.first == map.begin());
    REQUIRE(const_range.second == map.end());

    for (int i = 0; i < 100; ++i) {
        map.try_emplace(i, i);
    }
    range = map.equal_range(50);
    auto after_first = ++range.first;
    REQUIRE(range.second == after_first);
    REQUIRE(range.second != map.end());
}

// The check above is `second == ++first`, made of the non-const overload, and the const overload is asked with a
// single element in the table. With one element, the iterator after the hit and `end()` are the same iterator, so a
// `second` that is always `end()` passes. The key here is the one at `begin()` instead: with a hundred elements behind
// it, the iterator after the hit and the end are as far apart as they get.
namespace {

    /**
     * `key` has to be the key at `begin()`.
     */
    template<typename Container, typename Key>
    void check_hit_is_exactly_one(Container &container, Key const &key) {
        auto range = container.equal_range(key);
        REQUIRE(range.first == container.begin());
        REQUIRE(range.second == std::next(container.begin()));
        REQUIRE(range.second != container.end());

        auto const_range = std::as_const(container).equal_range(key);
        REQUIRE(const_range.first == std::as_const(container).begin());
        REQUIRE(const_range.second == std::next(std::as_const(container).begin()));
        REQUIRE(const_range.second != std::as_const(container).end());
    }

    template<typename Map, typename Key>
    void check_miss_is_empty(Map &map, Key const &absent) {
        auto range = map.equal_range(absent);
        REQUIRE(range.first == map.end());
        REQUIRE(range.second == map.end());

        auto const_range = std::as_const(map).equal_range(absent);
        REQUIRE(const_range.first == std::as_const(map).end());
        REQUIRE(const_range.second == std::as_const(map).end());
    }

} // namespace

TEST_CASE_MAP("equal_range_const_overload_with_more_than_one_element", int, int) {
    auto map = map_t();
    for (int i = 0; i < 100; ++i) {
        map.try_emplace(i, i);
    }
    check_hit_is_exactly_one(map, map.begin()->first);
    check_miss_is_empty(map, 1000);
}

TEST_CASE_SET("equal_range_set", int) {
    auto set = set_t();
    for (int i = 0; i < 100; ++i) {
        set.emplace(i);
    }
    check_hit_is_exactly_one(set, *set.begin());
    check_miss_is_empty(set, 1000);
}

// The transparent overloads are separate functions. A `std::string_view` argument selects them, an exact
// `std::string` would select the overloads that take the key type.
namespace {

    struct transparent_hash {
        using is_transparent = void;
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(std::string_view sv) const noexcept {
            return test_hash<std::string_view>{}(sv);
        }
    };

    using transparent_map = dice::sparse_map::sparse_map<std::string, int, transparent_hash, std::equal_to<>>;

} // namespace

TEST_CASE("equal_range_transparent") {
    auto map = transparent_map();
    for (int i = 0; i < 100; ++i) {
        map.try_emplace("key #"s + std::to_string(i), i);
    }

    auto const key = std::string_view(map.begin()->first);
    check_hit_is_exactly_one(map, key);
    check_miss_is_empty(map, "not in here"sv);

    // and the same overloads reached through count and contains
    REQUIRE(map.count(key) == 1);
    REQUIRE(map.count("not in here"sv) == 0);
    REQUIRE(map.contains(key));
    REQUIRE(!map.contains("not in here"sv));
}
