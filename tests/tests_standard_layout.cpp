#include "fixtures/offset_ptr_allocator.hpp"
#include "fixtures/test_types.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>
#include <metall/metall.hpp>

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

/**
 * A container that is stored in a metall datastore must be standard layout. These tests check
 * `sparse_map`, `sparse_set` and their bucket group `sparse_array` with `std::allocator`, with an
 * allocator that hands out `boost::interprocess::offset_ptr`, and with the metall allocator, for the
 * three sparsity levels. A map stores its elements as `map_slot`, which is standard layout if the key
 * and the mapped type are.
 */
namespace {
    using namespace dice::sparse_map;
    using dice::sparse_map::tests::offset_ptr_allocator;

    using entry_t = std::pair<std::uint64_t, std::uint64_t>;

    template<typename T>
    using metall_allocator = metall::manager::allocator_type<T>;

    template<template<typename> typename AllocatorOf, sh::sparsity Sparsity>
    using map_of = sparse_map<std::uint64_t,
                              std::uint64_t,
                              tests::test_hash<std::uint64_t>,
                              std::equal_to<std::uint64_t>,
                              AllocatorOf<entry_t>,
                              Sparsity>;

    template<template<typename> typename AllocatorOf, sh::sparsity Sparsity>
    using set_of = sparse_set<std::uint64_t,
                              tests::test_hash<std::uint64_t>,
                              std::equal_to<std::uint64_t>,
                              AllocatorOf<std::uint64_t>,
                              Sparsity>;

    template<template<typename> typename AllocatorOf, sh::sparsity Sparsity>
    using array_of = detail_sparse_hash::sparse_array<entry_t, AllocatorOf<entry_t>, Sparsity, dice::sparse_map::sh::allocation_failure::terminating>;

    constexpr auto high = sh::sparsity::high;
    constexpr auto medium = sh::sparsity::medium;
    constexpr auto low = sh::sparsity::low;

}  // namespace

TYPE_TO_STRING_AS("sparse_array<std::allocator, high>", array_of<std::allocator, high>);
TYPE_TO_STRING_AS("sparse_array<std::allocator, medium>", array_of<std::allocator, medium>);
TYPE_TO_STRING_AS("sparse_array<std::allocator, low>", array_of<std::allocator, low>);
TYPE_TO_STRING_AS("sparse_array<offset_ptr_allocator, high>", array_of<offset_ptr_allocator, high>);
TYPE_TO_STRING_AS("sparse_array<offset_ptr_allocator, medium>", array_of<offset_ptr_allocator, medium>);
TYPE_TO_STRING_AS("sparse_array<offset_ptr_allocator, low>", array_of<offset_ptr_allocator, low>);
TYPE_TO_STRING_AS("sparse_array<metall, high>", array_of<metall_allocator, high>);
TYPE_TO_STRING_AS("sparse_array<metall, medium>", array_of<metall_allocator, medium>);
TYPE_TO_STRING_AS("sparse_array<metall, low>", array_of<metall_allocator, low>);

TYPE_TO_STRING_AS("sparse_map<std::allocator, high>", map_of<std::allocator, high>);
TYPE_TO_STRING_AS("sparse_map<std::allocator, medium>", map_of<std::allocator, medium>);
TYPE_TO_STRING_AS("sparse_map<std::allocator, low>", map_of<std::allocator, low>);
TYPE_TO_STRING_AS("sparse_map<offset_ptr_allocator, high>", map_of<offset_ptr_allocator, high>);
TYPE_TO_STRING_AS("sparse_map<offset_ptr_allocator, medium>", map_of<offset_ptr_allocator, medium>);
TYPE_TO_STRING_AS("sparse_map<offset_ptr_allocator, low>", map_of<offset_ptr_allocator, low>);
TYPE_TO_STRING_AS("sparse_map<metall, high>", map_of<metall_allocator, high>);
TYPE_TO_STRING_AS("sparse_map<metall, medium>", map_of<metall_allocator, medium>);
TYPE_TO_STRING_AS("sparse_map<metall, low>", map_of<metall_allocator, low>);

TYPE_TO_STRING_AS("sparse_set<std::allocator, high>", set_of<std::allocator, high>);
TYPE_TO_STRING_AS("sparse_set<std::allocator, medium>", set_of<std::allocator, medium>);
TYPE_TO_STRING_AS("sparse_set<std::allocator, low>", set_of<std::allocator, low>);
TYPE_TO_STRING_AS("sparse_set<offset_ptr_allocator, high>", set_of<offset_ptr_allocator, high>);
TYPE_TO_STRING_AS("sparse_set<offset_ptr_allocator, medium>", set_of<offset_ptr_allocator, medium>);
TYPE_TO_STRING_AS("sparse_set<offset_ptr_allocator, low>", set_of<offset_ptr_allocator, low>);
TYPE_TO_STRING_AS("sparse_set<metall, high>", set_of<metall_allocator, high>);
TYPE_TO_STRING_AS("sparse_set<metall, medium>", set_of<metall_allocator, medium>);
TYPE_TO_STRING_AS("sparse_set<metall, low>", set_of<metall_allocator, low>);

TEST_CASE_TEMPLATE("sparse_array is standard layout",
                   array_t,
                   array_of<std::allocator, high>,
                   array_of<std::allocator, medium>,
                   array_of<std::allocator, low>,
                   array_of<offset_ptr_allocator, high>,
                   array_of<offset_ptr_allocator, medium>,
                   array_of<offset_ptr_allocator, low>,
                   array_of<metall_allocator, high>,
                   array_of<metall_allocator, medium>,
                   array_of<metall_allocator, low>) {
    MESSAGE(doctest::toString<array_t>() << ": sizeof " << sizeof(array_t));
    CHECK(std::is_standard_layout_v<array_t>);
}

TEST_CASE_TEMPLATE("sparse_map and sparse_set with a stateless allocator are standard layout",
                   container_t,
                   map_of<std::allocator, high>,
                   map_of<std::allocator, medium>,
                   map_of<std::allocator, low>,
                   map_of<offset_ptr_allocator, high>,
                   map_of<offset_ptr_allocator, medium>,
                   map_of<offset_ptr_allocator, low>,
                   set_of<std::allocator, high>,
                   set_of<std::allocator, medium>,
                   set_of<std::allocator, low>,
                   set_of<offset_ptr_allocator, high>,
                   set_of<offset_ptr_allocator, medium>,
                   set_of<offset_ptr_allocator, low>) {
    MESSAGE(doctest::toString<container_t>() << ": sizeof " << sizeof(container_t));
    CHECK(std::is_standard_layout_v<container_t>);
}

TEST_CASE_TEMPLATE("sparse_map and sparse_set with the metall allocator are standard layout",
                   container_t,
                   map_of<metall_allocator, high>,
                   map_of<metall_allocator, medium>,
                   map_of<metall_allocator, low>,
                   set_of<metall_allocator, high>,
                   set_of<metall_allocator, medium>,
                   set_of<metall_allocator, low>) {
    MESSAGE(doctest::toString<container_t>() << ": sizeof " << sizeof(container_t));
    CHECK(std::is_standard_layout_v<container_t>);
}

TEST_CASE("map_slot is standard layout") {
    CHECK(std::is_standard_layout_v<dice::sparse_map::detail_sparse_hash::map_slot<std::uint64_t, std::uint64_t>>);
}

namespace {
    /**
     * `map_slot<Key, T>` has the size and the member offsets of `std::pair<Key, T>`, so a map stored in a
     * datastore has the same bytes as one that stored its elements as `std::pair`.
     */
    template<typename Key, typename T>
    void check_map_slot_has_the_layout_of_pair() {
        using slot_t = dice::sparse_map::detail_sparse_hash::map_slot<Key, T>;
        using pair_t = std::pair<Key, T>;
        static_assert(sizeof(slot_t) == sizeof(pair_t));
        static_assert(alignof(slot_t) == alignof(pair_t));

        // `std::pair` is not standard layout by the standard, so its offsets are taken from an object
        auto const pair = pair_t{};
        auto const base = reinterpret_cast<char const *>(&pair);
        CHECK(reinterpret_cast<char const *>(&pair.first) - base == static_cast<std::ptrdiff_t>(offsetof(slot_t, key)));
        CHECK(reinterpret_cast<char const *>(&pair.second) - base == static_cast<std::ptrdiff_t>(offsetof(slot_t, value)));
    }

    /// a value whose move constructor can throw, so that the groups of a map of it have holes
    using copied_value = dice::sparse_map::tests::copied_value<int>;
}  // namespace

TEST_CASE("map_slot has the layout of std::pair") {
    check_map_slot_has_the_layout_of_pair<std::uint64_t, std::uint64_t>();
    check_map_slot_has_the_layout_of_pair<std::uint32_t, std::uint64_t>();
    check_map_slot_has_the_layout_of_pair<std::uint64_t, std::uint8_t>();
    check_map_slot_has_the_layout_of_pair<std::uint8_t, std::uint32_t>();
}

namespace {
    /**
     * Has two elements by `std::tuple_size`, but `std::get` does not take it.
     */
    struct tuple_size_without_get {
        int first;
        int second;
    };
}  // namespace

template<>
struct std::tuple_size<tuple_size_without_get> : std::integral_constant<std::size_t, 2> {};

TEST_CASE("map_slot is constructible from types with two elements that std::get returns") {
    using slot_t = dice::sparse_map::detail_sparse_hash::map_slot<int, int>;
    using alloc_t = std::allocator<slot_t>;
    static_assert(std::is_constructible_v<slot_t, std::pair<int, int>>);
    static_assert(std::is_constructible_v<slot_t, std::tuple<int, int>>);
    static_assert(std::is_constructible_v<slot_t, std::array<int, 2>>);
    static_assert(std::is_constructible_v<slot_t, std::allocator_arg_t, alloc_t const &, std::pair<int, int>>);
    static_assert(!std::is_constructible_v<slot_t, tuple_size_without_get>);
    static_assert(!std::is_constructible_v<slot_t, std::allocator_arg_t, alloc_t const &, tuple_size_without_get>);
}

TEST_CASE("sizes") {
    using map_t = dice::sparse_map::sparse_map<int, int>;
    using holes_map_t = dice::sparse_map::sparse_map<int, copied_value>;
    MESSAGE("sizeof(sparse_map<int, int>) = " << sizeof(map_t));
    MESSAGE("sizeof(sparse_set<int>) = " << sizeof(dice::sparse_map::sparse_set<int>));
    // eight members of 8 bytes: the bucket array, the number of groups, the bucket count, the number of elements, the
    // first group with an element, the number of deleted buckets and two load thresholds. Then the maximum load
    // factor, a float, with 4 bytes of padding.
    CHECK(sizeof(map_t) == 72);
    // the pointer to the values, two bitmaps, the number of values, the capacity and the flag of the last group
    CHECK(sizeof(dice::sparse_map::detail_sparse_hash::sparse_array<entry_t, std::allocator<entry_t>, sh::sparsity::medium, sh::allocation_failure::terminating>) == 32);
    // the group, the element, and the end of the elements of the group or, with holes, the run of the element (two
    // 32 bit counters)
    static_assert(!std::is_nothrow_move_constructible_v<dice::sparse_map::detail_sparse_hash::map_slot<int, copied_value>>);
    CHECK(sizeof(map_t::iterator) == 24);
    CHECK(sizeof(map_t::const_iterator) == 24);
    CHECK(sizeof(holes_map_t::iterator) == 24);
    CHECK(sizeof(holes_map_t::const_iterator) == 24);
}
