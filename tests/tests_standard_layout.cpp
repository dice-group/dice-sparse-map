#include "fixtures/offset_ptr_allocator.hpp"

#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>
#include <metall/metall.hpp>

#include <cstdint>
#include <functional>
#include <memory>
#include <string>
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
    using namespace dice::unordered_sparse;
    using dice::unordered_sparse::tests::offset_ptr_allocator;

    using entry_t = std::pair<std::uint64_t, std::uint64_t>;

    template<typename T>
    using metall_allocator = metall::manager::allocator_type<T>;

    template<template<typename> typename AllocatorOf, sparsity Sparsity>
    using map_of = sparse_map<std::uint64_t,
                              std::uint64_t,
                              std::hash<std::uint64_t>,
                              std::equal_to<std::uint64_t>,
                              AllocatorOf<entry_t>,
                              Sparsity>;

    template<template<typename> typename AllocatorOf, sparsity Sparsity>
    using set_of = sparse_set<std::uint64_t,
                              std::hash<std::uint64_t>,
                              std::equal_to<std::uint64_t>,
                              AllocatorOf<std::uint64_t>,
                              Sparsity>;

    template<template<typename> typename AllocatorOf, sparsity Sparsity>
    using array_of = detail::sparse_array<entry_t, AllocatorOf<entry_t>, Sparsity>;

    /// with 4 elements in the inline group
    template<template<typename> typename AllocatorOf>
    using inline_map_of = sparse_map<std::uint64_t, std::uint64_t, std::hash<std::uint64_t>, std::equal_to<std::uint64_t>, AllocatorOf<entry_t>, sparsity::medium, 4>;

    template<template<typename> typename AllocatorOf>
    using inline_set_of = sparse_set<std::uint64_t, std::hash<std::uint64_t>, std::equal_to<std::uint64_t>, AllocatorOf<std::uint64_t>, sparsity::medium, 4>;

    constexpr auto high = sparsity::high;
    constexpr auto medium = sparsity::medium;
    constexpr auto low = sparsity::low;

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

TYPE_TO_STRING_AS("sparse_map<std::allocator, medium, 4>", inline_map_of<std::allocator>);
TYPE_TO_STRING_AS("sparse_map<offset_ptr_allocator, medium, 4>", inline_map_of<offset_ptr_allocator>);
TYPE_TO_STRING_AS("sparse_map<metall, medium, 4>", inline_map_of<metall_allocator>);
TYPE_TO_STRING_AS("sparse_set<std::allocator, medium, 4>", inline_set_of<std::allocator>);
TYPE_TO_STRING_AS("sparse_set<offset_ptr_allocator, medium, 4>", inline_set_of<offset_ptr_allocator>);
TYPE_TO_STRING_AS("sparse_set<metall, medium, 4>", inline_set_of<metall_allocator>);

TEST_CASE_TEMPLATE("sparse_map and sparse_set with an inline capacity are standard layout",
                   container_t,
                   inline_map_of<std::allocator>,
                   inline_map_of<offset_ptr_allocator>,
                   inline_map_of<metall_allocator>,
                   inline_set_of<std::allocator>,
                   inline_set_of<offset_ptr_allocator>,
                   inline_set_of<metall_allocator>) {
    MESSAGE(doctest::toString<container_t>() << ": sizeof " << sizeof(container_t));
    CHECK(std::is_standard_layout_v<container_t>);
}

TEST_CASE("map_slot is standard layout") {
    CHECK(std::is_standard_layout_v<dice::unordered_sparse::detail::map_slot<std::uint64_t, std::uint64_t>>);
}

namespace {
    /// a value whose move constructor can throw, so that the groups of a map of it have holes
    struct copied_value {
        int value = 0;

        copied_value() = default;
        copied_value(copied_value const &) = default;

        copied_value(copied_value &&other) noexcept(false)  // NOLINT(performance-noexcept-move-constructor)
            : value(other.value) {
        }

        copied_value &operator=(copied_value const &) = default;
        copied_value &operator=(copied_value &&) noexcept(false) = default;
        ~copied_value() = default;
    };
}  // namespace

TEST_CASE("sizes") {
    using map_t = dice::sparse_map<int, int>;
    using holes_map_t = dice::sparse_map<int, copied_value>;
    MESSAGE("sizeof(sparse_map<int, int>) = " << sizeof(map_t));
    MESSAGE("sizeof(sparse_set<int>) = " << sizeof(dice::sparse_set<int>));
    // eight members of 8 bytes: the bucket array, the number of groups, the bucket count, the number of elements, the
    // first group with an element, the number of deleted buckets and two load thresholds. Then the maximum load
    // factor, a float, with 4 bytes of padding.
    CHECK(sizeof(map_t) == 72);
    // the pointer to the values, two bitmaps, the number of values, the capacity and the flag of the last group
    CHECK(sizeof(dice::unordered_sparse::detail::sparse_array<entry_t, std::allocator<entry_t>, sparsity::medium>) == 32);
    // the group, the element, and the end of the elements of the group or, with holes, the run of the element (two
    // 32 bit counters)
    static_assert(!std::is_nothrow_move_constructible_v<dice::unordered_sparse::detail::map_slot<int, copied_value>>);
    CHECK(sizeof(map_t::iterator) == 24);
    CHECK(sizeof(map_t::const_iterator) == 24);
    CHECK(sizeof(holes_map_t::iterator) == 24);
    CHECK(sizeof(holes_map_t::const_iterator) == 24);
}

TEST_CASE("sizes with the inline capacity 4") {
    using string_map_t = sparse_map<std::string, std::uint64_t, std::hash<std::string>, std::equal_to<std::string>, std::allocator<std::pair<std::string, std::uint64_t>>, sparsity::medium, 4>;
    MESSAGE("sizeof(sparse_map<std::uint64_t, std::uint64_t, ..., 4>) = " << sizeof(inline_map_of<std::allocator>));
    MESSAGE("sizeof(sparse_set<std::uint64_t, ..., 4>) = " << sizeof(inline_set_of<std::allocator>));
    MESSAGE("sizeof(sparse_map<std::string, std::uint64_t, ..., 4>) = " << sizeof(string_map_t));
    MESSAGE("sizeof(sparse_map<std::uint64_t, std::uint64_t, ..., metall, ..., 4>) = " << sizeof(inline_map_of<metall_allocator>));
    MESSAGE("sizeof(sparse_set<std::uint64_t, ..., metall, ..., 4>) = " << sizeof(inline_set_of<metall_allocator>));
    MESSAGE("sizeof(sparse_map<std::uint64_t, std::uint64_t, ..., metall>) = " << sizeof(map_of<metall_allocator, medium>));
    // the element count (8 bytes), the maximum load factor (4) and the flag (1, with 3 bytes of padding), then the
    // union of the state of the buckets (56 bytes) and the inline group: two bitmaps of 8 bytes and 4 elements
    CHECK(sizeof(inline_map_of<std::allocator>) == 16 + 16 + 4 * sizeof(entry_t));
    CHECK(sizeof(inline_set_of<std::allocator>) == 16 + 56);
    CHECK(sizeof(string_map_t) == 16 + 16 + 4 * sizeof(dice::unordered_sparse::detail::map_slot<std::string, std::uint64_t>));
    // the allocator of metall holds a pointer of 8 bytes
    CHECK(sizeof(inline_map_of<metall_allocator>) == 8 + 16 + 16 + 4 * sizeof(entry_t));
    CHECK(sizeof(map_of<metall_allocator, medium>) == 80);
}
