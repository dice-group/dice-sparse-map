#include "fixtures/offset_ptr_allocator.hpp"

#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>
#include <metall/metall.hpp>

#include <cstdint>
#include <functional>
#include <memory>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * A container that is stored in a metall datastore must be standard layout. These tests check
 * `sparse_map`, `sparse_set` and their bucket group `sparse_array` with `std::allocator`, with an
 * allocator that hands out `boost::interprocess::offset_ptr`, and with the metall allocator, for the
 * three sparsity levels.
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
                              power_of_two_growth_policy<2>,
                              exception_safety::basic,
                              Sparsity>;

    template<template<typename> typename AllocatorOf, sparsity Sparsity>
    using set_of = sparse_set<std::uint64_t,
                              std::hash<std::uint64_t>,
                              std::equal_to<std::uint64_t>,
                              AllocatorOf<std::uint64_t>,
                              power_of_two_growth_policy<2>,
                              exception_safety::basic,
                              Sparsity>;

    template<template<typename> typename AllocatorOf, sparsity Sparsity>
    using array_of = detail::sparse_array<entry_t, AllocatorOf<entry_t>, Sparsity>;

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

// not standard layout yet: `sparse_hash` keeps its buckets in a `std::vector`, which is not standard layout with a stateful allocator in libstdc++
TEST_CASE_TEMPLATE("sparse_map and sparse_set with the metall allocator are standard layout" * doctest::should_fail(),
                   container_t,
                   map_of<metall_allocator, high>,
                   map_of<metall_allocator, medium>,
                   map_of<metall_allocator, low>,
                   set_of<metall_allocator, high>,
                   set_of<metall_allocator, medium>,
                   set_of<metall_allocator, low>) {
    MESSAGE(doctest::toString<container_t>() << ": sizeof " << sizeof(container_t));
    MESSAGE("std::vector with the metall allocator is standard layout: " << std::is_standard_layout_v<std::vector<entry_t, metall_allocator<entry_t>>>);
    CHECK(std::is_standard_layout_v<container_t>);
}
