#ifndef DICE_SPARSE_MAP_TESTS_FIXTURES_TEST_TYPES_HPP
#define DICE_SPARSE_MAP_TESTS_FIXTURES_TEST_TYPES_HPP

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include "fixtures/offset_ptr_allocator.hpp"

#include <doctest/doctest.h>

#include <functional>
#include <memory>
#include <utility>

/**
 * The container configurations that every `TEST_CASE_MAP` and `TEST_CASE_SET` runs against:
 * the three sparsity levels, the prime growth policy, the strong exception guarantee, and an
 * allocator with fancy pointers.
 */
namespace dice::sparse_map::tests {

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_medium = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::power_of_two_growth_policy<2>, sh::exception_safety::basic, sh::sparsity::medium>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_high = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::power_of_two_growth_policy<2>, sh::exception_safety::basic, sh::sparsity::high>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_low = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::power_of_two_growth_policy<2>, sh::exception_safety::basic, sh::sparsity::low>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_prime = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::prime_growth_policy, sh::exception_safety::basic, sh::sparsity::medium>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_strong = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::power_of_two_growth_policy<2>, sh::exception_safety::strong, sh::sparsity::medium>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_offset_ptr = sparse_map<Key, T, Hash, KeyEqual, offset_ptr_allocator<std::pair<Key, T>>, sh::power_of_two_growth_policy<2>, sh::exception_safety::basic, sh::sparsity::high>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_medium = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::power_of_two_growth_policy<2>, sh::exception_safety::basic, sh::sparsity::medium>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_high = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::power_of_two_growth_policy<2>, sh::exception_safety::basic, sh::sparsity::high>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_low = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::power_of_two_growth_policy<2>, sh::exception_safety::basic, sh::sparsity::low>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_prime = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::prime_growth_policy, sh::exception_safety::basic, sh::sparsity::medium>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_strong = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::power_of_two_growth_policy<2>, sh::exception_safety::strong, sh::sparsity::medium>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_offset_ptr = sparse_set<Key, Hash, KeyEqual, offset_ptr_allocator<Key>, sh::power_of_two_growth_policy<2>, sh::exception_safety::basic, sh::sparsity::high>;

}  // namespace dice::sparse_map::tests

/**
 * Defines a doctest template test case named `name` with the type parameter `map_t`. It runs once
 * for every map configuration above. The remaining arguments are the template arguments of the map:
 * `Key, T` and optionally `Hash, KeyEqual`.
 */
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TEST_CASE_MAP(name, ...)                                           \
    TEST_CASE_TEMPLATE(name,                                               \
                       map_t,                                              \
                       ::dice::sparse_map::tests::map_medium<__VA_ARGS__>, \
                       ::dice::sparse_map::tests::map_high<__VA_ARGS__>,   \
                       ::dice::sparse_map::tests::map_low<__VA_ARGS__>,    \
                       ::dice::sparse_map::tests::map_prime<__VA_ARGS__>,  \
                       ::dice::sparse_map::tests::map_strong<__VA_ARGS__>, \
                       ::dice::sparse_map::tests::map_offset_ptr<__VA_ARGS__>)

/**
 * Like `TEST_CASE_MAP`, for sets. The type parameter is `set_t`, the remaining arguments are
 * `Key` and optionally `Hash, KeyEqual`.
 */
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TEST_CASE_SET(name, ...)                                           \
    TEST_CASE_TEMPLATE(name,                                               \
                       set_t,                                              \
                       ::dice::sparse_map::tests::set_medium<__VA_ARGS__>, \
                       ::dice::sparse_map::tests::set_high<__VA_ARGS__>,   \
                       ::dice::sparse_map::tests::set_low<__VA_ARGS__>,    \
                       ::dice::sparse_map::tests::set_prime<__VA_ARGS__>,  \
                       ::dice::sparse_map::tests::set_strong<__VA_ARGS__>, \
                       ::dice::sparse_map::tests::set_offset_ptr<__VA_ARGS__>)

#endif  // DICE_SPARSE_MAP_TESTS_FIXTURES_TEST_TYPES_HPP
