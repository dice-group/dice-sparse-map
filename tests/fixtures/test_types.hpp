#ifndef DICE_UNORDERED_SPARSE_TESTS_FIXTURES_TEST_TYPES_HPP
#define DICE_UNORDERED_SPARSE_TESTS_FIXTURES_TEST_TYPES_HPP

#include <dice/unordered_sparse.hpp>

#include "fixtures/offset_ptr_allocator.hpp"

#include <doctest/doctest.h>

#include <functional>
#include <memory>
#include <utility>

/**
 * The container configurations that every `TEST_CASE_MAP` and `TEST_CASE_SET` runs against:
 * the three sparsity levels, a hash function marked as avalanching (no mixing), the strong
 * exception guarantee, and an allocator with fancy pointers.
 */
namespace dice::unordered_sparse::tests {

    /**
     * `Hash`, marked as avalanching, so that the table uses its hash without mixing it.
     */
    template<typename Hash>
    struct avalanching : Hash {
        using is_avalanching = void;
    };

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_medium = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, exception_safety::basic, sparsity::medium>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_high = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, exception_safety::basic, sparsity::high>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_low = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, exception_safety::basic, sparsity::low>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_avalanching = sparse_map<Key, T, avalanching<Hash>, KeyEqual, std::allocator<std::pair<Key, T>>, exception_safety::basic, sparsity::medium>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_strong = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, exception_safety::strong, sparsity::medium>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_offset_ptr = sparse_map<Key, T, Hash, KeyEqual, offset_ptr_allocator<std::pair<Key, T>>, exception_safety::basic, sparsity::high>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_medium = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, exception_safety::basic, sparsity::medium>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_high = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, exception_safety::basic, sparsity::high>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_low = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, exception_safety::basic, sparsity::low>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_avalanching = sparse_set<Key, avalanching<Hash>, KeyEqual, std::allocator<Key>, exception_safety::basic, sparsity::medium>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_strong = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, exception_safety::strong, sparsity::medium>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_offset_ptr = sparse_set<Key, Hash, KeyEqual, offset_ptr_allocator<Key>, exception_safety::basic, sparsity::high>;

}  // namespace dice::unordered_sparse::tests

/**
 * Defines a doctest template test case named `name` with the type parameter `map_t`. It runs once
 * for every map configuration above. The remaining arguments are the template arguments of the map:
 * `Key, T` and optionally `Hash, KeyEqual`.
 */
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TEST_CASE_MAP(name, ...)                                                      \
    TEST_CASE_TEMPLATE(name,                                                          \
                       map_t,                                                         \
                       ::dice::unordered_sparse::tests::map_medium<__VA_ARGS__>,      \
                       ::dice::unordered_sparse::tests::map_high<__VA_ARGS__>,        \
                       ::dice::unordered_sparse::tests::map_low<__VA_ARGS__>,         \
                       ::dice::unordered_sparse::tests::map_avalanching<__VA_ARGS__>, \
                       ::dice::unordered_sparse::tests::map_strong<__VA_ARGS__>,      \
                       ::dice::unordered_sparse::tests::map_offset_ptr<__VA_ARGS__>)

/**
 * Like `TEST_CASE_MAP`, for sets. The type parameter is `set_t`, the remaining arguments are
 * `Key` and optionally `Hash, KeyEqual`.
 */
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TEST_CASE_SET(name, ...)                                                      \
    TEST_CASE_TEMPLATE(name,                                                          \
                       set_t,                                                         \
                       ::dice::unordered_sparse::tests::set_medium<__VA_ARGS__>,      \
                       ::dice::unordered_sparse::tests::set_high<__VA_ARGS__>,        \
                       ::dice::unordered_sparse::tests::set_low<__VA_ARGS__>,         \
                       ::dice::unordered_sparse::tests::set_avalanching<__VA_ARGS__>, \
                       ::dice::unordered_sparse::tests::set_strong<__VA_ARGS__>,      \
                       ::dice::unordered_sparse::tests::set_offset_ptr<__VA_ARGS__>)

#endif  // DICE_UNORDERED_SPARSE_TESTS_FIXTURES_TEST_TYPES_HPP
