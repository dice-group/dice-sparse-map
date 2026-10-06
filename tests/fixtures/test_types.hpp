#ifndef DICE_SPARSE_MAP_TESTS_FIXTURES_TEST_TYPES_HPP
#define DICE_SPARSE_MAP_TESTS_FIXTURES_TEST_TYPES_HPP

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include "fixtures/offset_ptr_allocator.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

/**
 * The container configurations that every `TEST_CASE_MAP` and `TEST_CASE_SET` runs against:
 * the three sparsity levels, `std::hash` without mixing (the identity for integers), the strong
 * exception guarantee, and an allocator with fancy pointers.
 *
 * Without a `Hash` argument, the `medium` configuration takes the default hash function of the
 * containers for the key types that `DiceHash` hashes (see `hashed_by_dice_hash`), and `test_hash<Key>`
 * otherwise. The `std_hash` configuration takes `std::hash<Key>` as it is, which is the identity for
 * integers, so that the tests also run with many collisions. The others take `test_hash<Key>`.
 */
namespace dice::sparse_map::tests {

    /**
     * Multiplies `a` and `b` to 128 bits and returns the xor of the two halves.
     */
    [[nodiscard]] constexpr std::uint64_t multiply_fold(std::uint64_t a, std::uint64_t b) noexcept {
#if defined(__SIZEOF_INT128__)
        __extension__ using uint128_t = unsigned __int128;
        auto const product = static_cast<uint128_t>(a) * b;
        return static_cast<std::uint64_t>(product) ^ static_cast<std::uint64_t>(product >> 64U);
#else
        std::uint64_t const a_lo = a & 0xFFFFFFFFU;
        std::uint64_t const a_hi = a >> 32U;
        std::uint64_t const b_lo = b & 0xFFFFFFFFU;
        std::uint64_t const b_hi = b >> 32U;
        std::uint64_t const lo_lo = a_lo * b_lo;
        std::uint64_t const hi_lo = a_hi * b_lo;
        std::uint64_t const lo_hi = a_lo * b_hi;
        std::uint64_t const hi_hi = a_hi * b_hi;
        std::uint64_t const cross = (lo_lo >> 32U) + (hi_lo & 0xFFFFFFFFU) + lo_hi;
        std::uint64_t const hi = hi_hi + (hi_lo >> 32U) + (cross >> 32U);
        std::uint64_t const lo = (cross << 32U) | (lo_lo & 0xFFFFFFFFU);
        return lo ^ hi;
#endif
    }

    /**
     * `std::hash<Key>` mixed with one multiplication by the golden ratio constant (the mixing of
     * `ankerl::unordered_dense`), marked as avalanching. It is the hash function of the configurations
     * other than `medium` and `std_hash`, of `medium` for key types that the default hash function
     * of the containers does not hash, and of the tests that need a marked hash function based on
     * `std::hash`.
     */
    template<typename Key>
    struct test_hash {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(Key const &key) const noexcept(noexcept(std::hash<Key>{}(key))) {
            return static_cast<std::size_t>(multiply_fold(std::hash<Key>{}(key), UINT64_C(0x9E3779B97F4A7C15)));
        }
    };

    /**
     * `Hash`, marked as avalanching, so that the containers accept it even if it is not avalanching. The
     * `std_hash` configuration uses it with `std::hash`, which is the identity for integers, so that the
     * tests also run with a weak hash function and many collisions.
     */
    template<typename Hash>
    struct marked_avalanching : Hash {
        using is_avalanching = void;
    };

    /**
     * True for the key types that `dice::hash::DiceHash` hashes without an overload of the user: arithmetic types,
     * pointers, `std::string`, and pairs of them. For other key types of the tests, like the counters and the
     * move-only types, `DiceHash` does not compile.
     */
    template<typename Key>
    inline constexpr bool hashed_by_dice_hash = std::is_arithmetic_v<Key> || std::is_pointer_v<Key> || std::is_same_v<Key, std::string>;

    template<typename First, typename Second>
    inline constexpr bool hashed_by_dice_hash<std::pair<First, Second>> = hashed_by_dice_hash<First> && hashed_by_dice_hash<Second>;

    /**
     * The default hash function of the containers if it hashes `Key`, otherwise `test_hash<Key>`.
     */
    template<typename Key>
    using default_or_test_hash = std::conditional_t<hashed_by_dice_hash<Key>,
                                                    detail_sparse_hash::default_hash<Key>,
                                                    test_hash<Key>>;

    template<typename Key, typename T, typename Hash = default_or_test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_medium = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::exception_safety::basic, sh::sparsity::medium>;

    template<typename Key, typename T, typename Hash = test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_high = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::exception_safety::basic, sh::sparsity::high>;

    template<typename Key, typename T, typename Hash = test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_low = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::exception_safety::basic, sh::sparsity::low>;

    template<typename Key, typename T, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_std_hash = sparse_map<Key, T, marked_avalanching<Hash>, KeyEqual, std::allocator<std::pair<Key, T>>, sh::exception_safety::basic, sh::sparsity::medium>;

    template<typename Key, typename T, typename Hash = test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_strong = sparse_map<Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>, sh::exception_safety::strong, sh::sparsity::medium>;

    template<typename Key, typename T, typename Hash = test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using map_offset_ptr = sparse_map<Key, T, Hash, KeyEqual, offset_ptr_allocator<std::pair<Key, T>>, sh::exception_safety::basic, sh::sparsity::high>;

    template<typename Key, typename Hash = default_or_test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_medium = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::exception_safety::basic, sh::sparsity::medium>;

    template<typename Key, typename Hash = test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_high = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::exception_safety::basic, sh::sparsity::high>;

    template<typename Key, typename Hash = test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_low = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::exception_safety::basic, sh::sparsity::low>;

    template<typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_std_hash = sparse_set<Key, marked_avalanching<Hash>, KeyEqual, std::allocator<Key>, sh::exception_safety::basic, sh::sparsity::medium>;

    template<typename Key, typename Hash = test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_strong = sparse_set<Key, Hash, KeyEqual, std::allocator<Key>, sh::exception_safety::strong, sh::sparsity::medium>;

    template<typename Key, typename Hash = test_hash<Key>, typename KeyEqual = std::equal_to<Key>>
    using set_offset_ptr = sparse_set<Key, Hash, KeyEqual, offset_ptr_allocator<Key>, sh::exception_safety::basic, sh::sparsity::high>;

    // The default hash function runs in the `medium` configuration for integer and string keys.
    static_assert(std::is_same_v<map_medium<int, int>::hasher, detail_sparse_hash::default_hash<int>>);
    static_assert(std::is_same_v<set_medium<std::string>::hasher, detail_sparse_hash::default_hash<std::string>>);

}  // namespace dice::sparse_map::tests

/**
 * Defines a doctest template test case named `name` with the type parameter `map_t`. It runs once
 * for every map configuration above. The remaining arguments are the template arguments of the map:
 * `Key, T` and optionally `Hash, KeyEqual`.
 */
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TEST_CASE_MAP(name, ...)                                                \
    TEST_CASE_TEMPLATE(name,                                                    \
                       map_t,                                                   \
                       ::dice::sparse_map::tests::map_medium<__VA_ARGS__>,      \
                       ::dice::sparse_map::tests::map_high<__VA_ARGS__>,        \
                       ::dice::sparse_map::tests::map_low<__VA_ARGS__>,         \
                       ::dice::sparse_map::tests::map_std_hash<__VA_ARGS__>, \
                       ::dice::sparse_map::tests::map_strong<__VA_ARGS__>,      \
                       ::dice::sparse_map::tests::map_offset_ptr<__VA_ARGS__>)

/**
 * Like `TEST_CASE_MAP`, for sets. The type parameter is `set_t`, the remaining arguments are
 * `Key` and optionally `Hash, KeyEqual`.
 */
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TEST_CASE_SET(name, ...)                                                \
    TEST_CASE_TEMPLATE(name,                                                    \
                       set_t,                                                   \
                       ::dice::sparse_map::tests::set_medium<__VA_ARGS__>,      \
                       ::dice::sparse_map::tests::set_high<__VA_ARGS__>,        \
                       ::dice::sparse_map::tests::set_low<__VA_ARGS__>,         \
                       ::dice::sparse_map::tests::set_std_hash<__VA_ARGS__>, \
                       ::dice::sparse_map::tests::set_strong<__VA_ARGS__>,      \
                       ::dice::sparse_map::tests::set_offset_ptr<__VA_ARGS__>)

#endif  // DICE_SPARSE_MAP_TESTS_FIXTURES_TEST_TYPES_HPP
