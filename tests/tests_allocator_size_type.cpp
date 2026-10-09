#include "fixtures/test_types.hpp"
#include "fixtures/utils.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <ranges>
#include <stdexcept>
#include <type_traits>
#include <utility>

/**
 * The allocator requirements allow any unsigned `size_type` that can hold the size of the largest object, and a
 * matching signed `difference_type`. An allocator for a 32-bit offset pointer, for example, has 32-bit types. The
 * containers take `size_type` and `difference_type` from the allocator, so they must work with such types. The
 * second part of the file fills a container up to `max_bucket_count()` and `max_size()` with a 16-bit `size_type`.
 */
namespace {
    using namespace dice::sparse_map;

    /**
     * `std::allocator` with a 32-bit `size_type` and `difference_type`. All instances compare equal.
     */
    template<typename T>
    struct uint32_size_allocator {
        using value_type = T;
        using size_type = std::uint32_t;
        using difference_type = std::int32_t;

        uint32_size_allocator() noexcept = default;

        template<typename U>
        uint32_size_allocator(
            uint32_size_allocator<U> const & /*other*/
        ) noexcept { // NOLINT(google-explicit-constructor)
        }

        T *allocate(size_type n) {
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, size_type n) noexcept {
            std::allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(uint32_size_allocator const & /*lhs*/, uint32_size_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

    using map_t = sparse_map<
        std::uint32_t,
        std::uint32_t,
        tests::test_hash<std::uint32_t>,
        std::equal_to<std::uint32_t>,
        uint32_size_allocator<std::pair<std::uint32_t, std::uint32_t>>
    >;
    using set_t = sparse_set<
        std::uint32_t,
        tests::test_hash<std::uint32_t>,
        std::equal_to<std::uint32_t>,
        uint32_size_allocator<std::uint32_t>
    >;

    /// the element with key `key`, for the map with the mapped value `key`
    template<typename Container>
    typename Container::value_type value_of(std::uint32_t key) {
        if constexpr (requires { typename Container::mapped_type; }) {
            return {key, key};
        } else {
            return key;
        }
    }

    /**
     * A range of one element whose `size()` says that it has 2^31 elements.
     */
    template<typename T>
    struct one_element_claiming_2_31 {
        T element;

        [[nodiscard]] T const *begin() const noexcept {
            return &element;
        }

        [[nodiscard]] T const *end() const noexcept {
            return &element + 1;
        }

        [[nodiscard]] std::uint32_t size() const noexcept {
            return std::uint32_t{1} << 31U;
        }
    };

    constexpr std::uint32_t nb_keys = 1000;
} // namespace

TYPE_TO_STRING_AS("map", map_t);
TYPE_TO_STRING_AS("set", set_t);

TEST_CASE_TEMPLATE("a container whose allocator has a 32-bit size_type holds elements", container_t, map_t, set_t) {
    static_assert(std::is_same_v<typename container_t::size_type, std::uint32_t>);

    auto container = container_t{};
    CHECK(container.max_bucket_count() > 0);
    CHECK(container.max_size() > 0);

    REQUIRE_NOTHROW(tests::insert_one(container, 0));
    for (std::uint32_t key = 1; key < nb_keys; ++key) {
        tests::insert_one(container, key);
    }
    CHECK(container.size() == nb_keys);

    std::uint32_t found = 0;
    for (std::uint32_t key = 0; key < nb_keys; ++key) {
        found += container.contains(key) ? 1 : 0;
    }
    CHECK(found == nb_keys);

    CHECK_NOTHROW(container.reserve(10 * nb_keys));
    CHECK(container.size() == nb_keys);
}

TEST_CASE_TEMPLATE(
    "a container whose allocator has a 32-bit difference_type uses it for its iterators",
    container_t,
    map_t,
    set_t
) {
    CHECK(
        std::is_same_v<
            typename container_t::difference_type,
            typename std::iterator_traits<typename container_t::iterator>::difference_type
        >
    );
    CHECK(
        std::is_same_v<
            typename container_t::difference_type,
            typename std::iterator_traits<typename container_t::const_iterator>::difference_type
        >
    );
}

// `reserve(n)` computes the bucket count for `n` elements as a `std::size_t`. For 2^31 elements at the default maximum
// load factor of 0.5 that is 2^32, which a 32-bit `size_type` cannot hold. `reserve` throws instead of reserving fewer
// buckets.
TEST_CASE_TEMPLATE(
    "reserve of more elements than a 32-bit size_type can count buckets for throws",
    container_t,
    map_t,
    set_t
) {
    auto container = container_t{};
    tests::insert_one(container, 1);
    auto const bucket_count = container.bucket_count();

    CHECK_THROWS_AS(container.reserve(std::uint32_t{1} << 31U), std::length_error);
    CHECK(container.bucket_count() == bucket_count);
    CHECK(container.size() == 1);
    CHECK(container.contains(1));
}

// `insert_range` of a sized range reserves room for `std::ranges::size(range)` more elements. That is a hint: the
// elements can have equal keys, so the range can be longer than `max_size()` and still fit. The range here has one
// element and says that it has 2^31, more than `max_size()`. `insert_range` must not throw `std::length_error`.
TEST_CASE_TEMPLATE(
    "insert_range of a sized range longer than max_size() inserts its elements",
    container_t,
    map_t,
    set_t
) {
    auto const range = one_element_claiming_2_31<typename container_t::value_type>{value_of<container_t>(7)};
    static_assert(std::ranges::sized_range<decltype(range)>);

    auto container = container_t{};
    REQUIRE(std::ranges::size(range) > container.max_size());

    CHECK_NOTHROW(container.insert_range(range));
    CHECK(container.size() == 1);
    CHECK(container.contains(7));
}

/*
 * `max_bucket_count()` and `max_size()` are limits that a container reaches. An allocator with a 16-bit `size_type`
 * makes them small enough to fill in a test.
 */
namespace {
    /**
     * `std::allocator` with a 16-bit `size_type`. All instances compare equal.
     *
     * With `limited_by_size`, `max_size()` is the default of `std::allocator_traits`: the largest `size_type` divided
     * by the size of `T`. Otherwise `max_size()` is the largest `size_type`, for every `T`.
     */
    template<typename T, bool limited_by_size>
    struct uint16_size_allocator {
        using value_type = T;
        using size_type = std::uint16_t;
        using difference_type = std::int16_t;

        template<typename U>
        struct rebind {
            using other = uint16_size_allocator<U, limited_by_size>;
        };

        uint16_size_allocator() noexcept = default;

        template<typename U>
        uint16_size_allocator(
            uint16_size_allocator<U, limited_by_size> const & /*other*/
        ) noexcept { // NOLINT(google-explicit-constructor)
        }

        T *allocate(size_type n) {
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, size_type n) noexcept {
            std::allocator<T>{}.deallocate(p, n);
        }

        [[nodiscard]] size_type max_size() const noexcept
        requires (!limited_by_size) {
            return std::numeric_limits<size_type>::max();
        }

        friend bool operator==(uint16_size_allocator const & /*lhs*/, uint16_size_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

    template<bool limited_by_size>
    using small_set = sparse_set<
        std::uint16_t,
        tests::test_hash<std::uint16_t>,
        std::equal_to<std::uint16_t>,
        uint16_size_allocator<std::uint16_t, limited_by_size>
    >;

    /// the largest power of two of a 16-bit `size_type`
    constexpr std::size_t largest_bucket_count = std::size_t{1} << 15U;

    /// inserts the keys 0 to `nb_keys - 1`
    template<typename Set>
    void insert_keys(Set &set, std::size_t nb_keys) {
        for (std::size_t key = 0; key < nb_keys; ++key) {
            set.insert(static_cast<std::uint16_t>(key));
        }
    }
} // namespace

// The allocator provides 65535 bytes per allocation. That is enough for 2047 groups of 64 buckets, so the bucket count
// is limited by `size_type` alone.
TEST_CASE("max_bucket_count counts 64 buckets for each group that the allocator provides") {
    auto set = small_set<true>{};
    CHECK(set.max_bucket_count() == largest_bucket_count);

    REQUIRE_NOTHROW(set.rehash(set.max_bucket_count()));
    CHECK(set.bucket_count() == set.max_bucket_count());

    CHECK_THROWS_AS(set.rehash(static_cast<std::uint16_t>(set.max_bucket_count() + 1)), std::length_error);
    CHECK(set.bucket_count() == set.max_bucket_count());
}

// The allocator does not limit the size here. With `max_bucket_count()` buckets the set holds
// `max_bucket_count() * max_load_factor()` elements, rounded down, and that is `max_size()`.
TEST_CASE("a set holds max_size() elements and throws std::length_error for one more") {
    for (float const max_load_factor : {0.5F, 0.8F}) {
        CAPTURE(max_load_factor);
        auto set = small_set<false>{};
        set.max_load_factor(max_load_factor);
        REQUIRE(set.max_bucket_count() == largest_bucket_count);

        auto const max_size = set.max_size();
        REQUIRE_NOTHROW(insert_keys(set, max_size));
        CHECK(set.size() == max_size);
        CHECK(set.bucket_count() == set.max_bucket_count());

        CHECK_THROWS_AS(set.insert(static_cast<std::uint16_t>(max_size)), std::length_error);
        CHECK(set.size() == max_size);
    }
}
