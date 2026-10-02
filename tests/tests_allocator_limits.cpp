#include "fixtures/test_types.hpp"

#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <memory>
#include <stdexcept>

/**
 * `max_bucket_count()` and `max_size()` are limits that a container reaches. An allocator with a 16-bit `size_type`
 * makes them small enough to fill in a test.
 */
namespace {
    using namespace dice::sparse_map;

    /**
     * `std::allocator` with a 16-bit `size_type`. All instances compare equal.
     *
     * With `limited_by_size`, `max_size()` is the default of `std::allocator_traits`: the largest `size_type` divided
     * by the size of `T`. Otherwise `max_size()` is the largest `size_type`, for every `T`.
     */
    template<typename T, bool limited_by_size>
    struct small_allocator {
        using value_type = T;
        using size_type = std::uint16_t;
        using difference_type = std::int16_t;

        template<typename U>
        struct rebind {
            using other = small_allocator<U, limited_by_size>;
        };

        small_allocator() noexcept = default;

        template<typename U>
        small_allocator(small_allocator<U, limited_by_size> const & /*other*/) noexcept {  // NOLINT(google-explicit-constructor)
        }

        T *allocate(size_type n) {
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, size_type n) noexcept {
            std::allocator<T>{}.deallocate(p, n);
        }

        [[nodiscard]] size_type max_size() const noexcept
            requires (!limited_by_size)
        {
            return std::numeric_limits<size_type>::max();
        }

        friend bool operator==(small_allocator const & /*lhs*/, small_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

    template<bool limited_by_size>
    using small_set = sparse_set<std::uint16_t, tests::test_hash<std::uint16_t>, std::equal_to<std::uint16_t>, small_allocator<std::uint16_t, limited_by_size>>;

    /// the largest power of two of a 16-bit `size_type`
    constexpr std::size_t largest_bucket_count = std::size_t{1} << 15U;
}  // namespace

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
