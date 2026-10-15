#ifndef DICE_SPARSE_MAP_BENCHMARKS_TRACKING_ALLOCATOR_HPP
#define DICE_SPARSE_MAP_BENCHMARKS_TRACKING_ALLOCATOR_HPP

#include <algorithm>
#include <cstddef>
#include <memory>

namespace dice::sparse_map::bench {

    /**
     * What a container requested through a `tracking_allocator`. Bytes are the bytes requested,
     * without the overhead of the memory allocator behind it.
     */
    struct allocation_stats {
        std::size_t current = 0;           ///< bytes allocated and not freed yet
        std::size_t peak = 0;              ///< the largest value `current` had
        std::size_t live_allocations = 0;  ///< allocations not freed yet
        std::size_t allocations = 0;       ///< all calls of `allocate`

        /**
         * Starts a new measurement. Only valid when nothing is allocated.
         */
        void reset() noexcept {
            *this = allocation_stats{};
        }
    };

    /**
     * Allocator that counts what a container requests in an `allocation_stats`. All copies and
     * rebinds of an allocator share its `allocation_stats`. The memory comes from `std::allocator`.
     * There is no default constructor, so a container cannot make an allocator that bypasses the
     * count.
     */
    template<typename T>
    struct tracking_allocator {
        using value_type = T;

        allocation_stats *stats;

        explicit tracking_allocator(allocation_stats *stats) noexcept
            : stats(stats) {
        }

        template<typename U>
        tracking_allocator(tracking_allocator<U> const &other) noexcept  // NOLINT(google-explicit-constructor)
            : stats(other.stats) {
        }

        [[nodiscard]] T *allocate(std::size_t n) {
            T *p = std::allocator<T>{}.allocate(n);
            stats->current += n * sizeof(T);
            stats->peak = std::max(stats->peak, stats->current);
            ++stats->live_allocations;
            ++stats->allocations;
            return p;
        }

        void deallocate(T *p, std::size_t n) noexcept {
            stats->current -= n * sizeof(T);
            --stats->live_allocations;
            std::allocator<T>{}.deallocate(p, n);
        }
    };

    template<typename T, typename U>
    bool operator==(tracking_allocator<T> const &lhs, tracking_allocator<U> const &rhs) noexcept {
        return lhs.stats == rhs.stats;
    }

}  // namespace dice::sparse_map::bench

#endif  // DICE_SPARSE_MAP_BENCHMARKS_TRACKING_ALLOCATOR_HPP
