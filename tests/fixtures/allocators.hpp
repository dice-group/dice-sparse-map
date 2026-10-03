#ifndef DICE_UNORDERED_SPARSE_TESTS_FIXTURES_ALLOCATORS_HPP
#define DICE_UNORDERED_SPARSE_TESTS_FIXTURES_ALLOCATORS_HPP

#include <algorithm>
#include <cstddef>
#include <memory>
#include <new>
#include <type_traits>
#include <utility>

/**
 * Allocators for tests that check how a container uses its allocator.
 *
 * Ported from the test fixtures of ankerl::unordered_dense (MIT license).
 */
namespace dice::unordered_sparse::tests {

    /**
     * Number of allocations that are left before `bombing_allocator` throws `std::bad_alloc`.
     * -1 never throws. The countdown is global, because a container copies and rebinds its allocator.
     */
    inline int allocations_until_throw = -1;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * Arms the countdown of `bombing_allocator` while it is in scope.
     */
    struct bomb_after {
        explicit bomb_after(int n) noexcept {
            allocations_until_throw = n;
        }
        bomb_after(bomb_after const &) = delete;
        bomb_after(bomb_after &&) = delete;
        bomb_after &operator=(bomb_after const &) = delete;
        bomb_after &operator=(bomb_after &&) = delete;
        ~bomb_after() {
            allocations_until_throw = -1;
        }
    };

    /**
     * Allocator that throws `std::bad_alloc` on the n-th allocation after a `bomb_after(n)`.
     */
    template<typename T>
    struct bombing_allocator {
        using value_type = T;

        bombing_allocator() noexcept = default;

        template<typename U>
        bombing_allocator(bombing_allocator<U> const & /*other*/) noexcept {  // NOLINT(google-explicit-constructor)
        }

        T *allocate(std::size_t n) {
            if (allocations_until_throw >= 0 && 0 == allocations_until_throw--) {
                throw std::bad_alloc{};
            }
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, std::size_t n) noexcept {
            std::allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(bombing_allocator const & /*lhs*/, bombing_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

    /// blocks that `leak_checking_allocator` handed out and did not get back yet
    inline std::ptrdiff_t live_blocks = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// bytes that `leak_checking_allocator` handed out and did not get back yet
    inline std::size_t live_bytes = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// the maximum of `live_bytes` since a test set it
    inline std::size_t peak_bytes = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// all allocations of `leak_checking_allocator`
    inline std::size_t nb_allocations = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * `bombing_allocator` that also counts the blocks and the bytes it hands out and gets back, so that a
     * test can check that a failed operation leaks no memory, and how much memory an operation needs at
     * most. All instances compare equal.
     */
    template<typename T>
    struct leak_checking_allocator {
        using value_type = T;

        leak_checking_allocator() noexcept = default;

        template<typename U>
        leak_checking_allocator(leak_checking_allocator<U> const & /*other*/) noexcept {  // NOLINT(google-explicit-constructor)
        }

        T *allocate(std::size_t n) {
            T *p = bombing_allocator<T>{}.allocate(n);
            ++live_blocks;
            ++nb_allocations;
            live_bytes += n * sizeof(T);
            peak_bytes = std::max(peak_bytes, live_bytes);
            return p;
        }

        void deallocate(T *p, std::size_t n) noexcept {
            --live_blocks;
            live_bytes -= n * sizeof(T);
            bombing_allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(leak_checking_allocator const & /*lhs*/, leak_checking_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

    /**
     * Counts allocations and deallocations made through an `id_allocator`.
     */
    struct alloc_counts {
        int allocations = 0;
        int deallocations = 0;
    };

    /**
     * Allocator with an id, so that a test can see which allocator a container uses.
     * Instances with different ids compare unequal. The propagation traits are template parameters.
     * The defaults behave like `std::pmr::polymorphic_allocator`: no propagation, and a copy of the
     * container gets a default constructed allocator when `Soccc` is `std::false_type`.
     * Allocations of size 0 are not counted.
     */
    template<typename T,
             typename Pocca = std::false_type,
             typename Soccc = std::true_type,
             typename Pocma = std::false_type,
             typename Pocs = std::false_type>
    struct id_allocator {
        using value_type = T;
        using propagate_on_container_copy_assignment = Pocca;
        using propagate_on_container_move_assignment = Pocma;
        using propagate_on_container_swap = Pocs;
        using is_always_equal = std::false_type;

        int id = 0;
        alloc_counts *counts = nullptr;

        id_allocator() noexcept = default;

        explicit id_allocator(int id, alloc_counts *counts = nullptr) noexcept
            : id(id),
              counts(counts) {
        }

        template<typename U>
        id_allocator(id_allocator<U, Pocca, Soccc, Pocma, Pocs> const &other) noexcept  // NOLINT(google-explicit-constructor)
            : id(other.id),
              counts(other.counts) {
        }

        template<typename U>
        struct rebind {
            using other = id_allocator<U, Pocca, Soccc, Pocma, Pocs>;
        };

        T *allocate(std::size_t n) {
            if (counts != nullptr && n != 0) {
                ++counts->allocations;
            }
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, std::size_t n) noexcept {
            if (counts != nullptr && n != 0) {
                ++counts->deallocations;
            }
            std::allocator<T>{}.deallocate(p, n);
        }

        [[nodiscard]] id_allocator select_on_container_copy_construction() const noexcept {
            if constexpr (Soccc::value) {
                return *this;
            } else {
                return id_allocator{};
            }
        }

        friend bool operator==(id_allocator const &lhs, id_allocator const &rhs) noexcept {
            return lhs.id == rhs.id;
        }
    };

    /// propagates on nothing, instances differ, a copy of the container gets a default allocator
    template<typename T>
    using pmr_like_allocator = id_allocator<T, std::false_type, std::false_type>;

    /// propagates on copy assignment
    template<typename T>
    using pocca_allocator = id_allocator<T, std::true_type>;

    /// propagates on move assignment
    template<typename T>
    using pocma_allocator = id_allocator<T, std::false_type, std::true_type, std::true_type>;

    /// propagates on swap
    template<typename T>
    using pocs_allocator = id_allocator<T, std::false_type, std::true_type, std::false_type, std::true_type>;

    /**
     * Stateless allocator that counts allocations in a global counter.
     * All instances compare equal.
     */
    inline std::size_t num_allocations = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    template<typename T>
    struct counting_allocator {
        using value_type = T;

        counting_allocator() noexcept = default;

        template<typename U>
        counting_allocator(counting_allocator<U> const & /*other*/) noexcept {  // NOLINT(google-explicit-constructor)
        }

        T *allocate(std::size_t n) {
            ++num_allocations;
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, std::size_t n) noexcept {
            std::allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(counting_allocator const & /*lhs*/, counting_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

    /**
     * Calls of `construct` and `destroy` of `construct_counting_allocator`.
     */
    struct construct_counts {
        std::size_t constructs = 0;
        std::size_t destroys = 0;
    };

    /// the calls of `construct` and `destroy` of all `construct_counting_allocator`s
    inline construct_counts construct_calls{};  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * Stateless allocator with its own `construct` and `destroy`, so that `std::allocator_traits` calls them for
     * every element. They count their calls in `construct_calls`, and construct and destroy in place. All instances
     * compare equal.
     */
    template<typename T>
    struct construct_counting_allocator {
        using value_type = T;

        construct_counting_allocator() noexcept = default;

        template<typename U>
        construct_counting_allocator(construct_counting_allocator<U> const & /*other*/) noexcept {  // NOLINT(google-explicit-constructor)
        }

        T *allocate(std::size_t n) {
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, std::size_t n) noexcept {
            std::allocator<T>{}.deallocate(p, n);
        }

        template<typename U, typename... Args>
        void construct(U *p, Args &&...args) {
            ++construct_calls.constructs;
            std::construct_at(p, std::forward<Args>(args)...);
        }

        template<typename U>
        void destroy(U *p) noexcept {
            ++construct_calls.destroys;
            std::destroy_at(p);
        }

        friend bool operator==(construct_counting_allocator const & /*lhs*/, construct_counting_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

}  // namespace dice::unordered_sparse::tests

#endif  // DICE_UNORDERED_SPARSE_TESTS_FIXTURES_ALLOCATORS_HPP
