#include "fixtures/allocators.hpp"

#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <functional>
#include <stdexcept>
#include <type_traits>
#include <utility>

/**
 * The `noexcept` contract of `sparse_map` and `sparse_set`, held against the specification of
 * `std::unordered_map` and `std::unordered_set`:
 *  - move assignment: `noexcept(allocator_traits<Allocator>::is_always_equal::value &&
 *    is_nothrow_move_assignable_v<Hash> && is_nothrow_move_assignable_v<Pred>)`
 *  - swap: `noexcept(allocator_traits<Allocator>::is_always_equal::value &&
 *    is_nothrow_swappable_v<Hash> && is_nothrow_swappable_v<Pred>)`
 *  - clear, size, empty, begin, end, cbegin, cend: `noexcept`
 *  - move constructor and find: no `noexcept`. An implementation may add `noexcept` where nothing
 *    can throw, but not where the hash can throw.
 *
 * Partly ported from the noexcept tests of ankerl::unordered_dense (MIT license).
 */
namespace {
    using namespace dice::unordered_sparse;
    using dice::unordered_sparse::tests::pmr_like_allocator;

    /**
     * Hash whose move, move assignment, swap and call may throw. They throw while `armed` is true.
     */
    struct throwing_hash {
        inline static bool armed = false;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

        throwing_hash() = default;
        throwing_hash(throwing_hash const &) = default;

        // NOLINTNEXTLINE(performance-noexcept-move-constructor)
        throwing_hash(throwing_hash && /*other*/) noexcept(false) {
            if (armed) {
                throw std::runtime_error{"throwing_hash move"};
            }
        }

        throwing_hash &operator=(throwing_hash const &) = default;

        // NOLINTNEXTLINE(performance-noexcept-move-constructor)
        throwing_hash &operator=(throwing_hash && /*other*/) noexcept(false) {
            if (armed) {
                throw std::runtime_error{"throwing_hash move assignment"};
            }
            return *this;
        }

        ~throwing_hash() = default;

        std::size_t operator()(int key) const {
            if (armed) {
                throw std::runtime_error{"throwing_hash call"};
            }
            return std::hash<int>{}(key);
        }
    };

    /// arms `throwing_hash` while it is in scope
    struct arm_throwing_hash {
        arm_throwing_hash() noexcept {
            throwing_hash::armed = true;
        }
        arm_throwing_hash(arm_throwing_hash const &) = delete;
        arm_throwing_hash(arm_throwing_hash &&) = delete;
        arm_throwing_hash &operator=(arm_throwing_hash const &) = delete;
        arm_throwing_hash &operator=(arm_throwing_hash &&) = delete;
        ~arm_throwing_hash() {
            throwing_hash::armed = false;
        }
    };

    using plain_map = sparse_map<int, int>;
    using plain_set = sparse_set<int>;

    using throwing_map = sparse_map<int, int, throwing_hash>;
    using throwing_set = sparse_set<int, throwing_hash>;

    /// containers with an allocator that is not always equal and does not propagate, like `std::pmr::polymorphic_allocator`
    using unequal_map = sparse_map<int, int, std::hash<int>, std::equal_to<int>, pmr_like_allocator<std::pair<int, int>>>;
    using unequal_set = sparse_set<int, std::hash<int>, std::equal_to<int>, pmr_like_allocator<int>>;

    // the premises of the expectations below
    static_assert(std::allocator_traits<std::allocator<int>>::is_always_equal::value);
    static_assert(!std::allocator_traits<pmr_like_allocator<int>>::is_always_equal::value);
    static_assert(!std::is_nothrow_move_constructible_v<throwing_hash>);
    static_assert(!std::is_nothrow_move_assignable_v<throwing_hash>);
    static_assert(!std::is_nothrow_swappable_v<throwing_hash>);
    static_assert(!std::is_nothrow_invocable_v<throwing_hash const &, int const &>);

    template<typename Container>
    void insert_some(Container &container) {
        for (int i = 0; i < 100; ++i) {
            if constexpr (requires { typename Container::mapped_type; }) {
                container.try_emplace(i, i);
            } else {
                container.insert(i);
            }
        }
    }

}  // namespace

TYPE_TO_STRING_AS("sparse_map<int, int>", plain_map);
TYPE_TO_STRING_AS("sparse_set<int>", plain_set);
TYPE_TO_STRING_AS("sparse_map<int, int, throwing_hash>", throwing_map);
TYPE_TO_STRING_AS("sparse_set<int, throwing_hash>", throwing_set);
TYPE_TO_STRING_AS("sparse_map<int, int, ..., pmr_like_allocator>", unequal_map);
TYPE_TO_STRING_AS("sparse_set<int, ..., pmr_like_allocator>", unequal_set);

// `std::unordered_map` does not require this, but the standard libraries give it, and a
// `std::vector` of maps moves its elements on growth only if it holds.
TEST_CASE_TEMPLATE("the move constructor is noexcept when nothing it moves can throw", container_t, plain_map, plain_set, unequal_map, unequal_set) {
    CHECK(std::is_nothrow_move_constructible_v<container_t>);
}

TEST_CASE_TEMPLATE("the move constructor is not noexcept when the hash can throw on a move", container_t, throwing_map, throwing_set) {
    CHECK_FALSE(std::is_nothrow_move_constructible_v<container_t>);

    // a move of the hash that throws reaches the caller, it does not terminate the program
    auto source = container_t{};
    insert_some(source);
    {
        auto const guard = arm_throwing_hash{};
        CHECK_THROWS_AS(container_t{std::move(source)}, std::runtime_error);
    }
    CHECK(source.size() == 100);  // NOLINT(bugprone-use-after-move)
    CHECK(source.contains(42));   // NOLINT(bugprone-use-after-move)
}

TEST_CASE_TEMPLATE("move assignment is noexcept when the allocator is always equal and the functors cannot throw", container_t, plain_map, plain_set) {
    CHECK(std::is_nothrow_move_assignable_v<container_t>);
}

// fails today: `sparse_hash::operator=(sparse_hash &&)` is `noexcept` without a condition, and it
// move assigns the hash, which can throw
TEST_CASE_TEMPLATE("move assignment is not noexcept when the hash can throw on a move assignment" * doctest::should_fail(), container_t, throwing_map, throwing_set) {
    CHECK_FALSE(std::is_nothrow_move_assignable_v<container_t>);
}

// fails today: `sparse_hash::operator=(sparse_hash &&)` is `noexcept` without a condition, but with
// unequal allocators that do not propagate it moves the elements one by one into new memory, which
// allocates
TEST_CASE_TEMPLATE("move assignment is not noexcept when the allocator is not always equal" * doctest::should_fail(), container_t, unequal_map, unequal_set) {
    CHECK_FALSE(std::is_nothrow_move_assignable_v<container_t>);
}

// fails today: `sparse_map::swap`, `sparse_set::swap`, the free `swap` functions and
// `sparse_hash::swap` have no `noexcept`
TEST_CASE_TEMPLATE("swap is noexcept when the allocator is always equal and the functors cannot throw" * doctest::should_fail(), container_t, plain_map, plain_set) {
    CHECK(noexcept(std::declval<container_t &>().swap(std::declval<container_t &>())));
    CHECK(std::is_nothrow_swappable_v<container_t>);
}

TEST_CASE_TEMPLATE("swap is not noexcept when the hash can throw on a swap", container_t, throwing_map, throwing_set) {
    CHECK_FALSE(noexcept(std::declval<container_t &>().swap(std::declval<container_t &>())));
    CHECK_FALSE(std::is_nothrow_swappable_v<container_t>);
}

TEST_CASE_TEMPLATE("clear, size, empty, begin and end are noexcept", container_t, plain_map, plain_set, throwing_map, throwing_set, unequal_map, unequal_set) {
    CHECK(noexcept(std::declval<container_t &>().clear()));
    CHECK(noexcept(std::declval<container_t const &>().size()));
    CHECK(noexcept(std::declval<container_t const &>().empty()));
    CHECK(noexcept(std::declval<container_t &>().begin()));
    CHECK(noexcept(std::declval<container_t &>().end()));
    CHECK(noexcept(std::declval<container_t const &>().begin()));
    CHECK(noexcept(std::declval<container_t const &>().end()));
    CHECK(noexcept(std::declval<container_t const &>().cbegin()));
    CHECK(noexcept(std::declval<container_t const &>().cend()));
}

TEST_CASE_TEMPLATE("find is not noexcept when the hash can throw", container_t, throwing_map, throwing_set) {
    CHECK_FALSE(noexcept(std::declval<container_t &>().find(1)));
    CHECK_FALSE(noexcept(std::declval<container_t const &>().find(1)));

    // a hash that throws reaches the caller, it does not terminate the program
    auto container = container_t{};
    insert_some(container);
    auto const &const_container = container;
    {
        auto const guard = arm_throwing_hash{};
        CHECK_THROWS_AS(static_cast<void>(container.find(1)), std::runtime_error);
        CHECK_THROWS_AS(static_cast<void>(const_container.find(1)), std::runtime_error);
    }
    CHECK(container.find(1) != container.end());
}
