#include "fixtures/test_types.hpp"
#include "fixtures/utils.hpp"

#include <dice/sparse-map/sparse_hash.hpp>
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <array>
#include <cstddef>
#include <cstdio>
#include <functional>
#include <iostream>
#include <memory>
#include <new>
#include <string>
#include <type_traits>
#include <utility>

#if __has_include(<sys/wait.h>)
#include <dice/template-library/sandbox.hpp>

#include <unistd.h>
#endif

/**
 * What `sparse_map` and `sparse_set` do when an allocation of the table fails. With the default
 * `sh::allocation_failure::terminating` the process ends with `std::abort()`. Each case runs the operation in a child
 * process (`DICE_SANDBOX` of the dice-template-library) and checks that the child ends with `SIGABRT` and writes
 * nothing to stderr. There is one case for each place where a group or the bucket array allocates. The cases are
 * skipped where `<sys/wait.h>` is missing.
 * `tests_exception_safety` checks `sh::allocation_failure::throwing`.
 */
namespace {
    using namespace dice::sparse_map;
    using namespace dice::sparse_map::tests;

#if __has_include(<sys/wait.h>)
    constexpr bool can_fork = true;

    /**
     * Runs `f` in a child process with `DICE_SANDBOX` and waits for it. The child returns 0 if `f` returns and 1 if
     * `f` throws, so an exception does not end it with `SIGABRT`. The tests compile the internal checks of the library
     * in, and a failed check also ends with `SIGABRT`, but it writes a message to stderr. So the child writes its
     * stderr to a temporary file, and the parent prints the content if it is not empty.
     * @return true if the child ended with the signal `SIGABRT` and wrote nothing to stderr
     */
    template<typename F>
    [[nodiscard]] bool expect_abort(F &&f) {
        struct close_file {
            void operator()(std::FILE *file) const noexcept {
                std::fclose(file);
            }
        };
        std::unique_ptr<std::FILE, close_file> const child_stderr_file{std::tmpfile()};
        if (child_stderr_file == nullptr) {
            return false;
        }
        auto const result = DICE_SANDBOX {
            dup2(fileno(child_stderr_file.get()), STDERR_FILENO);
            try {
                std::forward<F>(f)();
            } catch (...) {
                return 1;
            }
            return 0;
        };
        std::rewind(child_stderr_file.get());
        std::string child_stderr;
        std::array<char, 256> buffer{};
        while (std::size_t const n = std::fread(buffer.data(), 1, buffer.size(), child_stderr_file.get())) {
            child_stderr.append(buffer.data(), n);
        }
        if (!child_stderr.empty()) {
            std::cerr << "stderr of the child: " << child_stderr << '\n';
        }
        return result == dice::template_library::SubProcessResult::Aborted && child_stderr.empty();
    }
#else
    constexpr bool can_fork = false;

    template<typename F>
    [[nodiscard]] bool expect_abort(F && /*f*/) {
        return false;
    }
#endif

    /// the allocations of `failing_allocator` that throw `std::bad_alloc`
    enum class failing {
        nothing,
        groups, ///< the arrays of the elements of the groups, allocations of `Value`
        bucket_array, ///< the bucket array, the array of the groups, all other allocations
    };

    failing fail = failing::nothing; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// makes the allocations `kind` of `failing_allocator` throw while it is in scope
    struct fail_while_in_scope {
        explicit fail_while_in_scope(failing kind) noexcept {
            fail = kind;
        }
        fail_while_in_scope(fail_while_in_scope const &) = delete;
        fail_while_in_scope(fail_while_in_scope &&) = delete;
        fail_while_in_scope &operator=(fail_while_in_scope const &) = delete;
        fail_while_in_scope &operator=(fail_while_in_scope &&) = delete;
        ~fail_while_in_scope() {
            fail = failing::nothing;
        }
    };

    /**
     * Allocator that throws `std::bad_alloc` for the allocations that `fail` names. An allocation of `Value` is one of
     * the elements of a group, any other allocation is one of the bucket array. Instances with different `id`
     * compare unequal, and the allocator does not propagate, like `std::pmr::polymorphic_allocator`.
     */
    template<typename T, typename Value>
    struct failing_allocator {
        using value_type = T;
        using propagate_on_container_copy_assignment = std::false_type;
        using propagate_on_container_move_assignment = std::false_type;
        using propagate_on_container_swap = std::false_type;
        using is_always_equal = std::false_type;

        int id = 0;

        failing_allocator() noexcept = default;

        explicit failing_allocator(int id) noexcept : id(id) {}

        template<typename U>
        failing_allocator(failing_allocator<U, Value> const &other) noexcept // NOLINT(google-explicit-constructor)
            :
            id(other.id) {}

        T *allocate(std::size_t n) {
            bool const is_group = std::is_same_v<T, Value>;
            if ((fail == failing::groups && is_group) || (fail == failing::bucket_array && !is_group)) {
                throw std::bad_alloc{};
            }
            return std::allocator<T>{}.allocate(n);
        }

        void deallocate(T *p, std::size_t n) noexcept {
            std::allocator<T>{}.deallocate(p, n);
        }

        friend bool operator==(failing_allocator const &lhs, failing_allocator const &rhs) noexcept {
            return lhs.id == rhs.id;
        }
    };

    /**
     * A value whose move constructor can throw. So the groups copy it into a new array of elements when they insert
     * it into a bucket without a slot, and an erase leaves a hole.
     */
    struct copied {
        std::size_t value = 0;

        explicit copied(std::size_t value) noexcept : value(value) {}

        copied(copied const &other) = default;

        copied(copied &&other) noexcept(false) // NOLINT(performance-noexcept-move-constructor)
            :
            value(other.value) {}

        copied &operator=(copied const &other) = default;

        copied &operator=(copied &&other) noexcept(false) { // NOLINT(performance-noexcept-move-constructor)
            value = other.value;
            return *this;
        }

        ~copied() = default;
    };

    /**
     * A map with the default `sh::allocation_failure::terminating` that allocates through `failing_allocator`. Its
     * groups store the elements as `detail_sparse_hash::map_slot`.
     */
    template<typename T, typename Hash = detail_sparse_hash::default_hash<std::size_t>>
    using failing_map = sparse_map<
        std::size_t,
        T,
        Hash,
        std::equal_to<std::size_t>,
        failing_allocator<std::pair<std::size_t, T>, detail_sparse_hash::map_slot<std::size_t, T>>
    >;

    static_assert(std::is_nothrow_move_constructible_v<detail_sparse_hash::map_slot<std::size_t, std::size_t>>);
    static_assert(!std::is_nothrow_move_constructible_v<detail_sparse_hash::map_slot<std::size_t, copied>>);

    /// allocator whose `allocate` throws `std::length_error`
    struct length_error_allocator {
        using value_type = std::size_t;

        std::size_t *allocate(std::size_t /*n*/) {
            throw std::length_error{"size limit"};
        }

        void deallocate(std::size_t * /*p*/, std::size_t /*n*/) noexcept {}
    };

    /// inserts the keys 0 to `n - 1`
    template<typename Map>
    void insert_keys(Map &map, std::size_t n) {
        for (std::size_t key = 0; key < n; ++key) {
            map.try_emplace(key, key);
        }
    }

} // namespace

TEST_CASE("the default of AllocationFailure is terminating") {
    CHECK(
        std::is_same_v<
            sparse_map<int, int>,
            sparse_map<
                int,
                int,
                detail_sparse_hash::default_hash<int>,
                std::equal_to<int>,
                std::allocator<std::pair<int, int>>,
                sh::sparsity::medium,
                sh::allocation_failure::terminating
            >
        >
    );
    CHECK(
        std::is_same_v<
            sparse_set<int>,
            sparse_set<
                int,
                detail_sparse_hash::default_hash<int>,
                std::equal_to<int>,
                std::allocator<int>,
                sh::sparsity::medium,
                sh::allocation_failure::terminating
            >
        >
    );
}

TEST_CASE(
    "a failed allocation of the bucket array aborts in the constructor and in a rehash" * doctest::skip(!can_fork)
) {
    CHECK(expect_abort([] {
        auto const guard = fail_while_in_scope{failing::bucket_array};
        auto const map = failing_map<std::size_t>(64);
    }));

    auto map = failing_map<std::size_t>{};
    insert_keys(map, 100);
    CHECK(expect_abort([&] {
        auto const guard = fail_while_in_scope{failing::bucket_array};
        map.rehash(map.bucket_count() * 2);
    }));
}

TEST_CASE("a failed allocation of the bucket array aborts in a copy" * doctest::skip(!can_fork)) {
    auto source = failing_map<std::size_t>{};
    insert_keys(source, 100);
    CHECK(expect_abort([&] {
        auto const guard = fail_while_in_scope{failing::bucket_array};
        auto const copy = source;
    }));
}

TEST_CASE(
    "a failed allocation of the bucket array aborts in a move assignment to another allocator"
    * doctest::skip(!can_fork)
) {
    using map_t = failing_map<std::size_t>;
    auto source = map_t{map_t::allocator_type{1}};
    auto target = map_t{map_t::allocator_type{2}};
    insert_keys(source, 100);
    CHECK(expect_abort([&] {
        auto const guard = fail_while_in_scope{failing::bucket_array};
        target = std::move(source);
    }));
}

// `std::length_error` is a size limit and not a failed allocation, so `allocate` lets it through with `terminating` too.
TEST_CASE("allocate lets std::length_error through with terminating") {
    auto alloc = length_error_allocator{};
    CHECK_THROWS_AS(
        static_cast<void>(detail_sparse_hash::allocate<sh::allocation_failure::terminating>(alloc, 1)),
        std::length_error
    );
}

TEST_CASE(
    "a failed allocation of a group aborts when the group is constructed with a capacity" * doctest::skip(!can_fork)
) {
    using allocator_t = failing_allocator<std::size_t, std::size_t>;
    using array_t = detail_sparse_hash::sparse_array<
        std::size_t,
        allocator_t,
        sh::sparsity::medium,
        sh::allocation_failure::terminating
    >;
    CHECK(expect_abort([] {
        auto const guard = fail_while_in_scope{failing::groups};
        auto const array = array_t(4, allocator_t{});
    }));
}

TEST_CASE("a failed allocation of a group aborts in a copy" * doctest::skip(!can_fork)) {
    auto source = failing_map<std::size_t>{};
    insert_keys(source, 100);
    CHECK(expect_abort([&] {
        auto const guard = fail_while_in_scope{failing::groups};
        auto const copy = source;
    }));
}

TEST_CASE(
    "a failed allocation of a group aborts in a move assignment to another allocator" * doctest::skip(!can_fork)
) {
    using map_t = failing_map<std::size_t>;
    auto source = map_t{map_t::allocator_type{1}};
    auto target = map_t{map_t::allocator_type{2}};
    insert_keys(source, 100);
    CHECK(expect_abort([&] {
        auto const guard = fail_while_in_scope{failing::groups};
        target = std::move(source);
    }));
}

// The groups of a new table have no elements yet, so the first insert into a group allocates its elements.
TEST_CASE("a failed allocation of a group aborts in an insert" * doctest::skip(!can_fork)) {
    auto map = failing_map<std::size_t>(64);
    CHECK(expect_abort([&] {
        auto const guard = fail_while_in_scope{failing::groups};
        map.try_emplace(1, 1);
    }));
}

// The group of a new table has no element and no hole, so the insertion copies nothing and allocates an array for one
// element.
TEST_CASE(
    "a failed allocation of a group aborts in an insert of an element whose move constructor can throw into a group without holes"
    * doctest::skip(!can_fork)
) {
    auto map = failing_map<copied>(64);
    CHECK(expect_abort([&] {
        auto const guard = fail_while_in_scope{failing::groups};
        map.try_emplace(1, 1);
    }));
}

// With 64 buckets the table has one group, and `identity_hash` puts key `k` into bucket `k`. The erase of key 0 leaves
// a hole. The insertion of key 5 into another bucket copies the elements into a new array without the hole.
TEST_CASE(
    "a failed allocation of a group aborts in an insert of an element whose move constructor can throw into a group with holes"
    * doctest::skip(!can_fork)
) {
    auto map = failing_map<copied, identity_hash<std::size_t>>(64);
    insert_keys(map, 3);
    REQUIRE(map.bucket_count() == 64);
    REQUIRE(map.erase(0) == 1);
    CHECK(expect_abort([&] {
        auto const guard = fail_while_in_scope{failing::groups};
        map.try_emplace(5, 5);
    }));
}
