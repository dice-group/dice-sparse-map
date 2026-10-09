#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <algorithm>
#include <cstddef>
#include <initializer_list>
#include <memory>
#include <scoped_allocator>
#include <type_traits>
#include <utility>
#include <vector>

// number of values that the tests put into one sparse_array, at most BITMAP_NB_BITS (64)
constexpr auto max_index = 32;

template<typename T>
void compilation() {
    typename T::array_type test;
}

template<typename T>
void construction() {
    typename T::allocator_type a;
    typename T::array_type test(max_index, a);
    test.clear(a);
}

template<typename T>
void set(std::initializer_list<typename T::value_type> l) {
    typename T::allocator_type a;
    typename T::array_type array(max_index, a);
    std::vector<typename T::value_type> check;
    check.reserve(l.size());
    std::size_t counter = 0;
    for (auto const &value : l) {
        array.set(a, counter++, value);
        check.emplace_back(value);
    }
    //'set' did not create the correct order of items
    REQUIRE(std::equal(array.begin(), array.end(), check.begin()));
    array.clear(a);
}

template<typename T>
void uses_allocator() {
    // uses_allocator returns false
    REQUIRE((std::uses_allocator<typename T::array_type, typename T::allocator_type>::value));
}

template<typename T, typename... Args>
void trailing_allocator_convention(Args...) {
    using alloc_type = typename T::allocator_type;
    // trailing_allocator thinks construction is not possible
    REQUIRE((std::is_constructible<typename T::array_type, Args..., alloc_type const &>::value));
}

template<typename T>
void trailing_allocator_convention_without_parameters() {
    using alloc_type = typename std::allocator_traits<typename T::allocator_type>::template rebind_alloc<
        typename T::array_type
    >;
    // trailing_allocator thinks construction is not possible
    REQUIRE((std::is_constructible<typename T::array_type, alloc_type const &>::value));
}

template<typename T>
void is_move_insertable(std::initializer_list<typename T::value_type> l) {
    using array_alloc_type = typename std::allocator_traits<typename T::allocator_type>::template rebind_alloc<
        typename T::array_type
    >;
    array_alloc_type m;
    auto p = std::allocator_traits<array_alloc_type>::allocate(m, 1);
    typename T::allocator_type array_alloc;
    typename T::array_type rv(max_index, array_alloc);
    std::size_t counter = 0;
    for (auto const &value : l) {
        rv.set(array_alloc, counter++, value);
    }
    std::allocator_traits<array_alloc_type>::construct(m, p, std::move(rv));
    rv.clear(array_alloc);
    p->clear(array_alloc);
    std::allocator_traits<array_alloc_type>::destroy(m, p);
    std::allocator_traits<array_alloc_type>::deallocate(m, p, 1);
}

template<typename T>
void is_default_insertable() {
    using array_alloc_type = typename std::allocator_traits<typename T::allocator_type>::template rebind_alloc<
        typename T::array_type
    >;
    array_alloc_type m;
    typename T::array_type *p = std::allocator_traits<array_alloc_type>::allocate(m, 1);
    std::allocator_traits<array_alloc_type>::construct(m, p);
    std::allocator_traits<array_alloc_type>::deallocate(m, p, 1);
}

template<typename T, dice::sparse_map::sh::sparsity Sparsity = dice::sparse_map::sh::sparsity::medium>
struct normal_alloc {
    using value_type = T;
    using allocator_type = std::allocator<T>;
    using array_type = dice::sparse_map::detail_sparse_hash::sparse_array<
        T,
        allocator_type,
        Sparsity,
        dice::sparse_map::sh::allocation_failure::terminating
    >;
};

template<typename T, dice::sparse_map::sh::sparsity Sparsity = dice::sparse_map::sh::sparsity::medium>
struct scoped_alloc {
    using value_type = T;
    using allocator_type = std::scoped_allocator_adaptor<std::allocator<T>>;
    using array_type = dice::sparse_map::detail_sparse_hash::sparse_array<
        T,
        allocator_type,
        Sparsity,
        dice::sparse_map::sh::allocation_failure::terminating
    >;
};

TEST_SUITE("scoped_allocators/sparse_array_tests") {
    TEST_CASE("normal_compilation") {
        compilation<normal_alloc<int>>();
    }
    TEST_CASE("normal_construction") {
        construction<normal_alloc<int>>();
    }
    TEST_CASE("normal_set") {
        set<normal_alloc<int>>({0, 1, 2, 3, 4});
    }
    TEST_CASE("normal_uses_allocator") {
        uses_allocator<normal_alloc<int>>();
    }
    TEST_CASE("normal_trailing_allocator_convention") {
        trailing_allocator_convention<normal_alloc<int>>(0);
    }
    TEST_CASE("normal_is_move_insertable") {
        is_move_insertable<normal_alloc<int>>({0, 1, 2, 3, 4, 5});
    }
    TEST_CASE("normal_is_default_insertable") {
        is_default_insertable<normal_alloc<int>>();
    }

    TEST_CASE("scoped_compilation") {
        compilation<scoped_alloc<int>>();
    }
    TEST_CASE("scoped_construction") {
        construction<scoped_alloc<int>>();
    }
    TEST_CASE("scoped_set") {
        set<scoped_alloc<int>>({0, 1, 2, 3, 4});
    }
    TEST_CASE("scoped_uses_allocator") {
        uses_allocator<scoped_alloc<int>>();
    }
    TEST_CASE("scoped_trailing_allocator_convention") {
        trailing_allocator_convention<scoped_alloc<int>>(0);
    }
    TEST_CASE("scoped_is_move_insertable") {
        is_move_insertable<scoped_alloc<int>>({0, 1, 2, 3, 4, 5});
    }
}
