/** @file
 * @brief Checks for fancy pointer support in the sparse_array implementation.
 */

#include <dice/sparse-map/sparse_hash.hpp>

#include "fixtures/offset_ptr_allocator.hpp"

#include <doctest/doctest.h>

#include <boost/interprocess/offset_ptr.hpp>

#include <algorithm>
#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>
#include <vector>

// number of values that the tests put into one sparse_array, at most BITMAP_NB_BITS (64)
constexpr auto max_index = 32;

/* Tests are formulated via templates to reduce code duplication.
 * The template parameter holds the allocator type `allocator_type` and the type `array_type` of the
 * sparse_array (with all template parameters already inserted).
 */

template<typename T>
void compilation() {
    typename T::array_type test;
}

template<typename T>
void construction() {
    typename T::allocator_type a;
    typename T::array_type test(max_index, a);
    test.clear(a);  // needed because destructor asserts
}

namespace details {
    template<typename T>
    typename T::array_type generate_test_array(typename T::allocator_type &a) {
        typename T::array_type arr(max_index, a);
        for (std::size_t i = 0; i < max_index; ++i) {
            arr.set(a, i, i);
        }
        return arr;
    }

    template<typename T>
    std::vector<typename T::allocator_type::value_type> generate_check_for_test_array() {
        std::vector<typename T::allocator_type::value_type> check(max_index);
        for (std::size_t i = 0; i < max_index; ++i) {
            check[i] = i;
        }
        return check;
    }
}  // namespace details

template<typename T>
void set() {
    typename T::allocator_type a;
    auto test = details::generate_test_array<T>(a);
    auto check = details::generate_check_for_test_array<T>();
    //'set' did not create the correct order of items
    REQUIRE(std::equal(test.begin(), test.end(), check.begin()));
    test.clear(a);  // needed because destructor asserts
}

template<typename T>
void copy_construction() {
    typename T::allocator_type a;
    // needs to be its own line, otherwise the move-construction would take place
    auto test = details::generate_test_array<T>(a);
    typename T::array_type copy(test, a);
    auto check = details::generate_check_for_test_array<T>();
    //'copy' changed the order of the items
    REQUIRE(std::equal(copy.begin(), copy.end(), check.begin()));
    test.clear(a);
    copy.clear(a);
}

template<typename T>
void move_construction() {
    typename T::allocator_type a;
    // two lines needed. Otherwise move/copy elision
    auto moved_from = details::generate_test_array<T>(a);
    typename T::array_type moved_to(std::move(moved_from));
    auto check = details::generate_check_for_test_array<T>();
    //'move' changed the order of the items
    REQUIRE(std::equal(moved_to.begin(), moved_to.end(), check.begin()));
    moved_to.clear(a);
}

template<typename T>
void const_iterator() {
    typename T::allocator_type a;
    auto test = details::generate_test_array<T>(a);
    auto const_iter = test.cbegin();
    // const iterator has the wrong type
    REQUIRE((std::is_same<decltype(const_iter), typename T::const_iterator_type>::value));
    test.clear(a);
}


/*
 * The types to give the tests as template parameter.
 */
template<typename T, dice::sparse_map::sh::sparsity Sparsity = dice::sparse_map::sh::sparsity::medium>
struct std_alloc {
    using allocator_type = std::allocator<T>;
    using array_type = dice::sparse_map::detail_sparse_hash::sparse_array<T, std::allocator<T>, Sparsity, dice::sparse_map::sh::allocation_failure::terminating>;
    using const_iterator_type = T const *;
};

template<typename T, dice::sparse_map::sh::sparsity Sparsity = dice::sparse_map::sh::sparsity::medium>
struct custom_alloc {
    using allocator_type = dice::sparse_map::tests::offset_ptr_allocator<T>;
    using array_type = dice::sparse_map::detail_sparse_hash::sparse_array<T, dice::sparse_map::tests::offset_ptr_allocator<T>, Sparsity, dice::sparse_map::sh::allocation_failure::terminating>;
    using const_iterator_type = boost::interprocess::offset_ptr<T const>;
};


/* The instantiation of the tests.
 * They are not template test cases, so that every test case has its own name.
 */
TEST_SUITE("fancy_pointers/sparse_array_tests") {

    TEST_CASE("std_alloc_compile") {
        compilation<std_alloc<int>>();
    }
    TEST_CASE("std_alloc_construction") {
        construction<std_alloc<int>>();
    }
    TEST_CASE("std_alloc_set") {
        set<std_alloc<int>>();
    }
    TEST_CASE("std_alloc_copy_construction") {
        copy_construction<std_alloc<int>>();
    }
    TEST_CASE("std_alloc_move_construction") {
        move_construction<std_alloc<int>>();
    }
    TEST_CASE("std_const_iterator") {
        const_iterator<std_alloc<int>>();
    }

    TEST_CASE("custom_alloc_compile") {
        compilation<custom_alloc<int>>();
    }
    TEST_CASE("custom_alloc_construction") {
        construction<custom_alloc<int>>();
    }
    TEST_CASE("custom_alloc_set") {
        set<custom_alloc<int>>();
    }
    TEST_CASE("custom_alloc_copy_construction") {
        copy_construction<custom_alloc<int>>();
    }
    TEST_CASE("custom_alloc_move_construction") {
        move_construction<custom_alloc<int>>();
    }
    TEST_CASE("custom_const_iterator") {
        const_iterator<custom_alloc<int>>();
    }
}
