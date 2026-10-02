/** @file
 * @brief Checks for fancy pointer support in the sparse_hash implementation for single values (sets).
 */

#include <dice/unordered_sparse.hpp>

#include "fixtures/offset_ptr_allocator.hpp"

#include <doctest/doctest.h>

#include <algorithm>
#include <functional>
#include <initializer_list>
#include <memory>
#include <vector>

using dice::unordered_sparse::tests::offset_ptr_allocator;

/* Tests are analogous to the tests in tests_fancy_pointer_sparse_array.cpp.
 * The template parameter now also holds the value_type.
 */
namespace details {
    template<typename Key>
    struct key_select {
        using key_type = Key;
        key_type const &operator()(Key const &key) const noexcept {
            return key;
        }
        key_type &operator()(Key &key) noexcept {
            return key;
        }
    };

    template<typename T, typename Alloc>
    using sparse_set = dice::unordered_sparse::detail::sparse_hash<
        T,
        key_select<T>,
        void,
        std::hash<T>,
        std::equal_to<T>,
        Alloc,
        dice::unordered_sparse::exception_safety::basic,
        dice::unordered_sparse::sparsity::medium>;

    template<typename T>
    typename T::set_type default_construct_set() {
        using value_type = typename T::value_type;
        return typename T::set_type(T::set_type::DEFAULT_INIT_BUCKET_COUNT, std::hash<value_type>(), std::equal_to<value_type>(), typename T::allocator_type(), T::set_type::DEFAULT_MAX_LOAD_FACTOR);
    }

    /** checks if all values of the set are in the initializer_list and then if the lengths are equal.
     *  So basically Set \subset l and |Set| == |l|.
     *  Needs 'set.contains(.)' to work correctly.
     */
    template<typename Set>
    bool is_equal(Set const &set, std::initializer_list<typename Set::value_type> l) {
        return std::all_of(l.begin(), l.end(), [&set](typename Set::value_type i) {
                   return set.contains(i);
               })
               && set.size() == l.size();
    }
}  // namespace details

template<typename T>
void construction() {
    auto set = details::default_construct_set<T>();
}

template<typename T>
void insert(std::initializer_list<typename T::value_type> l) {
    auto set = details::default_construct_set<T>();
    for (auto const &i : l) {
        set.insert(i);
    }
    //'insert' did not create exactly the values needed
    REQUIRE(details::is_equal(set, l));
}

template<typename T>
void iterator_insert(std::initializer_list<typename T::value_type> l) {
    auto set = details::default_construct_set<T>();
    set.insert(l.begin(), l.end());
    //'insert' with iterators did not create exactly the values needed
    REQUIRE(details::is_equal(set, l));
}

template<typename T>
void iterator_access(typename T::value_type single_value) {
    auto set = details::default_construct_set<T>();
    set.insert(single_value);
    // iterator cannot access single value
    REQUIRE(*(set.begin()) == single_value);
}

template<typename T>
void iterator_access_multi(std::initializer_list<typename T::value_type> l) {
    auto set = details::default_construct_set<T>();
    set.insert(l.begin(), l.end());
    std::vector<typename T::value_type> l_sorted = l;
    std::vector<typename T::value_type> set_sorted(set.begin(), set.end());
    std::sort(l_sorted.begin(), l_sorted.end());
    std::sort(set_sorted.begin(), set_sorted.end());
    // iterating over the set didn't work
    REQUIRE(std::equal(l_sorted.begin(), l_sorted.end(), set_sorted.begin()));
}


template<typename T>
void const_iterator_access_multi(std::initializer_list<typename T::value_type> l) {
    auto set = details::default_construct_set<T>();
    set.insert(l.begin(), l.end());
    std::vector<typename T::value_type> l_sorted = l;
    std::vector<typename T::value_type> set_sorted(set.cbegin(), set.cend());
    std::sort(l_sorted.begin(), l_sorted.end());
    std::sort(set_sorted.begin(), set_sorted.end());
    // const iterating over the set didn't work
    REQUIRE(std::equal(l_sorted.begin(), l_sorted.end(), set_sorted.begin()));
}

template<typename T>
void find(std::initializer_list<typename T::value_type> l, typename T::value_type search_value, bool is_in_list) {
    auto set = details::default_construct_set<T>();
    set.insert(l.begin(), l.end());
    auto iter = set.find(search_value);
    bool found = iter != set.end();
    // find did not work as expected
    REQUIRE((found == is_in_list));
}

template<typename T>
void erase(std::initializer_list<typename T::value_type> l, typename T::value_type extra_value) {
    auto set = details::default_construct_set<T>();
    set.insert(extra_value);
    set.insert(l.begin(), l.end());
    // force non-const iterator
    auto iter = set.begin();
    for (; *iter != extra_value; ++iter) {
    }
    set.erase(iter);
    // erase did not work as expected
    REQUIRE(details::is_equal(set, l));
}

template<typename T>
void erase_with_const_iter(std::initializer_list<typename T::value_type> l, typename T::value_type extra_value) {
    auto set = details::default_construct_set<T>();
    set.insert(extra_value);
    set.insert(l.begin(), l.end());
    // force const iterator
    auto iter = set.cbegin();
    for (; *iter != extra_value; ++iter) {
    }
    set.erase(iter);
    // erase did not work as expected
    REQUIRE(details::is_equal(set, l));
}


template<typename T>
struct std_alloc {
    using value_type = T;
    using allocator_type = std::allocator<value_type>;
    using set_type = details::sparse_set<value_type, allocator_type>;
};

template<typename T>
struct custom_alloc {
    using value_type = T;
    using allocator_type = offset_ptr_allocator<value_type>;
    using set_type = details::sparse_set<value_type, allocator_type>;
};


TEST_SUITE("fancy_pointers/sparse_hash_set_tests") {

    TEST_CASE("std_alloc_compiles") {
        construction<std_alloc<int>>();
    }
    TEST_CASE("std_alloc_insert") {
        insert<std_alloc<int>>({1, 2, 3, 4});
    }
    TEST_CASE("std_alloc_iterator_insert") {
        iterator_insert<std_alloc<int>>({1, 2, 3, 4});
    }
    TEST_CASE("std_alloc_iterator_access") {
        iterator_access<std_alloc<int>>(42);
    }
    TEST_CASE("std_alloc_iterator_access_multi") {
        iterator_access_multi<std_alloc<int>>({1, 2, 3, 4});
    }
    TEST_CASE("std_alloc_const_iterator_access_multi") {
        const_iterator_access_multi<std_alloc<int>>({1, 2, 3, 4});
    }
    TEST_CASE("std_find_true") {
        find<std_alloc<int>>({1, 2, 3, 4}, 4, true);
    }
    TEST_CASE("std_find_false") {
        find<std_alloc<int>>({1, 2, 3, 4}, 5, false);
    }
    TEST_CASE("std_erase") {
        erase<std_alloc<int>>({1, 2, 3, 4}, 5);
    }
    TEST_CASE("std_erase_with_const_iter") {
        erase_with_const_iter<std_alloc<int>>({1, 2, 3, 4}, 5);
    }

    TEST_CASE("custom_alloc_compiles") {
        construction<custom_alloc<int>>();
    }
    TEST_CASE("custom_alloc_insert") {
        insert<custom_alloc<int>>({1, 2, 3, 4});
    }
    TEST_CASE("custom_alloc_iterator_insert") {
        iterator_insert<custom_alloc<int>>({1, 2, 3, 4});
    }
    TEST_CASE("custom_alloc_iterator_access") {
        iterator_access<custom_alloc<int>>(42);
    }
    TEST_CASE("custom_alloc_iterator_access_multi") {
        iterator_access_multi<custom_alloc<int>>({1, 2, 3, 4});
    }
    TEST_CASE("custom_alloc_const_iterator_access_multi") {
        const_iterator_access_multi<custom_alloc<int>>({1, 2, 3, 4});
    }
    TEST_CASE("custom_find_true") {
        find<custom_alloc<int>>({1, 2, 3, 4}, 4, true);
    }
    TEST_CASE("custom_find_false") {
        find<custom_alloc<int>>({1, 2, 3, 4}, 5, false);
    }
    TEST_CASE("custom_erase") {
        erase<custom_alloc<int>>({1, 2, 3, 4}, 5);
    }
    TEST_CASE("custom_erase_with_const_iter") {
        erase_with_const_iter<custom_alloc<int>>({1, 2, 3, 4}, 5);
    }

    TEST_CASE("full_set") {
        dice::sparse_set<int, std::hash<int>, std::equal_to<int>, offset_ptr_allocator<int>> set;
        std::vector<int> data = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        set.insert(data.begin(), data.end());
        auto check = [&set](int d) {
            return set.contains(d);
        };
        // size did not match
        REQUIRE(data.size() == set.size());
        // Set did not contain all values
        REQUIRE(std::all_of(data.begin(), data.end(), check));
    }
}
