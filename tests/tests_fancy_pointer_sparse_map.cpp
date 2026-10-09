/** @file
 * @brief Checks for fancy pointer support in the sparse_hash implementation for pair values (maps).
 */

#include <dice/sparse-map/sparse_hash.hpp>
#include <dice/sparse-map/sparse_map.hpp>

#include "fixtures/offset_ptr_allocator.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <algorithm>
#include <functional>
#include <initializer_list>
#include <memory>
#include <unordered_map>
#include <utility>
#include <vector>

using dice::sparse_map::tests::offset_ptr_allocator;

/* Tests are analogous to the tests in tests_fancy_pointer_sparse_array.cpp.
 * The template parameter now also holds the value_type.
 */
namespace details {
    template<typename Key, typename T, typename Alloc>
    using sparse_map = dice::sparse_map::detail_sparse_hash::sparse_hash<
        dice::sparse_map::detail_sparse_hash::map_access<Key, T>,
        dice::sparse_map::tests::test_hash<Key>,
        std::equal_to<Key>,
        Alloc,
        dice::sparse_map::sh::sparsity::medium,
        dice::sparse_map::sh::allocation_failure::terminating
    >;

    template<typename T>
    typename T::map_type default_construct_map() {
        using key_type = typename T::key_type;
        return typename T::map_type(
            T::map_type::default_init_bucket_count,
            dice::sparse_map::tests::test_hash<key_type>(),
            std::equal_to<key_type>(),
            typename T::allocator_type(),
            T::map_type::default_max_load_factor
        );
    }

    /** Checks if all values of the map are in the initializer_list and then if the lengths are equal.
     *  So basically Map \subset l and |Map| == |l|.
     *  Needs 'map.contains(.)' and 'map.at(.)' to work correctly.
     */
    template<typename Map>
    bool is_equal(Map const &map, std::initializer_list<typename Map::value_type> l) {
        auto check_in_map = [&map](typename Map::value_type p) {
            return map.contains(p.first) && map.at(p.first) == p.second;
        };
        return std::all_of(l.begin(), l.end(), check_in_map) && map.size() == l.size();
    }
    template<typename Map1, typename Map2>
    bool is_equal(Map1 const &custom_map, Map2 const &normal_map) {
        auto check_in_map = [&custom_map](typename Map2::value_type const &p) {
            return custom_map.count(p.first) == 1 && custom_map.at(p.first) == p.second;
        };
        return std::all_of(normal_map.begin(), normal_map.end(), check_in_map)
            && custom_map.size() == normal_map.size();
    }
} // namespace details

template<typename T>
void construction() {
    auto map = details::default_construct_map<T>();
}

template<typename T>
void insert(std::initializer_list<typename T::value_type> l) {
    auto map = details::default_construct_map<T>();
    for (auto data_pair : l) {
        map.insert(data_pair);
    }
    //'insert' did not create exactly the values needed
    REQUIRE(details::is_equal(map, l));
}

template<typename T>
void iterator_insert(std::initializer_list<typename T::value_type> l) {
    auto map = details::default_construct_map<T>();
    map.insert(l.begin(), l.end());
    //'insert' with iterators did not create exactly the values needed
    REQUIRE(details::is_equal(map, l));
}

template<typename T>
void iterator_access(typename T::value_type single_value) {
    auto map = details::default_construct_map<T>();
    map.insert(single_value);
    // iterator cannot access single value
    REQUIRE((*(map.begin()) == single_value));
}

template<typename T>
void iterator_access_multi(std::initializer_list<typename T::value_type> l) {
    auto map = details::default_construct_map<T>();
    map.insert(l.begin(), l.end());
    std::vector<typename T::value_type> l_sorted = l;
    std::vector<typename T::value_type> map_sorted(map.begin(), map.end());
    std::sort(l_sorted.begin(), l_sorted.end());
    std::sort(map_sorted.begin(), map_sorted.end());
    // iterating over the map didn't work
    REQUIRE(std::equal(l_sorted.begin(), l_sorted.end(), map_sorted.begin()));
}

template<typename T>
void value(std::initializer_list<typename T::value_type> l, typename T::value_type to_change) {
    auto map = details::default_construct_map<T>();
    map.insert(l.begin(), l.end());
    map[to_change.first] = to_change.second;

    std::unordered_map<typename T::value_type::first_type, typename T::value_type::second_type> check(
        l.begin(),
        l.end()
    );
    check[to_change.first] = to_change.second;

    // changing a single value didn't work
    REQUIRE(details::is_equal(map, check));
}

template<typename Key, typename T>
struct std_alloc {
    using key_type = Key;
    using value_type = std::pair<Key, T>;
    using allocator_type = std::allocator<value_type>;
    using map_type = details::sparse_map<Key, T, allocator_type>;
};

template<typename Key, typename T>
struct custom_alloc {
    using key_type = Key;
    using value_type = std::pair<Key, T>;
    using allocator_type = offset_ptr_allocator<value_type>;
    using map_type = details::sparse_map<Key, T, allocator_type>;
};

TEST_SUITE("fancy_pointers/sparse_hash_map_tests") {
    TEST_CASE("std_alloc_compiles") {
        construction<std_alloc<int, int>>();
    }
    TEST_CASE("std_alloc_insert") {
        insert<std_alloc<int, int>>({{1, 2}, {3, 4}, {5, 6}});
    }
    TEST_CASE("std_alloc_iterator_insert") {
        insert<std_alloc<int, int>>({{1, 2}, {3, 4}, {5, 6}});
    }
    TEST_CASE("std_alloc_iterator_access") {
        iterator_access<std_alloc<int, int>>({1, 42});
    }
    TEST_CASE("std_alloc_iterator_access_multi") {
        iterator_access_multi<std_alloc<int, int>>({{1, 2}, {3, 4}, {5, 6}});
    }
    TEST_CASE("std_alloc_value") {
        value<std_alloc<int, int>>({{1, 2}, {3, 4}, {5, 6}}, {1, 42});
    }

    TEST_CASE("custom_alloc_compiles") {
        construction<custom_alloc<int, int>>();
    }
    TEST_CASE("custom_alloc_insert") {
        insert<custom_alloc<int, int>>({{1, 2}, {3, 4}, {5, 6}});
    }
    TEST_CASE("custom_alloc_iterator_insert") {
        insert<custom_alloc<int, int>>({{1, 2}, {3, 4}, {5, 6}});
    }
    TEST_CASE("custom_alloc_iterator_access") {
        iterator_access<custom_alloc<int, int>>({1, 42});
    }
    TEST_CASE("custom_alloc_iterator_access_multi") {
        iterator_access_multi<custom_alloc<int, int>>({{1, 2}, {3, 4}, {5, 6}});
    }
    TEST_CASE("custom_alloc_value") {
        value<custom_alloc<int, int>>({{1, 2}, {3, 4}, {5, 6}}, {1, 42});
    }

    TEST_CASE("full_map") {
        dice::sparse_map::sparse_map<
            int,
            int,
            dice::sparse_map::tests::test_hash<int>,
            std::equal_to<int>,
            offset_ptr_allocator<std::pair<int, int>>
        >
            map;
        std::vector<std::pair<int, int>> data = {{0, 1}, {2, 3}, {4, 5}, {6, 7}, {8, 9}};
        map.insert(data.begin(), data.end());
        auto check = [&map](std::pair<int, int> p) {
            if (!map.contains(p.first)) {
                return false;
            }
            return map.at(p.first) == p.second;
        };
        // size did not match
        REQUIRE(data.size() == map.size());
        // map did not contain all values
        REQUIRE(std::all_of(data.begin(), data.end(), check));
    }
}
