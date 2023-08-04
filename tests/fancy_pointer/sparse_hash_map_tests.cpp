/** @file
 * @brief Checks for fancy pointer support in the sparse_hash implementation for pair values (maps).
 */

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <unordered_map>
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_hash.hpp>
#include "CustomAllocator.hpp"

/* Tests are analogous to the  tests in sparse_array_tests.cpp.
 * The template parameter now also holds the value_type.
 */
namespace details {
    template<typename Key, typename T>
    struct KeyValueSelect {
        using key_type = Key;
		using value_type = T;
		using both_type = std::pair<Key const, T>;

		template<typename K>
		static key_type const &key(std::pair<K, T> const &key_value) noexcept {
			return key_value.first;
		}

		template<typename K>
		static value_type const &value(std::pair<K, T> const &key_value) noexcept {
			return key_value.second;
		}

		template<typename K>
		static value_type &value(std::pair<K, T> &key_value) noexcept {
			return key_value.second;
		}

		template<typename K>
		static both_type const &both(std::pair<K, T> const &key_value) noexcept {
			return reinterpret_cast<both_type const &>(key_value);
		}

		template<typename K>
		static both_type &both(std::pair<K, T> &key_value) noexcept {
			return reinterpret_cast<both_type &>(key_value);
		}
    };


    template<typename Key, typename T, typename Alloc>
    using sparse_map = dice::sparse_map::detail::sparse_hash<
            std::pair<Key, T>, KeyValueSelect<Key, T>, std::hash<T>, std::equal_to<T>, Alloc,
            dice::sparse_map::power_of_two_growth_policy<2>,
            dice::sparse_map::exception_safety::basic,
            dice::sparse_map::sparsity::medium,
            dice::sparse_map::probing::quadratic,
			dice::sparse_map::default_max_load_factor>;

    template<typename T>
    typename T::Map default_construct_map() {
        using Key = typename T::key_type;
        return typename T::Map(T::Map::default_init_bucket_count,
                        std::hash<Key>(),
                        std::equal_to<Key>(),
                        typename T::Allocator());
    }

    /** Checks if all values of the map are in the initializer_list and than if the lengths are equal.
     *  So basically Map \subset l and |Map| == |l|.
     *  Needs 'map.contains(.)' and 'map.at(.)' to work correctly.
     */
    template <typename Map>
    bool is_equal(Map const& map, std::initializer_list<typename Map::value_type> l) {
        auto check_in_map = [&map](typename Map::value_type p) {
           return map.contains(p.first) && map.at(p.first) == p.second;
        };
        return std::all_of(l.begin(), l.end(), check_in_map) && map.size() == l.size();
    }
    template <typename Map1, typename Map2>
    bool is_equal(Map1 const& custom_map, Map2 const &normal_map) {
        auto check_in_map = [&custom_map](typename Map2::value_type const& p) {
            return custom_map.count(p.first) == 1 && custom_map.at(p.first) == p.second;
        };
        return std::all_of(normal_map.begin(), normal_map.end(), check_in_map) && custom_map.size() == normal_map.size();
    }
}

template<typename Key, typename T>
struct STD {
    using key_type = Key;
    using value_type = std::pair<Key, T>;
    using Allocator = std::allocator<value_type>;
    using Map = details::sparse_map<Key, T, Allocator>;
};

template<typename Key, typename T>
struct CUSTOM {
    using key_type = Key;
    using value_type = std::pair<Key, T>;
    using Allocator = OffsetAllocator<value_type>;
    using Map = details::sparse_map<Key, T, Allocator>;
};


#define TEST_MAPS STD<int, int>, CUSTOM<int, int>

TEST_SUITE("sparse map with fancy pointers") {
	TEST_CASE_TEMPLATE("construction", T, TEST_MAPS) {
		auto map = details::default_construct_map<T>();
	}

	TEST_CASE_TEMPLATE("insert", T, TEST_MAPS) {
		std::initializer_list<typename T::value_type> l{{1,2},{3,4},{5,6}};

		auto map = details::default_construct_map<T>();
		for (auto dataPair : l)  map.insert(dataPair);
		//'insert' did not create exactly the values needed
		REQUIRE(details::is_equal(map, l));
	}

	TEST_CASE_TEMPLATE("iter insert", T, TEST_MAPS) {
		std::initializer_list<typename T::value_type> l{{1,2},{3,4},{5,6}};

		auto map = details::default_construct_map<T>();
		map.insert(l.begin(), l.end());
		//'insert' with iterators did not create exactly the values needed
		REQUIRE(details::is_equal(map, l));
	}

	TEST_CASE_TEMPLATE("iter access", T, TEST_MAPS) {
		typename T::value_type single_value{1,42};

		auto map = details::default_construct_map<T>();
		map.insert(single_value);
		//iterator cannot access single value
		REQUIRE((*map.begin()).first == single_value.first);
		REQUIRE((*map.begin()).second == single_value.second);
	}

	TEST_CASE_TEMPLATE("iter access multi", T, TEST_MAPS) {
		std::initializer_list<typename T::value_type> l{{1,2},{3,4},{5,6}};

		auto map = details::default_construct_map<T>();
		map.insert(l.begin(), l.end());
		std::vector<typename T::value_type> l_sorted = l;
		std::vector<typename T::value_type> map_sorted(map.begin(), map.end());
		std::sort(l_sorted.begin(), l_sorted.end());
		std::sort(map_sorted.begin(), map_sorted.end());
		//iterating over the map didn't work
		REQUIRE(std::equal(l_sorted.begin(), l_sorted.end(),
								 map_sorted.begin()));
	}

	TEST_CASE_TEMPLATE("value", T, TEST_MAPS) {
		typename T::value_type to_change{1, 42};
		std::initializer_list<typename T::value_type> l{{1,2},{3,4},{5,6}};

		auto map = details::default_construct_map<T>();
		map.insert(l.begin(), l.end());
		map[to_change.first] = to_change.second;

		std::unordered_map<typename T::value_type::first_type, typename T::value_type::second_type> check(l.begin(), l.end());
		check[to_change.first] = to_change.second;

		//changing a single value didn't work
		REQUIRE(details::is_equal(map, check));
	}

	TEST_CASE("full map") {
		dice::sparse_map::sparse_map<int, int, std::hash<int>, std::equal_to<int>, OffsetAllocator<std::pair<int,int>>> map;
		std::vector<std::pair<int,int>> data = {
				{0,1},{2,3},{4,5},{6,7},{8,9}
		};
		map.insert(data.begin(), data.end());
		auto check = [&map](std::pair<int,int> p) {
			if (!map.contains(p.first)) return false;
			return map.at(p.first) == p.second;
		};
		//size did not match
		REQUIRE(data.size() == map.size());
		//map did not contain all values
		REQUIRE(std::all_of(data.begin(), data.end(), check));
	}
}
