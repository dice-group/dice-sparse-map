/** @file
 * @brief Checks for fancy pointer support in the sparse_hash implementation for single values (sets).
 */

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <dice/sparse-map/sparse_set.hpp>
#include <dice/sparse-map/sparse_hash.hpp>
#include "CustomAllocator.hpp"

/* Tests are analogous to the  tests in sparse_array_tests.cpp.
 * The template parameter now also holds the value_type.
 */
namespace details {
    template<typename Key>
    struct KeySelect {
        using key_type = Key;
		using both_type = Key const;

		static key_type const &both(Key const &key) noexcept { return key; }
        static key_type const &key(Key const &key) noexcept { return key; }
    };

    template<typename T, typename Alloc>
    using sparse_set = dice::sparse_map::detail::sparse_hash<
            T, KeySelect<T>, std::hash<T>, std::equal_to<T>, Alloc,
            dice::sparse_map::power_of_two_growth_policy<2>,
            dice::sparse_map::exception_safety::basic,
            dice::sparse_map::sparsity::medium,
            dice::sparse_map::probing::quadratic>;

    template<typename Set>
	Set default_construct_set() {
        return Set{Set::DEFAULT_INIT_BUCKET_COUNT, {}, {}, {}, Set::DEFAULT_MAX_LOAD_FACTOR};
    }

    /** checks if all values of the set are in the initializer_list and than if the lengths are equal.
     *  So basically Set \subset l and |Set| == |l|.
     *  Needs 'set.contains(.)' to work correctly.
     */
    template <typename Set>
    bool is_equal(Set const& set, std::initializer_list<typename Set::value_type> l) {
        return std::all_of(l.begin(), l.end(), [&set](typename Set::value_type i){return set.contains(i);})
            and set.size() == l.size();
    }
}

template<typename T>
struct STD {
    using value_type = T;
    using Allocator = std::allocator<value_type>;
    using Set = details::sparse_set<value_type, Allocator>;
};


#define TEST_TYPES details::sparse_set<int, std::allocator<int>>, \
				   details::sparse_set<int, OffsetAllocator<int>>

TEST_SUITE("sparse set with fancy pointers") {
	TEST_CASE_TEMPLATE("construction", T, TEST_TYPES) {
		auto set = details::default_construct_set<T>();
	}

	TEST_CASE_TEMPLATE("insert", T, TEST_TYPES) {
		std::initializer_list<typename T::value_type> l{1,2,3,4};

		auto set = details::default_construct_set<T>();
		for (auto const& i: l)  set.insert(i);
		//'insert' did not create exactly the values needed
		REQUIRE(details::is_equal(set, l));
	}

	TEST_CASE_TEMPLATE("iter insert", T, TEST_TYPES) {
		std::initializer_list<typename T::value_type> l{1,2,3,4};

		auto set = details::default_construct_set<T>();
		set.insert(l.begin(), l.end());
		//'insert' with iterators did not create exactly the values needed
		REQUIRE(details::is_equal(set, l));
	}

	TEST_CASE_TEMPLATE("iter access", T, TEST_TYPES) {
		typename T::value_type single_value = 42;

		auto set = details::default_construct_set<T>();
		set.insert(single_value);
		//iterator cannot access single value
		REQUIRE(*(set.begin()) == single_value);
	}

	TEST_CASE_TEMPLATE("iter access multi", T, TEST_TYPES) {
		std::initializer_list<typename T::value_type> l{1,2,3,4};

		auto set = details::default_construct_set<T>();
		set.insert(l.begin(), l.end());
		std::vector<typename T::value_type> l_sorted = l;
		std::vector<typename T::value_type> set_sorted(set.begin(), set.end());
		std::sort(l_sorted.begin(), l_sorted.end());
		std::sort(set_sorted.begin(), set_sorted.end());
		//iterating over the set didn't work
		REQUIRE(std::equal(l_sorted.begin(), l_sorted.end(),
								 set_sorted.begin()));
	}

	TEST_CASE_TEMPLATE("const iter access multi", T, TEST_TYPES) {
		std::initializer_list<typename T::value_type> l{1,2,3,4};

		auto set = details::default_construct_set<T>();
		set.insert(l.begin(), l.end());
		std::vector<typename T::value_type> l_sorted = l;
		std::vector<typename T::value_type> set_sorted(set.cbegin(), set.cend());
		std::sort(l_sorted.begin(), l_sorted.end());
		std::sort(set_sorted.begin(), set_sorted.end());
		//const iterating over the set didn't work
		REQUIRE(std::equal(l_sorted.begin(), l_sorted.end(),
								 set_sorted.begin()));
	}

	TEST_CASE_TEMPLATE("find", T, TEST_TYPES) {
		std::initializer_list<typename T::value_type> l{1,2,3,4};

		auto set = details::default_construct_set<T>();
		set.insert(l.begin(), l.end());

		SUBCASE("exists") {
			auto iter = set.find(4);
			REQUIRE(iter != set.end());
		}

		SUBCASE("not exists") {
			auto iter = set.find(5);
			REQUIRE(iter == set.end());
		}
	}

	TEST_CASE_TEMPLATE("erase", T, TEST_TYPES) {
		std::initializer_list<typename T::value_type> l{1,2,3,4};
		typename T::value_type extra_value = 5;

		SUBCASE("iter") {
			auto set = details::default_construct_set<T>();
			set.insert(extra_value);
			set.insert(l.begin(), l.end());
			// force non-const iterator
			auto iter = set.begin();
			for(; *iter != extra_value; ++iter);
			set.erase(iter);
			//erase did not work as expected
			REQUIRE(details::is_equal(set, l));
		}

		SUBCASE("const iter") {
			auto set = details::default_construct_set<T>();
			set.insert(extra_value);
			set.insert(l.begin(), l.end());
			//force const iterator
			auto iter = set.cbegin();
			for(; *iter != extra_value; ++iter);
			set.erase(iter);
			//erase did not work as expected
			REQUIRE(details::is_equal(set, l));
		}
	}

	TEST_CASE("full set") {
		dice::sparse_map::sparse_set<int, std::hash<int>, std::equal_to<int>, OffsetAllocator<int>> set;
		std::vector<int> data = {1,2,3,4,5,6,7,8,9};
		set.insert(data.begin(), data.end());
		auto check = [&set](int d) {return set.contains(d);};
		//size did not match
		REQUIRE(data.size() == set.size());
		//Set did not contain all values
		REQUIRE(std::all_of(data.begin(), data.end(), check));
	}
}
