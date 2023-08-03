/** @file
 * @brief Checks for fancy pointer support in the sparse_array_type implementation.
 */

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <dice/sparse-map/sparse_hash.hpp>
#include "CustomAllocator.hpp"

// Globals
constexpr auto MAX_INDEX = 32; //BITMAP_NB_BITS

namespace details {
    template<typename T>
    typename T::Array generate_test_array(typename T::Allocator &a) {
        typename T::Array arr(MAX_INDEX, a);
        for (std::size_t i = 0; i < MAX_INDEX; ++i) {
            arr.set(a, i, static_cast<typename T::Value_Type>(i));
        }
        return arr;
    }

    template<typename T>
    std::vector<typename T::Allocator::value_type> generate_check_for_test_array() {
        std::vector<typename T::Allocator::value_type> check(MAX_INDEX);
        for (std::size_t i = 0; i < MAX_INDEX; ++i) {
            check[i] = i;
        }
        return check;
    }
}

template <typename T, dice::sparse_map::sparsity Sparsity = dice::sparse_map::sparsity::medium>
struct STD {
    using Allocator = std::allocator<T>;
    using Array = dice::sparse_map::detail::sparse_bucket<T, std::allocator<T>, Sparsity>;
    using Const_Iterator = T const*;
	using Value_Type = T;
};

template<typename T, dice::sparse_map::sparsity Sparsity = dice::sparse_map::sparsity::medium>
struct CUSTOM {
    using Allocator = OffsetAllocator<T>;
    using Array = dice::sparse_map::detail::sparse_bucket<T, OffsetAllocator<T>, Sparsity>;
    using Const_Iterator = boost::interprocess::offset_ptr<const T>;
	using Value_Type = T;
};


#define TEST_ARRAYS STD<int>, CUSTOM<int>

TEST_SUITE("sparse array with fancy pointers") {
	TEST_CASE_TEMPLATE("compile", T, TEST_ARRAYS) {
		typename T::Array test;
		(void) test;
	}

	TEST_CASE_TEMPLATE("construction", T, TEST_ARRAYS) {
		typename T::Allocator a;
		typename T::Array test(MAX_INDEX, a);
		test.clear(a); //needed because destructor asserts
	}

	TEST_CASE_TEMPLATE("set", T, TEST_ARRAYS) {
		typename T::Allocator a;
		auto test = details::generate_test_array<T>(a);
		auto check = details::generate_check_for_test_array<T>();
		//'set' did not create the correct order of items
		REQUIRE(std::equal(test.begin(), test.end(), check.begin()));
		test.clear(a); //needed because destructor asserts
	}

	TEST_CASE_TEMPLATE("copy ctor", T, TEST_ARRAYS) {
		typename T::Allocator a;
		//needs to be its own line, otherwise the move-construction would take place
		auto test = details::generate_test_array<T>(a);
		typename T::Array copy(test, a);
		auto check = details::generate_check_for_test_array<T>();
		//'copy' changed the order of the items
		REQUIRE(std::equal(copy.begin(), copy.end(), check.begin()));
		test.clear(a);
		copy.clear(a);
	}

	TEST_CASE_TEMPLATE("move ctor", T, TEST_ARRAYS) {
		typename T::Allocator a;
		//two lines needed. Otherwise move/copy elision
		auto moved_from = details::generate_test_array<T>(a);
		typename T::Array moved_to(std::move(moved_from));
		auto check = details::generate_check_for_test_array<T>();
		//'move' changed the order of the items
		REQUIRE(std::equal(moved_to.begin(), moved_to.end(), check.begin()));
		moved_to.clear(a);
	}

	TEST_CASE_TEMPLATE("const iterator", T, TEST_ARRAYS) {
		typename T::Allocator a;
		auto test = details::generate_test_array<T>(a);
		auto const_iter = test.cbegin();
		//const iterator has the wrong type
		REQUIRE((std::is_same<decltype(const_iter), typename T::Const_Iterator>::value));
		test.clear(a);
	}
}
