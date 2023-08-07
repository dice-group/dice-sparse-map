/**
 * MIT License
 *
 * Copyright (c) 2017 Thibaut Goetghebuer-Planchon <tessil@gmx.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <dice/sparse_map/sparse_set.hpp>

#include <functional>
#include <string>
#include <tuple>

#include "utils.h"

#define TEST_SETS dice::sparse_map::sparse_set<std::int64_t>,                               \
				  dice::sparse_map::sparse_set<std::string>,                                \
				  dice::sparse_map::sparse_set<self_reference_member_test>,                 \
				  dice::sparse_map::sparse_set<move_only_test>,                             \
				  dice::sparse_map::sparse_pg_set<self_reference_member_test>,              \
				  dice::sparse_map::sparse_set<move_only_test, std::hash<move_only_test>,   \
											   std::equal_to<move_only_test>,               \
											   std::allocator<move_only_test>,              \
											   dice::sparse_map::prime_growth_policy>,      \
				  dice::sparse_map::sparse_set<self_reference_member_test,                  \
											   std::hash<self_reference_member_test>,       \
											   std::equal_to<self_reference_member_test>,   \
											   std::allocator<self_reference_member_test>,  \
											   dice::sparse_map::mod_growth_policy<>>,      \
				  dice::sparse_map::sparse_set < move_only_test, std::hash<move_only_test>, \
				  std::equal_to<move_only_test>,                                            \
				  std::allocator<move_only_test>,                                           \
				  dice::sparse_map::mod_growth_policy<>>

TEST_SUITE("sparse set") {
	TEST_CASE_TEMPLATE("insert", HSet, TEST_SETS) {
		// insert x values, insert them again, check values
		using key_t = typename HSet::key_type;

		const std::size_t nb_values = 1000;
		HSet set;

		for (std::size_t i = 0; i < nb_values; i++) {
			auto k = utils::get_key<key_t>(i);
			auto [it, inserted] = set.insert(std::move(k));

			CHECK_EQ(*it, utils::get_key<key_t>(i));
			CHECK(inserted);
		}
		CHECK_EQ(set.size(), nb_values);

		for (std::size_t i = 0; i < nb_values; i++) {
			auto [it, inserted] = set.insert(utils::get_key<key_t>(i));

			CHECK_EQ(*it, utils::get_key<key_t>(i));
			CHECK(!inserted);
		}

		for (std::size_t i = 0; i < nb_values; i++) {
			auto it = set.find(utils::get_key<key_t>(i));

			CHECK_EQ(*it, utils::get_key<key_t>(i));
		}
	}

	TEST_CASE("compare") {
		const dice::sparse_map::sparse_set<std::string> set1 = {"a", "e", "d", "c", "b"};
		const dice::sparse_map::sparse_set<std::string> set1_copy = {"e", "c", "b", "a", "d"};
		const dice::sparse_map::sparse_set<std::string> set2 = {"e", "c", "b", "a", "d", "f"};
		const dice::sparse_map::sparse_set<std::string> set3 = {"e", "c", "b", "a"};
		const dice::sparse_map::sparse_set<std::string> set4 = {"a", "e", "d", "c", "z"};

		CHECK(set1 == set1_copy);
		CHECK(set1_copy == set1);

		CHECK(set1 != set2);
		CHECK(set2 != set1);

		CHECK(set1 != set3);
		CHECK(set3 != set1);

		CHECK(set1 != set4);
		CHECK(set4 != set1);

		CHECK(set2 != set3);
		CHECK(set3 != set2);

		CHECK(set2 != set4);
		CHECK(set4 != set2);

		CHECK(set3 != set4);
		CHECK(set4 != set3);
	}

	TEST_CASE("insert pointer") {
		// Test added mainly to be sure that the code compiles with MSVC
		std::string value;
		std::string* value_ptr = &value;

		dice::sparse_map::sparse_set<std::string*> set;
		set.insert(value_ptr);
		set.emplace(value_ptr);

		CHECK_EQ(set.size(), 1);
		CHECK_EQ(**set.begin(), value);
	}
}
