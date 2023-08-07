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

#include <dice/sparse_map/sparse_map.hpp>

#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>
#include <random>

#include "utils.h"

#define TEST_MAPS dice::sparse_map::sparse_map<std::int64_t, std::int64_t>,                                          \
				  dice::sparse_map::sparse_map<std::string, std::string>,                                            \
				  dice::sparse_map::sparse_map<std::int64_t, std::int64_t, mod_hash<9>>,                             \
				  dice::sparse_map::sparse_map<std::string, std::string, mod_hash<9>>,                               \
				  dice::sparse_map::sparse_map<move_only_test, move_only_test, mod_hash<9>>,                         \
				  dice::sparse_map::sparse_map<copy_only_test, copy_only_test, mod_hash<9>>,                         \
				  dice::sparse_map::sparse_map<self_reference_member_test, self_reference_member_test, mod_hash<9>>, \
                                                                                                                     \
				  dice::sparse_map::sparse_map<move_only_test, move_only_test, mod_hash<9>,                          \
											   std::equal_to<move_only_test>,                                        \
											   std::allocator<std::pair<move_only_test, move_only_test>>,            \
											   dice::sparse_map::power_of_two_growth_policy<4>>,                     \
				  dice::sparse_map::sparse_pg_map<move_only_test, move_only_test, mod_hash<9>>,                      \
				  dice::sparse_map::sparse_map<move_only_test, move_only_test, mod_hash<9>,                          \
											   std::equal_to<move_only_test>,                                        \
											   std::allocator<std::pair<move_only_test, move_only_test>>,            \
											   dice::sparse_map::mod_growth_policy<>>,                               \
                                                                                                                     \
				  dice::sparse_map::sparse_map<copy_only_test, copy_only_test, mod_hash<9>,                          \
											   std::equal_to<copy_only_test>,                                        \
											   std::allocator<std::pair<copy_only_test, copy_only_test>>,            \
											   dice::sparse_map::power_of_two_growth_policy<4>>,                     \
				  dice::sparse_map::sparse_pg_map<copy_only_test, copy_only_test, mod_hash<9>>,                      \
				  dice::sparse_map::sparse_map<copy_only_test, copy_only_test, mod_hash<9>,                          \
											   std::equal_to<copy_only_test>,                                        \
											   std::allocator<std::pair<copy_only_test, copy_only_test>>,            \
											   dice::sparse_map::mod_growth_policy<>>,                               \
                                                                                                                     \
                                                                                                                     \
				  dice::sparse_map::sparse_map<std::string, std::string, mod_hash<9>,                                \
											   std::equal_to<std::string>,                                           \
											   std::allocator<std::pair<std::string, std::string>>,                  \
											   dice::sparse_map::power_of_two_growth_policy<2>,                      \
											   dice::sparse_map::exception_safety::strong>,                          \
                                                                                                                     \
                                                                                                                     \
				  dice::sparse_map::sparse_map<std::string, std::string, mod_hash<9>,                                \
											   std::equal_to<std::string>,                                           \
											   std::allocator<std::pair<std::string, std::string>>,                  \
											   dice::sparse_map::power_of_two_growth_policy<2>,                      \
											   dice::sparse_map::exception_safety::basic,                            \
											   dice::sparse_map::sparsity::high>,                                    \
				  dice::sparse_map::sparse_map<std::string, std::string, mod_hash<9>,                                \
											   std::equal_to<std::string>,                                           \
											   std::allocator<std::pair<std::string, std::string>>,                  \
											   dice::sparse_map::power_of_two_growth_policy<2>,                      \
											   dice::sparse_map::exception_safety::basic, dice::sparse_map::sparsity::low>

TEST_SUITE("sparse map") {

	/**
	 * insert
	 */
	TEST_CASE_TEMPLATE("insert", HMap, TEST_MAPS) {
		// insert x values, insert them again, check values
		using key_t = typename HMap::key_type;
		using value_t = typename HMap::mapped_type;

		const std::size_t nb_values = 1000;
		HMap map(0);
		CHECK_EQ(map.bucket_count(), 0);


		for (std::size_t i = 0; i < nb_values; i++) {
			auto [it, inserted] = map.insert({utils::get_key<key_t>(i), utils::get_value<value_t>(i)});

			CHECK_EQ(it->first, utils::get_key<key_t>(i));
			CHECK_EQ(it->second, utils::get_value<value_t>(i));
			CHECK(inserted);
		}
		CHECK_EQ(map.size(), nb_values);

		for (std::size_t i = 0; i < nb_values; i++) {
			auto [it, inserted] = map.insert({utils::get_key<key_t>(i), utils::get_value<value_t>(i + 1)});

			CHECK_EQ(it->first, utils::get_key<key_t>(i));
			CHECK_EQ(it->second, utils::get_value<value_t>(i));
			CHECK(!inserted);
		}

		for (std::size_t i = 0; i < nb_values; i++) {
			auto it = map.find(utils::get_key<key_t>(i));

			CHECK_EQ(it->first, utils::get_key<key_t>(i));
			CHECK_EQ(it->second, utils::get_value<value_t>(i));
		}
	}

	TEST_CASE("range insert") {
		// create a vector<std::pair> of values to insert, insert part of them in the
		// map, check values
		const int nb_values = 1000;
		std::vector<std::pair<int, int>> values_to_insert(nb_values);
		for (int i = 0; i < nb_values; i++) {
			values_to_insert[i] = std::make_pair(i, i + 1);
		}

		dice::sparse_map::sparse_map<int, int> map = {{-1, 1}, {-2, 2}};
		map.insert(std::next(values_to_insert.begin(), 10),
				   values_to_insert.end() - 5);

		CHECK_EQ(map.size(), 987);

		CHECK_EQ(map[-1], 1);
		CHECK_EQ(map[-2], 2);

		for (int i = 10; i < nb_values - 5; i++) {
			CHECK_EQ(map[i], i + 1);
		}
	}

	TEST_CASE("insert with hint") {
		dice::sparse_map::sparse_map<int, int> map{{1, 0}, {2, 1}, {3, 2}};

		// Wrong hint
		CHECK_EQ(map.insert(map.find(2), std::make_pair(3, 4)), map.find(3));

		// Good hint
		CHECK_EQ(map.insert(map.find(2), std::make_pair(2, 4)), map.find(2));

		// end() hint
		CHECK_EQ(map.insert(map.find(10), std::make_pair(2, 4)), map.find(2));

		CHECK_EQ(map.size(), 3);

		// end() hint, new value
		CHECK_EQ(map.insert(map.find(10), std::make_pair(4, 3))->first, 4);

		// Wrong hint, new value
		CHECK_EQ(map.insert(map.find(2), std::make_pair(5, 4))->first, 5);

		CHECK_EQ(map.size(), 5);
	}

	/**
 * emplace_hint
 */
	TEST_CASE("emplace hint") {
		dice::sparse_map::sparse_map<int, int> map{{1, 0}, {2, 1}, {3, 2}};

		// Wrong hint
		CHECK_EQ(map.emplace_hint(map.find(2), std::piecewise_construct,
								  std::forward_as_tuple(3),
								  std::forward_as_tuple(4)),
				 map.find(3));

		// Good hint
		CHECK_EQ(map.emplace_hint(map.find(2), std::piecewise_construct,
								  std::forward_as_tuple(2),
								  std::forward_as_tuple(4)),
				 map.find(2));

		// end() hint
		CHECK_EQ(map.emplace_hint(map.find(10), std::piecewise_construct,
								  std::forward_as_tuple(2),
								  std::forward_as_tuple(4)),
				 map.find(2));

		CHECK_EQ(map.size(), 3);

		// end() hint, new value
		CHECK_EQ(map.emplace_hint(map.find(10), std::piecewise_construct,
								 std::forward_as_tuple(4), std::forward_as_tuple(3))->first,
				 4);

		// Wrong hint, new value
		CHECK_EQ(map.emplace_hint(map.find(2), std::piecewise_construct,
								  std::forward_as_tuple(5), std::forward_as_tuple(4))->first,
				 5);

		CHECK_EQ(map.size(), 5);
	}

	TEST_CASE("emplace") {
		dice::sparse_map::sparse_map<std::int64_t, move_only_test> map;

		auto [it, inserted] = map.emplace(std::piecewise_construct,
										  std::forward_as_tuple(10),
										  std::forward_as_tuple(1));
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(1));
		CHECK(inserted);

		std::tie(it, inserted) = map.emplace(std::piecewise_construct,
											 std::forward_as_tuple(10),
											 std::forward_as_tuple(3));
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(1));
		CHECK(!inserted);
	}

	TEST_CASE("try emplace") {
		dice::sparse_map::sparse_map<std::int64_t, move_only_test> map;

		auto [it, inserted] = map.try_emplace(10, 1);
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(1));
		CHECK(inserted);

		std::tie(it, inserted) = map.try_emplace(10, 3);
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(1));
		CHECK(!inserted);
	}

	TEST_CASE("try emplace 2") {
		// Insert x values with try_emplace, insert them again, check with find.
		dice::sparse_map::sparse_map<std::string, move_only_test> map;

		const std::size_t nb_values = 1000;
		for (std::size_t i = 0; i < nb_values; i++) {
			auto [it, inserted] = map.try_emplace(utils::get_key<std::string>(i), i);

			CHECK_EQ(it->first, utils::get_key<std::string>(i));
			CHECK_EQ(it->second, move_only_test(i));
			CHECK(inserted);
		}
		CHECK_EQ(map.size(), nb_values);

		for (std::size_t i = 0; i < nb_values; i++) {
			auto [it, inserted] = map.try_emplace(utils::get_key<std::string>(i), i + 1);

			CHECK_EQ(it->first, utils::get_key<std::string>(i));
			CHECK_EQ(it->second, move_only_test(i));
			CHECK(!inserted);
		}

		for (std::size_t i = 0; i < nb_values; i++) {
			auto it = map.find(utils::get_key<std::string>(i));

			CHECK_EQ(it->first, utils::get_key<std::string>(i));
			CHECK_EQ(it->second, move_only_test(i));
		}
	}

	TEST_CASE("emplace hint") {
		dice::sparse_map::sparse_map<std::int64_t, move_only_test> map(0);

		// end() hint, new value
		auto it = map.try_emplace(map.find(10), 10, 1);
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(1));

		// Good hint
		it = map.try_emplace(map.find(10), 10, 3);
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(1));

		// Wrong hint, new value
		it = map.try_emplace(map.find(10), 1, 3);
		CHECK_EQ(it->first, 1);
		CHECK_EQ(it->second, move_only_test(3));
	}

	TEST_CASE("insert or assign") {
		dice::sparse_map::sparse_map<std::int64_t, move_only_test> map;

		auto [it, inserted] = map.insert_or_assign(10, move_only_test(1));
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(1));
		CHECK(inserted);

		std::tie(it, inserted) = map.insert_or_assign(10, move_only_test(3));
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(3));
		CHECK(!inserted);
	}

	TEST_CASE("insert or assign hint") {
		dice::sparse_map::sparse_map<std::int64_t, move_only_test> map(0);

		// end() hint, new value
		auto it = map.insert_or_assign(map.find(10), 10, move_only_test(1));
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(1));

		// Good hint
		it = map.insert_or_assign(map.find(10), 10, move_only_test(3));
		CHECK_EQ(it->first, 10);
		CHECK_EQ(it->second, move_only_test(3));

		// Bad hint, new value
		it = map.insert_or_assign(map.find(10), 1, move_only_test(3));
		CHECK_EQ(it->first, 1);
		CHECK_EQ(it->second, move_only_test(3));
	}

	TEST_CASE("range erase all") {
		// insert x values, delete all
		using HMap = dice::sparse_map::sparse_map<std::string, std::int64_t>;

		const std::size_t nb_values = 1000;
		HMap map = utils::get_filled_hash_map<HMap>(nb_values);

		auto it = map.erase(map.begin(), map.end());
		CHECK(it == map.end());
		CHECK(map.empty());
	}

	TEST_CASE("range erase") {
		// insert x values, delete all except 10 first and 780 last values
		using HMap = dice::sparse_map::sparse_map<std::string, std::int64_t>;

		const std::size_t nb_values = 1000;
		HMap map = utils::get_filled_hash_map<HMap>(nb_values);

		auto it_first = std::next(map.begin(), 10);
		auto it_last = std::next(map.begin(), 220);

		auto it = map.erase(it_first, it_last);
		CHECK_EQ(std::distance(it, map.end()), 780);
		CHECK_EQ(map.size(), 790);
		CHECK_EQ(std::distance(map.begin(), map.end()), 790);

		for (auto &val : map) {
			CHECK_EQ(map.count(val.first), 1);
		}
	}

	TEST_CASE_TEMPLATE("erase loop", HMap, TEST_MAPS) {
		// insert x values, delete all one by one with iterator
		std::size_t nb_values = 1000;

		HMap map = utils::get_filled_hash_map<HMap>(nb_values);
		HMap map2 = utils::get_filled_hash_map<HMap>(nb_values);

		auto it = map.begin();
		// Use second map to check for key after delete as we may not copy the key
		// with move-only types.
		auto it2 = map2.begin();
		while (it != map.end()) {
			it = map.erase(it);
			--nb_values;

			CHECK_EQ(map.count(it2->first), 0);
			CHECK_EQ(map.size(), nb_values);
			++it2;
		}

		CHECK(map.empty());
	}

	TEST_CASE_TEMPLATE("erase loop range", HMap, TEST_MAPS) {
		// insert x values, delete all five by five with iterators
		const std::size_t hop = 5;
		std::size_t nb_values = 1000;

		REQUIRE_EQ(nb_values % hop, 0);

		HMap map = utils::get_filled_hash_map<HMap>(nb_values);

		auto it = map.begin();
		while (it != map.end()) {
			it = map.erase(it, std::next(it, hop));
			nb_values -= hop;

			CHECK_EQ(map.size(), nb_values);
		}

		CHECK(map.empty());
	}

	TEST_CASE_TEMPLATE("insert erase insert", HMap, TEST_MAPS) {
		// insert x/2 values, delete x/4 values, insert x/2 values, find each value
		using key_t = typename HMap::key_type;
		using value_t = typename HMap::mapped_type;

		const std::size_t nb_values = 2000;
		HMap map(10);

		// Insert nb_values/2
		for (std::size_t i = 0; i < nb_values / 2; i++) {
			auto [it, inserted] = map.insert({utils::get_key<key_t>(i), utils::get_value<value_t>(i)});

			CHECK_EQ(it->first, utils::get_key<key_t>(i));
			CHECK_EQ(it->second, utils::get_value<value_t>(i));
			CHECK(inserted);
		}
		CHECK_EQ(map.size(), nb_values / 2);

		// Delete nb_values/4
		for (std::size_t i = 0; i < nb_values / 2; i++) {
			if (i % 2 == 0) {
				CHECK_EQ(map.erase(utils::get_key<key_t>(i)), 1);
			}
		}
		CHECK_EQ(map.size(), nb_values / 4);

		// Insert nb_values/2
		for (std::size_t i = nb_values / 2; i < nb_values; i++) {
			auto [it, inserted] = map.insert({utils::get_key<key_t>(i), utils::get_value<value_t>(i)});

			CHECK_EQ(it->first, utils::get_key<key_t>(i));
			CHECK_EQ(it->second, utils::get_value<value_t>(i));
			CHECK(inserted);
		}
		CHECK_EQ(map.size(), nb_values - nb_values / 4);

		// Find
		for (std::size_t i = 0; i < nb_values; i++) {
			if (i % 2 == 0 && i < nb_values / 2) {
				auto it = map.find(utils::get_key<key_t>(i));

				CHECK(it == map.end());
			} else {
				auto it = map.find(utils::get_key<key_t>(i));

				REQUIRE(it != map.end());
				CHECK_EQ(it->first, utils::get_key<key_t>(i));
				CHECK_EQ(it->second, utils::get_value<value_t>(i));
			}
		}
	}

	TEST_CASE("range erase same iter") {
		// insert x values, test erase with same iterator as each parameter, check if
		// returned mutable iterator is valid.
		const std::size_t nb_values = 100;
		auto map =
				utils::get_filled_hash_map<dice::sparse_map::sparse_map<std::int64_t, std::int64_t>>(
						nb_values);

		dice::sparse_map::sparse_map<std::int64_t, std::int64_t>::const_iterator it_const =
				map.cbegin();
		std::advance(it_const, 10);

		dice::sparse_map::sparse_map<std::int64_t, std::int64_t>::iterator it_mutable =
				map.erase(it_const, it_const);
		CHECK(it_const == it_mutable);
		//CHECK(map.mutable_iterator(it_const) == it_mutable);
		CHECK_EQ(map.size(), 100);

		it_mutable->second = -100;
		CHECK_EQ(it_const->second, -100);
	}

	/**
 * rehash
 */
	TEST_CASE("rehash empty") {
		// test rehash(0), test find/erase/insert on map.
		const std::size_t nb_values = 100;
		auto map =
				utils::get_filled_hash_map<dice::sparse_map::sparse_map<std::int64_t, std::int64_t>>(
						nb_values);

		const std::size_t bucket_count = map.bucket_count();
		CHECK(bucket_count >= nb_values);

		map.clear();
		CHECK_EQ(map.bucket_count(), bucket_count);
		CHECK(map.empty());

		map.rehash(0);
		CHECK_EQ(map.bucket_count(), 0);
		CHECK(map.empty());

		CHECK(map.find(1) == map.end());
		CHECK_EQ(map.erase(1), 0);
		CHECK(map.insert({1, 10}).second);
		CHECK_EQ(map.at(1), 10);
	}

	/**
 * operator== and operator!=
 */
	TEST_CASE_TEMPLATE("compare", HMap, TEST_MAPS) {
		const dice::sparse_map::sparse_map<std::string, std::int64_t> map1 = {
				{"a", 1},
				{"e", 5},
				{"d", 4},
				{"c", 3},
				{"b", 2}};
		const dice::sparse_map::sparse_map<std::string, std::int64_t> map1_copy = {
				{"e", 5},
				{"c", 3},
				{"b", 2},
				{"a", 1},
				{"d", 4}};
		const dice::sparse_map::sparse_map<std::string, std::int64_t> map2 = {
				{"e", 5},
				{"c", 3},
				{"b", 2},
				{"a", 1},
				{"d", 4},
				{"f", 6}};
		const dice::sparse_map::sparse_map<std::string, std::int64_t> map3 = {
				{"e", 5},
				{"c", 3},
				{"b", 2},
				{"a", 1}};
		const dice::sparse_map::sparse_map<std::string, std::int64_t> map4 = {
				{"a", 1},
				{"e", 5},
				{"d", 4},
				{"c", 3},
				{"b", 26}};
		const dice::sparse_map::sparse_map<std::string, std::int64_t> map5 = {
				{"a", 1},
				{"e", 5},
				{"d", 4},
				{"c", 3},
				{"z", 2}};

		CHECK(map1 == map1_copy);
		CHECK(map1_copy == map1);

		CHECK(map1 != map2);
		CHECK(map2 != map1);

		CHECK(map1 != map3);
		CHECK(map3 != map1);

		CHECK(map1 != map4);
		CHECK(map4 != map1);

		CHECK(map1 != map5);
		CHECK(map5 != map1);

		CHECK(map2 != map3);
		CHECK(map3 != map2);

		CHECK(map2 != map4);
		CHECK(map4 != map2);

		CHECK(map2 != map5);
		CHECK(map5 != map2);

		CHECK(map3 != map4);
		CHECK(map4 != map3);

		CHECK(map3 != map5);
		CHECK(map5 != map3);

		CHECK(map4 != map5);
		CHECK(map5 != map4);
	}

	TEST_CASE("clear") {
		// insert x values, clear map
		using HMap = dice::sparse_map::sparse_map<std::int64_t, std::int64_t>;

		const std::size_t nb_values = 1000;
		auto map = utils::get_filled_hash_map<HMap>(nb_values);

		map.clear();
		CHECK_EQ(map.size(), 0);
		CHECK_EQ(std::distance(map.begin(), map.end()), 0);

		map.insert({5, -5});
		map.insert({{1, -1}, {2, -1}, {4, -4}, {3, -3}});

		CHECK(map == (HMap({{5, -5}, {1, -1}, {2, -1}, {4, -4}, {3, -3}})));
	}

	TEST_CASE("modify value through iterator") {
		// insert x values, modify value of even keys, check values
		const std::size_t nb_values = 100;
		auto map =
				utils::get_filled_hash_map<dice::sparse_map::sparse_map<std::int64_t, std::int64_t>>(
						nb_values);

		for (auto it = map.begin(); it != map.end(); it++) {
			if (it->first % 2 == 0) {
				it->second = -1;
			}
		}

		for (auto &val : map) {
			if (val.first % 2 == 0) {
				CHECK_EQ(val.second, -1);
			} else {
				CHECK_NE(val.second, -1);
			}
		}
	}

	/**
 * constructor
 */
	TEST_CASE("extreme bucket count value construction") {
		CHECK_THROWS(
				(dice::sparse_map::sparse_map<int, int, std::hash<int>, std::equal_to<int>,
											  std::allocator<std::pair<int, int>>,
											  dice::sparse_map::power_of_two_growth_policy<2>>(
						std::numeric_limits<std::size_t>::max())));

		CHECK_THROWS(
				(dice::sparse_map::sparse_map<int, int, std::hash<int>, std::equal_to<int>,
											  std::allocator<std::pair<int, int>>,
											  dice::sparse_map::power_of_two_growth_policy<2>>(
						std::numeric_limits<std::size_t>::max() / 2 + 1)));

		CHECK_THROWS(
				(dice::sparse_map::sparse_map<int, int, std::hash<int>, std::equal_to<int>,
											  std::allocator<std::pair<int, int>>,
											  dice::sparse_map::prime_growth_policy>(
						std::numeric_limits<std::size_t>::max())));

		CHECK_THROWS(
				(dice::sparse_map::sparse_map<int, int, std::hash<int>, std::equal_to<int>,
											  std::allocator<std::pair<int, int>>,
											  dice::sparse_map::prime_growth_policy>(
						std::numeric_limits<std::size_t>::max() / 2)));

		CHECK_THROWS(
				(dice::sparse_map::sparse_map<int, int, std::hash<int>, std::equal_to<int>,
											  std::allocator<std::pair<int, int>>,
											  dice::sparse_map::mod_growth_policy<>>(
						std::numeric_limits<std::size_t>::max())));
	}

	TEST_CASE("range construct") {
		dice::sparse_map::sparse_map<int, int> map = {{2, 1}, {1, 0}, {3, 2}};

		dice::sparse_map::sparse_map<int, int> map2(map.begin(), map.end());
		dice::sparse_map::sparse_map<int, int> map3(map.cbegin(), map.cend());
	}

	/**
	 * operator=(std::initializer_list)
	 */
	TEST_CASE("assign op") {
		dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map = {{0, 10}, {-2, 20}};
		CHECK_EQ(map.size(), 2);

		map = {{1, 3}, {2, 4}};
		CHECK_EQ(map.size(), 2);
		CHECK_EQ(map.at(1), 3);
		CHECK_EQ(map.at(2), 4);
		CHECK(map.find(0) == map.end());

		map = {};
		CHECK(map.empty());
	}

	/**
	 * move/copy constructor/operator
	 */
	TEST_CASE("move ctor") {
		// insert x values in map, move map into map_move with move constructor, check
		// map and map_move, insert additional values in map_move, check map_move
		using HMap = dice::sparse_map::sparse_map<std::string, move_only_test>;

		const std::size_t nb_values = 100;
		HMap map = utils::get_filled_hash_map<HMap>(nb_values);
		HMap map_move(std::move(map));

		CHECK(map_move == utils::get_filled_hash_map<HMap>(nb_values));
		CHECK(map == (HMap()));

		for (std::size_t i = nb_values; i < nb_values * 2; i++) {
			map_move.insert(
					{utils::get_key<std::string>(i), utils::get_value<move_only_test>(i)});
		}

		CHECK_EQ(map_move.size(), nb_values * 2);
		CHECK(map_move == utils::get_filled_hash_map<HMap>(nb_values * 2));
	}

	TEST_CASE("move ctor empty") {
		dice::sparse_map::sparse_map<std::string, move_only_test> map(0);
		dice::sparse_map::sparse_map<std::string, move_only_test> map_move(std::move(map));

		CHECK(map.empty());
		CHECK(map_move.empty());

		CHECK(map.find("") == map.end());
		CHECK(map_move.find("") == map_move.end());
	}

	TEST_CASE("move op") {
		// insert x values in map, move map into map_move, check map and map_move,
		// insert additional values in map_move, check map_move
		using HMap = dice::sparse_map::sparse_map<std::string, move_only_test>;

		const std::size_t nb_values = 100;
		HMap map = utils::get_filled_hash_map<HMap>(nb_values);
		HMap map_move = utils::get_filled_hash_map<HMap>(1);
		map_move = std::move(map);

		CHECK(map_move == utils::get_filled_hash_map<HMap>(nb_values));
		CHECK(map == (HMap()));

		for (std::size_t i = nb_values; i < nb_values * 2; i++) {
			map_move.insert(
					{utils::get_key<std::string>(i), utils::get_value<move_only_test>(i)});
		}

		CHECK_EQ(map_move.size(), nb_values * 2);
		CHECK(map_move == utils::get_filled_hash_map<HMap>(nb_values * 2));
	}

	TEST_CASE("move op empty") {
		dice::sparse_map::sparse_map<std::string, move_only_test> map(0);
		dice::sparse_map::sparse_map<std::string, move_only_test> map_move;
		map_move = (std::move(map));

		CHECK(map.empty());
		CHECK(map_move.empty());

		CHECK(map.find("") == map.end());
		CHECK(map_move.find("") == map_move.end());
	}

	TEST_CASE("reassign moved object move ctor") {
		using HMap = dice::sparse_map::sparse_map<std::string, std::string>;

		HMap map = {{"Key1", "Value1"}, {"Key2", "Value2"}, {"Key3", "Value3"}};
		HMap map_move(std::move(map));

		CHECK_EQ(map_move.size(), 3);
		CHECK_EQ(map.size(), 0);

		map = {{"Key4", "Value4"}, {"Key5", "Value5"}};
		CHECK(map == (HMap({{"Key4", "Value4"}, {"Key5", "Value5"}})));
	}

	TEST_CASE("reassign moved object move op") {
		using HMap = dice::sparse_map::sparse_map<std::string, std::string>;

		HMap map = {{"Key1", "Value1"}, {"Key2", "Value2"}, {"Key3", "Value3"}};
		HMap map_move = std::move(map);

		CHECK_EQ(map_move.size(), 3);
		CHECK_EQ(map.size(), 0);

		map = {{"Key4", "Value4"}, {"Key5", "Value5"}};
		CHECK(map == (HMap({{"Key4", "Value4"}, {"Key5", "Value5"}})));
	}

	TEST_CASE("use after move ctor") {
		using HMap = dice::sparse_map::sparse_map<std::string, move_only_test>;

		const std::size_t nb_values = 100;
		HMap map = utils::get_filled_hash_map<HMap>(nb_values);
		HMap map_move(std::move(map));

		CHECK(map == (HMap()));
		CHECK_EQ(map.size(), 0);
		CHECK_EQ(map.bucket_count(), 0);
		CHECK_EQ(map.erase("a"), 0);
		CHECK(map.find("a") == map.end());

		for (std::size_t i = 0; i < nb_values; i++) {
			map.insert(
					{utils::get_key<std::string>(i), utils::get_value<move_only_test>(i)});
		}

		CHECK_EQ(map.size(), nb_values);
		CHECK(map == map_move);
	}

	TEST_CASE("use after move op") {
		using HMap = dice::sparse_map::sparse_map<std::string, move_only_test>;

		const std::size_t nb_values = 100;
		HMap map = utils::get_filled_hash_map<HMap>(nb_values);
		HMap map_move(0);
		map_move = std::move(map);

		CHECK(map == (HMap()));
		CHECK_EQ(map.size(), 0);
		CHECK_EQ(map.bucket_count(), 0);
		CHECK_EQ(map.erase("a"), 0);
		CHECK(map.find("a") == map.end());

		for (std::size_t i = 0; i < nb_values; i++) {
			map.insert(
					{utils::get_key<std::string>(i), utils::get_value<move_only_test>(i)});
		}

		CHECK_EQ(map.size(), nb_values);
		CHECK(map == map_move);
	}

	TEST_CASE("copy ctor and op") {
		using HMap = dice::sparse_map::sparse_map<std::string, std::string, mod_hash<9>>;

		const std::size_t nb_values = 100;
		HMap map = utils::get_filled_hash_map<HMap>(nb_values);

		HMap map_copy = map;
		HMap map_copy2(map);
		HMap map_copy3 = utils::get_filled_hash_map<HMap>(1);
		map_copy3 = map;

		CHECK(map == map_copy);
		map.clear();

		CHECK(map_copy == map_copy2);
		CHECK(map_copy == map_copy3);
	}

	TEST_CASE("copy ctor empty") {
		dice::sparse_map::sparse_map<std::string, int> map(0);
		dice::sparse_map::sparse_map<std::string, int> map_copy(map);

		CHECK(map.empty());
		CHECK(map_copy.empty());

		CHECK(map.find("") == map.end());
		CHECK(map_copy.find("") == map_copy.end());
	}

	TEST_CASE("copy op empty") {
		dice::sparse_map::sparse_map<std::string, int> map(0);
		dice::sparse_map::sparse_map<std::string, int> map_copy(16);
		map_copy = map;

		CHECK(map.empty());
		CHECK(map_copy.empty());

		CHECK(map.find("") == map.end());
		CHECK(map_copy.find("") == map_copy.end());
	}

	TEST_CASE("at") {
		// insert x values, use at for known and unknown values.
		const dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map = {{0, 10}, {-2, 20}};

		CHECK_EQ(map.at(0), 10);
		CHECK_EQ(map.at(-2), 20);

		std::int64_t no_discard_dummy;
		CHECK_THROWS_AS(no_discard_dummy = map.at(1), std::out_of_range);
		(void) no_discard_dummy;
	}

	TEST_CASE("contains") {
		const dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map = {{0, 10}, {-2, 20}};

		CHECK(map.contains(0));
		CHECK(map.contains(-2));
		CHECK(!map.contains(-3));
	}

	TEST_CASE("equal range") {
		const dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map = {{0, 10}, {-2, 20}};

		auto it_pair = map.equal_range(0);
		REQUIRE_EQ(std::distance(it_pair.first, it_pair.second), 1);
		CHECK_EQ(it_pair.first->second, 10);

		it_pair = map.equal_range(1);
		CHECK(it_pair.first == it_pair.second);
		CHECK(it_pair.first == map.end());
	}

	TEST_CASE("index op") {
		// insert x values, use at for known and unknown values.
		dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map = {{0, 10}, {-2, 20}};

		CHECK_EQ(map[0], 10);
		CHECK_EQ(map[-2], 20);
		CHECK_EQ(map[2], std::int64_t());

		CHECK_EQ(map.size(), 3);
	}

	/**
	 * swap
	 */
	TEST_CASE("swap") {
		dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map = {{1, 10}, {8, 80}, {3, 30}};
		dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map2 = {{4, 40}, {5, 50}};

		using std::swap;
		swap(map, map2);

		CHECK(map ==
			  (dice::sparse_map::sparse_map<std::int64_t, std::int64_t>{{4, 40}, {5, 50}}));
		CHECK(map2 == (dice::sparse_map::sparse_map<std::int64_t, std::int64_t>{
							  {1, 10},
							  {8, 80},
							  {3, 30}}));

		map.insert({6, 60});
		map2.insert({4, 40});

		CHECK(map == (dice::sparse_map::sparse_map<std::int64_t, std::int64_t>{
							 {4, 40},
							 {5, 50},
							 {6, 60}}));
		CHECK(map2 == (dice::sparse_map::sparse_map<std::int64_t, std::int64_t>{
							  {1, 10},
							  {8, 80},
							  {3, 30},
							  {4, 40}}));
	}

	TEST_CASE("swap empty") {
		dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map = {{1, 10}, {8, 80}, {3, 30}};
		dice::sparse_map::sparse_map<std::int64_t, std::int64_t> map2;

		using std::swap;
		swap(map, map2);

		CHECK(map == (dice::sparse_map::sparse_map<std::int64_t, std::int64_t>{}));
		CHECK(map2 == (dice::sparse_map::sparse_map<std::int64_t, std::int64_t>{
							  {1, 10},
							  {8, 80},
							  {3, 30}}));

		map.insert({6, 60});
		map2.insert({4, 40});

		CHECK(map == (dice::sparse_map::sparse_map<std::int64_t, std::int64_t>{{6, 60}}));
		CHECK(map2 == (dice::sparse_map::sparse_map<std::int64_t, std::int64_t>{
							  {1, 10},
							  {8, 80},
							  {3, 30},
							  {4, 40}}));
	}

	TEST_CASE("key equal") {
		// Use a KeyEqual and Hash where any odd unsigned number 'x' is equal to
		// 'x-1'. Make sure that KeyEqual is called (and not ==).
		struct hash {
			std::size_t operator()(std::uint64_t v) const {
				if (v % 2u == 1u) {
					return std::hash<std::uint64_t>()(v - 1);
				} else {
					return std::hash<std::uint64_t>()(v);
				}
			}
		};

		struct key_equal {
			bool operator()(std::uint64_t lhs, std::uint64_t rhs) const {
				if (lhs % 2u == 1u) {
					lhs--;
				}

				if (rhs % 2u == 1u) {
					rhs--;
				}

				return lhs == rhs;
			}
		};

		dice::sparse_map::sparse_map<std::uint64_t, std::uint64_t, hash, key_equal> map;
		CHECK(map.insert({2, 10}).second);
		CHECK_EQ(map.at(2), 10);
		CHECK_EQ(map.at(3), 10);
		CHECK(!map.insert({3, 10}).second);

		CHECK_EQ(map.size(), 1);
	}

	/**
	 * other
	 */
	TEST_CASE("operations with all buckets marked as deleted or with a value") {
		// Test find/erase/insert operations on a map which we craft to have all its
		// buckets marked as deleted or containing a value to be sure that everything
		// works well in this edge case. Intrusive test (it's tightly coupled with the
		// implementation of the map).
		struct identity_hash {
			std::size_t operator()(unsigned int value) const {
				return std::size_t(value);
			}
		};

		dice::sparse_map::sparse_map<unsigned int, unsigned int, identity_hash,
									 std::equal_to<unsigned int>,
									 std::allocator<std::pair<unsigned int, unsigned int>>,
									 dice::sparse_map::power_of_two_growth_policy<2>,
									 dice::sparse_map::exception_safety::basic,
									 dice::sparse_map::sparsity::medium,
									 std::ratio<8, 10>> map;
		map.rehash(64);

		CHECK_EQ(map.bucket_count(), 64);
		CHECK_EQ(map.max_load_factor, 0.8f);

		for (unsigned int i = 0; i < 51; i++) {
			CHECK(map.insert({i, i}).second);
		}

		for (unsigned int i = 0; i < 14; i++) {
			CHECK_EQ(map.erase(i), 1);
		}

		for (unsigned int i = 51; i < 64; i++) {
			CHECK(map.insert({i, i}).second);
		}

		CHECK_EQ(map.size(), 50);
		CHECK_EQ(map.bucket_count(), 64);

		/**
		 * Map full of buckets marked as deleted or with a value. Check that find,
		 * erase and insert operations work well.
		 */

		// Find inexistent values.
		for (unsigned int i = 0; i < 14; i++) {
			CHECK(map.find(i) == map.end());
		}

		// Erase inexistent values.
		for (unsigned int i = 0; i < 14; i++) {
			CHECK_EQ(map.erase(i), 0);
		}
		CHECK_EQ(map.size(), 50);
		CHECK_EQ(map.bucket_count(), 64);

		// Try to insert existing values.
		for (unsigned int i = 14; i < 64; i++) {
			CHECK(!map.insert({i, i}).second);
		}
		CHECK_EQ(map.size(), 50);
		CHECK_EQ(map.bucket_count(), 64);

		// Insert new values
		for (unsigned int i = 0; i < 14; i++) {
			CHECK(map.insert({i, i}).second);
		}
		CHECK_EQ(map.size(), 64);
		CHECK_EQ(map.bucket_count(), 128);
	}

	TEST_CASE("heterogeneous lookup") {
		struct hash_ptr {
			std::size_t operator()(const std::unique_ptr<int> &p) const {
				return std::hash<std::uintptr_t>()(
						reinterpret_cast<std::uintptr_t>(p.get()));
			}

			std::size_t operator()(std::uintptr_t p) const {
				return std::hash<std::uintptr_t>()(p);
			}

			std::size_t operator()(const int *const &p) const {
				return std::hash<std::uintptr_t>()(reinterpret_cast<std::uintptr_t>(p));
			}
		};

		struct equal_to_ptr {
			using is_transparent = std::true_type;

			bool operator()(const std::unique_ptr<int> &p1,
							const std::unique_ptr<int> &p2) const {
				return p1 == p2;
			}

			bool operator()(const std::unique_ptr<int> &p1, std::uintptr_t p2) const {
				return reinterpret_cast<std::uintptr_t>(p1.get()) == p2;
			}

			bool operator()(std::uintptr_t p1, const std::unique_ptr<int> &p2) const {
				return p1 == reinterpret_cast<std::uintptr_t>(p2.get());
			}

			bool operator()(const std::unique_ptr<int> &p1,
							const int *const &p2) const {
				return p1.get() == p2;
			}

			bool operator()(const int *const &p1,
							const std::unique_ptr<int> &p2) const {
				return p1 == p2.get();
			}
		};

		std::unique_ptr<int> ptr1(new int(1));
		std::unique_ptr<int> ptr2(new int(2));
		std::unique_ptr<int> ptr3(new int(3));
		int other = -1;

		const std::uintptr_t addr1 = reinterpret_cast<std::uintptr_t>(ptr1.get());
		const int *const addr2 = ptr2.get();
		const int *const addr_unknown = &other;

		dice::sparse_map::sparse_map<std::unique_ptr<int>, int, hash_ptr, equal_to_ptr> map;
		map.insert({std::move(ptr1), 4});
		map.insert({std::move(ptr2), 5});
		map.insert({std::move(ptr3), 6});

		CHECK_EQ(map.size(), 3);

		CHECK_EQ(map.at(addr1), 4);
		CHECK_EQ(map.at(addr2), 5);

		int no_discard_dummy;
		CHECK_THROWS_AS(no_discard_dummy = map.at(addr_unknown), std::out_of_range);
		(void) no_discard_dummy;

		REQUIRE(map.find(addr1) != map.end());
		CHECK_EQ(*map.find(addr1)->first, 1);

		REQUIRE(map.find(addr2) != map.end());
		CHECK_EQ(*map.find(addr2)->first, 2);

		CHECK(map.find(addr_unknown) == map.end());

		CHECK_EQ(map.count(addr1), 1);
		CHECK_EQ(map.count(addr2), 1);
		CHECK_EQ(map.count(addr_unknown), 0);

		CHECK_EQ(map.erase(addr1), 1);
		CHECK_EQ(map.erase(addr2), 1);
		CHECK_EQ(map.erase(addr_unknown), 0);

		CHECK_EQ(map.size(), 1);
	}

	/**
	 * Various operations on empty map
	 */
	TEST_CASE("empty map") {
		dice::sparse_map::sparse_map<std::string, int> map(0);

		CHECK_EQ(map.bucket_count(), 0);
		CHECK_EQ(map.size(), 0);
		CHECK_EQ(map.load_factor(), 0);
		CHECK(map.empty());

		CHECK(map.begin() == map.end());
		CHECK(map.begin() == map.cend());
		CHECK(map.cbegin() == map.cend());

		CHECK(map.find("") == map.end());
		CHECK(map.find("test") == map.end());

		CHECK_EQ(map.count(""), 0);
		CHECK_EQ(map.count("test"), 0);

		CHECK(!map.contains(""));
		CHECK(!map.contains("test"));

		int no_discard_dummy;
		CHECK_THROWS_AS(no_discard_dummy = map.at(""), std::out_of_range);
		CHECK_THROWS_AS(no_discard_dummy = map.at("test"), std::out_of_range);
		(void) no_discard_dummy;

		auto range = map.equal_range("test");
		CHECK(range.first == range.second);

		CHECK_EQ(map.erase("test"), 0);
		CHECK(map.erase(map.begin(), map.end()) == map.end());

		CHECK_EQ(map["new value"], int{});
	}

	TEST_CASE("precalculated hash") {
		dice::sparse_map::sparse_map<int, int, identity_hash<int>> map = {
				{1, -1},
				{2, -2},
				{3, -3},
				{4, -4},
				{5, -5},
				{6, -6}};
		const dice::sparse_map::sparse_map<int, int, identity_hash<int>> map_const = map;

		/**
		 * find
		 */
		REQUIRE(map.find(3, map.hash_function()(3)) != map.end());
		CHECK_EQ(map.find(3, map.hash_function()(3))->second, -3);

		REQUIRE(map_const.find(3, map_const.hash_function()(3)) !=
				map_const.end());
		CHECK_EQ(map_const.find(3, map_const.hash_function()(3))->second,
				 -3);

		/**
   	 	 * at
   	 	 */
		CHECK_EQ(map.at(3, map.hash_function()(3)), -3);
		CHECK_EQ(map_const.at(3, map_const.hash_function()(3)), -3);

		/**
	   	 * contains
	   	 */
		CHECK(map.contains(3, map.hash_function()(3)));
		CHECK(map_const.contains(3, map_const.hash_function()(3)));

		/**
	     * count
	     */
		CHECK_EQ(map.count(3, map.hash_function()(3)), 1);
		CHECK_EQ(map_const.count(3, map_const.hash_function()(3)), 1);

		/**
	     * equal_range
	     */
		auto it_range = map.equal_range(3, map.hash_function()(3));
		REQUIRE_EQ(std::distance(it_range.first, it_range.second), 1);
		CHECK_EQ(it_range.first->second, -3);

		auto it_range_const = map_const.equal_range(3, map_const.hash_function()(3));
		REQUIRE_EQ(
				std::distance(it_range_const.first, it_range_const.second), 1);
		CHECK_EQ(it_range_const.first->second, -3);

		/**
		 * erase
		 */
		CHECK_EQ(map.erase(3, map.hash_function()(3)), 1);
	}

	TEST_CASE("insert iterate then remove 10M ints") {
		dice::sparse_map::sparse_map<int, int> m;
		std::default_random_engine rng{std::random_device{}()};

		for (size_t ix = 0; ix < 10'000'000; ++ix) {
			(void) m[static_cast<int>(rng())];
		}

		std::cout << "map size: " << m.size() << std::endl;

		size_t sum = 0;
		for (auto [x, _] : m) {
			sum += x;
		}
		std::cout << sum << std::endl;

		for (auto it = m.begin(); it != m.end(); ) {
			it = m.erase(it);
		}
	}
}
