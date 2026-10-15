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
#include <dice/sparse-map/sparse_set.hpp>

#include "fixtures/utils.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>

using namespace dice::sparse_map::tests;

TEST_SUITE("test_sparse_set") {

    using test_types = std::tuple<dice::sparse_map::sparse_set<std::int64_t>,
                                  dice::sparse_map::sparse_set<std::string>,
                                  dice::sparse_map::sparse_set<self_reference_member_test>,
                                  dice::sparse_map::sparse_set<move_only_test>,
                                  dice::sparse_map::sparse_pg_set<self_reference_member_test>,
                                  dice::sparse_map::sparse_set<move_only_test, std::hash<move_only_test>, std::equal_to<move_only_test>, std::allocator<move_only_test>, dice::sparse_map::sh::prime_growth_policy>,
                                  dice::sparse_map::sparse_set<self_reference_member_test,
                                                               std::hash<self_reference_member_test>,
                                                               std::equal_to<self_reference_member_test>,
                                                               std::allocator<self_reference_member_test>,
                                                               dice::sparse_map::sh::mod_growth_policy<>>,
                                  dice::sparse_map::sparse_set<move_only_test, std::hash<move_only_test>, std::equal_to<move_only_test>, std::allocator<move_only_test>, dice::sparse_map::sh::mod_growth_policy<>>>;

    TEST_CASE_TEMPLATE_DEFINE("test_standard_layout", HSet, test_standard_layout_id) {
        static_assert(std::is_standard_layout_v<HSet>);
        CHECK(std::is_standard_layout_v<HSet>);
    }
    TEST_CASE_TEMPLATE_APPLY(test_standard_layout_id, test_types);

    TEST_CASE_TEMPLATE_DEFINE("test_insert", HSet, test_insert_id) {
        // insert x values, insert them again, check values
        using key_t = typename HSet::key_type;

        std::size_t const nb_values = 1000;
        HSet set;
        typename HSet::iterator it;
        bool inserted;

        for (std::size_t i = 0; i < nb_values; i++) {
            std::tie(it, inserted) = set.insert(utils::get_key<key_t>(i));

            CHECK_EQ(*it, utils::get_key<key_t>(i));
            CHECK(inserted);
        }
        CHECK_EQ(set.size(), nb_values);

        for (std::size_t i = 0; i < nb_values; i++) {
            std::tie(it, inserted) = set.insert(utils::get_key<key_t>(i));

            CHECK_EQ(*it, utils::get_key<key_t>(i));
            CHECK(!inserted);
        }

        for (std::size_t i = 0; i < nb_values; i++) {
            it = set.find(utils::get_key<key_t>(i));

            CHECK_EQ(*it, utils::get_key<key_t>(i));
        }
    }
    TEST_CASE_TEMPLATE_APPLY(test_insert_id, test_types);

    TEST_CASE("test_compare") {
        dice::sparse_map::sparse_set<std::string> const set1 = {"a", "e", "d", "c", "b"};
        dice::sparse_map::sparse_set<std::string> const set1_copy = {"e", "c", "b", "a", "d"};
        dice::sparse_map::sparse_set<std::string> const set2 = {"e", "c", "b", "a", "d", "f"};
        dice::sparse_map::sparse_set<std::string> const set3 = {"e", "c", "b", "a"};
        dice::sparse_map::sparse_set<std::string> const set4 = {"a", "e", "d", "c", "z"};

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

    TEST_CASE("test_insert_pointer") {
        // Test added mainly to be sure that the code compiles with MSVC
        std::string value;
        std::string *value_ptr = &value;

        dice::sparse_map::sparse_set<std::string *> set;
        set.insert(value_ptr);
        set.emplace(value_ptr);

        CHECK_EQ(set.size(), 1);
        CHECK_EQ(**set.begin(), value);
    }

    /**
     * serialize and deserialize
     */
    TEST_CASE("test_serialize_deserialize_reserve") {
        // insert x values values without intermediate resizes; serialize set;
        // deserialize in new set; check equal. for deserialization,
        // test it with and without hash compatibility.
        for (std::size_t nb_values : {0, 1, 3, 17, 1000}) {
            dice::sparse_map::sparse_set<move_only_test> set;
            set.reserve(nb_values);
            for (std::size_t i = 0; i < nb_values; i++) {
                set.insert(utils::get_key<move_only_test>(i));
            }

            serializer serial;
            set.serialize(serial);

            deserializer dserial(serial.str());
            auto set_deserialized = decltype(set)::deserialize(dserial, true);
            CHECK(set == set_deserialized);

            deserializer dserial2(serial.str());
            set_deserialized = decltype(set)::deserialize(dserial2, false);
            CHECK(set_deserialized == set);
        }
    }

    TEST_CASE("test_serialize_deserialize") {
        // insert x values; delete some values; serialize set; deserialize in new
        // set; check equal. for deserialization, test it with and without hash
        // compatibility.
        for (std::size_t nb_values : {0, 1, 3, 17, 1000}) {
            dice::sparse_map::sparse_set<move_only_test> set;
            for (std::size_t i = 0; i < nb_values + 40; i++) {
                set.insert(utils::get_key<move_only_test>(i));
            }

            for (std::size_t i = nb_values; i < nb_values + 40; i++) {
                set.erase(utils::get_key<move_only_test>(i));
            }
            CHECK_EQ(set.size(), nb_values);

            serializer serial;
            set.serialize(serial);

            deserializer dserial(serial.str());
            auto set_deserialized = decltype(set)::deserialize(dserial, true);
            CHECK(set == set_deserialized);

            deserializer dserial2(serial.str());
            set_deserialized = decltype(set)::deserialize(dserial2, false);
            CHECK(set_deserialized == set);
        }
    }
}
