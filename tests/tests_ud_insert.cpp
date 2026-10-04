// Ported from the unit tests ctors, insert, try_emplace, insert_or_assign, initializer_list and range_insert of ankerl::unordered_dense (MIT license).
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <list>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace dice::unordered_sparse::tests;
using namespace std::literals;

namespace {

    /**
     * A minimal input iterator over pairs of `counter::obj`. Both members of the pair count up from the start value.
     * sparse_map reads `std::iterator_traits<InputIt>::iterator_category`, so the iterator declares the member types
     * of a legacy input iterator.
     */
    struct counting_input_iterator {
        using iterator_category = std::input_iterator_tag;
        using value_type = std::pair<counter::obj, counter::obj>;
        using difference_type = std::ptrdiff_t;
        using pointer = value_type const *;
        using reference = value_type const &;

        counting_input_iterator(std::size_t val, counter &counts)
            : kv_({val, counts}, {val, counts}) {
        }

        counting_input_iterator &operator++() {
            ++kv_.first.get();
            ++kv_.second.get();
            return *this;
        }

        reference operator*() const {
            return kv_;
        }

        friend bool operator==(counting_input_iterator const &lhs, counting_input_iterator const &rhs) {
            return lhs.kv_.first.get() == rhs.kv_.first.get() && lhs.kv_.second.get() == rhs.kv_.second.get();
        }

    private:
        std::pair<counter::obj, counter::obj> kv_;
    };

}  // namespace

// ctors

TEST_CASE_MAP("ctors_map", counter::obj, counter::obj) {
    using alloc_t = typename map_t::allocator_type;
    using hash_t = typename map_t::hasher;
    using key_eq_t = typename map_t::key_equal;

    auto counts = counter();
    INFO(counts);

    {
        auto m = map_t{};
    }
    {
        auto m = map_t{0, alloc_t{}};
    }
    {
        auto m = map_t{0, hash_t{}, alloc_t{}};
    }
    {
        auto m = map_t{alloc_t{}};
    }
    REQUIRE(counts.dtor() == 0);

    {
        auto begin_it = counting_input_iterator{std::size_t{0}, counts};
        auto end_it = counting_input_iterator{std::size_t{10}, counts};
        auto m = map_t{begin_it, end_it};
        REQUIRE(m.size() == 10);
    }
    {
        auto begin_it = counting_input_iterator{std::size_t{0}, counts};
        auto end_it = counting_input_iterator{std::size_t{10}, counts};
        auto m = map_t{begin_it, end_it, 0, alloc_t{}};
        REQUIRE(m.size() == 10);
    }
    {
        auto begin_it = counting_input_iterator{std::size_t{0}, counts};
        auto end_it = counting_input_iterator{std::size_t{10}, counts};
        auto m = map_t{begin_it, end_it, 0, hash_t{}, alloc_t{}};
        REQUIRE(m.size() == 10);
    }
    {
        auto begin_it = counting_input_iterator{std::size_t{0}, counts};
        auto end_it = counting_input_iterator{std::size_t{10}, counts};
        auto m = map_t{begin_it, end_it, 0, hash_t{}, key_eq_t{}};
        REQUIRE(m.size() == 10);
    }
}

// A requested bucket count is rounded up to the next power of two.
TEST_CASE_MAP("ctor_bucket_count_map", counter::obj, counter::obj) {
    auto m = map_t{150U};
    REQUIRE(m.bucket_count() == 256U);
}

TEST_CASE_SET("ctor_bucket_count_set", int) {
    auto m = set_t{{1, 2, 3, 4}, 300U};
    REQUIRE(m.size() == 4U);
    REQUIRE(m.bucket_count() == 512U);
}

// insert

TEST_CASE_MAP("insert", unsigned int, int) {
    auto map = map_t();
    auto const val = typename map_t::value_type(123U, 321);
    map.insert(val);
    REQUIRE(map.size() == 1);

    REQUIRE(map[123U] == 321);
}

// sparse_map reads the hint (an element with the same key is returned without an insert), and an insert invalidates
// all iterators. So every call gets a hint that is valid at that point.
TEST_CASE_MAP("insert_hint", unsigned int, int) {
    auto map = map_t();
    auto it = map.insert(map.begin(), {1, 2});
    REQUIRE(it == map.find(1));

    auto vt = typename map_t::value_type{3, 4};
    map.insert(map.find(1), vt);
    REQUIRE(map.size() == 2);
    REQUIRE(map[1] == 2);
    REQUIRE(map[3] == 4);

    auto const vt2 = typename map_t::value_type{10, 11};
    map.insert(map.end(), vt2);
    REQUIRE(map.size() == 3);
    REQUIRE(map[10] == 11);

    it = map.emplace_hint(map.find(3), std::piecewise_construct, std::forward_as_tuple(123), std::forward_as_tuple(321));
    REQUIRE(map.size() == 4);
    REQUIRE(it == map.find(123));
    REQUIRE(map[123] == 321);
}

// insert() looks the key up before it constructs anything, so a key that is present costs no copy, and a value handed
// over as an rvalue is not moved from.
TEST_CASE_MAP("insert_present_key_leaves_the_argument", std::string, std::string) {
    auto map = map_t();
    map.try_emplace("key", "first");
    auto vt = typename map_t::value_type("key", std::string(100, 'x'));
    auto const r = map.insert(std::move(vt));
    REQUIRE_FALSE(r.second);
    REQUIRE(r.first->second == "first");
    REQUIRE(map.size() == 1);
    REQUIRE(vt.second == std::string(100, 'x'));  // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
}

TEST_CASE_SET("insert_present_key_leaves_the_argument_set", std::string) {
    auto set = set_t();
    set.insert(std::string(100, 'x'));
    auto key = std::string(100, 'x');
    REQUIRE_FALSE(set.insert(std::move(key)).second);
    REQUIRE(set.size() == 1);
    REQUIRE(key == std::string(100, 'x'));  // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
}

TEST_CASE_MAP("insert_present_key_copies_nothing", counter::obj, counter::obj) {
    auto counts = counter();
    INFO(counts);
    auto map = map_t();
    auto const vt = typename map_t::value_type({1, counts}, {2, counts});
    map.insert(vt);
    auto const before = counts.data();
    REQUIRE_FALSE(map.insert(vt).second);
    REQUIRE(counts.copy_ctor() == before.copy_ctor);
    REQUIRE(counts.dtor() == before.dtor);
}

// `emplace` of a key and a mapped value of the exact types looks up the key first and leaves the arguments as they are
// on a present key (`tests_emplace_lookup.cpp` counts that). This checks that the element that is already there stays.
TEST_CASE_MAP("emplace_key_and_mapped_present_key_keeps_the_element", std::string, std::string) {
    auto map = map_t();
    map.try_emplace("key", "first");
    auto key = std::string("key");
    auto mapped = std::string(100, 'x');
    REQUIRE_FALSE(map.emplace(key, std::move(mapped)).second);
    REQUIRE(map.size() == 1);
    REQUIRE(map["key"] == "first");
}

namespace {

    /**
     * A mapped value built from something else, which counts how often that happens.
     */
    struct converted {
        inline static int constructed = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
        int value;

        explicit converted(int v)
            : value(v) {
            ++constructed;
        }
    };

}  // namespace

// Arguments the value is converted from are consumed on a present key: the value is built and destroyed. So
// `emplace(k, new int)` cannot leak its pointer.
TEST_CASE_MAP("emplace_converting_arguments_are_consumed_on_a_present_key", int, converted) {
    auto map = map_t();
    map.emplace(1, 10);
    auto const before = converted::constructed;
    REQUIRE_FALSE(map.emplace(1, 20).second);
    REQUIRE(converted::constructed == before + 1);
    REQUIRE(map.find(1)->second.value == 10);
}

// emplace() without arguments builds a default value and inserts that, once.
TEST_CASE_MAP("emplace_without_arguments", int, int) {
    auto map = map_t();
    REQUIRE(map.emplace().second);
    REQUIRE_FALSE(map.emplace().second);
    REQUIRE(map.size() == 1);
    REQUIRE(map.find(0)->second == 0);
}

TEST_CASE_SET("emplace_without_arguments_set", int) {
    auto set = set_t();
    REQUIRE(set.emplace().second);
    REQUIRE_FALSE(set.emplace().second);
    REQUIRE(set.size() == 1);
}

// try_emplace

namespace {

    struct regular_type {
        regular_type(std::size_t i, std::string s) noexcept
            : s_(std::move(s)),
              i_(i) {
        }

        friend bool operator==(regular_type const &r1, regular_type const &r2) {
            return r1.i_ == r2.i_ && r1.s_ == r2.s_;
        }

    private:
        std::string s_;
        std::size_t i_;
    };

}  // namespace

TEST_CASE_MAP("try_emplace", std::string, regular_type) {
    map_t map;
    auto ret = map.try_emplace("a", 1U, "b");
    REQUIRE(ret.second);
    REQUIRE(ret.first == map.find("a"));
    REQUIRE(ret.first->second == regular_type(1U, "b"));
    REQUIRE(map.size() == 1);

    ret = map.try_emplace("c", 2U, "d");
    REQUIRE(ret.second);
    REQUIRE(ret.first == map.find("c"));
    REQUIRE(ret.first->second == regular_type(2U, "d"));
    REQUIRE(map.size() == 2U);

    std::string key = "c";
    ret = map.try_emplace(key, 3U, "dd");
    REQUIRE_FALSE(ret.second);
    REQUIRE(ret.first == map.find("c"));
    REQUIRE(ret.first->second == regular_type(2U, "d"));
    REQUIRE(map.size() == 2U);

    key = "a";
    regular_type value(3U, "dd");
    ret = map.try_emplace(key, value);
    REQUIRE_FALSE(ret.second);
    REQUIRE(ret.first == map.find("a"));
    REQUIRE(ret.first->second == regular_type(1U, "b"));
    REQUIRE(map.size() == 2U);

    auto pos = map.try_emplace(map.end(), "e", 67U, "f");
    REQUIRE(pos == map.find("e"));
    REQUIRE(pos->second == regular_type(67U, "f"));
    REQUIRE(map.size() == 3U);

    key = "e";
    regular_type value2(66U, "ff");
    pos = map.try_emplace(map.begin(), key, value2);
    REQUIRE(pos == map.find("e"));
    REQUIRE(pos->second == regular_type(67U, "f"));
    REQUIRE(map.size() == 3U);
}

// insert_or_assign

TEST_CASE_MAP("insert_or_assign", std::string, std::string) {
    auto map = map_t();

    auto ret = map.insert_or_assign("a", "b");
    REQUIRE(ret.second);
    REQUIRE(ret.first == map.find("a"));
    REQUIRE(map.size() == 1);
    REQUIRE(map["a"] == "b");

    ret = map.insert_or_assign("c", "d");
    REQUIRE(ret.second);
    REQUIRE(ret.first == map.find("c"));
    REQUIRE(map.size() == 2);
    REQUIRE(map["c"] == "d");

    ret = map.insert_or_assign("c", "dd");
    REQUIRE_FALSE(ret.second);
    REQUIRE(ret.first == map.find("c"));
    REQUIRE(map.size() == 2);
    REQUIRE(map["c"] == "dd");

    std::string key = "a";
    std::string value = "bb";
    ret = map.insert_or_assign(key, value);
    REQUIRE_FALSE(ret.second);
    REQUIRE(ret.first == map.find("a"));
    REQUIRE(map.size() == 2);
    REQUIRE(map["a"] == "bb");

    key = "e";
    value = "f";
    auto pos = map.insert_or_assign(map.end(), key, value);
    REQUIRE(pos == map.find("e"));
    REQUIRE(map.size() == 3);
    REQUIRE(map["e"] == "f");

    pos = map.insert_or_assign(map.begin(), "e", "ff");
    REQUIRE(pos == map.find("e"));
    REQUIRE(map.size() == 3);
    REQUIRE(map["e"] == "ff");
}

// initializer_list

namespace {

    template<typename Map, typename First, typename Second>
    bool has(Map const &map, First const &first, Second const &second) {
        auto it = map.find(first);
        return it != map.end() && it->second == second;
    }

}  // namespace

TEST_CASE_MAP("insert_initializer_list", int, int) {
    auto m = map_t();
    m.insert({{1, 2}, {3, 4}, {5, 6}});
    REQUIRE(m.size() == 3U);
    REQUIRE(m[1] == 2);
    REQUIRE(m[3] == 4);
    REQUIRE(m[5] == 6);
}

TEST_CASE_MAP("initializerlist_string", std::string, std::size_t) {
    std::size_t const n1 = 17;
    std::size_t const n2 = 10;

    auto m1 = map_t{{"duck", n1}, {"lion", n2}};
    auto m2 = map_t{{"duck", n1}, {"lion", n2}};

    REQUIRE(m1.size() == 2);
    REQUIRE(m1["duck"] == n1);
    REQUIRE(m1["lion"] == n2);

    REQUIRE(m2.size() == 2);
    auto it = m2.find("duck");
    REQUIRE((it != m2.end() && it->second == n1));
    REQUIRE(m2["lion"] == n2);
}

TEST_CASE_MAP("insert_initializer_list_string", int, std::string) {
    auto m = map_t();
    m.insert({{1, "a"}, {3, "b"}, {5, "c"}});
    REQUIRE(m.size() == 3U);
    REQUIRE(m[1] == "a");
    REQUIRE(m[3] == "b");
    REQUIRE(m[5] == "c");
}

TEST_CASE_MAP("initializer_list_assign", int, char const *) {
    auto map = map_t();
    map[3] = "nope";
    map = {{1, "a"}, {2, "hello"}, {12346, "world!"}};
    REQUIRE(map.size() == 3);
    REQUIRE(has(map, 1, "a"sv));
    REQUIRE(has(map, 2, "hello"sv));
    REQUIRE(has(map, 12346, "world!"sv));
}

TEST_CASE_MAP("initializer_list_ctor_alloc", int, char const *) {
    using alloc_t = typename map_t::allocator_type;
    auto map = map_t({{1, "a"}, {2, "hello"}, {12346, "world!"}}, 0, alloc_t{});
    REQUIRE(map.size() == 3);
    REQUIRE(has(map, 1, "a"sv));
    REQUIRE(has(map, 2, "hello"sv));
    REQUIRE(has(map, 12346, "world!"sv));
}

TEST_CASE_MAP("initializer_list_ctor_hash_alloc", int, char const *) {
    using hash_t = typename map_t::hasher;
    using alloc_t = typename map_t::allocator_type;
    auto map = map_t({{1, "a"}, {2, "hello"}, {12346, "world!"}}, 0, hash_t{}, alloc_t{});
    REQUIRE(map.size() == 3);
    REQUIRE(has(map, 1, "a"sv));
    REQUIRE(has(map, 2, "hello"sv));
    REQUIRE(has(map, 12346, "world!"sv));
}

// range_insert
//
// insert(first, last) with a forward range reserves room for the whole range first, and then inserts the elements one
// by one. These tests check that it cannot be told apart from a loop of single inserts: same elements, and the first
// of each duplicate key wins.

namespace {

    using u64_map = dice::sparse_map<std::uint64_t, std::uint64_t>;
    using int_map = dice::sparse_map<int, int>;

    std::uint64_t range_insert_key(std::uint64_t i) {
        return i * UINT64_C(0x9E3779B97F4A7C15);
    }

    using range_insert_input = std::vector<std::pair<std::uint64_t, std::uint64_t>>;

    /**
     * `len` elements drawn from `distinct` keys, at random rather than in order.
     */
    range_insert_input range_insert_input_of(std::size_t len, std::size_t distinct) {
        auto input = range_insert_input();
        auto s = std::uint64_t{12345};
        for (std::size_t i = 0; i < len; ++i) {
            s ^= s << 13U;
            s ^= s >> 7U;
            s ^= s << 17U;
            input.emplace_back(range_insert_key(s % distinct), static_cast<std::uint64_t>(i));
        }
        return input;
    }

}  // namespace

TEST_CASE("range_insert_matches_a_loop_of_single_inserts") {
    auto input = range_insert_input();
    for (std::uint64_t i = 0; i < 5000; ++i) {
        input.emplace_back(range_insert_key(i), i);
    }

    auto lengths = std::vector<std::size_t>();
    for (std::size_t len = 0; len <= 40; ++len) {
        lengths.push_back(len);
    }
    lengths.push_back(input.size());
    for (auto const len : lengths) {
        auto ranged = u64_map();
        auto looped = u64_map();
        ranged.insert(input.begin(), input.begin() + static_cast<std::ptrdiff_t>(len));
        for (std::size_t i = 0; i < len; ++i) {
            looped.insert(input[i]);
        }
        REQUIRE(ranged.size() == looped.size());
        for (auto const &[k, v] : looped) {
            REQUIRE(ranged.at(k) == v);
        }
    }
}

// insert() keeps the element already there, so a duplicate later in the range must not overwrite an earlier one.
TEST_CASE("range_insert_keeps_the_first_of_each_duplicate") {
    auto input = std::vector<std::pair<int, int>>();
    for (int round = 0; round < 40; ++round) {
        for (int i = 0; i < 20; ++i) {
            input.emplace_back(i, round);
        }
    }
    auto map = int_map();
    map.insert(input.begin(), input.end());
    REQUIRE(map.size() == 20U);
    for (int i = 0; i < 20; ++i) {
        REQUIRE(map.at(i) == 0);  // the value of the first round, not of the last
    }
}

// Into a map that already holds elements, and into one that has no bucket array yet.
TEST_CASE("range_insert_into_a_populated_and_an_empty_map") {
    auto input = std::vector<std::pair<int, int>>();
    for (int i = 0; i < 1000; ++i) {
        input.emplace_back(i, i * 2);
    }
    auto fresh = int_map();
    REQUIRE(fresh.bucket_count() == 0U);
    fresh.insert(input.begin(), input.end());
    REQUIRE(fresh.size() == 1000U);

    auto populated = int_map();
    for (int i = 500; i < 1500; ++i) {
        populated[i] = -1;
    }
    populated.insert(input.begin(), input.end());
    REQUIRE(populated.size() == 1500U);
    REQUIRE(populated.at(0) == 0);      // new, from the range
    REQUIRE(populated.at(999) == -1);   // already there, so the range did not overwrite it
    REQUIRE(populated.at(1499) == -1);  // untouched
}

// A single pass iterator cannot be walked twice to measure the range, so it takes the plain loop.
TEST_CASE("range_insert_from_an_input_iterator") {
    auto text = std::string("7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25");
    auto in = std::istringstream(text);
    auto set = dice::sparse_set<int>();
    set.insert(std::istream_iterator<int>(in), std::istream_iterator<int>());
    REQUIRE(set.size() == 19U);
    REQUIRE(set.count(7) == 1U);
    REQUIRE(set.count(25) == 1U);
    REQUIRE(set.count(26) == 0U);
}

// A forward range that is not contiguous.
TEST_CASE("range_insert_from_a_list") {
    auto values = std::list<std::pair<int, int>>();
    for (int i = 0; i < 500; ++i) {
        values.emplace_back(i, i);
    }
    auto map = int_map();
    map.insert(values.begin(), values.end());
    REQUIRE(map.size() == 500U);
    REQUIRE(map.at(499) == 499);
}

// Against a reference, over a range big enough to grow the table several times.
TEST_CASE("range_insert_across_several_growths") {
    auto input = range_insert_input();
    auto ref = std::unordered_map<std::uint64_t, std::uint64_t>();
    for (std::uint64_t i = 0; i < 40000; ++i) {
        auto const k = range_insert_key(i);
        input.emplace_back(k, i);
        ref.emplace(k, i);
    }
    auto map = u64_map();
    map.insert(input.begin(), input.end());
    REQUIRE(map.size() == ref.size());
    for (auto const &[k, v] : ref) {
        REQUIRE(map.at(k) == v);
    }
    for (std::uint64_t i = 0; i < 1000; ++i) {
        REQUIRE(map.count(range_insert_key(i) + 1) == ref.count(range_insert_key(i) + 1));
    }
}

TEST_CASE("range_insert_initializer_list_and_from_a_copy") {
    auto map = int_map{{1, 10}, {2, 20}, {3, 30}};
    REQUIRE(map.size() == 3U);
    REQUIRE(map.at(2) == 20);

    // inserting a copy of the elements of a map into the map changes nothing
    auto copy = map;
    map.insert(copy.begin(), copy.end());
    REQUIRE(map.size() == 3U);
    REQUIRE(map.at(3) == 30);
}

// sparse_map sizes the table for the whole length of a forward range, duplicates included. So the range insert never
// ends with fewer buckets than the loop, and it may end with more.
TEST_CASE("range_insert_matches_a_loop_at_many_lengths_and_duplicate_rates") {
    for (auto const len : {std::size_t{1},
                           std::size_t{2},
                           std::size_t{3},
                           std::size_t{4095},
                           std::size_t{4096},
                           std::size_t{4097},
                           std::size_t{8191},
                           std::size_t{8192},
                           std::size_t{8193},
                           std::size_t{12289}}) {
        for (auto const distinct : {std::size_t{7}, std::size_t{1000}}) {
            auto const input = range_insert_input_of(len, distinct);

            auto ranged = u64_map();
            ranged.insert(input.begin(), input.end());
            auto looped = u64_map();
            for (auto const &kv : input) {
                looped.insert(kv);
            }

            INFO("len=" << len << " distinct=" << distinct);
            REQUIRE(ranged.size() == looped.size());
            REQUIRE(ranged.bucket_count() >= looped.bucket_count());
            for (auto const &[k, v] : looped) {
                REQUIRE(ranged.at(k) == v);
            }
        }
    }
}

TEST_CASE("range_insert_into_a_populated_map") {
    auto map = u64_map();
    for (std::uint64_t i = 0; i < 10000; ++i) {
        map.insert({range_insert_key(i), i});
    }
    auto input = range_insert_input();
    for (std::uint64_t i = 0; i < 10000; ++i) {
        // half of them already present, half of them new
        input.emplace_back(range_insert_key(i % 2 == 0 ? i : i + 100000), i);
    }
    map.insert(input.begin(), input.end());
    REQUIRE(map.size() == 15000U);
    for (std::uint64_t i = 0; i < 10000; ++i) {
        REQUIRE(map.contains(range_insert_key(i)));
    }
    for (std::uint64_t i = 1; i < 10000; i += 2) {
        REQUIRE(map.contains(range_insert_key(i + 100000)));
    }
}
