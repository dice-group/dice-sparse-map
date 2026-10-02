// Ported from the unit test transparent of ankerl::unordered_dense (MIT license).
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <array>
#include <cstddef>
#include <functional>
#include <map>
#include <stdexcept>
#include <string>
#include <string_view>
#include <typeinfo>
#include <utility>

using namespace dice::unordered_sparse::tests;
using namespace std::literals;

// sparse_map has heterogeneous overloads of find, count, contains, equal_range, at, erase, try_emplace,
// insert_or_assign and operator[], and sparse_set of insert. They take part in overload resolution when the hash function
// and the key equality both have a member type `is_transparent`. emplace takes the arguments of the element, so it
// converts its argument to a `std::string` first.
//
// Every container here starts with 16 buckets, so no insert grows the table. An insert that grows the table hashes
// its key once more after the rehash, and that would blur the hash counts.

namespace {

    /**
     * Transparent key equality that counts its calls per pair of argument types.
     */
    struct string_eq {
        using is_transparent = void;

        template<typename T, typename U>
        bool operator()(T const &lhs, U const &rhs) const {
            ++names_to_counts_[std::string(typeid(T).name()) + " -> " + typeid(U).name()];
            return lhs == rhs;
        }

        [[nodiscard]] std::map<std::string, std::size_t> const &counts() const noexcept {
            return names_to_counts_;
        }

    private:
        mutable std::map<std::string, std::size_t> names_to_counts_;
    };

    /**
     * Transparent hash that counts its calls per argument type.
     */
    struct string_hash {
        using is_transparent = void;

        [[nodiscard]] std::size_t operator()(char const *str) const noexcept {
            ++num_charstar_;
            return std::hash<std::string_view>{}(str);
        }

        [[nodiscard]] std::size_t operator()(std::string_view str) const noexcept {
            ++num_stringview_;
            return std::hash<std::string_view>{}(str);
        }

        [[nodiscard]] std::size_t operator()(std::string const &str) const noexcept {
            ++num_string_;
            return std::hash<std::string_view>{}(str);
        }

        [[nodiscard]] std::array<std::size_t, 3> counts() const noexcept {
            return {num_charstar_, num_stringview_, num_string_};
        }

    private:
        mutable std::size_t num_charstar_{};
        mutable std::size_t num_stringview_{};
        mutable std::size_t num_string_{};
    };

    /**
     * Checks the number of hash calls with a `char const *`, a `std::string_view` and a `std::string`.
     */
    template<typename C>
    void check(int line, C const &container, std::size_t num_charstar, std::size_t num_stringview, std::size_t num_string) {
        auto sh = container.hash_function();
        auto counts = sh.counts();
        INFO("check line " << line << ": expect (" << num_charstar << ", " << num_stringview << ", " << num_string << ") got ("
                           << counts[0] << ", " << counts[1] << ", " << counts[2]
                           << ") for (num_charstar, num_stringview, num_string)");
        REQUIRE(counts[0] == num_charstar);
        REQUIRE(counts[1] == num_stringview);
        REQUIRE(counts[2] == num_string);
    }

}  // namespace

TEST_CASE_MAP("transparent_find", std::string, std::size_t, string_hash, std::equal_to<>) {
    auto map = map_t(16);
    map.try_emplace("hello", 1);
    check(__LINE__, map, 1, 0, 0);

    auto it = map.find("huh");
    check(__LINE__, map, 2, 0, 0);
    REQUIRE(it == map.end());
    it = map.find("hello");
    check(__LINE__, map, 3, 0, 0);
    REQUIRE(it != map.end());

    auto cit = std::as_const(map).find("huh");
    check(__LINE__, map, 4, 0, 0);
    REQUIRE(cit == map.end());
    REQUIRE(cit == map.cend());
    cit = std::as_const(map).find("hello");
    check(__LINE__, map, 5, 0, 0);
    REQUIRE(cit != map.end());

    // string_view
    it = map.find("huh"sv);
    REQUIRE(it == map.end());
    check(__LINE__, map, 5, 1, 0);
    it = map.find("hello"sv);
    REQUIRE(it != map.end());
    check(__LINE__, map, 5, 2, 0);

    // string
    it = map.find("huh"s);
    REQUIRE(it == map.end());
    check(__LINE__, map, 5, 2, 1);
    it = map.find("hello"s);
    REQUIRE(it != map.end());
    check(__LINE__, map, 5, 2, 2);
}

TEST_CASE_MAP("transparent_count", std::string, std::size_t, string_hash, std::equal_to<>) {
    auto map = map_t(16);
    map.try_emplace("hello", 1);
    check(__LINE__, map, 1, 0, 0);

    REQUIRE(0 == map.count("huh"));
    check(__LINE__, map, 2, 0, 0);
    REQUIRE(1 == map.count("hello"));
    check(__LINE__, map, 3, 0, 0);

    REQUIRE(0 == map.count("huh"sv));
    check(__LINE__, map, 3, 1, 0);
    REQUIRE(1 == map.count("hello"sv));
    check(__LINE__, map, 3, 2, 0);

    REQUIRE(0 == map.count("huh"s));
    check(__LINE__, map, 3, 2, 1);
    REQUIRE(1 == map.count("hello"s));
    check(__LINE__, map, 3, 2, 2);
}

TEST_CASE_MAP("transparent_contains", std::string, std::size_t, string_hash, std::equal_to<>) {
    auto map = map_t(16);
    map.try_emplace("hello", 1);
    check(__LINE__, map, 1, 0, 0);

    REQUIRE(!map.contains("huh"));
    check(__LINE__, map, 2, 0, 0);
    REQUIRE(map.contains("hello"));
    check(__LINE__, map, 3, 0, 0);

    REQUIRE(!map.contains("huh"sv));
    check(__LINE__, map, 3, 1, 0);
    REQUIRE(map.contains("hello"sv));
    check(__LINE__, map, 3, 2, 0);

    REQUIRE(!map.contains("huh"s));
    check(__LINE__, map, 3, 2, 1);
    REQUIRE(map.contains("hello"s));
    check(__LINE__, map, 3, 2, 2);
}

TEST_CASE_MAP("transparent_erase", std::string, std::size_t, string_hash, std::equal_to<>) {
    auto map = map_t(16);
    map.try_emplace("hello", 1);
    check(__LINE__, map, 1, 0, 0);
    REQUIRE(0 == map.erase("huh"));
    check(__LINE__, map, 2, 0, 0);
    REQUIRE(1 == map.erase("hello"));
    check(__LINE__, map, 3, 0, 0);

    map.try_emplace("hello", 1);
    check(__LINE__, map, 4, 0, 0);
    REQUIRE(0 == map.erase("huh"sv));
    check(__LINE__, map, 4, 1, 0);
    REQUIRE(1 == map.erase("hello"sv));
    check(__LINE__, map, 4, 2, 0);

    map.try_emplace("hello", 1);
    check(__LINE__, map, 5, 2, 0);
    REQUIRE(0 == map.erase("huh"s));
    check(__LINE__, map, 5, 2, 1);
    REQUIRE(1 == map.erase("hello"s));
    check(__LINE__, map, 5, 2, 2);
}

TEST_CASE_MAP("transparent_equal_range", std::string, std::size_t, string_hash, std::equal_to<>) {
    auto map = map_t(16);
    map.try_emplace("hello", 1);
    check(__LINE__, map, 1, 0, 0);

    auto range = map.equal_range("hello");
    check(__LINE__, map, 2, 0, 0);
    REQUIRE(range.first != range.second);
    REQUIRE(range.first->first == "hello");
    REQUIRE(range.second == map.end());

    auto crange = std::as_const(map).equal_range("hello"sv);
    check(__LINE__, map, 2, 1, 0);
    REQUIRE(crange.first != range.second);
    REQUIRE(crange.first->first == "hello");
    REQUIRE(crange.second == map.end());
}

// The key equality is called once per lookup, with the type of the lookup argument on the left.
TEST_CASE_MAP("transparent_string_eq", std::string, std::size_t, string_hash, string_eq) {
    auto map = map_t(16);
    map.try_emplace("hello", 1);

    REQUIRE(map.count("hello"));
    REQUIRE(map.count("hello"sv));
    REQUIRE(map.count("hello"s));

    auto ke = map.key_eq();
    REQUIRE(ke.counts().size() == 3);
    for (auto const &kv : ke.counts()) {
        REQUIRE(kv.second == 1U);
    }
}

TEST_CASE_MAP("transparent_at", std::string, std::size_t, string_hash, string_eq) {
    auto map = map_t(16);
    map.try_emplace("asdf", 123);
    check(__LINE__, map, 1, 0, 0);

    auto &vt = map.at("asdf");
    check(__LINE__, map, 2, 0, 0);
    REQUIRE(vt == 123);

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(map.at("nope"sv), std::out_of_range);
    check(__LINE__, map, 2, 1, 0);
}

// Without `is_transparent` on the key equality, the lookup converts its argument to a `std::string`.
TEST_CASE_MAP("transparent_at_not", std::string, std::size_t, string_hash) {
    auto map = map_t(16);
    map.try_emplace("asdf", 123);
    check(__LINE__, map, 0, 0, 1);

    auto &vt = map.at("asdf");
    check(__LINE__, map, 0, 0, 2);
    REQUIRE(vt == 123);

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(map.at("nope"), std::out_of_range);
    check(__LINE__, map, 0, 0, 3);
}

// The const overloads of at().
TEST_CASE_MAP("transparent_at_on_a_const_table", std::string, std::size_t, string_hash, string_eq) {
    auto map = map_t(16);
    map.try_emplace("asdf", 123);
    check(__LINE__, map, 1, 0, 0);

    auto const &cmap = map;
    REQUIRE(cmap.at("asdf"sv) == 123);

    // and it went through the transparent overload rather than building a std::string on the way in
    check(__LINE__, map, 1, 1, 0);

    REQUIRE_THROWS_AS(static_cast<void>(cmap.at("nope"sv)), std::out_of_range);
    check(__LINE__, map, 1, 2, 0);
}

TEST_CASE_MAP("transparent_insert_or_assign_not", std::string, std::size_t, string_hash) {
    auto map = map_t(16);
    auto r = map.insert_or_assign("asdf", 123U);
    check(__LINE__, map, 0, 0, 1);
    REQUIRE(r.first->first == "asdf");
    REQUIRE(r.first->second == 123U);
    REQUIRE(r.second);

    r = map.insert_or_assign("asdf", 42U);
    check(__LINE__, map, 0, 0, 2);
    REQUIRE(r.first->first == "asdf");
    REQUIRE(r.first->second == 42U);
    REQUIRE(!r.second);
    REQUIRE(map.size() == 1U);
}

// sparse_map reads the hint, so it has to be a valid iterator of the map. A hint that points to the element with the
// key assigns to that element without a lookup, so it does not hash.
TEST_CASE_MAP("transparent_insert_or_assign_iterator_not", std::string, std::size_t, string_hash) {
    auto map = map_t(16);
    auto r = map.insert_or_assign(map.cend(), "asdf", 123U);
    check(__LINE__, map, 0, 0, 1);
    REQUIRE(r->first == "asdf");
    REQUIRE(r->second == 123U);

    r = map.insert_or_assign(map.cend(), "asdf", 42U);
    check(__LINE__, map, 0, 0, 2);
    REQUIRE(r->first == "asdf");
    REQUIRE(r->second == 42U);
    REQUIRE(map.size() == 1U);

    r = map.insert_or_assign(map.find("asdf"), "asdf", 7U);
    check(__LINE__, map, 0, 0, 3);  // the hash of the find()
    REQUIRE(r == map.find("asdf"));
    REQUIRE(r->second == 7U);
    REQUIRE(map.size() == 1U);
}

// With a transparent key equality, try_emplace with a hint takes the string_view as it is (C++26).
TEST_CASE_MAP("transparent_try_emplace_with_a_hint", std::string, std::size_t, string_hash, string_eq) {
    auto map = map_t(16);

    auto it = map.try_emplace(map.cend(), "abc"sv, std::size_t{1});
    check(__LINE__, map, 0, 1, 0);
    REQUIRE(it->first == "abc");
    REQUIRE(it->second == 1);
    REQUIRE(map.size() == 1);

    // a second call with the same key keeps the first value, unlike insert_or_assign
    auto again = map.try_emplace(map.cend(), "abc"sv, std::size_t{2});
    check(__LINE__, map, 0, 2, 0);
    REQUIRE(again->second == 1);
    REQUIRE(map.size() == 1);
}

// With a transparent key equality, insert_or_assign hashes the `char const *` and builds no `std::string` for the
// lookup (C++26).
TEST_CASE_MAP("transparent_insert_or_assign", std::string, std::size_t, string_hash, string_eq) {
    auto map = map_t(16);
    auto r = map.insert_or_assign("asdf", 123U);
    check(__LINE__, map, 1, 0, 0);
    REQUIRE(r.first->first == "asdf");
    REQUIRE(r.first->second == 123U);
    REQUIRE(r.second);

    r = map.insert_or_assign("asdf", 42U);
    check(__LINE__, map, 2, 0, 0);
    REQUIRE(r.first->first == "asdf");
    REQUIRE(r.first->second == 42U);
    REQUIRE(!r.second);
    REQUIRE(map.size() == 1U);
}

TEST_CASE_MAP("transparent_insert_or_assign_iterator", std::string, std::size_t, string_hash, string_eq) {
    auto map = map_t(16);
    auto r = map.insert_or_assign(map.cend(), "asdf", 123U);
    check(__LINE__, map, 1, 0, 0);
    REQUIRE(r->first == "asdf");
    REQUIRE(r->second == 123U);

    r = map.insert_or_assign(map.cend(), "asdf", 42U);
    check(__LINE__, map, 2, 0, 0);
    REQUIRE(r->first == "asdf");
    REQUIRE(r->second == 42U);
    REQUIRE(map.size() == 1U);
}

// With a transparent key equality, a set inserts a `char const *` without building a `std::string` for the
// lookup (C++26).
TEST_CASE_SET("transparent_set_insert", std::string, string_hash, string_eq) {
    auto set = set_t(16);
    set.insert("abcdefg");
    check(__LINE__, set, 1, 0, 0);
    set.insert("abcdefg");
    check(__LINE__, set, 2, 0, 0);
}

TEST_CASE_SET("transparent_set_insert_not", std::string, string_hash) {
    auto set = set_t(16);
    set.insert("abcdefg");
    check(__LINE__, set, 0, 0, 1);
    set.insert("abcdefg");
    check(__LINE__, set, 0, 0, 2);
}

TEST_CASE_SET("transparent_set_emplace_not", std::string, string_hash) {
    auto set = set_t(16);
    set.emplace("abcdefg");
    check(__LINE__, set, 0, 0, 1);
    set.emplace("abcdefg");
    check(__LINE__, set, 0, 0, 2);
}

namespace {

    struct string_hash_simple {
        using is_transparent = void;

        [[nodiscard]] std::size_t operator()(std::string_view str) const noexcept {
            return std::hash<std::string_view>{}(str);
        }
    };

}  // namespace

TEST_CASE_MAP("transparent_find_simple", std::string, std::size_t, string_hash_simple, std::equal_to<>) {
    auto map = map_t();
    map.try_emplace("hello", 1);
    auto it = map.find("huh");
    REQUIRE(it == map.end());
    it = map.find("hello");
    REQUIRE(it != map.end());

    auto cit = std::as_const(map).find("huh");
    REQUIRE(cit == map.end());
    REQUIRE(cit == map.cend());
    cit = std::as_const(map).find("hello");
    REQUIRE(cit != map.end());

    // string_view
    it = map.find("huh"sv);
    REQUIRE(it == map.end());
    it = map.find("hello"sv);
    REQUIRE(it != map.end());

    // string
    it = map.find("huh"s);
    REQUIRE(it == map.end());
    it = map.find("hello"s);
    REQUIRE(it != map.end());
}

namespace {

    /**
     * `string_hash` without `is_transparent`. It counts its calls in the same way.
     */
    struct opaque_string_hash {
        template<typename S>
        [[nodiscard]] std::size_t operator()(S const &str) const noexcept {
            return hash_(str);
        }

        [[nodiscard]] std::array<std::size_t, 3> counts() const noexcept {
            return hash_.counts();
        }

    private:
        string_hash hash_;
    };

}  // namespace

// The heterogeneous overloads need `is_transparent` on the hash function and on the key equality, as in the standard
// library. Here only the key equality has it, so the lookup converts its argument to a `std::string`.
TEST_CASE_MAP("transparent_find_hash_not", std::string, std::size_t, opaque_string_hash, string_eq) {
    auto map = map_t(16);
    map.try_emplace("asdf", 123);
    check(__LINE__, map, 0, 0, 1);

    REQUIRE(map.find("asdf") != map.end());
    check(__LINE__, map, 0, 0, 2);

    REQUIRE(map.at("asdf") == 123);
    check(__LINE__, map, 0, 0, 3);

    REQUIRE(map.erase("asdf") == 1);
    check(__LINE__, map, 0, 0, 4);
}

// `std::equal_to<>` is transparent, `std::hash<int>` is not. A `double` is converted to the key type `int` before the
// lookup, as in `std::unordered_map`, so 1.5 finds the element with the key 1. Without the conversion, 1.5 gets the hash
// of 1 and compares unequal to 1: a lookup misses, and an insertion adds a second element with the key 1. The
// containers reserve room first, so that no insertion rehashes.
TEST_CASE("transparent_key_equal_with_a_hash_that_is_not_converts_the_key") {
    double const key = 1.5;

    auto map = dice::sparse_map<int, int, std::hash<int>, std::equal_to<>>{};
    map.reserve(16);
    map[1] = 10;
    CHECK(map.find(key) != map.end());
    CHECK(map.contains(key));
    CHECK(map.count(key) == 1);
    CHECK(map.equal_range(key).first != map.end());
    map[key] = 20;
    CHECK(map.size() == 1);
    CHECK(map.at(1) == 20);
    CHECK(map.erase(key) == 1);
    CHECK(map.empty());

    auto set = dice::sparse_set<int, std::hash<int>, std::equal_to<>>{};
    set.reserve(16);
    set.insert(1);
    CHECK(set.contains(key));
    CHECK_FALSE(set.insert(key).second);
    CHECK(set.size() == 1);
}

// emplace() on a set with a hundred elements, with an argument of a type that is not the key type. sparse_set has no
// heterogeneous emplace: it builds the `std::string` and then looks it up. Nothing may be inserted for a key that is
// there, and the iterator has to be the element that was already there.
TEST_CASE_SET("transparent_set_emplace_returns_what_was_already_there", std::string, string_hash, string_eq) {
    auto set = set_t();
    for (int i = 0; i < 100; ++i) {
        set.emplace("key #" + std::to_string(i));
    }
    REQUIRE(set.size() == 100U);

    for (int i = 0; i < 100; ++i) {
        auto const key = "key #" + std::to_string(i);
        auto const r = set.emplace(std::string_view(key));
        REQUIRE_FALSE(r.second);
        REQUIRE(r.first != set.end());
        REQUIRE(*r.first == key);
        REQUIRE(set.size() == 100U);
    }

    // a key that is not there is inserted, and reported as inserted
    auto const fresh = set.emplace(std::string_view("brand new"));
    REQUIRE(fresh.second);
    REQUIRE(*fresh.first == "brand new");
    REQUIRE(set.size() == 101U);

    // emplacing it a second time finds it
    auto const again = set.emplace(std::string_view("brand new"));
    REQUIRE_FALSE(again.second);
    REQUIRE(again.first == fresh.first);
    REQUIRE(set.size() == 101U);
}
