// Ported from the unit test precomputed_hash of ankerl::unordered_dense (MIT license).
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

// Looking the same key up again and again hashes it every time, and for a long key the hashing is most of the cost of
// a lookup. sparse_map's lookups take a precalculated hash: the value of `hash_function()(key)`. The lookups with a
// precalculated hash use it instead of hashing the key.

using namespace dice::unordered_sparse::tests;
using namespace std::literals;

namespace {

    /**
     * Long enough that hashing it is real work.
     */
    std::string long_key(std::string const &name) {
        return name + std::string(200, '.');
    }

    /**
     * Number of calls of `hash_call_counter`. It is global, because the table stores the hasher by value and copies
     * it.
     */
    std::size_t num_hashed = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    struct hash_call_counter {
        [[nodiscard]] std::size_t operator()(std::string const &str) const noexcept {
            ++num_hashed;
            return std::hash<std::string>{}(str);
        }
    };

    using hash_counting_map = dice::sparse_map<std::string, int, hash_call_counter>;

    struct transparent_hash {
        using is_transparent = void;

        [[nodiscard]] std::size_t operator()(std::string_view sv) const noexcept {
            return std::hash<std::string_view>{}(sv);
        }
    };

    using transparent_map = dice::sparse_map<std::string, int, transparent_hash, std::equal_to<>>;

}  // namespace

TEST_CASE_MAP("precomputed_hash_answers_the_same_as_hashing_again", std::string, int) {
    auto map = map_t();
    for (int i = 0; i < 100; ++i) {
        map[long_key(std::to_string(i))] = i;
    }

    for (int i = 0; i < 120; ++i) {
        auto const key = long_key(std::to_string(i));
        auto const hash = map.hash_function()(key);

        REQUIRE((map.find(key, hash) == map.end()) == (map.find(key) == map.end()));
        REQUIRE(map.contains(key, hash) == map.contains(key));
        REQUIRE(map.count(key, hash) == map.count(key));
        REQUIRE(map.equal_range(key, hash) == map.equal_range(key));

        if (map.contains(key)) {
            REQUIRE(map.find(key, hash)->second == i);
            REQUIRE(map.at(key, hash) == i);
        }
    }
}

TEST_CASE_SET("precomputed_hash_answers_the_same_in_a_set", std::string) {
    auto set = set_t();
    for (int i = 0; i < 100; ++i) {
        set.insert(long_key(std::to_string(i)));
    }

    for (int i = 0; i < 120; ++i) {
        auto const key = long_key(std::to_string(i));
        auto const hash = set.hash_function()(key);

        REQUIRE((set.find(key, hash) == set.end()) == (set.find(key) == set.end()));
        REQUIRE(set.contains(key, hash) == set.contains(key));
        REQUIRE(set.count(key, hash) == set.count(key));
        REQUIRE(set.equal_range(key, hash) == set.equal_range(key));
    }
}

// The point of the whole feature: the hasher does not run again.
TEST_CASE("a_precomputed_lookup_does_not_hash") {
    auto map = hash_counting_map();
    for (int i = 0; i < 100; ++i) {
        map[long_key(std::to_string(i))] = i;
    }

    auto const present = long_key("42");
    auto const absent = long_key("nowhere");
    auto const present_hash = map.hash_function()(present);
    auto const absent_hash = map.hash_function()(absent);

    auto const before = num_hashed;
    for (int i = 0; i < 10; ++i) {
        REQUIRE(map.find(present, present_hash)->second == 42);
        REQUIRE(map.find(absent, absent_hash) == map.end());
        REQUIRE(map.contains(present, present_hash));
        REQUIRE(map.count(absent, absent_hash) == 0);
        REQUIRE(map.at(present, present_hash) == 42);
    }
    REQUIRE(num_hashed == before);

    // whereas the ordinary lookups do
    REQUIRE(map.find(present)->second == 42);
    REQUIRE(num_hashed == before + 1);
}

// A default constructed table has no buckets until the first insert, so a lookup that skips the hashing must still not
// probe an empty bucket array.
TEST_CASE_MAP("an_empty_table_answers_without_probing", std::string, int) {
    auto map = map_t();
    auto const key = long_key("anything");
    auto const hash = map.hash_function()(key);

    REQUIRE(map.bucket_count() == 0);
    REQUIRE(map.find(key, hash) == map.end());
    REQUIRE_FALSE(map.contains(key, hash));
    REQUIRE(map.count(key, hash) == 0);
    REQUIRE(map.equal_range(key, hash) == std::pair(map.end(), map.end()));

    // NOLINTNEXTLINE(llvm-else-after-return,readability-else-after-return)
    REQUIRE_THROWS_AS(static_cast<void>(map.at(key, hash)), std::out_of_range);

    // emptied by erasing rather than never filled: buckets exist, elements do not
    map[key] = 1;
    map.erase(key);
    REQUIRE(map.bucket_count() != 0);
    REQUIRE(map.find(key, hash) == map.end());
}

TEST_CASE_MAP("precomputed_lookups_work_on_a_const_table", std::string, int) {
    auto map = map_t();
    map[long_key("a")] = 1;

    auto const &cmap = std::as_const(map);
    auto const hash = cmap.hash_function()(long_key("a"));

    typename map_t::const_iterator const it = cmap.find(long_key("a"), hash);
    REQUIRE(it != cmap.end());
    REQUIRE(it->second == 1);
    REQUIRE(cmap.contains(long_key("a"), hash));
    REQUIRE(cmap.count(long_key("a"), hash) == 1);
    REQUIRE(cmap.equal_range(long_key("a"), hash).first == it);
    REQUIRE(cmap.at(long_key("a"), hash) == 1);
}

// The hash belongs to the hasher, not to the table it was taken from. A map, a set and a map with another mapped type
// that hash the key the same way all take the same one.
TEST_CASE("a_hash_carries_over_to_every_table_with_the_same_hasher") {
    auto map = dice::sparse_map<std::string, int>();
    auto set = dice::sparse_set<std::string>();
    auto other = dice::sparse_map<std::string, long>();

    auto const key = long_key("shared");
    map[key] = 1;
    set.insert(key);
    other[key] = 2;

    auto const hash = map.hash_function()(key);
    REQUIRE(hash == set.hash_function()(key));
    REQUIRE(map.find(key, hash)->second == 1);
    REQUIRE(set.find(key, hash) != set.end());
    REQUIRE(other.find(key, hash)->second == 2);
}

// Heterogeneous lookup: hash a string_view once, look up with whatever compares equal to it.
TEST_CASE("a_precomputed_hash_works_across_key_types") {
    auto map = transparent_map();
    map[long_key("a")] = 1;

    auto const key = long_key("a");
    auto const from_view = map.hash_function()(std::string_view(key));
    auto const from_string = map.hash_function()(key);

    REQUIRE(from_view == from_string);
    REQUIRE(map.find(std::string_view(key), from_string)->second == 1);
    REQUIRE(map.find(key, from_view)->second == 1);
    REQUIRE(map.at(std::string_view(key), from_view) == 1);
    REQUIRE(map.count(std::string_view(key), from_view) == 1);
    REQUIRE(map.contains(std::string_view(key), from_view));
    REQUIRE(map.equal_range(std::string_view(key), from_view).first->second == 1);

    // the const transparent at() with a precalculated hash
    auto const &cmap = map;
    REQUIRE(cmap.at(std::string_view(key), from_view) == 1);

    auto const absent = long_key("absent");
    auto const absent_hash = map.hash_function()(std::string_view(absent));
    REQUIRE_THROWS_AS(static_cast<void>(cmap.at(std::string_view(absent), absent_hash)), std::out_of_range);
}

// equal_range and count come in four overloads each: key type or transparent, const table or not. Each is asked with
// a hit that is not the last element (the iterator after the hit is not `end()`) and with a miss (`count()` is 0).
TEST_CASE("precomputed_equal_range_and_count_in_every_overload") {
    auto map = transparent_map();
    for (int i = 0; i < 100; ++i) {
        map[long_key(std::to_string(i))] = i;
    }
    auto const &cmap = std::as_const(map);

    // the key at begin(), so that the iterator after the hit is as far from the end as it gets
    auto const key = map.begin()->first;
    auto const view = std::string_view(key);
    auto const absent = long_key("nowhere");
    auto const absent_view = std::string_view(absent);

    auto const hash = map.hash_function()(key);
    auto const absent_hash = map.hash_function()(absent);

    REQUIRE(map.equal_range(key, hash) == std::pair(map.begin(), std::next(map.begin())));
    REQUIRE(cmap.equal_range(key, hash) == std::pair(cmap.begin(), std::next(cmap.begin())));
    REQUIRE(map.equal_range(view, hash) == std::pair(map.begin(), std::next(map.begin())));
    REQUIRE(cmap.equal_range(view, hash) == std::pair(cmap.begin(), std::next(cmap.begin())));

    REQUIRE(map.equal_range(absent, absent_hash) == std::pair(map.end(), map.end()));
    REQUIRE(cmap.equal_range(absent, absent_hash) == std::pair(cmap.end(), cmap.end()));
    REQUIRE(map.equal_range(absent_view, absent_hash) == std::pair(map.end(), map.end()));
    REQUIRE(cmap.equal_range(absent_view, absent_hash) == std::pair(cmap.end(), cmap.end()));

    REQUIRE(map.count(key, hash) == 1);
    REQUIRE(map.count(view, hash) == 1);
    REQUIRE(map.count(absent, absent_hash) == 0);
    REQUIRE(map.count(absent_view, absent_hash) == 0);

    REQUIRE(map.contains(key, hash));
    REQUIRE(map.contains(view, hash));
    REQUIRE(!map.contains(absent, absent_hash));
    REQUIRE(!map.contains(absent_view, absent_hash));
}

// sparse_map uses the value of the hasher as it is, so a lookup with a precalculated hash takes the same path as one
// that hashes the key. This checks it with an identity hash and with a 32 bit hash.
namespace {

    struct weak_hash {
        [[nodiscard]] std::size_t operator()(int x) const noexcept {
            return static_cast<std::size_t>(x);
        }
    };

    struct narrow_hash {
        [[nodiscard]] std::uint32_t operator()(int x) const noexcept {
            return static_cast<std::uint32_t>(x) * UINT32_C(2654435761);
        }
    };

}  // namespace

TEST_CASE("precomputed_lookups_work_with_weak_and_narrow_hashes") {
    auto weak = dice::sparse_map<int, int, weak_hash, std::equal_to<int>>();
    auto narrow = dice::sparse_map<int, int, narrow_hash, std::equal_to<int>>();

    for (int i = 0; i < 1000; ++i) {
        weak[i] = i;
        narrow[i] = i;
    }

    for (int i = 0; i < 1000; ++i) {
        REQUIRE(weak.find(i, weak.hash_function()(i))->second == i);
        REQUIRE(narrow.find(i, narrow.hash_function()(i))->second == i);
    }

    REQUIRE(weak.find(1000, weak.hash_function()(1000)) == weak.end());
    REQUIRE(narrow.find(1000, narrow.hash_function()(1000)) == narrow.end());
}

// A hash does not depend on the size of the table, so one taken before a growth stays good after it.
TEST_CASE_MAP("a_hash_survives_rehashing", std::string, int) {
    auto map = map_t();
    auto const key = long_key("kept");
    map[key] = 7;

    auto const hash = map.hash_function()(key);
    for (int i = 0; i < 10000; ++i) {
        map[long_key(std::to_string(i))] = i;
    }
    REQUIRE(map.find(key, hash)->second == 7);

    map.rehash(0);
    REQUIRE(map.find(key, hash)->second == 7);

    auto moved = std::move(map);
    REQUIRE(moved.find(key, hash)->second == 7);
}
