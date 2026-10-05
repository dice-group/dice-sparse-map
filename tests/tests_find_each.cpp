#include "fixtures/inline_containers.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <forward_list>
#include <functional>
#include <iterator>
#include <memory>
#include <random>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace dice::unordered_sparse::tests;

/*
 * `find_each` calls `f(key, it)` for every key of a range, in order, with what `find(key)` returns. A table of more
 * groups and elements than `find_each_prefetch_bytes` takes the path with prefetching (64 MiB and a pipeline for a
 * trivially destructible key type, 24 MiB and blocks of 16 keys for other key types), a smaller one the loop of
 * `find`. `prefetches` checks which path a test takes.
 */

namespace {

    /**
     * Arguments of `reserve` that make a table large enough for prefetching with few elements: `pipeline_reserve`
     * gives at least 2^28 buckets, 4194304 groups of 32 bytes, 128 MiB, more than 64 MiB. `blocks_reserve` gives at
     * least 2^26 buckets, 32 MiB, more than 24 MiB. `sparse_elements` elements go into such a table.
     */
    constexpr std::size_t pipeline_reserve = std::size_t{1} << 27U;
    constexpr std::size_t blocks_reserve = std::size_t{1} << 25U;
    constexpr std::size_t sparse_elements = 200000;

    /**
     * Numbers of elements of the dense tables that are large enough for prefetching, for the tests with holes: 4.5
     * million elements of 16 bytes (`std::size_t` keys and values) and their groups are about 77 MiB, more than 64 MiB.
     * 1.2 million elements of 40 bytes (`std::string` keys, `std::size_t` values) and their groups are about 48 MiB,
     * more than 24 MiB. In these tables the groups hold many elements, and probe sequences are often longer than one
     * bucket.
     */
    constexpr std::size_t pipeline_elements = 4500000;
    constexpr std::size_t blocks_elements = 1200000;

    /**
     * Inserts the keys `i * 3` for `i` below `nb_elements`, and returns `nb_keys` keys below `nb_elements * 4` in a
     * random order: about a quarter are in `map`.
     */
    template<typename Map>
    [[nodiscard]] std::vector<std::size_t> mixed_keys(Map &map, std::size_t nb_elements, std::size_t nb_keys) {
        std::mt19937_64 rng(7);
        for (std::size_t i = 0; i < nb_elements; ++i) {
            map.try_emplace(i * 3, i);
        }
        std::vector<std::size_t> keys;
        for (std::size_t i = 0; i < nb_keys; ++i) {
            keys.push_back(rng() % (nb_elements * 4));
        }
        return keys;
    }

    /**
     * Inserts the keys `std::to_string(i * 3)` for `i` below `nb_elements`, and returns `nb_keys` keys
     * `std::to_string(j)` for `j` below `nb_elements * 4` in a random order: about a quarter are in `map`.
     */
    template<typename Map>
    [[nodiscard]] std::vector<std::string> mixed_string_keys(Map &map, std::size_t nb_elements, std::size_t nb_keys) {
        std::mt19937_64 rng(17);
        for (std::size_t i = 0; i < nb_elements; ++i) {
            map.try_emplace(std::to_string(i * 3), i);
        }
        std::vector<std::string> keys;
        for (std::size_t i = 0; i < nb_keys; ++i) {
            keys.push_back(std::to_string(rng() % (nb_elements * 4)));
        }
        return keys;
    }

    /**
     * Calls `find_each` with the first `nb_keys` of `keys` and checks every call against `find`.
     * @return the number of keys that were found
     */
    template<typename Map, typename Key>
    std::size_t check_against_find(Map &map, std::vector<Key> const &keys, std::size_t nb_keys) {
        auto const batch = std::vector<Key>(keys.begin(), keys.begin() + static_cast<std::ptrdiff_t>(nb_keys));
        std::size_t calls = 0;
        std::size_t hits = 0;
        map.find_each(batch, [&](Key const &key, auto it) {
            REQUIRE(calls < batch.size());
            CHECK(&key == &batch[calls]);
            CHECK(it == map.find(key));
            hits += it != map.end() ? 1 : 0;
            ++calls;
        });
        CHECK(calls == nb_keys);
        return hits;
    }

    /**
     * @return whether `find_each` of `map` takes the path with prefetching for `keys`. From a range of prvalues, the
     * path with prefetching takes every key twice (once for the hash ahead, once for the lookup and `f`), the loop of
     * `find` takes it once.
     */
    template<typename Map, typename Key>
    [[nodiscard]] bool prefetches(Map const &map, std::vector<Key> const &keys) {
        std::size_t nb_dereferences = 0;
        auto const view = keys | std::views::transform([&nb_dereferences](Key const &key) {
                              ++nb_dereferences;
                              return key;
                          });
        map.find_each(view, [](auto const &, auto) {
        });
        REQUIRE((nb_dereferences == keys.size() || nb_dereferences == 2 * keys.size()));
        return nb_dereferences == 2 * keys.size();
    }

    /**
     * Calls `find_each` of `map` with a range that gives `keys` as prvalues and counts how often a key is taken from
     * it. Checks that `f` gets the key that it looked up.
     * @return for every call of `f`, the number of keys taken from the range up to this call
     */
    template<typename Map, typename Key>
    [[nodiscard]] std::vector<std::size_t> dereferences_per_call(Map &map, std::vector<Key> const &keys) {
        std::size_t nb_dereferences = 0;
        auto const view = keys | std::views::transform([&nb_dereferences](Key const &key) {
                              ++nb_dereferences;
                              return key;
                          });
        std::vector<std::size_t> counts;
        map.find_each(view, [&](Key const &key, auto it) {
            REQUIRE(counts.size() < keys.size());
            CHECK(key == keys[counts.size()]);
            CHECK(it == map.find(key));
            counts.push_back(nb_dereferences);
        });
        CHECK(counts.size() == keys.size());
        return counts;
    }

    /**
     * The batch sizes around the distances of the prefetching.
     */
    constexpr std::size_t batch_sizes[] = {0, 1, 7, 8, 9, 15, 16, 17, 100003};

    struct string_hash {
        using is_transparent = void;

        [[nodiscard]] std::size_t operator()(std::string_view key) const noexcept {
            return std::hash<std::string_view>{}(key);
        }
    };

    struct pair_hash {
        [[nodiscard]] std::size_t operator()(std::pair<std::size_t, std::size_t> const &key) const noexcept {
            return std::hash<std::size_t>{}(key.first) ^ (key.second * UINT64_C(0x9E3779B97F4A7C15));
        }
    };

    /**
     * A mapped value whose move constructor can throw, so that an erase leaves a hole.
     */
    struct copied_value {
        std::size_t value = 0;

        copied_value(std::size_t v) noexcept  // NOLINT(google-explicit-constructor)
            : value(v) {
        }

        copied_value(copied_value const &) = default;

        copied_value(copied_value &&other) noexcept(false)  // NOLINT(performance-noexcept-move-constructor)
            : value(other.value) {
        }

        copied_value &operator=(copied_value const &) = default;

        copied_value &operator=(copied_value &&other) noexcept(false) {  // NOLINT(performance-noexcept-move-constructor)
            value = other.value;
            return *this;
        }

        ~copied_value() = default;
    };

}  // namespace

TEST_CASE_MAP("find_each in a small map gives what find gives, for every key in order", std::size_t, std::size_t) {
    auto map = map_t();
    auto const keys = mixed_keys(map, 5000, 10007);
    std::size_t calls = 0;
    std::size_t hits = 0;
    map.find_each(keys, [&](std::size_t const &key, typename map_t::iterator it) {
        REQUIRE(calls < keys.size());
        CHECK(&key == &keys[calls]);
        CHECK(it == map.find(key));
        if (it != map.end()) {
            ++hits;
            it->second += 1;
        }
        ++calls;
    });
    CHECK(calls == keys.size());
    CHECK(hits > 0);
    CHECK(hits < keys.size());
    CHECK_FALSE(prefetches(map, keys));

    // the mapped values were changed through the iterators
    std::size_t changed = 0;
    for (auto const &[key, value] : map) {
        if (value != key / 3) {
            ++changed;
        }
    }
    CHECK(changed > 0);

    auto const &const_map = map;
    calls = 0;
    const_map.find_each(keys, [&](std::size_t const &key, auto it) {
        static_assert(std::is_same_v<decltype(it), typename map_t::const_iterator>);
        CHECK(it == const_map.find(key));
        ++calls;
    });
    CHECK(calls == keys.size());
}

TEST_CASE_MAP("find_each in a map large enough for prefetching gives what find gives", std::size_t, std::size_t) {
    auto map = map_t();
    map.reserve(pipeline_reserve);
    auto const keys = mixed_keys(map, sparse_elements, 100003);
    CHECK(prefetches(map, keys));
    for (std::size_t const nb_keys : batch_sizes) {
        auto const hits = check_against_find(map, keys, nb_keys);
        if (nb_keys == keys.size()) {
            CHECK(hits > 0);
            CHECK(hits < keys.size());
        }
    }
}

TEST_CASE("find_each in a large map with holes gives what find gives") {
    auto map = dice::sparse_map<std::size_t, copied_value>();
    auto const keys = mixed_keys(map, pipeline_elements / 3 * 4, 100003);
    // every fourth element is erased in place and leaves a hole, `pipeline_elements` elements of 16 bytes stay
    for (std::size_t i = 0; i < pipeline_elements / 3 * 4; i += 4) {
        REQUIRE(map.erase(i * 3) == 1);
    }
    CHECK(prefetches(map, keys));
    for (std::size_t const nb_keys : batch_sizes) {
        auto const hits = check_against_find(map, keys, nb_keys);
        if (nb_keys == keys.size()) {
            CHECK(hits > 0);
            CHECK(hits < keys.size());
        }
    }
}

TEST_CASE_MAP("find_each on an empty map and with an empty range", std::size_t, std::size_t) {
    auto map = map_t();
    auto const keys = std::vector<std::size_t>{1, 2, 3};
    std::size_t calls = 0;
    map.find_each(keys, [&](std::size_t const &, auto it) {
        CHECK(it == map.end());
        ++calls;
    });
    CHECK(calls == 3);
    map.try_emplace(1, 1);
    calls = 0;
    map.find_each(std::vector<std::size_t>{}, [&](std::size_t const &, auto) {
        ++calls;
    });
    CHECK(calls == 0);
}

TEST_CASE_SET("find_each of a set with a forward range", std::size_t) {
    auto set = set_t();
    for (std::size_t i = 0; i < 1000; ++i) {
        set.insert(i * 5);
    }
    std::forward_list<std::size_t> keys;
    for (std::size_t i = 0; i < 101; ++i) {
        keys.push_front(i * 25 + 1);
        keys.push_front(i * 25);
    }
    std::vector<std::size_t> found;
    std::size_t calls = 0;
    set.find_each(keys, [&](std::size_t const &key, auto it) {
        CHECK(it == set.find(key));
        if (it != set.end()) {
            found.push_back(*it);
        }
        ++calls;
    });
    CHECK(calls == 202);
    CHECK(found.size() == 101);
}

TEST_CASE("find_each with heterogeneous keys") {
    auto map = dice::sparse_map<std::string, int, string_hash, std::equal_to<>>();
    for (int i = 0; i < 100; ++i) {
        map.try_emplace(std::to_string(i), i);
    }
    auto const keys = std::vector<std::string_view>{"1", "x", "99", "100", "42"};
    std::vector<int> values;
    map.find_each(keys, [&](std::string_view key, auto it) {
        values.push_back(it == map.end() ? -1 : it->second);
        CHECK(it == map.find(key));
    });
    CHECK(values == std::vector<int>{1, -1, 99, -1, 42});
}

TEST_CASE("find_each gives f the key that it looked up, also for a range of prvalues") {
    for (std::size_t const nb_buckets : {std::size_t{0}, pipeline_reserve}) {
        CAPTURE(nb_buckets);
        auto map = dice::sparse_map<std::size_t, std::size_t>();
        map.reserve(nb_buckets);
        auto const keys = mixed_keys(map, 5000, 10007);
        std::size_t nb_dereferences = 0;
        auto const view = keys | std::views::transform([&nb_dereferences](std::size_t key) {
                              ++nb_dereferences;
                              return key;
                          });
        std::size_t calls = 0;
        map.find_each(view, [&](std::size_t key, auto it) {
            REQUIRE(calls < keys.size());
            CHECK(key == keys[calls]);
            CHECK(it == map.find(key));
            ++calls;
        });
        CHECK(calls == keys.size());
        if (nb_buckets == 0) {
            // the loop of `find`: one key for the hash, the lookup and `f`
            CHECK(nb_dereferences == keys.size());
        } else {
            // the pipeline: one key for the hash ahead, one for the lookup and `f`
            CHECK(nb_dereferences == 2 * keys.size());
        }
    }
}

TEST_CASE_SET("find_each of a set large enough for prefetching with a forward range", std::size_t) {
    auto set = set_t();
    set.reserve(pipeline_reserve);
    std::size_t const nb_elements = sparse_elements;
    for (std::size_t i = 0; i < nb_elements; ++i) {
        set.insert(i * 3);
    }
    std::mt19937_64 rng(11);
    std::forward_list<std::size_t> keys;
    std::size_t const nb_keys = 100003;
    for (std::size_t i = 0; i < nb_keys; ++i) {
        keys.push_front(rng() % (nb_elements * 4));
    }
    CHECK(prefetches(set, std::vector<std::size_t>(keys.begin(), keys.end())));
    auto expected = keys.begin();
    std::size_t calls = 0;
    std::size_t hits = 0;
    set.find_each(keys, [&](std::size_t const &key, auto it) {
        REQUIRE(expected != keys.end());
        CHECK(&key == &*expected);
        CHECK(it == set.find(key));
        hits += it != set.end() ? 1 : 0;
        ++expected;
        ++calls;
    });
    CHECK(calls == nb_keys);
    CHECK(hits > 0);
    CHECK(hits < nb_keys);
}

TEST_CASE("find_each with heterogeneous keys in a map large enough for prefetching") {
    auto map = dice::sparse_map<std::string, std::size_t, string_hash, std::equal_to<>>();
    map.reserve(blocks_reserve);
    for (std::size_t i = 0; i < sparse_elements; ++i) {
        map.try_emplace(std::to_string(i * 3), i);
    }
    std::mt19937_64 rng(13);
    std::vector<std::string> strings;
    for (std::size_t i = 0; i < 100003; ++i) {
        strings.push_back(std::to_string(rng() % (sparse_elements * 4)));
    }
    auto const keys = std::vector<std::string_view>(strings.begin(), strings.end());
    CHECK(prefetches(map, keys));
    std::size_t calls = 0;
    std::size_t hits = 0;
    map.find_each(keys, [&](std::string_view const &key, auto it) {
        REQUIRE(calls < keys.size());
        CHECK(&key == &keys[calls]);
        CHECK(it == map.find(key));
        if (it != map.end()) {
            CHECK(it->first == key);
            ++hits;
        }
        ++calls;
    });
    CHECK(calls == keys.size());
    CHECK(hits > 0);
    CHECK(hits < keys.size());
}

TEST_CASE("find_each with heterogeneous keys in a map of trivially destructible keys large enough for prefetching") {
    // `std::string_view` keys are trivially destructible and are looked up in a pipeline, here with `std::string` keys
    std::vector<std::string> stored;
    stored.reserve(sparse_elements);
    auto map = dice::sparse_map<std::string_view, std::size_t, string_hash, std::equal_to<>>();
    map.reserve(pipeline_reserve);
    for (std::size_t i = 0; i < sparse_elements; ++i) {
        stored.push_back(std::to_string(i * 3));
        map.try_emplace(stored.back(), i);
    }
    std::mt19937_64 rng(23);
    std::vector<std::string> keys;
    for (std::size_t i = 0; i < 100003; ++i) {
        keys.push_back(std::to_string(rng() % (sparse_elements * 4)));
    }
    CHECK(prefetches(map, keys));
    auto const counts = dereferences_per_call(map, keys);
    REQUIRE(counts.size() == keys.size());
    for (std::size_t c = 0; c < counts.size(); ++c) {
        // the call for key `c` comes after the hashes of the keys up to `c + 16` and the lookups up to `c`, as in a
        // pipeline (blocks of 16 keys take the keys in another order)
        CHECK(counts[c] == std::min(c + 17, counts.size()) + c + 1);
    }
    std::size_t calls = 0;
    std::size_t hits = 0;
    map.find_each(keys, [&](std::string const &key, auto it) {
        REQUIRE(calls < keys.size());
        CHECK(&key == &keys[calls]);
        CHECK(it == map.find(key));
        if (it != map.end()) {
            CHECK(it->first == key);
            ++hits;
        }
        ++calls;
    });
    CHECK(calls == keys.size());
    CHECK(hits > 0);
    CHECK(hits < keys.size());
}

TEST_CASE_MAP("find_each in a map with many groups and few elements", std::size_t, std::size_t) {
    auto map = map_t();
    // 2^28 buckets in 4194304 groups of 32 bytes, 128 MiB, more than 64 MiB
    map.reserve(std::size_t{1} << 27U);
    REQUIRE(map.bucket_count() >= (std::size_t{1} << 28U));
    std::vector<std::size_t> keys;
    for (std::size_t i = 0; i < 100; ++i) {
        keys.push_back(i);
    }
    CHECK(prefetches(map, keys));
    CHECK(check_against_find(map, keys, keys.size()) == 0);
    for (std::size_t i = 0; i < 10; ++i) {
        map.try_emplace(i * 7, i);
    }
    CHECK(check_against_find(map, keys, keys.size()) == 10);
}

TEST_CASE_MAP("find_each in a map of std::string keys large enough for prefetching gives what find gives", std::string, std::size_t) {
    auto map = map_t();
    map.reserve(blocks_reserve);
    auto const keys = mixed_string_keys(map, sparse_elements, 100003);
    CHECK(prefetches(map, keys));
    for (std::size_t const nb_keys : batch_sizes) {
        auto const hits = check_against_find(map, keys, nb_keys);
        if (nb_keys == keys.size()) {
            CHECK(hits > 0);
            CHECK(hits < keys.size());
        }
    }
}

TEST_CASE("find_each in a large map of std::string keys with holes gives what find gives") {
    auto map = dice::sparse_map<std::string, copied_value>();
    auto const keys = mixed_string_keys(map, blocks_elements / 3 * 4, 100003);
    // every fourth element is erased in place and leaves a hole, `blocks_elements` elements of 40 bytes stay
    for (std::size_t i = 0; i < blocks_elements / 3 * 4; i += 4) {
        REQUIRE(map.erase(std::to_string(i * 3)) == 1);
    }
    CHECK(prefetches(map, keys));
    for (std::size_t const nb_keys : batch_sizes) {
        auto const hits = check_against_find(map, keys, nb_keys);
        if (nb_keys == keys.size()) {
            CHECK(hits > 0);
            CHECK(hits < keys.size());
        }
    }
}

TEST_CASE_SET("find_each of a set of std::string keys large enough for prefetching with a forward range", std::string) {
    auto set = set_t();
    set.reserve(blocks_reserve);
    for (std::size_t i = 0; i < sparse_elements; ++i) {
        set.insert(std::to_string(i * 3));
    }
    std::mt19937_64 rng(19);
    std::forward_list<std::string> keys;
    std::size_t const nb_keys = 100003;
    for (std::size_t i = 0; i < nb_keys; ++i) {
        keys.push_front(std::to_string(rng() % (sparse_elements * 4)));
    }
    CHECK(prefetches(set, std::vector<std::string>(keys.begin(), keys.end())));
    auto expected = keys.begin();
    std::size_t calls = 0;
    std::size_t hits = 0;
    set.find_each(keys, [&](std::string const &key, auto it) {
        REQUIRE(expected != keys.end());
        CHECK(&key == &*expected);
        CHECK(it == set.find(key));
        hits += it != set.end() ? 1 : 0;
        ++expected;
        ++calls;
    });
    CHECK(calls == nb_keys);
    CHECK(hits > 0);
    CHECK(hits < nb_keys);
}

TEST_CASE_MAP("find_each in a map of std::string keys with many groups and few elements", std::string, std::size_t) {
    auto map = map_t();
    // 2^26 buckets in 1048576 groups of 32 bytes, 32 MiB, more than 24 MiB
    map.reserve(std::size_t{1} << 25U);
    REQUIRE(map.bucket_count() >= (std::size_t{1} << 26U));
    std::vector<std::string> keys;
    for (std::size_t i = 0; i < 100; ++i) {
        keys.push_back(std::to_string(i));
    }
    CHECK(prefetches(map, keys));
    CHECK(check_against_find(map, keys, keys.size()) == 0);
    for (std::size_t i = 0; i < 10; ++i) {
        map.try_emplace(std::to_string(i * 7), i);
    }
    CHECK(check_against_find(map, keys, keys.size()) == 10);
}

TEST_CASE("find_each prefetches in a pipeline for a trivially destructible key type and in blocks for other key types") {
    std::size_t const n = 1001;
    SUBCASE("std::size_t keys, a pipeline") {
        auto map = dice::sparse_map<std::size_t, std::size_t>();
        map.reserve(pipeline_reserve);
        auto const keys = mixed_keys(map, sparse_elements, n);
        auto const counts = dereferences_per_call(map, keys);
        REQUIRE(counts.size() == n);
        for (std::size_t c = 0; c < n; ++c) {
            // the call for key `c` comes after the hashes of the keys up to `c + 16` and the lookups up to `c`
            CHECK(counts[c] == std::min(c + 17, n) + c + 1);
        }
    }
    SUBCASE("std::pair<std::size_t, std::size_t> keys, which are not trivially copyable, a pipeline") {
        static_assert(!std::is_trivially_copyable_v<std::pair<std::size_t, std::size_t>>);
        auto map = dice::sparse_map<std::pair<std::size_t, std::size_t>, std::size_t, pair_hash>();
        map.reserve(pipeline_reserve);
        std::vector<std::pair<std::size_t, std::size_t>> keys;
        for (std::size_t i = 0; i < n; ++i) {
            map.try_emplace(std::pair{i * 3, i}, i);
            keys.emplace_back(i * 2, i % 7 == 0 ? i * 2 / 3 : i);
        }
        auto const counts = dereferences_per_call(map, keys);
        REQUIRE(counts.size() == n);
        for (std::size_t c = 0; c < n; ++c) {
            CHECK(counts[c] == std::min(c + 17, n) + c + 1);
        }
    }
    SUBCASE("std::string keys, blocks of 16 keys") {
        auto map = dice::sparse_map<std::string, std::size_t>();
        map.reserve(blocks_reserve);
        auto const keys = mixed_string_keys(map, sparse_elements, n);
        auto const counts = dereferences_per_call(map, keys);
        REQUIRE(counts.size() == n);
        for (std::size_t c = 0; c < n; ++c) {
            // the call for key `c` comes after the hashes of all keys of its block and the lookups up to `c`
            CHECK(counts[c] == std::min((c / 16 + 1) * 16, n) + c + 1);
        }
    }
}

namespace {

    /**
     * The number of hash calls of `throwing_hash` since the last reset, and the call that throws (0: none).
     */
    std::size_t hash_calls = 0;
    std::size_t hash_throw_at = 0;

    struct hash_error {};

    struct key_error {};

    /**
     * `std::hash<Key>` that throws `hash_error` on the call `hash_throw_at`.
     */
    template<typename Key>
    struct throwing_hash {
        [[nodiscard]] std::size_t operator()(Key const &key) const {
            ++hash_calls;
            if (hash_calls == hash_throw_at) {
                throw hash_error{};
            }
            return std::hash<Key>{}(key);
        }
    };

    template<typename Key>
    using throwing_table = dice::unordered_sparse::detail::sparse_hash<dice::unordered_sparse::detail::map_policy<Key, std::size_t>,
                                                                       throwing_hash<Key>,
                                                                       std::equal_to<Key>,
                                                                       std::allocator<std::pair<Key, std::size_t>>,
                                                                       dice::unordered_sparse::sparsity::medium>;

    /**
     * Calls `find_each` of `map` with `keys` so that the hash function throws for the key at position `j`, and then so
     * that `*i` throws for the key at position `j`, for every `j` of `positions`. Checks that `find_each` throws, that
     * `f` was called `expected_calls(j)` times with the keys in order, that at most 16 keys before `j` (the number in
     * the docs of `find_each`) were not given to `f`, and that the map is unchanged.
     */
    template<typename Map, typename Key, typename E>
    void check_exceptions(Map &map, std::vector<Key> const &keys, std::vector<std::size_t> const &positions, E expected_calls) {
        std::size_t const size = map.size();
        for (std::size_t const j : positions) {
            CAPTURE(j);
            REQUIRE(j < keys.size());
            std::size_t calls = 0;
            hash_calls = 0;
            hash_throw_at = j + 1;
            CHECK_THROWS_AS(map.find_each(keys,
                                          [&](Key const &key, auto it) {
                                              REQUIRE(calls < j);
                                              CHECK(&key == &keys[calls]);
                                              CHECK((it == map.end() || it->first == key));
                                              ++calls;
                                          }),
                            hash_error);
            hash_throw_at = 0;
            CHECK(calls == expected_calls(j));
            CHECK(j - calls <= 16);

            calls = 0;
            auto const view = std::views::iota(std::size_t{0}, keys.size()) | std::views::transform([&keys, j](std::size_t p) {
                                  if (p == j) {
                                      throw key_error{};
                                  }
                                  return keys[p];
                              });
            CHECK_THROWS_AS(map.find_each(view,
                                          [&](Key const &key, auto it) {
                                              REQUIRE(calls < j);
                                              CHECK(key == keys[calls]);
                                              CHECK((it == map.end() || it->first == key));
                                              ++calls;
                                          }),
                            key_error);
            CHECK(calls == expected_calls(j));
            CHECK(j - calls <= 16);
        }
        CHECK(map.size() == size);
        std::size_t found = 0;
        for (auto const &key : keys) {
            found += map.count(key);
        }
        CHECK(found > 0);
    }

}  // namespace

TEST_CASE("find_each calls f for the keys before a key whose hash or dereference throws, except for the keys ahead") {
    using size_table = throwing_table<std::size_t>;
    using string_table = throwing_table<std::string>;
    CHECK(size_table::find_each_pipelines);
    CHECK_FALSE(string_table::find_each_pipelines);
    // the docs of `find_each` name 16 keys for the pipeline and for the blocks
    constexpr std::size_t ring = 2 * size_table::find_each_distance;
    constexpr std::size_t block = string_table::find_each_block;
    CHECK(ring == 16);
    CHECK(block == 16);
    std::size_t const n = 203;

    SUBCASE("the loop of find") {
        auto map = dice::sparse_map<std::size_t, std::size_t, throwing_hash<std::size_t>>();
        auto const keys = mixed_keys(map, 5000, n);
        CHECK_FALSE(prefetches(map, keys));
        check_exceptions(map, keys, {0, 1, 15, 16, 17, 100, n - 1}, [](std::size_t j) {
            return j;
        });
    }
    SUBCASE("a pipeline") {
        auto map = dice::sparse_map<std::size_t, std::size_t, throwing_hash<std::size_t>>();
        map.reserve(pipeline_reserve);
        auto const keys = mixed_keys(map, 1000, n);
        CHECK(prefetches(map, keys));
        // the keys up to `j - ring - 1` were looked up before the hash of the key at `j` was taken ahead
        check_exceptions(map, keys, {0, 1, ring - 1, ring, ring + 1, 2 * ring - 1, 2 * ring, 100, n - 1}, [](std::size_t j) {
            return j < ring ? std::size_t{0} : j - ring;
        });
    }
    SUBCASE("blocks") {
        auto map = dice::sparse_map<std::string, std::size_t, throwing_hash<std::string>>();
        map.reserve(blocks_reserve);
        auto const keys = mixed_string_keys(map, 1000, n);
        CHECK(prefetches(map, keys));
        // the keys of the blocks before the block of `j` were looked up
        check_exceptions(map, keys, {0, 1, block - 1, block, block + 1, 2 * block - 1, 2 * block, 100, n - 1}, [](std::size_t j) {
            return j / block * block;
        });
    }
}

namespace {

    struct constexpr_hash {
        using is_avalanching = void;

        [[nodiscard]] constexpr std::size_t operator()(int key) const noexcept {
            return static_cast<std::size_t>(key) * UINT64_C(0x9E3779B97F4A7C15);
        }
    };

    constexpr int find_each_in_a_constant_expression() {
        auto map = dice::sparse_map<int, int, constexpr_hash>();
        for (int i = 0; i < 50; ++i) {
            map.try_emplace(i, i);
        }
        int const keys[] = {1, 2, 100, 49, -1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
        int sum = 0;
        map.find_each(keys, [&](int, auto it) {
            sum += it == map.end() ? 1000 : it->second;
        });
        return sum;
    }

}  // namespace

TEST_CASE("find_each in a constant expression") {
    // 1 + 2 + 49 + 3 + ... + 17 and two misses
    constexpr int expected = 1 + 2 + 49 + (3 + 17) * 15 / 2 + 2000;
    static_assert(find_each_in_a_constant_expression() == expected);
    CHECK(find_each_in_a_constant_expression() == expected);
}

/*
 * A container with an inline capacity keeps up to that many elements in the container object, in the inline group:
 * group 0 of a table of 64 buckets. It has no buckets to prefetch, and `find_each` looks the keys up in the inline
 * group. `identity_hash` puts key `k` into bucket `k % 64` there, so that the tests know the buckets and the probe
 * sequences.
 */
namespace {

    template<typename Container>
    constexpr bool is_map_v = requires { typename Container::mapped_type; };

    /// the key of an element as `*it` gives it
    template<typename Element>
    [[nodiscard]] std::size_t key_of(Element const &e) {
        if constexpr (requires { e.second; }) {
            return e.first;
        } else {
            return e;
        }
    }

    /**
     * The iterator of the element with the key `key`, from a loop over the iterators that starts at `begin()`. It
     * gets its end of the inline elements from `begin()`, not from `find`.
     */
    template<typename Container>
    [[nodiscard]] auto iterator_by_loop(Container &container, std::size_t key) {
        auto it = container.begin();
        while (it != container.end() && key_of(*it) != key) {
            ++it;
        }
        return it;
    }

    /**
     * `5` in bucket 5, `1` in bucket 1, `65` and `129` with the home bucket 1 in the buckets 2 and 4 (quadratic
     * probing: 1, 1 + 1, 1 + 3). In bucket order: 1, 65, 129, 5.
     */
    constexpr std::array<std::size_t, 4> colliding_keys{5, 1, 65, 129};

    /**
     * `capacity` keys that fill the inline group, the first four `colliding_keys`, then keys with the home bucket 1 or
     * 3, so that many elements are outside their home bucket.
     */
    [[nodiscard]] std::vector<std::size_t> filling_keys(std::size_t capacity) {
        std::vector<std::size_t> keys(colliding_keys.begin(), colliding_keys.end());
        for (std::size_t n = 3; keys.size() < capacity; ++n) {
            keys.push_back(64 * n + 1 + 2 * (n % 2));
        }
        return keys;
    }

    /**
     * Keys that are not in a container of `filling_keys`: keys with the home buckets of its elements, whose probe
     * sequences pass its elements (193 and 449 with the home bucket 1, 69 with the home bucket 5), and keys with other
     * home buckets.
     */
    constexpr std::array<std::size_t, 6> missing_keys{193, 449, 69, 0, 6, 63};

    /// inserts `keys` into `container`, as keys of a set or as keys with the value `key + 1` of a map
    template<typename Container>
    void insert_keys(Container &container, std::vector<std::size_t> const &keys) {
        for (std::size_t const key : keys) {
            if constexpr (is_map_v<Container>) {
                REQUIRE(container.try_emplace(key, key + 1).second);
            } else {
                REQUIRE(container.insert(key).second);
            }
        }
    }

    /**
     * Calls `find_each` of `container`, and of `container` as const, with `keys` and checks every call against `find`:
     * the order of the keys, the key that `f` gets (by address), the iterator and its type, and the element it points
     * to. For a map that is not const, `f` adds 1 to the mapped value of every key it finds through the iterator, and
     * the call for the const map checks the new values.
     * @return the number of keys that were found
     */
    template<typename Container>
    std::size_t check_inline_find_each(Container &container, std::vector<std::size_t> const &keys) {
        std::unordered_map<std::size_t, std::size_t> values_before;
        if constexpr (is_map_v<Container>) {
            for (auto const &[key, value] : container) {
                values_before[key] = value;
            }
        }
        std::size_t calls = 0;
        std::size_t hits = 0;
        container.find_each(keys, [&](std::size_t const &key, auto it) {
            static_assert(std::is_same_v<decltype(it), typename Container::iterator>);
            REQUIRE(calls < keys.size());
            CHECK(&key == &keys[calls]);
            CHECK(it == container.find(key));
            if (it != container.end()) {
                CHECK(key_of(*it) == key);
                CHECK(std::next(it) == std::next(iterator_by_loop(container, key)));
                if constexpr (is_map_v<Container>) {
                    it->second += 1;
                }
                ++hits;
            }
            ++calls;
        });
        CHECK(calls == keys.size());

        auto const &const_container = container;
        std::size_t const_calls = 0;
        std::size_t const_hits = 0;
        const_container.find_each(keys, [&](std::size_t const &key, auto it) {
            static_assert(std::is_same_v<decltype(it), typename Container::const_iterator>);
            REQUIRE(const_calls < keys.size());
            CHECK(&key == &keys[const_calls]);
            CHECK(it == const_container.find(key));
            if (it != const_container.end()) {
                CHECK(key_of(*it) == key);
                CHECK(std::next(it) == std::next(iterator_by_loop(const_container, key)));
                if constexpr (is_map_v<Container>) {
                    // the value before, plus 1 for every time the key is in `keys`
                    CHECK(it->second == values_before.at(key) + static_cast<std::size_t>(std::ranges::count(keys, key)));
                }
                ++const_hits;
            }
            ++const_calls;
        });
        CHECK(const_calls == keys.size());
        CHECK(const_hits == hits);
        return hits;
    }

    /// `missing_keys`, then `present`, then `missing_keys` and `present` again in reverse order
    [[nodiscard]] std::vector<std::size_t> lookup_keys(std::vector<std::size_t> const &present) {
        std::vector<std::size_t> keys(missing_keys.begin(), missing_keys.end());
        keys.insert(keys.end(), present.begin(), present.end());
        keys.insert(keys.end(), missing_keys.rbegin(), missing_keys.rend());
        keys.insert(keys.end(), present.rbegin(), present.rend());
        return keys;
    }

}  // namespace

TEST_CASE_TEMPLATE("find_each of an inline container looks the keys up in the inline group", container_t, inline_map_4, inline_map_4_offset_ptr, inline_map_32, inline_set_4, inline_set_4_offset_ptr, inline_set_32) {
    constexpr std::size_t capacity = inline_capacity_of_v<container_t>;

    SUBCASE("an empty inline container") {
        auto container = container_t();
        auto const keys = lookup_keys(filling_keys(capacity));
        CHECK(check_inline_find_each(container, keys) == 0);
        CHECK(container.empty());
        CHECK_FALSE(prefetches(container, keys));
    }

    SUBCASE("a full inline group") {
        auto container = container_t();
        auto const present = filling_keys(capacity);
        insert_keys(container, present);
        REQUIRE(container.size() == capacity);
        REQUIRE(elements_in_object(container));
        auto const keys = lookup_keys(present);
        CHECK(check_inline_find_each(container, keys) == 2 * capacity);
        CHECK(elements_in_object(container));
        CHECK_FALSE(prefetches(container, keys));
    }

    SUBCASE("an inline group with deleted buckets") {
        auto container = container_t();
        auto present = filling_keys(capacity);
        insert_keys(container, present);
        // `1` is the home bucket of `65` and `129`, which are outside their home bucket, so the bucket of `1` becomes a
        // deleted bucket that the lookups of `65` and `129` pass
        REQUIRE(container.erase(1) == 1);
        present.erase(std::ranges::find(present, std::size_t{1}));
        REQUIRE(elements_in_object(container));
        auto keys = lookup_keys(present);
        keys.push_back(1);
        CHECK(check_inline_find_each(container, keys) == 2 * (capacity - 1));
        // a new key with the home bucket 1 takes the deleted bucket
        insert_keys(container, std::vector<std::size_t>{321});
        CHECK(key_of(*container.begin()) == 321);
        present.push_back(321);
        REQUIRE(elements_in_object(container));
        CHECK(check_inline_find_each(container, lookup_keys(present)) == 2 * capacity);
    }

    SUBCASE("a container that moved out of the inline group and back") {
        auto container = container_t();
        auto present = filling_keys(capacity);
        present.push_back(1000);
        insert_keys(container, present);
        REQUIRE_FALSE(elements_in_object(container));
        CHECK(check_inline_find_each(container, lookup_keys(present)) == 2 * present.size());
        // fewer elements than the inline capacity, still in the allocated group
        for (std::size_t i = 0; i < 3; ++i) {
            REQUIRE(container.erase(present.back()) == 1);
            present.pop_back();
        }
        REQUIRE(container.size() < capacity);
        REQUIRE_FALSE(elements_in_object(container));
        CHECK(check_inline_find_each(container, lookup_keys(present)) == 2 * present.size());
        // `rehash(0)` of an empty container gives it the inline group again
        container.clear();
        container.rehash(0);
        CHECK(container.bucket_count() == 0);
        CHECK(check_inline_find_each(container, lookup_keys(present)) == 0);
        insert_keys(container, present);
        REQUIRE(elements_in_object(container));
        CHECK(check_inline_find_each(container, lookup_keys(present)) == 2 * present.size());
    }
}

TEST_CASE_MAP("find_each in a map of up to 3 elements gives what find gives", std::size_t, std::size_t) {
    // in the configurations with the inline capacity 4 the elements are inline
    auto map = map_t();
    std::vector<std::size_t> const keys{7, 1, 7, 2, 3, 1000, 0};
    for (std::size_t n = 0; n <= 3; ++n) {
        CAPTURE(n);
        if (n > 0) {
            map.try_emplace(n, n * 10);
        }
        if (inline_capacity_of_v<map_t> > 0 && n > 0) {
            CHECK(elements_in_object(map));
        }
        std::size_t calls = 0;
        std::size_t hits = 0;
        map.find_each(keys, [&](std::size_t const &key, typename map_t::iterator it) {
            REQUIRE(calls < keys.size());
            CHECK(&key == &keys[calls]);
            CHECK(it == map.find(key));
            if (it != map.end()) {
                CHECK(it->second == key * 10);
                CHECK(std::next(it) == std::next(iterator_by_loop(map, key)));
                ++hits;
            }
            ++calls;
        });
        CHECK(calls == keys.size());
        CHECK(hits == n);
    }
}

namespace {

    /**
     * An inline map in a constant evaluation: the inline group holds no element there, `find_each` of the empty map
     * looks the keys up in the empty inline group, and the first insertion allocates a group. At run time (the
     * `CHECK`), the 3 elements stay in the inline group.
     */
    constexpr int find_each_inline_constant() {
        auto map = dice::unordered_sparse::sparse_map<std::size_t, int, identity_hash, std::equal_to<std::size_t>, std::allocator<std::pair<std::size_t, int>>, dice::unordered_sparse::sparsity::medium, 4>();
        std::size_t const keys[] = {1, 2, 3, 4};
        int sum = 0;
        map.find_each(keys, [&](std::size_t, auto it) {
            sum += it == map.end() ? 1000 : it->second;
        });
        map.try_emplace(1, 1);
        map.try_emplace(2, 20);
        map.try_emplace(65, 300);
        map.find_each(keys, [&](std::size_t, auto it) {
            sum += it == map.end() ? 1000 : it->second;
        });
        return sum;
    }

}  // namespace

TEST_CASE("find_each of an inline map in a constant expression") {
    // 4 misses in the empty map, then 1 + 20 and 2 misses
    constexpr int expected = 4000 + 1 + 20 + 2000;
    static_assert(find_each_inline_constant() == expected);
    CHECK(find_each_inline_constant() == expected);
}

namespace {

    /// true if `container.find_each(keys, f)` is a valid call
    template<typename Container, typename R, typename F>
    concept CanFindEach = requires (Container &container, R const &keys, F &f) { container.find_each(keys, f); };

    /// true if `std::move(container).find_each(keys, f)` is a valid call, with the container as an rvalue
    template<typename Container, typename R, typename F>
    concept CanFindEachOnRvalue = requires (Container &container, R const &keys, F &f) { std::move(container).find_each(keys, f); };

    /// true if `container.find_each(keys, F{})` is a valid call, with `f` as a prvalue
    template<typename Container, typename R, typename F>
    concept CanFindEachWithPrvalue = requires (Container &container, R const &keys) { container.find_each(keys, F{}); };

    /// a function object that can be called only with a `std::size_t` key and an iterator of type `Iterator`
    template<typename Iterator>
    struct key_and_iterator_function {
        void operator()(std::size_t const &, Iterator) const {
        }
    };

    /**
     * A function object with a deduced result type whose body compiles only with an iterator of type `Iterator`: it
     * stores the iterator in a `std::vector<Iterator>`. A constraint that checks a call with another iterator type
     * compiles the body with it and does not compile.
     */
    template<typename Iterator>
    struct iterator_only_function {
        template<typename K, typename I>
        auto operator()(K const &, I it) const {
            std::vector<Iterator> iterators;
            iterators.push_back(it);
        }
    };

    /// a function object that can be called only with a key, without an iterator
    struct key_only_function {
        void operator()(std::size_t const &) const {
        }
    };

    /// a function object that needs a third argument
    struct three_arguments_function {
        template<typename Iterator>
        void operator()(std::size_t const &, Iterator, int) const {
        }
    };

    /// a function object that can be called only with a `std::string` key, and any iterator
    struct string_key_function {
        template<typename Iterator>
        void operator()(std::string const &, Iterator) const {
        }
    };

    /// a function object that can be called with any key and any iterator, but only as an rvalue
    struct rvalue_only_find_function {
        template<typename K, typename Iterator>
        void operator()(K const &, Iterator) && {
        }
    };

    /// a function object that can be called with any key and any iterator, as an lvalue and as an rvalue
    struct any_find_function {
        template<typename K, typename Iterator>
        void operator()(K const &, Iterator) const {
        }
    };

    /**
     * A function object for `find_each` that counts its calls, and the calls that got the key exactly as `Key` and
     * the iterator exactly as `Iterator`, both as the constraint of `find_each` names them (`Key` is
     * `std::ranges::range_reference_t` of the range). A key or an iterator of another type or another value category is
     * not counted in `exact`.
     */
    template<typename Key, typename Iterator>
    struct exact_arguments_function {
        std::size_t *calls;
        std::size_t *exact;

        template<typename K, typename I>
        void operator()(K &&, I &&) const {
            ++*calls;
            if constexpr (std::is_same_v<K &&, Key &&> && std::is_same_v<I &&, Iterator &&>) {
                ++*exact;
            }
        }
    };

    /**
     * Calls `find_each` of `container` with `keys` as lvalues and with a view that gives them as prvalues, and of
     * `container` as an rvalue with `keys` as lvalues, with an `exact_arguments_function` that expects the iterator
     * type of the call: `const_iterator` for a const container, `iterator` otherwise. Checks that every call got
     * exactly these argument types.
     */
    template<typename Container, typename Key>
    void check_exact_arguments(Container &container, std::vector<Key> const &keys) {
        using iterator_type = std::conditional_t<std::is_const_v<Container>, typename Container::const_iterator, typename Container::iterator>;
        std::size_t calls = 0;
        std::size_t exact = 0;
        container.find_each(keys, exact_arguments_function<Key const &, iterator_type>{&calls, &exact});
        CHECK(calls == keys.size());
        CHECK(exact == keys.size());

        auto const view = keys | std::views::transform([](Key const &key) {
                              return key;
                          });
        calls = 0;
        exact = 0;
        container.find_each(view, exact_arguments_function<Key, iterator_type>{&calls, &exact});
        CHECK(calls == keys.size());
        CHECK(exact == keys.size());

        // the container as an rvalue: `std::move` only casts, `find_each` does not move from the container
        calls = 0;
        exact = 0;
        std::move(container).find_each(keys, exact_arguments_function<Key const &, iterator_type>{&calls, &exact});
        CHECK(calls == keys.size());
        CHECK(exact == keys.size());
    }

    /// a map type with its own member `ht_`, which hides the member of `sparse_map` with this name
    struct map_with_own_ht : dice::sparse_map<std::size_t, std::size_t> {
        int ht_ = 0;
    };

    /// a set type with its own member `ht_`, which hides the member of `sparse_set` with this name
    struct set_with_own_ht : dice::sparse_set<std::size_t> {
        int ht_ = 0;
    };

    /// a type with the set as a private base, which calls `find_each` through a `static_cast`
    struct find_each_private_set_base : private dice::sparse_set<std::size_t> {
        using sparse_set::insert;

        /// @return the keys of `keys` that are in the set, in the order of `keys`, from the set that is not const
        [[nodiscard]] std::vector<std::size_t> found(std::vector<std::size_t> const &keys) {
            std::vector<std::size_t> result;
            static_cast<sparse_set &>(*this).find_each(keys, [this, &result](std::size_t const &, auto it) {
                static_assert(std::is_same_v<decltype(it), sparse_set::iterator>);
                if (it != end()) {
                    result.push_back(*it);
                }
            });
            return result;
        }

        /// @return the keys of `keys` that are in the set, in the order of `keys`, from the const set
        [[nodiscard]] std::vector<std::size_t> found(std::vector<std::size_t> const &keys) const {
            std::vector<std::size_t> result;
            static_cast<sparse_set const &>(*this).find_each(keys, [this, &result](std::size_t const &, auto it) {
                static_assert(std::is_same_v<decltype(it), sparse_set::const_iterator>);
                if (it != end()) {
                    result.push_back(*it);
                }
            });
            return result;
        }
    };

    /**
     * Inserts the keys 1 and 3 into `container`, which is not const, and calls its `find_each` for the keys 1, 2 and 3
     * with an `f` whose result type is deduced and whose body compiles only with `iterator`: it stores the iterators
     * of the keys that are found in a `std::vector<iterator>`. Checks the stored iterators.
     */
    template<typename Container>
    void check_iterator_only_f(Container &container) {
        insert_keys(container, {1, 3});
        std::vector<typename Container::iterator> found;
        container.find_each(std::vector<std::size_t>{1, 2, 3}, [&container, &found](std::size_t const &, auto it) {
            if (it != container.end()) {
                found.push_back(it);
            }
        });
        REQUIRE(found.size() == 2);
        CHECK(key_of(*found[0]) == 1);
        CHECK(key_of(*found[1]) == 3);
        CHECK(found[0] == container.find(1));
        CHECK(found[1] == container.find(3));
    }

    /// a type with the map as a private base, which calls `find_each` through a `static_cast`
    struct find_each_private_base : private dice::sparse_map<std::size_t, std::size_t> {
        using sparse_map::try_emplace;

        /// adds 1 to the mapped value of every key of `keys` that is in the map
        void add_one(std::vector<std::size_t> const &keys) {
            static_cast<sparse_map &>(*this).find_each(keys, [this](std::size_t const &, auto it) {
                if (it != end()) {
                    it->second += 1;
                }
            });
        }

        /// @return the sum of the mapped values of the keys of `keys` that are in the map
        [[nodiscard]] std::size_t sum(std::vector<std::size_t> const &keys) const {
            std::size_t sum = 0;
            static_cast<sparse_map const &>(*this).find_each(keys, [this, &sum](std::size_t const &, auto it) {
                if (it != end()) {
                    sum += it->second;
                }
            });
            return sum;
        }
    };

}  // namespace

TEST_CASE("find_each takes an f that can be called with the key and the iterator of the call, and no other f") {
    using map_type = dice::sparse_map<std::size_t, std::size_t>;
    using set_type = dice::sparse_set<std::size_t>;
    using keys_type = std::vector<std::size_t>;

    static_assert(CanFindEach<map_type, keys_type, any_find_function>);
    static_assert(CanFindEach<map_type const, keys_type, any_find_function>);
    static_assert(CanFindEach<set_type, keys_type, any_find_function>);
    static_assert(CanFindEach<set_type const, keys_type, any_find_function>);

    // `f` must take two arguments
    static_assert(!CanFindEach<map_type, keys_type, key_only_function>);
    static_assert(!CanFindEach<map_type const, keys_type, key_only_function>);
    static_assert(!CanFindEach<set_type, keys_type, key_only_function>);
    static_assert(!CanFindEach<set_type const, keys_type, key_only_function>);
    static_assert(!CanFindEach<map_type, keys_type, three_arguments_function>);
    static_assert(!CanFindEach<set_type const, keys_type, three_arguments_function>);

    // `f` must take the key as the range gives it
    static_assert(!CanFindEach<map_type, keys_type, string_key_function>);
    static_assert(!CanFindEach<map_type const, keys_type, string_key_function>);
    static_assert(!CanFindEach<set_type, keys_type, string_key_function>);
    static_assert(!CanFindEach<set_type const, keys_type, string_key_function>);
    static_assert(CanFindEach<dice::sparse_map<std::string, std::size_t>, std::vector<std::string>, string_key_function>);

    // a container that is not const gives `iterator`, a const container gives `const_iterator`, which an `iterator`
    // converts to
    static_assert(CanFindEach<map_type, keys_type, key_and_iterator_function<map_type::iterator>>);
    static_assert(!CanFindEach<map_type const, keys_type, key_and_iterator_function<map_type::iterator>>);
    static_assert(CanFindEach<map_type const, keys_type, key_and_iterator_function<map_type::const_iterator>>);
    static_assert(CanFindEach<map_type, keys_type, key_and_iterator_function<map_type::const_iterator>>);
    static_assert(CanFindEach<set_type, keys_type, key_and_iterator_function<set_type::iterator>>);
    static_assert(!CanFindEach<set_type const, keys_type, key_and_iterator_function<set_type::iterator>>);
    static_assert(CanFindEach<set_type const, keys_type, key_and_iterator_function<set_type::const_iterator>>);
    static_assert(CanFindEach<set_type, keys_type, key_and_iterator_function<set_type::const_iterator>>);

    // `find_each` calls `f` as an lvalue, also when the caller passes a prvalue
    static_assert(!CanFindEachWithPrvalue<map_type, keys_type, rvalue_only_find_function>);
    static_assert(!CanFindEachWithPrvalue<map_type const, keys_type, rvalue_only_find_function>);
    static_assert(!CanFindEachWithPrvalue<set_type, keys_type, rvalue_only_find_function>);
    static_assert(!CanFindEachWithPrvalue<set_type const, keys_type, rvalue_only_find_function>);
    static_assert(CanFindEachWithPrvalue<map_type, keys_type, any_find_function>);
    static_assert(CanFindEachWithPrvalue<set_type const, keys_type, any_find_function>);

    // the constraint is checked only with the iterator type of the call: an `f` with a deduced result type whose body
    // compiles only with `iterator` works on a container that is not const
    static_assert(CanFindEach<map_type, keys_type, iterator_only_function<map_type::iterator>>);
    static_assert(CanFindEach<set_type, keys_type, iterator_only_function<set_type::iterator>>);
    static_assert(CanFindEach<set_with_own_ht, keys_type, iterator_only_function<set_type::iterator>>);

    // an rvalue container gives the iterator type of its constness
    static_assert(CanFindEachOnRvalue<map_type, keys_type, key_and_iterator_function<map_type::iterator>>);
    static_assert(!CanFindEachOnRvalue<map_type const, keys_type, key_and_iterator_function<map_type::iterator>>);
    static_assert(CanFindEachOnRvalue<map_type const, keys_type, key_and_iterator_function<map_type::const_iterator>>);
    static_assert(CanFindEachOnRvalue<set_type, keys_type, key_and_iterator_function<set_type::iterator>>);
    static_assert(!CanFindEachOnRvalue<set_type const, keys_type, key_and_iterator_function<set_type::iterator>>);
    static_assert(CanFindEachOnRvalue<set_type const, keys_type, key_and_iterator_function<set_type::const_iterator>>);
    static_assert(!CanFindEachOnRvalue<set_type, keys_type, key_only_function>);
    static_assert(!CanFindEachOnRvalue<set_type const, keys_type, string_key_function>);

    // the same for a derived set, whose member `ht_` hides the one of `sparse_set`
    static_assert(CanFindEach<set_with_own_ht, keys_type, key_and_iterator_function<set_type::iterator>>);
    static_assert(!CanFindEach<set_with_own_ht const, keys_type, key_and_iterator_function<set_type::iterator>>);
    static_assert(CanFindEach<set_with_own_ht const, keys_type, key_and_iterator_function<set_type::const_iterator>>);
    static_assert(!CanFindEach<set_with_own_ht, keys_type, key_only_function>);
    static_assert(!CanFindEach<set_with_own_ht const, keys_type, string_key_function>);
}

TEST_CASE("find_each of a map works on a derived map, on an rvalue map and through a private base") {
    auto const keys = std::vector<std::size_t>{1, 2, 3};

    // the member `ht_` of the derived type does not hide the one of `sparse_map`
    map_with_own_ht map;
    map.try_emplace(1, 10);
    map.try_emplace(3, 30);
    map.find_each(keys, [&map](std::size_t const &, auto it) {
        if (it != map.end()) {
            it->second += 1;
        }
    });
    CHECK(map.at(1) == 11);
    CHECK(map.at(3) == 31);
    std::size_t sum = 0;
    std::as_const(map).find_each(keys, [&](std::size_t const &, auto it) {
        static_assert(std::is_same_v<decltype(it), map_with_own_ht::const_iterator>);
        if (it != map.cend()) {
            sum += it->second;
        }
    });
    CHECK(sum == 42);

    std::size_t calls = 0;
    dice::sparse_map<std::size_t, std::size_t>{{1, 2}, {3, 4}}.find_each(keys, [&calls](std::size_t const &, [[maybe_unused]] auto it) {
        static_assert(std::is_same_v<decltype(it), dice::sparse_map<std::size_t, std::size_t>::iterator>);
        ++calls;
    });
    CHECK(calls == 3);

    find_each_private_base base;
    base.try_emplace(2, 5);
    base.add_one(keys);
    CHECK(base.sum(keys) == 6);
}

TEST_CASE("find_each of a set works on a derived set, on an rvalue set and through a private base") {
    auto const keys = std::vector<std::size_t>{1, 2, 3};

    // the member `ht_` of the derived type does not hide the one of `sparse_set`
    set_with_own_ht set;
    set.insert(1);
    set.insert(3);
    std::vector<set_with_own_ht::iterator> found;
    set.find_each(keys, [&set, &found](std::size_t const &, auto it) {
        if (it != set.end()) {
            found.push_back(it);
        }
    });
    REQUIRE(found.size() == 2);
    CHECK(*found[0] == 1);
    CHECK(*found[1] == 3);
    std::size_t hits = 0;
    std::as_const(set).find_each(keys, [&set, &hits](std::size_t const &, auto it) {
        static_assert(std::is_same_v<decltype(it), set_with_own_ht::const_iterator>);
        if (it != set.cend()) {
            ++hits;
        }
    });
    CHECK(hits == 2);
    check_exact_arguments(set, keys);
    check_exact_arguments(std::as_const(set), keys);

    std::size_t calls = 0;
    dice::sparse_set<std::size_t>{1, 3}.find_each(keys, [&calls](std::size_t const &, [[maybe_unused]] auto it) {
        static_assert(std::is_same_v<decltype(it), dice::sparse_set<std::size_t>::iterator>);
        ++calls;
    });
    CHECK(calls == 3);

    find_each_private_set_base base;
    base.insert(2);
    CHECK(base.found(keys) == std::vector<std::size_t>{2});
    CHECK(std::as_const(base).found(keys) == std::vector<std::size_t>{2});
}

TEST_CASE_SET("find_each of a set that is not const takes an f with a deduced result type that compiles only with iterator", std::size_t) {
    auto set = set_t();
    check_iterator_only_f(set);
}

TEST_CASE_MAP("find_each of a map that is not const takes an f with a deduced result type that compiles only with iterator", std::size_t, std::size_t) {
    auto map = map_t();
    check_iterator_only_f(map);
}

TEST_CASE_MAP("find_each of a map calls f with exactly the key type of the range and the iterator type of the call", std::size_t, std::size_t) {
    auto const keys = std::vector<std::size_t>{0, 1, 2, 3, 4, 5, 6};
    auto map = map_t();
    SUBCASE("3 elements: the inline group in the inline configurations, the loop of find in the others") {
        for (std::size_t key = 0; key < 3; ++key) {
            map.try_emplace(key, key);
        }
        CHECK(elements_in_object(map) == (inline_capacity_of_v<map_t> > 0));
    }
    SUBCASE("1000 elements: the loop of find") {
        for (std::size_t key = 0; key < 1000; ++key) {
            map.try_emplace(key, key);
        }
        CHECK_FALSE(elements_in_object(map));
        CHECK_FALSE(prefetches(map, keys));
    }
    SUBCASE("a map large enough for prefetching: the pipeline") {
        map.reserve(pipeline_reserve);
        for (std::size_t key = 0; key < 3; ++key) {
            map.try_emplace(key, key);
        }
        CHECK(prefetches(map, keys));
    }
    check_exact_arguments(map, keys);
    check_exact_arguments(std::as_const(map), keys);
}

TEST_CASE_MAP("find_each of a map of std::string keys calls f with exactly the key type of the range and the iterator type of the call", std::string, std::size_t) {
    auto const keys = std::vector<std::string>{"0", "1", "2", "3", "4"};
    auto map = map_t();
    SUBCASE("3 elements: the inline group in the inline configurations, the loop of find in the others") {
        for (std::size_t key = 0; key < 3; ++key) {
            map.try_emplace(std::to_string(key), key);
        }
        CHECK(elements_in_object(map) == (inline_capacity_of_v<map_t> > 0));
    }
    SUBCASE("a map large enough for prefetching: the blocks") {
        map.reserve(blocks_reserve);
        for (std::size_t key = 0; key < 3; ++key) {
            map.try_emplace(std::to_string(key), key);
        }
        CHECK(prefetches(map, keys));
    }
    check_exact_arguments(map, keys);
    check_exact_arguments(std::as_const(map), keys);
}

TEST_CASE_SET("find_each of a set calls f with exactly the key type of the range and the iterator type of the call", std::size_t) {
    auto const keys = std::vector<std::size_t>{0, 1, 2, 3, 4, 5, 6};
    auto set = set_t();
    SUBCASE("3 keys: the inline group in the inline configurations, the loop of find in the others") {
        for (std::size_t key = 0; key < 3; ++key) {
            set.insert(key);
        }
        CHECK(elements_in_object(set) == (inline_capacity_of_v<set_t> > 0));
    }
    SUBCASE("a set large enough for prefetching: the pipeline") {
        set.reserve(pipeline_reserve);
        for (std::size_t key = 0; key < 3; ++key) {
            set.insert(key);
        }
        CHECK(prefetches(set, keys));
    }
    check_exact_arguments(set, keys);
    check_exact_arguments(std::as_const(set), keys);
}

TEST_CASE_SET("find_each of a set of std::string keys calls f with exactly the key type of the range and the iterator type of the call", std::string) {
    auto const keys = std::vector<std::string>{"0", "1", "2", "3", "4"};
    auto set = set_t();
    SUBCASE("3 keys: the inline group in the inline configurations, the loop of find in the others") {
        for (std::size_t key = 0; key < 3; ++key) {
            set.insert(std::to_string(key));
        }
        CHECK(elements_in_object(set) == (inline_capacity_of_v<set_t> > 0));
    }
    SUBCASE("a set large enough for prefetching: the blocks") {
        set.reserve(blocks_reserve);
        for (std::size_t key = 0; key < 3; ++key) {
            set.insert(std::to_string(key));
        }
        CHECK(prefetches(set, keys));
    }
    check_exact_arguments(set, keys);
    check_exact_arguments(std::as_const(set), keys);
}
