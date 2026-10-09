// Ported from the unit tests erase, erase_range and erase_churn of ankerl::unordered_dense (MIT license).
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <unordered_set>
#include <vector>

using namespace dice::sparse_map::tests;

namespace {

    template<typename A, typename B>
    [[nodiscard]] bool is_eq(A const &a, B const &b) {
        if (a.size() != b.size()) {
            return false;
        }
        return std::all_of(a.begin(), a.end(), [&b](auto const &k) { return b.end() != b.find(k); });
    }

} // namespace

// erase

TEST_CASE_SET("insert_erase_random", std::uint32_t) {
    auto uds = set_t();
    auto us = std::unordered_set<std::uint32_t>();
    auto rng = ankerl::nanobench::Rng(123);
    for (std::size_t i = 0; i < 10000; ++i) {
        auto key = rng.bounded(1000);
        uds.insert(key);
        us.insert(key);
        REQUIRE(uds.size() == us.size());

        key = rng.bounded(1000);
        REQUIRE(uds.erase(key) == us.erase(key));
        REQUIRE(uds.size() == us.size());
    }
    REQUIRE(is_eq(uds, us));
    auto k = *uds.begin();
    uds.erase(k);
    REQUIRE(!is_eq(uds, us));
    us.erase(k);
    REQUIRE(is_eq(uds, us));
}

TEST_CASE_MAP("erase", int, int) {
    auto map = map_t();
    REQUIRE(0 == map.erase(123));
    REQUIRE(0 == map.count(0));
}

// erase_range
//
// sparse_map has forward iterators, so the bounds of the range are reached with `std::next`. Erasing does not move
// the remaining elements in the iteration order, so the returned iterator is at the position of the first erased
// element.

TEST_CASE_MAP("erase_range", counter::obj, counter::obj) {
    std::size_t const num_elements = 10;

    for (std::size_t first_pos = 0; first_pos <= num_elements; ++first_pos) {
        for (std::size_t last_pos = first_pos; last_pos <= num_elements; ++last_pos) {
            auto counts = counter();
            INFO(counts);

            auto map = map_t();

            for (std::size_t i = 0; i < num_elements; ++i) {
                auto key = i;
                auto val = i * 1000;
                map.try_emplace({key, counts}, val, counts);
            }
            REQUIRE(map.size() == num_elements);

            auto const first = std::next(map.cbegin(), static_cast<std::ptrdiff_t>(first_pos));
            auto const last = std::next(map.cbegin(), static_cast<std::ptrdiff_t>(last_pos));
            auto erased_keys = std::vector<std::size_t>();
            for (auto it = first; it != last; ++it) {
                erased_keys.push_back(it->first.get());
            }

            auto it_ret = map.erase(first, last);
            REQUIRE(map.size() == num_elements - (last_pos - first_pos));
            REQUIRE(it_ret == std::next(map.begin(), static_cast<std::ptrdiff_t>(first_pos)));
            for (auto const key : erased_keys) {
                REQUIRE(map.count(counter::obj{key, counts}) == 0);
            }
        }
    }
}

// erase_churn
//
// Insert and erase at a fixed size, for a long time, without growth. erase() leaves a deleted marker in the bucket,
// and a lookup has to probe past such markers. An insert reuses the first deleted bucket on its probe sequence, and
// when the elements and the deleted markers together reach a threshold, the next insert rehashes the table into a
// bucket array of the same size, which removes all markers. A lookup that stops at a marker, or an insert that loses
// track of the markers, makes a live key impossible to find.
//
// So: reserve, so the bucket count is fixed and checked to stay so, fill to just under the load at which the table
// grows, then churn, and every so often look up every live key.
TEST_CASE_MAP("erase_churn_keeps_every_live_key_findable", std::uint64_t, std::uint64_t) {
    // splitmix64: random 64 bit keys
    std::uint64_t state = 0x9E3779B97F4A7C15;
    auto random_key = [&state] {
        state += 0x9E3779B97F4A7C15;
        auto z = state;
        z = (z ^ (z >> 30U)) * 0xBF58476D1CE4E5B9;
        z = (z ^ (z >> 27U)) * 0x94D049BB133111EB;
        return z ^ (z >> 31U);
    };

    auto map = map_t();
    map.reserve(4000);
    auto const buckets = map.bucket_count();
    // the number of elements at which the next insert grows the table
    auto const max_size = static_cast<std::size_t>(static_cast<float>(buckets) * map.max_load_factor());
    REQUIRE(max_size >= 4000U);

    std::vector<std::uint64_t> live;
    while (live.size() < max_size - 8) {
        auto const key = random_key();
        if (map.try_emplace(key, ~key).second) {
            live.push_back(key);
        }
    }
    REQUIRE(map.bucket_count() == buckets);

    constexpr std::size_t cycles = 200000;
    constexpr std::size_t check_every = 8192;
    for (std::size_t cycle = 0; cycle < cycles; ++cycle) {
        // erase one live key at random, then insert a new one, so size and load stay put
        auto const victim = static_cast<std::size_t>(random_key() % live.size());
        auto const gone = live[victim];
        REQUIRE(map.erase(gone) == 1U);
        live[victim] = live.back();
        live.pop_back();

        auto const fresh = random_key();
        REQUIRE(map.try_emplace(fresh, ~fresh).second);
        live.push_back(fresh);

        if (cycle % check_every == check_every - 1) {
            INFO("after " << cycle + 1 << " cycles");
            REQUIRE(map.size() == live.size());
            REQUIRE(map.bucket_count() == buckets); // no growth
            for (auto const key : live) {
                auto it = map.find(key);
                REQUIRE(it != map.end());
                REQUIRE(it->second == ~key);
            }
            REQUIRE(map.find(gone) == map.end());
        }
    }

    // Then empty it one element at a time by iterator.
    while (!map.empty()) {
        map.erase(map.begin());
    }
    REQUIRE(map.size() == 0U);
}
