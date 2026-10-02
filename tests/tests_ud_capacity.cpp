// Ported from the unit tests load_factor, max, rehash, reserve and reserve_and_assign of ankerl::unordered_dense (MIT license).
#include "fixtures/checksum.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <string>

using namespace dice::unordered_sparse::tests;

// load_factor
//
// sparse_map clamps the max load factor to [0.1, 0.8], and the default is 0.5.

TEST_CASE_MAP("load_factor", int, int) {
    auto m = map_t();

    REQUIRE(static_cast<double>(m.load_factor()) == doctest::Approx(0.0));
    REQUIRE(m.max_load_factor() == 0.5F);

    for (int i = 0; i < 10000; ++i) {
        m.emplace(i, i);
        REQUIRE(m.load_factor() > 0.0F);
        REQUIRE(m.load_factor() <= m.max_load_factor());
    }
}

// A max load factor that the caller sets has to be the one the table grows by, for every new bucket array and not
// only for the first one.
TEST_CASE_MAP("max_load_factor_is_honoured_through_growth", int, int) {
    for (float const requested : {0.25F, 0.5F, 0.75F}) {
        auto m = map_t();
        m.max_load_factor(requested);
        REQUIRE(m.max_load_factor() == requested);

        for (int i = 0; i < 20000; ++i) {
            m.emplace(i, i);
            REQUIRE(m.load_factor() <= requested);
        }

        // The same in terms of what was allocated: a table holding n elements at load factor f needs at least n / f
        // buckets.
        REQUIRE(static_cast<float>(m.bucket_count()) >= static_cast<float>(m.size()) / requested);
        REQUIRE(m.max_load_factor() == requested);
    }
}

// A max load factor outside of [0.1, 0.8] is clamped to the nearest bound. With 0.8 the table is used up to its
// highest load at every size, and it has to survive erasing too.
TEST_CASE_MAP("max_load_factor_is_clamped", int, int) {
    auto m = map_t();
    m.max_load_factor(2.0F);
    REQUIRE(m.max_load_factor() == 0.8F);

    for (int i = 0; i < 10000; ++i) {
        m.emplace(i, i);
        REQUIRE(m.load_factor() <= 0.8F);
    }
    REQUIRE(m.size() == 10000U);
    for (int i = 0; i < 10000; ++i) {
        REQUIRE(m.find(i) != m.end());
    }

    for (int i = 0; i < 10000; i += 2) {
        REQUIRE(m.erase(i) == 1U);
    }
    REQUIRE(m.size() == 5000U);
    for (int i = 1; i < 10000; i += 2) {
        REQUIRE(m.find(i) != m.end());
    }

    // The lower bound: the next insert grows the table until the load is at most 0.1.
    m.max_load_factor(0.0F);
    REQUIRE(m.max_load_factor() == 0.1F);
    m.emplace(10000, 10000);
    REQUIRE(m.load_factor() <= 0.1F);
    REQUIRE(m.size() == 5001U);
    for (int i = 1; i < 10000; i += 2) {
        REQUIRE(m.find(i) != m.end());
    }
}

// max_load_factor() also recomputes the size at which the table counts as full. Without that, the new setting would
// only take effect with the next bucket array. Setting it on a populated table tells: a load factor that the table is
// already over has to make the next insert grow it.
TEST_CASE_MAP("lowering_max_load_factor_applies_to_an_existing_bucket_array", int, int) {
    auto map = map_t();
    map.reserve(100);
    for (int i = 0; i < 50; ++i) {
        map.try_emplace(i, i);
    }
    auto const before = map.bucket_count();
    REQUIRE(map.load_factor() < map.max_load_factor());  // nothing is due to grow

    map.max_load_factor(0.1F);
    REQUIRE(map.max_load_factor() == 0.1F);

    map.try_emplace(1000, 1000);
    REQUIRE(map.bucket_count() > before);

    // and the table still holds everything it did
    REQUIRE(map.size() == 51);
    for (int i = 0; i < 50; ++i) {
        REQUIRE(map.find(i) != map.end());
    }
}

// max
//
// On a map without buckets, the first insert grows the table until one element fits, and with a lower max load factor
// that takes more buckets. So both maps start from the same explicit bucket count.

TEST_CASE_MAP("max_load_factor", int, int) {
    auto map_40 = map_t(64);
    auto map_80 = map_t(64);
    REQUIRE(map_40.max_load_factor() == doctest::Approx(0.5));
    map_40.max_load_factor(0.4F);
    map_80.max_load_factor(0.8F);
    REQUIRE(map_40.max_load_factor() == doctest::Approx(0.4));
    REQUIRE(map_80.max_load_factor() == doctest::Approx(0.8));

    map_40[0];
    map_80[0];
    REQUIRE(map_40.bucket_count() == map_80.bucket_count());

    auto const old_bucket_count = map_40.bucket_count();

    // fill up the maps until bucket_count increases
    int i = 0;
    while (map_40.bucket_count() == old_bucket_count) {
        map_40[++i];
    }
    while (map_80.bucket_count() == old_bucket_count) {
        map_80[++i];
    }

    // a max load factor of 0.8 fits more than one of 0.4
    REQUIRE(map_80.size() > map_40.size());

    map_40.max_load_factor(0.8F);
    REQUIRE(map_40.max_load_factor() == map_80.max_load_factor());
}

TEST_CASE_MAP("key_eq", int, int) {
    auto const map = map_t();
    auto eq = map.key_eq();
    REQUIRE(eq(5, 5));
}

// rehash
//
// rehash(0) gives the smallest bucket count that holds the elements at the max load factor. clear() keeps the bucket
// array, and rehash(0) on an empty map releases it: the bucket count is 0, like the one of a default constructed map.

TEST_CASE_MAP("rehash", std::size_t, int) {
    auto map = map_t();

    for (std::size_t i = 0; i < 1000; ++i) {
        map[i];
    }
    auto old_bucket_size = map.bucket_count();

    map.rehash(10000);
    REQUIRE(map.bucket_count() >= 10000);
    map.rehash(0);
    REQUIRE(map.bucket_count() == old_bucket_size);

    map.clear();
    REQUIRE(map.bucket_count() == old_bucket_size);
    map.rehash(0);
    REQUIRE(map.bucket_count() == 0);

    // and the map still works
    map[7] = 8;
    REQUIRE(map.size() == 1);
    REQUIRE(map.at(7) == 8);
}

// reserve
//
// sparse_map has no `values()`, so this checks the contract of reserve() through the bucket array: after
// `reserve(n)`, n elements fit without a rehash.

TEST_CASE_MAP("reserve", int, int) {
    auto map = map_t();
    map.reserve(1000);
    REQUIRE(map.size() == 0U);
    auto const buckets = map.bucket_count();
    REQUIRE(static_cast<float>(buckets) * map.max_load_factor() >= 1000.0F);

    for (int i = 0; i < 1000; ++i) {
        map.try_emplace(i, i);
    }
    REQUIRE(map.size() == 1000U);
    REQUIRE(map.bucket_count() == buckets);
}

// reserve_and_assign

TEST_CASE_MAP("reserve_and_assign", std::string, std::uint64_t) {
    map_t a = {
        {"button", {}},
        {"selectbox-tr", {}},
        {"slidertrack-t", {}},
        {"sliderarrowinc-hover", {}},
        {"text-l", {}},
        {"title-bar-l", {}},
        {"checkbox-checked-hover", {}},
        {"datagridexpand-active", {}},
        {"icon-waves", {}},
        {"sliderarrowdec-hover", {}},
        {"datagridexpand-collapsed", {}},
        {"sliderarrowinc-active", {}},
        {"radio-active", {}},
        {"radio-checked", {}},
        {"selectvalue-hover", {}},
        {"huditem-l", {}},
        {"datagridexpand-collapsed-active", {}},
        {"slidertrack-b", {}},
        {"selectarrow-hover", {}},
        {"window-r", {}},
        {"selectbox-tl", {}},
        {"icon-score", {}},
        {"datagridheader-r", {}},
        {"icon-game", {}},
        {"sliderbar-c", {}},
        {"window-c", {}},
        {"datagridexpand-hover", {}},
        {"button-hover", {}},
        {"icon-hiscore", {}},
        {"sliderbar-hover-t", {}},
        {"sliderbar-hover-c", {}},
        {"selectarrow-active", {}},
        {"window-tl", {}},
        {"checkbox-active", {}},
        {"sliderarrowdec-active", {}},
        {"sliderbar-active-b", {}},
        {"sliderarrowdec", {}},
        {"window-bl", {}},
        {"datagridheader-l", {}},
        {"sliderbar-t", {}},
        {"sliderbar-active-t", {}},
        {"text-c", {}},
        {"window-br", {}},
        {"huditem-c", {}},
        {"selectbox-l", {}},
        {"icon-flag", {}},
        {"sliderbar-hover-b", {}},
        {"icon-help", {}},
        {"selectvalue", {}},
        {"title-bar-r", {}},
        {"sliderbar-active-c", {}},
        {"huditem-r", {}},
        {"radio-checked-active", {}},
        {"selectbox-c", {}},
        {"selectbox-bl", {}},
        {"icon-invader", {}},
        {"checkbox-checked-active", {}},
        {"slidertrack-c", {}},
        {"sliderarrowinc", {}},
        {"checkbox", {}},
    };

    map_t b;
    b = a;
    REQUIRE(b.find("button") != b.end());

    map_t c;
    c.reserve(51);
    c = a;
    REQUIRE(checksum::map(a) == checksum::map(c));
    REQUIRE(c.find("button") != c.end());
}

TEST_CASE_MAP("reserve_only", std::string, std::uint64_t) {
    auto map = map_t();
    map.reserve(51);
    REQUIRE(map.size() == 0U);
    REQUIRE(static_cast<float>(map.bucket_count()) * map.max_load_factor() >= 51.0F);
}
