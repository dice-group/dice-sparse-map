// Ported from the unit test multiple_apis of ankerl::unordered_dense (MIT license).
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <cstddef>
#include <cstdint>
#include <unordered_map>
#include <utility>

using namespace dice::unordered_sparse::tests;

// The random part runs with a new seed every time. The seed is printed when a check fails, and
// `ankerl::nanobench::Rng(x, y)` repeats the run.
TEST_CASE_MAP("multiple_different_APIs" * doctest::test_suite("stochastic"), counter::obj, counter::obj) {
    counter counts;
    INFO(counts);

    map_t map;
    REQUIRE(map.size() == static_cast<std::size_t>(0));
    std::pair<typename map_t::iterator, bool> it_outer = map.insert(typename map_t::value_type{{32145, counts}, {123, counts}});
    REQUIRE(it_outer.second);
    REQUIRE(it_outer.first->first.get() == 32145);
    REQUIRE(it_outer.first->second.get() == 123);
    REQUIRE(map.size() == 1);

    std::size_t const times = 10000;
    for (std::size_t i = 0; i < times; ++i) {
        INFO(i);
        std::pair<typename map_t::iterator, bool> it_inner = map.insert(typename map_t::value_type({i * 4U, counts}, {i, counts}));

        REQUIRE(it_inner.second);
        REQUIRE(it_inner.first->first.get() == i * 4);
        REQUIRE(it_inner.first->second.get() == i);

        auto found = map.find(counter::obj{i * 4, counts});
        REQUIRE(map.end() != found);
        REQUIRE(found->second.get() == i);
        REQUIRE(map.size() == 2 + i);
    }

    // check if everything can be found
    for (std::size_t i = 0; i < times; ++i) {
        auto found = map.find(counter::obj{i * 4, counts});
        REQUIRE(map.end() != found);
        REQUIRE(found->second.get() == i);
        REQUIRE(found->first.get() == i * 4);
    }

    // check non-elements
    for (std::size_t i = 0; i < times; ++i) {
        auto found = map.find(counter::obj{(i + times) * 4U, counts});
        REQUIRE(map.end() == found);
    }

    // random test against std::unordered_map
    map.clear();
    std::unordered_map<std::uint64_t, std::uint64_t> uo;

    auto rng = ankerl::nanobench::Rng();
    auto const seed = rng.state();
    INFO("seed=" << seed[0] << ", " << seed[1]);

    auto const bound = static_cast<std::uint32_t>(times / 4);
    for (std::uint64_t i = 0; i < times; ++i) {
        auto r = static_cast<std::size_t>(rng.bounded(bound));
        auto rhh_it = map.insert(typename map_t::value_type({r, counts}, {r * 2, counts}));
        auto uo_it = uo.insert(std::make_pair(r, r * 2));
        REQUIRE(rhh_it.second == uo_it.second);
        REQUIRE(rhh_it.first->first.get() == uo_it.first->first);
        REQUIRE(rhh_it.first->second.get() == uo_it.first->second);
        REQUIRE(map.size() == uo.size());

        r = rng.bounded(bound);
        auto map_it = map.find(counter::obj{r, counts});
        auto uo_it2 = uo.find(r);
        REQUIRE((map.end() == map_it) == (uo.end() == uo_it2));
        if (map.end() != map_it) {
            REQUIRE(map_it->first.get() == uo_it2->first);
            REQUIRE(map_it->second.get() == uo_it2->second);
        }
    }

    uo.clear();
    map.clear();
    for (std::size_t i = 0; i < times; ++i) {
        auto const r = static_cast<std::size_t>(rng.bounded(bound));
        map[{r, counts}] = {r * 2, counts};
        uo[r] = r * 2;
        REQUIRE(map.find(counter::obj{r, counts})->second.get() == uo.find(r)->second);
        REQUIRE(map.size() == uo.size());
    }

    std::size_t num_checks = 0;
    for (auto it = map.begin(); it != map.end(); ++it) {
        REQUIRE(uo.end() != uo.find(it->first.get()));
        ++num_checks;
    }
    REQUIRE(map.size() == num_checks);

    num_checks = 0;
    map_t const &const_rhhs = map;
    for (typename map_t::value_type const &vt : const_rhhs) {
        REQUIRE(uo.end() != uo.find(vt.first.get()));
        ++num_checks;
    }
    REQUIRE(map.size() == num_checks);
}
