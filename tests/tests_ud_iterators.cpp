// Ported from the unit tests iterators_conversion, iterators_erase, iterators_empty and iterators_insert of ankerl::unordered_dense (MIT license).
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <unordered_set>
#include <utility>
#include <vector>

using namespace dice::sparse_map::tests;

// iterators_conversion
//
// An iterator converts to a const_iterator. Assigning to a const_iterator that already holds a position has to
// overwrite both parts of it: the bucket group and the position in the group. The loop variable pattern below needs
// exactly that.

TEST_CASE_MAP("assigning_an_iterator_to_an_existing_const_iterator", int, int) {
    auto map = map_t();
    for (int i = 0; i < 10; ++i) {
        map.try_emplace(i, i * 10);
    }

    // a const_iterator that already holds something else
    typename map_t::const_iterator cit = map.cend();
    REQUIRE(cit == map.cend());

    cit = map.begin();
    REQUIRE(cit == map.cbegin());
    REQUIRE(cit != map.cend());
    REQUIRE(cit->second == cit->first * 10);

    // and again to a different position
    cit = std::next(map.begin(), 4);
    REQUIRE(cit == std::next(map.cbegin(), 4));
    REQUIRE(cit->second == cit->first * 10);
    REQUIRE(std::next(cit) != map.cend());

    // walking to the end through the assigned iterator has to arrive exactly at end()
    auto count = 0;
    for (cit = map.begin(); cit != map.cend(); ++cit) {
        ++count;
    }
    REQUIRE(count == 10);

    // Every assignment above is between two iterators into the same map. Assigning from an iterator into a
    // different map is the only thing that can tell whether the whole iterator is copied.
    auto other = map_t();
    other.try_emplace(99, 990);

    cit = other.begin();
    REQUIRE(cit->first == 99);
    REQUIRE(cit->second == 990);
}

// iterators_erase

TEST_CASE_MAP("iterators_erase", counter::obj, counter::obj) {
    auto counts = counter();
    INFO(counts);
    {
        counts("begin");
        auto map = map_t();
        for (std::size_t i = 0; i < 100; ++i) {
            map[counter::obj(i * 101, counts)] = counter::obj(i * 101, counts);
        }

        auto it = map.find(counter::obj(std::size_t{20} * 101, counts));
        REQUIRE(map.size() == 100);
        REQUIRE(map.end() != map.find(counter::obj(std::size_t{20} * 101, counts)));
        it = map.erase(it);
        REQUIRE(map.size() == 99);
        REQUIRE(map.end() == map.find(counter::obj(std::size_t{20} * 101, counts)));

        it = map.begin();
        std::size_t current_size = map.size();
        std::unordered_set<std::uint64_t> keys;
        while (it != map.end()) {
            REQUIRE(keys.emplace(it->first.get()).second);
            it = map.erase(it);
            current_size--;
            REQUIRE(map.size() == current_size);
        }
        REQUIRE(map.size() == static_cast<std::size_t>(0));
        counts("done");
    }
    counts("destructed");
    REQUIRE(
        counts.dtor() + counter::static_dtor
        == counts.ctor() + counter::static_ctor + counts.copy_ctor() + counts.default_ctor() + counts.move_ctor()
    );
}

// What erase() returns. The loop above erases from begin() every time, so it passes just as well if erase() returns
// begin() and never the position after the erased element. Erasing from the middle tells those apart. sparse_map's
// erase(iterator) returns the iterator to the element after the erased one, and erasing does not move the remaining
// elements in the iteration order.
TEST_CASE_MAP("erase_returns_the_position_after_the_erased_element", int, int) {
    auto map = map_t();
    for (int i = 0; i < 10; ++i) {
        map.try_emplace(i, i * 10);
    }

    auto const idx = typename map_t::difference_type{3};
    auto const pos = std::next(map.begin(), idx);
    auto const erased_key = pos->first;
    auto const next_key = std::next(pos)->first;

    auto const it = map.erase(pos);
    REQUIRE(it == std::next(map.begin(), idx));
    REQUIRE(it->first == next_key);
    REQUIRE(map.size() == 9);
    REQUIRE(map.find(erased_key) == map.end());
    REQUIRE(map.find(next_key) == it);

    // Erasing the last element returns end(). The erase gets a statement of its own: in `erase(...) == map.end()`
    // the two sides are indeterminately sequenced.
    auto const past_the_last = map.erase(std::next(map.begin(), 8));
    REQUIRE(past_the_last == map.end());
    REQUIRE(map.size() == 8);
}

// erase(const_iterator) with an iterator that is not begin().
TEST_CASE_MAP("erase_accepts_a_const_iterator_from_anywhere", int, int) {
    auto map = map_t();
    for (int i = 0; i < 10; ++i) {
        map.try_emplace(i, i * 10);
    }

    auto const idx = typename map_t::difference_type{4};
    typename map_t::const_iterator const cit = std::next(map.cbegin(), idx);
    auto const erased_key = cit->first;
    auto const next_key = std::next(cit)->first;

    auto const it = map.erase(cit);
    REQUIRE(it == std::next(map.begin(), idx));
    REQUIRE(it->first == next_key);
    REQUIRE(map.find(erased_key) == map.end());
    REQUIRE(map.size() == 9);
}

// iterators_empty
//
// clear() keeps the bucket array, so the last check walks a bucket array without elements.

TEST_CASE_MAP("iterators_empty", std::size_t, std::size_t) {
    for (std::size_t i = 0; i < 10; ++i) {
        auto m = map_t();
        REQUIRE(m.begin() == m.end());

        REQUIRE(m.end() == m.find(132));

        m[1];
        REQUIRE(m.begin() != m.end());
        REQUIRE(++m.begin() == m.end());
        m.clear();

        REQUIRE(m.begin() == m.end());
    }
}

// iterators_insert

TEST_CASE_MAP("iterators_insert", int, int) {
    auto v = std::vector<std::pair<int, int>>();
    v.reserve(1000);
    for (int i = 0; i < 1000; ++i) {
        v.emplace_back(i, i);
    }

    auto map = map_t(v.begin(), v.end());
    REQUIRE(map.size() == v.size());
    for (auto const &kv : v) {
        REQUIRE(map.count(kv.first) == 1);
        auto it = map.find(kv.first);
        REQUIRE(it != map.end());
    }
}
