#include "fixtures/allocators.hpp"
#include "fixtures/checksum.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <new>

using namespace dice::sparse_map::tests;

TEST_CASE("counter counts constructions and destructions") {
    auto counts = counter{};
    {
        auto a = counter::obj{1, counts};
        auto b = a;
        auto c = std::move(b);
        CHECK(a == c);
    }
    CHECK(counts.ctor() == 1);
    CHECK(counts.copy_ctor() == 1);
    CHECK(counts.move_ctor() == 1);
    CHECK(counts.dtor() == 3);
    CHECK(counts.equals() == 1);
}

TEST_CASE("bombing_allocator throws on the requested allocation") {
    auto alloc = bombing_allocator<int>{};
    auto const guard = bomb_after{1};
    int *p = alloc.allocate(1);
    CHECK_THROWS_AS(static_cast<void>(alloc.allocate(1)), std::bad_alloc);
    alloc.deallocate(p, 1);
}

TEST_CASE_MAP("the map configurations hold what is inserted", counter::obj, counter::obj) {
    auto counts = counter{};
    {
        auto map = map_t{};
        for (std::size_t i = 0; i < 100; ++i) {
            map.try_emplace(counter::obj{i, counts}, i, counts);
        }
        CHECK(map.size() == 100);
        CHECK(checksum::map(map) == checksum::map(map_t{map}));
    }
    CHECK(counts.dtor() + counter::static_dtor == counts.ctor() + counter::static_ctor + counts.copy_ctor() + counts.move_ctor());
}

TEST_CASE_SET("the set configurations hold what is inserted", int) {
    auto set = set_t{};
    for (int i = 0; i < 100; ++i) {
        set.insert(i);
    }
    CHECK(set.size() == 100);
    CHECK(set.contains(42));
}
