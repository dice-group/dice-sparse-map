#include "fixtures/allocators.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <functional>
#include <utility>

/**
 * How `sparse_map` and `sparse_set` use a stateful allocator: which allocator a copy gets, what copy
 * assignment, move assignment and swap do with the allocators, and that every block goes back to the
 * allocator that made it. The allocators are `tests::id_allocator` with different propagation traits.
 *
 * Partly ported from the table allocator tests of ankerl::unordered_dense (MIT license).
 */
namespace {
    using namespace dice::sparse_map;
    using dice::sparse_map::tests::alloc_counts;
    using dice::sparse_map::tests::id_allocator;
    using dice::sparse_map::tests::pmr_like_allocator;
    using dice::sparse_map::tests::pocca_allocator;
    using dice::sparse_map::tests::pocma_allocator;
    using dice::sparse_map::tests::pocs_allocator;

    /// propagates on nothing, and a copy of the container gets a copy of the allocator
    template<typename T>
    using inheriting_allocator = id_allocator<T>;

    /// `sparse_map<int, int>` with the allocator `AllocatorOf`
    struct map_kind {
        template<template<typename> typename AllocatorOf>
        using with = sparse_map<int, int, std::hash<int>, std::equal_to<int>, AllocatorOf<std::pair<int, int>>>;
    };

    /// `sparse_set<int>` with the allocator `AllocatorOf`
    struct set_kind {
        template<template<typename> typename AllocatorOf>
        using with = sparse_set<int, std::hash<int>, std::equal_to<int>, AllocatorOf<int>>;
    };

    template<typename Container>
    constexpr bool is_map_v = requires { typename Container::mapped_type; };

    /// inserts the keys 0 to `count - 1`, in a map mapped to themselves
    template<typename Container>
    void insert_keys(Container &container, int count) {
        for (int i = 0; i < count; ++i) {
            if constexpr (is_map_v<Container>) {
                container.try_emplace(i, i);
            } else {
                container.insert(i);
            }
        }
    }

    /// a container that holds the keys 0 to `count - 1`, in a map mapped to themselves
    template<typename Container>
    Container filled(int count, typename Container::allocator_type const &alloc) {
        auto container = Container(0, alloc);
        insert_keys(container, count);
        return container;
    }

    /// true if `container` holds exactly the keys 0 to `count - 1`, in a map mapped to themselves
    template<typename Container>
    bool holds(Container const &container, int count) {
        if (container.size() != static_cast<std::size_t>(count)) {
            return false;
        }
        for (int i = 0; i < count; ++i) {
            auto const it = container.find(i);
            if (it == container.end()) {
                return false;
            }
            if constexpr (is_map_v<Container>) {
                if (it->second != i) {
                    return false;
                }
            }
        }
        return true;
    }

    /// an allocator of `Container` with the id `id` that counts in `counts`
    template<typename Container>
    typename Container::allocator_type make_allocator(int id, alloc_counts &counts) {
        return typename Container::allocator_type{id, &counts};
    }

}  // namespace

TYPE_TO_STRING_AS("sparse_map", map_kind);
TYPE_TO_STRING_AS("sparse_set", set_kind);

TEST_CASE_TEMPLATE("a copy asks select_on_container_copy_construction for its allocator", kind_t, map_kind, set_kind) {
    SUBCASE("an allocator that declines is not inherited") {
        using container_t = typename kind_t::template with<pmr_like_allocator>;
        auto counts = alloc_counts{};
        auto const source = filled<container_t>(20, make_allocator<container_t>(7, counts));
        auto const before = counts.allocations;

        auto const copy = source;

        CHECK(copy.get_allocator().id == 0);
        CHECK(holds(copy, 20));
        CHECK(holds(source, 20));
        // nothing of the copy comes from the allocator of the source
        CHECK(counts.allocations == before);
    }

    SUBCASE("an allocator that accepts is inherited") {
        using container_t = typename kind_t::template with<inheriting_allocator>;
        auto counts = alloc_counts{};
        auto const source = filled<container_t>(20, make_allocator<container_t>(7, counts));
        auto const before = counts.allocations;

        auto const copy = source;

        CHECK(copy.get_allocator().id == 7);
        CHECK(holds(copy, 20));
        CHECK(counts.allocations > before);
    }
}

TEST_CASE_TEMPLATE("every block goes back to the allocator that made it", kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<inheriting_allocator>;
    auto counts = alloc_counts{};
    {
        auto container = filled<container_t>(200, make_allocator<container_t>(3, counts));
        CHECK(holds(container, 200));
        for (int i = 0; i < 200; i += 2) {
            container.erase(i);
        }
        container.rehash(0);
        container.reserve(1000);
        auto const copy = container;
        CHECK(copy.size() == 100);
        container.clear();
        insert_keys(container, 300);
        CHECK(holds(container, 300));
    }
    CHECK(counts.allocations > 0);
    CHECK(counts.deallocations == counts.allocations);
}

TEST_CASE_TEMPLATE("copy assignment without propagation keeps the allocator of the target", kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<inheriting_allocator>;
    auto source_counts = alloc_counts{};
    auto target_counts = alloc_counts{};
    {
        auto const source = filled<container_t>(200, make_allocator<container_t>(1, source_counts));
        auto target = filled<container_t>(50, make_allocator<container_t>(2, target_counts));
        auto const source_before = source_counts.allocations;
        auto const target_before = target_counts.allocations;

        target = source;

        CHECK(target.get_allocator().id == 2);
        CHECK(holds(target, 200));
        CHECK(holds(source, 200));
        CHECK(source_counts.allocations == source_before);
        CHECK(target_counts.allocations > target_before);
    }
    CHECK(source_counts.deallocations == source_counts.allocations);
    CHECK(target_counts.deallocations == target_counts.allocations);
}

TEST_CASE_TEMPLATE("copy assignment with propagation takes the allocator of the source", kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<pocca_allocator>;
    auto source_counts = alloc_counts{};
    auto target_counts = alloc_counts{};
    auto const source = filled<container_t>(200, make_allocator<container_t>(1, source_counts));
    auto target = filled<container_t>(50, make_allocator<container_t>(2, target_counts));

    target = source;

    CHECK(target.get_allocator().id == 1);
    CHECK(holds(target, 200));

    // growing the target takes all new memory from the allocator of the source
    auto const target_before = target_counts.allocations;
    auto const source_before = source_counts.allocations;
    target.reserve(20000);
    CHECK(target_counts.allocations == target_before);
    CHECK(source_counts.allocations > source_before);
    CHECK(holds(target, 200));
}

// skipped: `sparse_hash::operator=(sparse_hash const &)` replaces `m_alloc`, but keeps the bucket
// vector, and with it the old allocator and its memory. It resets the vector by move assigning an
// empty vector, and `std::vector` does not take the new allocator in a move assignment when
// `propagate_on_container_move_assignment` is false. A later rehash swaps this vector with one of
// the new allocator, so both vectors give their memory back to the wrong allocator.
TEST_CASE_TEMPLATE("after a copy assignment with propagation the target holds no memory of its old allocator" * doctest::skip(), kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<pocca_allocator>;
    auto source_counts = alloc_counts{};
    auto target_counts = alloc_counts{};
    {
        auto const source = filled<container_t>(200, make_allocator<container_t>(1, source_counts));
        auto target = filled<container_t>(50, make_allocator<container_t>(2, target_counts));

        target = source;

        CHECK(target.get_allocator().id == 1);
        CHECK(target_counts.deallocations == target_counts.allocations);

        // a rehash to no buckets gives the bucket vector of the target to a temporary table
        target.clear();
        target.rehash(0);
    }
    CHECK(source_counts.deallocations == source_counts.allocations);
    CHECK(target_counts.deallocations == target_counts.allocations);
}

TEST_CASE_TEMPLATE("move assignment with propagation takes the allocator and the memory of the source", kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<pocma_allocator>;
    auto source_counts = alloc_counts{};
    auto target_counts = alloc_counts{};
    {
        auto source = filled<container_t>(200, make_allocator<container_t>(1, source_counts));
        auto target = filled<container_t>(50, make_allocator<container_t>(2, target_counts));
        auto const source_before = source_counts.allocations;
        auto const target_before = target_counts.allocations;

        target = std::move(source);

        CHECK(target.get_allocator().id == 1);
        CHECK(holds(target, 200));
        CHECK(source_counts.allocations == source_before);
        CHECK(target_counts.allocations == target_before);
        // the target gave its old memory back to its old allocator
        CHECK(target_counts.deallocations == target_counts.allocations);

        // the source is still usable
        source.clear();  // NOLINT(bugprone-use-after-move)
        insert_keys(source, 10);
        CHECK(holds(source, 10));
    }
    CHECK(source_counts.deallocations == source_counts.allocations);
    CHECK(target_counts.deallocations == target_counts.allocations);
}

TEST_CASE_TEMPLATE("move assignment with equal allocators takes the memory of the source", kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<inheriting_allocator>;
    auto counts = alloc_counts{};
    {
        auto source = filled<container_t>(200, make_allocator<container_t>(1, counts));
        auto target = filled<container_t>(50, make_allocator<container_t>(1, counts));
        auto const before = counts.allocations;

        target = std::move(source);

        CHECK(target.get_allocator().id == 1);
        CHECK(holds(target, 200));
        CHECK(counts.allocations == before);
    }
    CHECK(counts.deallocations == counts.allocations);
}

// skipped: `sparse_hash::operator=(sparse_hash &&)` moves each bucket group with
// `sparse_array(sparse_array &&, Allocator const &)`, which leaves the source group holding its
// elements and its memory. `other.m_sparse_buckets_data.clear()` then destroys these groups without
// `sparse_array::clear(alloc)`: `~sparse_array` asserts, and without the assertion the memory and the
// moved from elements leak. A target that already has buckets also keeps its old, empty groups in
// front of the moved ones, so `find` looks in the old groups.
TEST_CASE_TEMPLATE("move assignment with unequal allocators moves the elements into the memory of the target" * doctest::skip(), kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<inheriting_allocator>;
    auto source_counts = alloc_counts{};
    auto target_counts = alloc_counts{};

    SUBCASE("into an empty target") {
        {
            auto source = filled<container_t>(200, make_allocator<container_t>(1, source_counts));
            auto target = container_t(0, make_allocator<container_t>(2, target_counts));
            auto const target_before = target_counts.allocations;

            target = std::move(source);

            CHECK(target.get_allocator().id == 2);
            CHECK(holds(target, 200));
            CHECK(target_counts.allocations > target_before);
        }
        CHECK(source_counts.deallocations == source_counts.allocations);
        CHECK(target_counts.deallocations == target_counts.allocations);
    }

    SUBCASE("into a target that holds elements") {
        {
            auto source = filled<container_t>(200, make_allocator<container_t>(1, source_counts));
            auto target = filled<container_t>(50, make_allocator<container_t>(2, target_counts));
            auto const target_before = target_counts.allocations;

            target = std::move(source);

            CHECK(target.get_allocator().id == 2);
            CHECK(holds(target, 200));
            CHECK(target_counts.allocations > target_before);
        }
        CHECK(source_counts.deallocations == source_counts.allocations);
        CHECK(target_counts.deallocations == target_counts.allocations);
    }
}

TEST_CASE_TEMPLATE("swap with propagation exchanges the allocators", kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<pocs_allocator>;
    auto a_counts = alloc_counts{};
    auto b_counts = alloc_counts{};
    {
        auto a = filled<container_t>(100, make_allocator<container_t>(1, a_counts));
        auto b = filled<container_t>(30, make_allocator<container_t>(2, b_counts));
        auto const a_before = a_counts.allocations;
        auto const b_before = b_counts.allocations;

        a.swap(b);

        CHECK(a.get_allocator().id == 2);
        CHECK(b.get_allocator().id == 1);
        CHECK(holds(a, 30));
        CHECK(holds(b, 100));
        CHECK(a_counts.allocations == a_before);
        CHECK(b_counts.allocations == b_before);

        swap(a, b);

        CHECK(a.get_allocator().id == 1);
        CHECK(b.get_allocator().id == 2);
        CHECK(holds(a, 100));
        CHECK(holds(b, 30));

        // after the swap, each container grows with the allocator it holds now
        a.swap(b);
        auto const a_before_growth = a_counts.allocations;
        b.reserve(10000);
        CHECK(a_counts.allocations > a_before_growth);
        CHECK(holds(b, 100));
    }
    CHECK(a_counts.deallocations == a_counts.allocations);
    CHECK(b_counts.deallocations == b_counts.allocations);
}

TEST_CASE_TEMPLATE("swap with equal allocators allocates nothing", kind_t, map_kind, set_kind) {
    using container_t = typename kind_t::template with<inheriting_allocator>;
    auto counts = alloc_counts{};
    {
        auto a = filled<container_t>(100, make_allocator<container_t>(1, counts));
        auto b = filled<container_t>(30, make_allocator<container_t>(1, counts));
        auto const before = counts.allocations;

        a.swap(b);

        CHECK(counts.allocations == before);
        CHECK(holds(a, 30));
        CHECK(holds(b, 100));
    }
    CHECK(counts.deallocations == counts.allocations);
}
