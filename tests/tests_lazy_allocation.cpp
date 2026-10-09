#include "fixtures/allocators.hpp"
#include "fixtures/test_types.hpp"
#include "fixtures/utils.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <functional>
#include <tuple>
#include <utility>

/**
 * An empty container gets its buckets with the first insert. Constructing one without a bucket count,
 * and asking it questions while it is empty, allocates nothing.
 *
 * Partly ported from the lazy bucket allocation tests of ankerl::unordered_dense (MIT license).
 */
namespace {
    using namespace dice::sparse_map;
    using dice::sparse_map::tests::counting_allocator;
    using dice::sparse_map::tests::num_allocations;

    template<sh::sparsity Sparsity>
    using counted_map = sparse_map<int, int, tests::test_hash<int>, std::equal_to<int>, counting_allocator<std::pair<int, int>>, Sparsity>;

    template<sh::sparsity Sparsity>
    using counted_set = sparse_set<int, tests::test_hash<int>, std::equal_to<int>, counting_allocator<int>, Sparsity>;

    /// the sparsity levels, with `test_hash` and an allocator that counts
    using counted_containers = std::tuple<counted_map<sh::sparsity::high>,
                                          counted_map<sh::sparsity::medium>,
                                          counted_map<sh::sparsity::low>,
                                          counted_set<sh::sparsity::high>,
                                          counted_set<sh::sparsity::medium>,
                                          counted_set<sh::sparsity::low>>;

}  // namespace

TYPE_TO_STRING_AS("map<high>", counted_map<sh::sparsity::high>);
TYPE_TO_STRING_AS("map<medium>", counted_map<sh::sparsity::medium>);
TYPE_TO_STRING_AS("map<low>", counted_map<sh::sparsity::low>);
TYPE_TO_STRING_AS("set<high>", counted_set<sh::sparsity::high>);
TYPE_TO_STRING_AS("set<medium>", counted_set<sh::sparsity::medium>);
TYPE_TO_STRING_AS("set<low>", counted_set<sh::sparsity::low>);

TEST_CASE_TEMPLATE_DEFINE("a default constructed container allocates nothing", container_t, default_construction) {
    auto const before = num_allocations;
    {
        auto const container = container_t{};
        CHECK(container.empty());
        CHECK(container.bucket_count() == 0);
    }
    CHECK(num_allocations == before);
}
TEST_CASE_TEMPLATE_APPLY(default_construction, counted_containers);

TEST_CASE_TEMPLATE_DEFINE("a container constructed with a bucket count of 0 allocates nothing", container_t, zero_bucket_count) {
    auto const before = num_allocations;
    {
        auto const container = container_t(0);
        CHECK(container.empty());
        CHECK(container.bucket_count() == 0);

        auto const with_allocator = container_t(0, typename container_t::allocator_type{});
        CHECK(with_allocator.bucket_count() == 0);
    }
    CHECK(num_allocations == before);
}
TEST_CASE_TEMPLATE_APPLY(zero_bucket_count, counted_containers);

TEST_CASE_TEMPLATE_DEFINE("an empty container answers queries without allocating", container_t, empty_queries) {
    auto container = container_t{};
    auto const &const_container = container;
    auto const before = num_allocations;

    CHECK(container.find(1) == container.end());
    CHECK(const_container.find(1) == const_container.end());
    CHECK(container.count(1) == 0);
    CHECK_FALSE(container.contains(1));
    CHECK(container.erase(1) == 0);
    CHECK(container.begin() == container.end());
    CHECK(const_container.begin() == const_container.end());
    CHECK(container.cbegin() == container.cend());

    auto const range = container.equal_range(1);
    CHECK(range.first == container.end());
    CHECK(range.second == container.end());
    auto const const_range = const_container.equal_range(1);
    CHECK(const_range.first == const_container.end());
    CHECK(const_range.second == const_container.end());

    CHECK(num_allocations == before);
    CHECK(container.bucket_count() == 0);
}
TEST_CASE_TEMPLATE_APPLY(empty_queries, counted_containers);

// The counter of the tests above sees the allocations of the container: the first insert allocates.
TEST_CASE_TEMPLATE_DEFINE("the first insert allocates the buckets", container_t, first_insert) {
    auto container = container_t{};
    auto const before = num_allocations;
    tests::insert_one(container, 1);
    CHECK(num_allocations > before);
    CHECK(container.bucket_count() > 0);
    CHECK(container.contains(1));
}
TEST_CASE_TEMPLATE_APPLY(first_insert, counted_containers);

TEST_CASE_TEMPLATE_DEFINE("an explicit bucket count is allocated up front", container_t, explicit_bucket_count) {
    auto const before = num_allocations;
    auto const container = container_t(100);
    CHECK(num_allocations > before);
    CHECK(container.bucket_count() >= 100);
}
TEST_CASE_TEMPLATE_APPLY(explicit_bucket_count, counted_containers);

TEST_CASE_TEMPLATE_DEFINE("a copy of an empty container allocates nothing", container_t, empty_copy) {
    auto const source = container_t{};
    auto const before = num_allocations;
    auto const copy = source;
    CHECK(num_allocations == before);
    CHECK(copy.bucket_count() == 0);
    CHECK(copy.empty());
}
TEST_CASE_TEMPLATE_APPLY(empty_copy, counted_containers);

TEST_CASE_TEMPLATE_DEFINE("a moved from container keeps no buckets and works", container_t, moved_from) {
    auto source = container_t{};
    for (int i = 0; i < 100; ++i) {
        tests::insert_one(source, i);
    }
    auto const before = num_allocations;

    auto const target = std::move(source);
    CHECK(num_allocations == before);
    CHECK(target.size() == 100);
    CHECK(source.bucket_count() == 0);  // NOLINT(bugprone-use-after-move)
    CHECK(source.empty());              // NOLINT(bugprone-use-after-move)
    CHECK(source.find(1) == source.end());

    tests::insert_one(source, 1);
    CHECK(source.contains(1));
}
TEST_CASE_TEMPLATE_APPLY(moved_from, counted_containers);

// `reserve(n)` once per batch of insertions is a common pattern. It must not rebuild the table when the table has room.
TEST_CASE_TEMPLATE_DEFINE("reserve and rehash allocate nothing when the bucket count stays", container_t, reserve_with_room) {
    auto container = container_t{};
    container.reserve(100);
    for (int i = 0; i < 50; ++i) {
        tests::insert_one(container, i);
    }
    auto const bucket_count = container.bucket_count();
    auto const before = num_allocations;

    container.reserve(100);
    container.reserve(80);
    container.rehash(bucket_count);
    CHECK(num_allocations == before);
    CHECK(container.bucket_count() == bucket_count);
    CHECK(container.size() == 50);
}
TEST_CASE_TEMPLATE_APPLY(reserve_with_room, counted_containers);
