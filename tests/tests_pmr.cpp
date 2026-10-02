#include "fixtures/counting_resource.hpp"

#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <functional>
#include <initializer_list>
#include <memory_resource>
#include <string>
#include <utility>
#include <vector>

/**
 * `sparse_map` and `sparse_set` with `std::pmr::polymorphic_allocator`. The container allocates its
 * buckets from its memory resource, and `polymorphic_allocator` constructs the keys and values with
 * the same resource (uses-allocator construction).
 *
 * Partly ported from the pmr tests of ankerl::unordered_dense (MIT license).
 */
namespace {
    using namespace dice::unordered_sparse;
    using dice::unordered_sparse::tests::counting_resource;

    using pmr_value = std::pair<std::pmr::string, std::pmr::vector<int>>;
    using pmr_map = sparse_map<std::pmr::string, std::pmr::vector<int>, std::hash<std::pmr::string>, std::equal_to<std::pmr::string>, std::pmr::polymorphic_allocator<pmr_value>>;
    using pmr_set = sparse_set<std::pmr::string, std::hash<std::pmr::string>, std::equal_to<std::pmr::string>, std::pmr::polymorphic_allocator<std::pmr::string>>;
    using pmr_int_map = sparse_map<int, int, std::hash<int>, std::equal_to<int>, std::pmr::polymorphic_allocator<std::pair<int, int>>>;

    /// a key that is too long for the small string buffer, so that the string allocates
    std::pmr::string long_key(int i) {
        return std::pmr::string{"a key that is too long for the small string buffer " + std::to_string(i)};
    }

    /// true if the key and the value of every entry of `map` allocate from `resource`
    bool entries_use(pmr_map const &map, std::pmr::memory_resource *resource) {
        for (auto const &entry : map) {
            if (entry.first.get_allocator().resource() != resource || entry.second.get_allocator().resource() != resource) {
                return false;
            }
        }
        return true;
    }

    /// true if every key of `set` allocates from `resource`
    bool keys_use(pmr_set const &set, std::pmr::memory_resource *resource) {
        for (auto const &key : set) {
            if (key.get_allocator().resource() != resource) {
                return false;
            }
        }
        return true;
    }

    /// sets the default memory resource while it is in scope
    struct default_resource_guard {
        explicit default_resource_guard(std::pmr::memory_resource *resource) noexcept
            : previous_(std::pmr::set_default_resource(resource)) {
        }

        default_resource_guard(default_resource_guard const &) = delete;
        default_resource_guard(default_resource_guard &&) = delete;
        default_resource_guard &operator=(default_resource_guard const &) = delete;
        default_resource_guard &operator=(default_resource_guard &&) = delete;

        ~default_resource_guard() {
            std::pmr::set_default_resource(previous_);
        }

    private:
        std::pmr::memory_resource *previous_;
    };

}  // namespace

TEST_CASE("a pmr map takes its buckets from its resource") {
    auto resource = counting_resource{};
    auto default_resource = counting_resource{};
    {
        auto const guard = default_resource_guard{&default_resource};
        auto map = pmr_int_map{&resource};
        CHECK(map.get_allocator().resource() == &resource);
        CHECK(resource.allocations() == 0);

        for (int i = 0; i < 1000; ++i) {
            map[i] = i;
        }
        CHECK(resource.allocations() > 0);
        CHECK(resource.bytes_in_use() > 0);
        map.rehash(5000);
        for (int i = 0; i < 1000; i += 2) {
            map.erase(i);
        }
        CHECK(map.size() == 500);
        CHECK(map.at(1) == 1);
        // nothing of the map comes from the default resource
        CHECK(default_resource.allocations() == 0);
    }
    CHECK(resource.bytes_in_use() == 0);
    CHECK(resource.deallocations() == resource.allocations());
}

TEST_CASE("the keys and values of a pmr map use the resource of the map") {
    auto resource = counting_resource{};
    {
        auto map = pmr_map{&resource};
        auto const vector_of = [](std::initializer_list<int> values) {
            return std::pmr::vector<int>{values};
        };

        SUBCASE("insert") {
            auto const value = pmr_value{long_key(0), vector_of({1, 2, 3})};
            map.insert(value);
            map.insert(pmr_value{long_key(1), vector_of({4, 5})});
            CHECK(map.size() == 2);
        }
        SUBCASE("emplace") {
            map.emplace(long_key(0), vector_of({1, 2, 3}));
            CHECK(map.size() == 1);
        }
        SUBCASE("try_emplace") {
            map.try_emplace(long_key(0), std::initializer_list<int>{1, 2, 3});
            map.try_emplace(long_key(1), 3, 7);
            CHECK(map.at(long_key(1)) == vector_of({7, 7, 7}));
        }
        SUBCASE("operator[]") {
            map[long_key(0)].push_back(1);
            map[long_key(0)].push_back(2);
            CHECK(map.at(long_key(0)) == vector_of({1, 2}));
        }
        SUBCASE("insert_or_assign") {
            map.insert_or_assign(long_key(0), vector_of({1, 2, 3}));
            map.insert_or_assign(long_key(0), vector_of({4, 5, 6, 7}));
            CHECK(map.at(long_key(0)) == vector_of({4, 5, 6, 7}));
        }

        CHECK(resource.allocations() > 0);
        CHECK(entries_use(map, &resource));

        // a rehash moves the entries, and they stay with the resource of the map
        for (int i = 100; i < 300; ++i) {
            map.try_emplace(long_key(i), std::initializer_list<int>{i, i});
        }
        map.rehash(4096);
        CHECK(entries_use(map, &resource));
        for (int i = 100; i < 300; i += 2) {
            map.erase(long_key(i));
        }
        CHECK(entries_use(map, &resource));
        CHECK(map.at(long_key(101)) == vector_of({101, 101}));
    }
    CHECK(resource.bytes_in_use() == 0);
    CHECK(resource.deallocations() == resource.allocations());
}

TEST_CASE("the keys of a pmr set use the resource of the set") {
    auto resource = counting_resource{};
    {
        auto set = pmr_set{&resource};
        auto const key = long_key(0);
        set.insert(key);
        set.insert(long_key(1));
        set.emplace("a string literal that is too long for the small string buffer");
        for (int i = 2; i < 300; ++i) {
            set.insert(long_key(i));
        }
        CHECK(set.size() == 301);
        CHECK(set.get_allocator().resource() == &resource);
        CHECK(keys_use(set, &resource));

        set.rehash(4096);
        for (int i = 0; i < 300; i += 2) {
            set.erase(long_key(i));
        }
        CHECK(keys_use(set, &resource));
        CHECK(set.contains(long_key(1)));
        CHECK_FALSE(set.contains(long_key(2)));
    }
    CHECK(resource.bytes_in_use() == 0);
    CHECK(resource.deallocations() == resource.allocations());
}

TEST_CASE("a pmr map never asks its resource for 0 bytes and never gives back a null pointer") {
    auto resource = counting_resource{};
    {
        auto const map = pmr_map{&resource};
    }
    {
        auto map = pmr_map{&resource};
        map[long_key(0)].push_back(1);
    }
    {
        auto map = pmr_map{&resource};
        map[long_key(0)].push_back(1);
        auto const moved = std::move(map);
        CHECK(moved.size() == 1);
    }
    {
        auto map = pmr_map{&resource};
        for (int i = 0; i < 100; ++i) {
            map[long_key(i)].push_back(i);
        }
        map.clear();
        map.rehash(0);
    }
    CHECK(resource.allocations() > 0);
    CHECK(resource.empty_requests() == 0);
    CHECK(resource.bytes_in_use() == 0);
}

TEST_CASE("moving a pmr map keeps its resource and allocates nothing") {
    auto resource = counting_resource{};
    {
        auto map = pmr_map{&resource};
        for (int i = 0; i < 100; ++i) {
            map[long_key(i)].push_back(i);
        }
        auto const allocations = resource.allocations();

        auto const moved = std::move(map);

        CHECK(resource.allocations() == allocations);
        CHECK(moved.get_allocator().resource() == &resource);
        CHECK(moved.size() == 100);
        CHECK(moved.at(long_key(42)) == std::pmr::vector<int>{42});
        CHECK(entries_use(moved, &resource));
    }
    CHECK(resource.bytes_in_use() == 0);
}

TEST_CASE("swapping two pmr maps with the same resource allocates nothing") {
    auto resource = counting_resource{};
    {
        auto a = pmr_map{&resource};
        auto b = pmr_map{&resource};
        for (int i = 0; i < 100; ++i) {
            a[long_key(i)].push_back(i);
        }
        b[long_key(1000)].push_back(1000);
        auto const allocations = resource.allocations();

        a.swap(b);

        CHECK(resource.allocations() == allocations);
        CHECK(a.size() == 1);
        CHECK(b.size() == 100);
        CHECK(a.contains(long_key(1000)));
        CHECK(b.at(long_key(42)) == std::pmr::vector<int>{42});
    }
    CHECK(resource.bytes_in_use() == 0);
}

namespace {
    /// A copy gets the default resource, because `polymorphic_allocator` does not propagate on copy construction.
    template<typename Map>
    void check_copy_construction() {
        auto resource = counting_resource{};
        auto default_resource = counting_resource{};
        {
            auto const guard = default_resource_guard{&default_resource};
            auto source = Map{&resource};
            for (int i = 0; i < 100; ++i) {
                source[long_key(i)].push_back(i);
            }
            auto const copy = source;
            CHECK(copy.get_allocator().resource() == &default_resource);
            CHECK(copy == source);
            CHECK(entries_use(copy, &default_resource));
            CHECK(entries_use(source, &resource));
        }
        CHECK(resource.bytes_in_use() == 0);
        CHECK(default_resource.bytes_in_use() == 0);
    }

    /// The target keeps its resource and copies the entries into it.
    template<typename Map>
    void check_copy_assignment() {
        auto resource_1 = counting_resource{};
        auto resource_2 = counting_resource{};
        {
            auto target = Map{&resource_1};
            target[long_key(1)].push_back(2);
            auto source = Map{&resource_2};
            source[long_key(3)].push_back(4);
            auto const source_allocations = resource_2.allocations();

            target = source;

            CHECK(target.size() == 1);
            CHECK(target.contains(long_key(3)));
            CHECK(target.get_allocator().resource() == &resource_1);
            CHECK(entries_use(target, &resource_1));
            CHECK(resource_2.allocations() == source_allocations);
        }
        CHECK(resource_1.bytes_in_use() == 0);
        CHECK(resource_2.bytes_in_use() == 0);
    }

    /// With the same resource the target takes the memory of the source, with another one it moves the entries into its own resource.
    template<typename Map>
    void check_move_assignment() {
        auto resource_1 = counting_resource{};
        auto resource_2 = counting_resource{};
        {
            auto target = Map{&resource_1};
            target[long_key(0)].push_back(0);
            auto source = Map{&resource_1};
            source[long_key(1)].push_back(1);
            auto const allocations = resource_1.allocations();

            target = std::move(source);

            CHECK(resource_1.allocations() == allocations);
            CHECK(target.size() == 1);
            CHECK(target.at(long_key(1)) == std::pmr::vector<int>{1});
        }
        {
            auto target = Map{&resource_1};
            target[long_key(0)].push_back(0);
            auto source = Map{&resource_2};
            for (int i = 0; i < 100; ++i) {
                source[long_key(i)].push_back(i);
            }

            target = std::move(source);

            CHECK(target.get_allocator().resource() == &resource_1);
            CHECK(target.size() == 100);
            CHECK(target.at(long_key(42)) == std::pmr::vector<int>{42});
            CHECK(entries_use(target, &resource_1));

            // the moved from source still works
            source.clear();  // NOLINT(bugprone-use-after-move)
            source[long_key(7)].push_back(7);
            CHECK(source.size() == 1);
        }
        CHECK(resource_1.bytes_in_use() == 0);
        CHECK(resource_2.bytes_in_use() == 0);
    }

}  // namespace

TEST_CASE("a copy of a pmr map uses the default resource") {
    check_copy_construction<pmr_map>();
}

TEST_CASE("copy assignment of a pmr map keeps the resource of the target") {
    check_copy_assignment<pmr_map>();
}

TEST_CASE("move assignment of a pmr map keeps the resource of the target") {
    check_move_assignment<pmr_map>();
}
