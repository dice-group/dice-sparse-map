#include "fixtures/allocators.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/offset_ptr_allocator.hpp"
#include "fixtures/test_types.hpp"

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <map>
#include <memory>
#include <new>
#include <set>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * A container with an inline capacity keeps its first elements in the container object: group 0 of a table of 64
 * buckets, without a bucket array. These tests check which elements can be inline and the size of the containers, the
 * move of the elements into an allocated group when the inline group is full, the erasure of inline elements, copies,
 * moves, swaps and merges for every combination of inline elements and elements in buckets, the allocators, the
 * exceptions, and constant evaluation.
 */
namespace {
    using namespace dice::sparse_map;
    using namespace dice::sparse_map::tests;

    template<
        typename Key,
        typename T,
        std::size_t inline_capacity,
        typename Allocator = std::allocator<std::pair<Key, T>>,
        typename Hash = test_hash<Key>,
        sh::allocation_failure AllocationFailure = sh::allocation_failure::terminating
    >
    using map_with = sparse_map<
        Key,
        T,
        Hash,
        std::equal_to<Key>,
        Allocator,
        sh::sparsity::medium,
        AllocationFailure,
        inline_capacity
    >;

    template<typename Key, std::size_t inline_capacity, typename Allocator = std::allocator<Key>>
    using set_with = sparse_set<
        Key,
        test_hash<Key>,
        std::equal_to<Key>,
        Allocator,
        sh::sparsity::medium,
        sh::allocation_failure::terminating,
        inline_capacity
    >;

    /// a mapped type whose move constructor can throw
    struct throwing_move {
        int value = 0;

        throwing_move() = default;
        throwing_move(throwing_move const &) = default;
        throwing_move(throwing_move &&other) noexcept(false) // NOLINT(performance-noexcept-move-constructor)
            :
            value(other.value) {}
        throwing_move &operator=(throwing_move const &) = default;
        throwing_move &operator=(throwing_move &&) = default;
        ~throwing_move() = default;
    };

    /// a mapped type with a larger alignment than the state of the buckets has
    struct alignas(32) over_aligned {
        std::uint64_t value = 0;
    };

    /// a mapped type with the alignment of a pointer
    struct pointer_aligned {
        std::uint64_t value = 0;
    };

    template<typename Container>
    constexpr bool is_map_v = requires { typename Container::mapped_type; };

    /// inserts `key`, mapped to `key * 10` in a map
    template<typename Container>
    std::pair<typename Container::iterator, bool> insert_number(Container &container, int key) {
        if constexpr (is_map_v<Container>) {
            return container.try_emplace(key, key * 10);
        } else {
            return container.insert(key);
        }
    }

    template<typename Iterator>
    int key_at(Iterator const &it) {
        if constexpr (requires { it->first; }) {
            return it->first;
        } else {
            return *it;
        }
    }

    /// checks that `container` holds exactly the keys `0` to `n - 1`, each found and visited once, mapped to `key * 10`
    template<typename Container>
    void check_holds_numbers(Container const &container, int n) {
        CHECK(container.size() == static_cast<std::size_t>(n));
        bool all_found = true;
        for (int key = 0; key < n; ++key) {
            auto const it = container.find(key);
            all_found = all_found && it != container.end() && key_at(it) == key;
            if constexpr (is_map_v<Container>) {
                all_found = all_found && it != container.end() && it->second == key * 10;
            }
        }
        CHECK(all_found);
        CHECK_FALSE(container.contains(n));
        CHECK_FALSE(container.contains(-1));

        std::vector<int> visited;
        for (auto it = container.begin(); it != container.end(); ++it) {
            visited.push_back(key_at(it));
        }
        std::ranges::sort(visited);
        std::vector<int> expected(static_cast<std::size_t>(n));
        for (int key = 0; key < n; ++key) {
            expected[static_cast<std::size_t>(key)] = key;
        }
        CHECK(visited == expected);
    }

    template<typename T>
    using counted_alloc = counting_allocator<T>;
} // namespace

// The trait that the `static_assert` of the inline capacity uses. An inline capacity other than 0 does not compile for
// the elements it rejects, and for elements that are incomplete where the container type is instantiated.
TEST_CASE("which elements can be inline") {
    constexpr std::size_t pointer_alignment = alignof(void *);
    static_assert(
        detail_sparse_hash::inline_storable<detail_sparse_hash::map_slot<int, int>, pointer_alignment>::value
    );
    static_assert(detail_sparse_hash::inline_storable<
        detail_sparse_hash::map_slot<std::string, std::uint64_t>,
        pointer_alignment
    >::value);
    static_assert(detail_sparse_hash::inline_storable<
        detail_sparse_hash::map_slot<int, pointer_aligned>,
        pointer_alignment
    >::value);
    static_assert(detail_sparse_hash::inline_storable<std::uint64_t, pointer_alignment>::value);
    // a move constructor that can throw
    static_assert(
        !detail_sparse_hash::inline_storable<detail_sparse_hash::map_slot<int, throwing_move>, pointer_alignment>::value
    );
    static_assert(!detail_sparse_hash::inline_storable<throwing_move, pointer_alignment>::value);
    // an alignment larger than the one of the state of the buckets
    static_assert(
        !detail_sparse_hash::inline_storable<detail_sparse_hash::map_slot<int, over_aligned>, pointer_alignment>::value
    );
    static_assert(!detail_sparse_hash::inline_storable<over_aligned, pointer_alignment>::value);
    static_assert(detail_sparse_hash::inline_storable<over_aligned, alignof(over_aligned)>::value);

    // the inline capacity 0 compiles for all of them
    CHECK(sparse_map<int, throwing_move>{}.empty());
    CHECK(sparse_map<int, over_aligned>{}.empty());
}

TEST_CASE(
    "the inline group takes the place of the state of the buckets, and more inline elements make the object larger"
) {
    // 64 bit targets with std::allocator: the element count, the maximum load factor and the flag take 16 bytes, the
    // state of the buckets 56, the inline group 16 bytes of bitmaps and the elements
    if constexpr (sizeof(void *) == 8 && sizeof(std::size_t) == 8) {
        static_assert(sizeof(sparse_map<std::uint64_t, std::uint64_t>) == 72);
        static_assert(sizeof(map_with<std::uint64_t, std::uint64_t, 0>) == 72);
        static_assert(sizeof(map_with<std::uint64_t, std::uint64_t, 1>) == 72);
        static_assert(sizeof(map_with<std::uint64_t, std::uint64_t, 2>) == 72);
        static_assert(sizeof(map_with<std::uint64_t, std::uint64_t, 3>) == 80);
        static_assert(sizeof(map_with<std::uint64_t, std::uint64_t, 4>) == 96);
        static_assert(sizeof(map_with<std::uint64_t, std::uint64_t, 6>) == 128);
        static_assert(sizeof(map_with<std::uint64_t, std::uint64_t, 32>) == 16 + 16 + 32 * 16);
        static_assert(sizeof(sparse_set<std::uint64_t>) == 72);
        static_assert(sizeof(set_with<std::uint64_t, 4>) == 72);
        static_assert(sizeof(set_with<std::uint64_t, 5>) == 72);
        static_assert(sizeof(set_with<std::uint64_t, 6>) == 80);
        constexpr std::size_t string_slot = sizeof(detail_sparse_hash::map_slot<std::string, std::uint64_t>);
        static_assert(
            sizeof(map_with<std::string, std::uint64_t, 4>) == 16 + std::max<std::size_t>(56, 16 + 4 * string_slot)
        );
        CHECK(sizeof(map_with<std::uint64_t, std::uint64_t, 4>) == 96);
    }
}

TEST_CASE_TEMPLATE(
    "insertions up to the inline capacity allocate nothing, the next one moves the elements into buckets",
    container_t,
    map_with<int, int, 1, counted_alloc<std::pair<int, int>>>,
    map_with<int, int, 4, counted_alloc<std::pair<int, int>>>,
    map_with<int, int, 32, counted_alloc<std::pair<int, int>>>,
    set_with<int, 4, counted_alloc<int>>
) {
    auto container = container_t{};
    auto const capacity = static_cast<int>(inline_capacity_of<container_t>());
    auto const before = num_allocations;

    for (int key = 0; key < capacity; ++key) {
        auto const [it, inserted] = insert_number(container, key);
        CHECK(inserted);
        CHECK(key_at(it) == key);
    }
    CHECK(num_allocations == before);
    CHECK(container.bucket_count() == 64);
    check_holds_numbers(container, capacity);

    // a key that is there already is not inserted again
    auto const [existing, inserted_again] = insert_number(container, 0);
    CHECK_FALSE(inserted_again);
    CHECK(key_at(existing) == 0);
    CHECK(num_allocations == before);

    auto const [it, inserted] = insert_number(container, capacity);
    CHECK(inserted);
    CHECK(key_at(it) == capacity);
    CHECK(num_allocations > before);
    // the elements keep their buckets, and 64 buckets hold 32 elements at the maximum load factor 0.5
    CHECK(container.bucket_count() == (capacity < 32 ? 64 : 128));
    check_holds_numbers(container, capacity + 1);
}

TEST_CASE("reserve and rehash keep the elements inline if they fit") {
    using map_t = map_with<int, int, 4>;
    auto map = map_t{};
    map.reserve(4);
    CHECK(map.bucket_count() == 0);
    for (int key = 0; key < 4; ++key) {
        map.try_emplace(key, key * 10);
    }
    map.rehash(0);
    CHECK(map.bucket_count() == 64);
    check_holds_numbers(map, 4);
    map.rehash(64);
    CHECK(map.bucket_count() == 64);
    check_holds_numbers(map, 4);

    // a range that does not fit moves the elements into buckets at once
    std::vector<std::pair<int, int>> more;
    for (int key = 4; key < 100; ++key) {
        more.emplace_back(key, key * 10);
    }
    map.insert(more.begin(), more.end());
    CHECK(map.bucket_count() >= static_cast<std::size_t>(100 / map.max_load_factor()));
    check_holds_numbers(map, 100);

    // a bucket count above 64 moves the elements into buckets
    auto small = map_t{};
    small.try_emplace(1, 10);
    small.rehash(200);
    CHECK(small.bucket_count() == 256);
    CHECK(small.at(1) == 10);

    // an empty table without buckets goes back to the inline group
    auto emptied = map_t{};
    for (int key = 0; key < 10; ++key) {
        emptied.try_emplace(key, key * 10);
    }
    REQUIRE(emptied.bucket_count() == 64);
    emptied.clear();
    CHECK(emptied.bucket_count() == 64);
    emptied.rehash(0);
    CHECK(emptied.bucket_count() == 0);
    emptied.try_emplace(1, 10);
    CHECK(emptied.bucket_count() == 64);
    CHECK(emptied.at(1) == 10);
}

TEST_CASE("an erasure keeps the order of the other inline elements") {
    using map_t = map_with<int, int, 6>;
    auto map = map_t{};
    for (int key = 0; key < 6; ++key) {
        map.try_emplace(key, key * 10);
    }
    REQUIRE(map.bucket_count() == 64);

    auto keys = [&map] {
        std::vector<int> result;
        for (auto const &[key, value] : map) {
            result.push_back(key);
        }
        return result;
    };
    // inline elements are in the order of their buckets
    auto expected = keys();
    REQUIRE(expected.size() == 6);
    auto without = [&expected](int key) {
        std::erase(expected, key);
        return expected;
    };

    int const second = std::next(map.cbegin())->first;
    int const third = std::next(map.cbegin(), 2)->first;
    int const fourth = std::next(map.cbegin(), 3)->first;
    auto it = map.erase(std::next(map.cbegin()), std::next(map.cbegin(), 3));
    CHECK(it->first == fourth);
    without(second);
    CHECK(keys() == without(third));

    int const first = map.begin()->first;
    it = map.erase(map.begin());
    CHECK(it->first == fourth);
    CHECK(keys() == without(first));

    int const middle = std::next(map.begin())->first;
    CHECK(map.erase(middle) == 1);
    CHECK(map.erase(42) == 0);
    CHECK(keys() == without(middle));

    int const last = std::next(map.begin())->first;
    it = map.erase(std::next(map.begin()));
    CHECK(it == map.end());
    CHECK(keys() == without(last));

    int const remaining = map.begin()->first;
    CHECK(erase_if(map, [remaining](auto const &element) { return element.first == remaining; }) == 1);
    CHECK(map.empty());
    CHECK(map.begin() == map.end());

    map.try_emplace(7, 70);
    CHECK(map.at(7) == 70);
    CHECK(map.bucket_count() == 64);
}

namespace {
    /// elements of `counter::obj`, so that every element that is not destroyed, or destroyed twice, is seen
    template<std::size_t inline_capacity, typename Allocator = std::allocator<std::pair<counter::obj, counter::obj>>>
    using counted_map = map_with<counter::obj, counter::obj, inline_capacity, Allocator>;

    /// number of `counter::obj` that were constructed and not destroyed yet
    std::size_t alive(counter const &counts) {
        return counts.ctor()
            + counts.default_ctor()
            + counter::static_ctor
            + counts.copy_ctor()
            + counts.move_ctor()
            - counts.dtor()
            - counter::static_dtor;
    }

    /// a map with the keys `first` to `first + n - 1`, each mapped to `key + 1000`
    template<typename Map>
    Map make_counted(counter &counts, std::size_t first, std::size_t n) {
        Map map;
        for (std::size_t key = first; key < first + n; ++key) {
            map.try_emplace(counter::obj{key, counts}, counter::obj{key + 1000, counts});
        }
        return map;
    }

    /// the keys of `map`, sorted, or an empty vector and a failed check if a mapped value is wrong
    template<typename Map>
    std::vector<std::size_t> counted_keys(Map const &map) {
        std::vector<std::size_t> keys;
        bool values_right = true;
        for (auto const &[key, value] : map) {
            keys.push_back(key.get());
            values_right = values_right && value.get() == key.get() + 1000;
        }
        CHECK(values_right);
        std::ranges::sort(keys);
        return keys;
    }

    std::vector<std::size_t> numbers(std::size_t first, std::size_t n) {
        std::vector<std::size_t> result;
        for (std::size_t key = first; key < first + n; ++key) {
            result.push_back(key);
        }
        return result;
    }
} // namespace

TEST_CASE_TEMPLATE(
    "copies, moves and swaps keep the elements for every combination of inline and bucket storage",
    map_t,
    counted_map<1>,
    counted_map<3>,
    counted_map<3, offset_ptr_allocator<std::pair<counter::obj, counter::obj>>>
) {
    std::size_t const capacity = inline_capacity_of<map_t>();
    std::vector<std::size_t> const sizes{0, 1, capacity, capacity + 1, 40};
    auto counts = counter{};
    for (auto const a : sizes) {
        for (auto const b : sizes) {
            CAPTURE(a);
            CAPTURE(b);
            {
                auto first = make_counted<map_t>(counts, 0, a);
                auto second = make_counted<map_t>(counts, 100, b);
                CHECK((first.bucket_count() == 0) == (a == 0));

                first.swap(second);
                CHECK(counted_keys(first) == numbers(100, b));
                CHECK(counted_keys(second) == numbers(0, a));
                swap(first, second);
                CHECK(counted_keys(first) == numbers(0, a));
                CHECK(counted_keys(second) == numbers(100, b));

                auto copy = first;
                CHECK(counted_keys(copy) == numbers(0, a));
                copy = second;
                CHECK(counted_keys(copy) == numbers(100, b));
                copy = first;
                CHECK(counted_keys(copy) == numbers(0, a));

                auto moved = std::move(copy);
                CHECK(counted_keys(moved) == numbers(0, a));
                CHECK(copy.empty()); // NOLINT(bugprone-use-after-move)
                CHECK(copy.begin() == copy.end());
                copy.try_emplace(counter::obj{7, counts}, counter::obj{1007, counts});
                CHECK(counted_keys(copy) == numbers(7, 1));

                moved = std::move(second);
                CHECK(counted_keys(moved) == numbers(100, b));
                CHECK(second.empty()); // NOLINT(bugprone-use-after-move)
                second = std::move(copy);
                CHECK(counted_keys(second) == numbers(7, 1));

                // the elements of `first` survive the insertions after the copies and moves
                first.try_emplace(counter::obj{50, counts}, counter::obj{1050, counts});
                auto expected = numbers(0, a);
                expected.push_back(50);
                std::ranges::sort(expected);
                CHECK(counted_keys(first) == expected);
            }
            CHECK(alive(counts) == 0);
        }
    }
}

namespace {
    /**
     * Knows the addresses of its live objects. A copy or a move checks its source before it registers itself, so a
     * copy or a move from a destroyed object is seen, also when the new object is at the same address.
     */
    struct tracked {
        inline static std::set<tracked const *> alive; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
        inline static bool used_dead = false; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

        int value = 0;

        explicit tracked(int v) : value(v) {
            alive.insert(this);
        }

        tracked(tracked const &other) : value(value_of(other)) {
            alive.insert(this);
        }

        tracked(tracked &&other) noexcept : value(value_of(other)) {
            alive.insert(this);
        }

        tracked &operator=(tracked const &) = delete;
        tracked &operator=(tracked &&) = delete;

        ~tracked() {
            alive.erase(this);
        }

        static int value_of(tracked const &other) noexcept {
            if (!alive.contains(&other)) {
                used_dead = true;
            }
            return other.value;
        }
    };
} // namespace

TEST_CASE("a swap with itself keeps the elements, inline or not") {
    using map_t = map_with<int, tracked, 5>;
    for (int const n : {0, 1, 5, 6, 40}) {
        CAPTURE(n);
        tracked::used_dead = false;
        {
            auto map = map_t{};
            for (int key = 0; key < n; ++key) {
                map.try_emplace(key, key * 10);
            }
            map.swap(map);
            using std::swap;
            swap(map, map);
            CHECK(map.size() == static_cast<std::size_t>(n));
            bool values_right = true;
            for (int key = 0; key < n; ++key) {
                values_right = values_right && map.contains(key) && map.at(key).value == key * 10;
            }
            CHECK(values_right);
        }
        CHECK_FALSE(tracked::used_dead);
        CHECK(tracked::alive.empty());
    }
}

namespace {
    using swapping_map = map_with<int, int, 4, pocs_allocator<std::pair<int, int>>>;
    using unequal_map = map_with<int, int, 4, pmr_like_allocator<std::pair<int, int>>>;

    template<typename Map>
    Map make_numbers(int n, typename Map::allocator_type const &alloc) {
        Map map(alloc);
        for (int key = 0; key < n; ++key) {
            map.try_emplace(key, key * 10);
        }
        return map;
    }
} // namespace

TEST_CASE("an allocator that propagates on swap goes with the elements, inline or not") {
    for (int const a : {0, 2, 5}) {
        for (int const b : {1, 4, 50}) {
            CAPTURE(a);
            CAPTURE(b);
            auto first = make_numbers<swapping_map>(a, swapping_map::allocator_type{1});
            auto second = make_numbers<swapping_map>(b, swapping_map::allocator_type{2});
            first.swap(second);
            CHECK(first.get_allocator().id == 2);
            CHECK(second.get_allocator().id == 1);
            check_holds_numbers(first, b);
            check_holds_numbers(second, a);
            insert_number(first, b);
            insert_number(second, a);
            check_holds_numbers(first, b + 1);
            check_holds_numbers(second, a + 1);
        }
    }
}

TEST_CASE("a move to an unequal allocator keeps the elements, inline or not") {
    for (int const n : {0, 1, 4, 5, 50}) {
        CAPTURE(n);
        auto source = make_numbers<unequal_map>(n, unequal_map::allocator_type{1});
        auto target = make_numbers<unequal_map>(3, unequal_map::allocator_type{2});
        target = std::move(source);
        CHECK(target.get_allocator().id == 2);
        check_holds_numbers(target, n);
        CHECK(source.empty()); // NOLINT(bugprone-use-after-move)

        auto constructed = unequal_map(std::move(target), unequal_map::allocator_type{3});
        CHECK(constructed.get_allocator().id == 3);
        check_holds_numbers(constructed, n);
        CHECK(target.empty()); // NOLINT(bugprone-use-after-move)

        auto copy = unequal_map(constructed, unequal_map::allocator_type{4});
        check_holds_numbers(copy, n);
        insert_number(copy, n);
        check_holds_numbers(copy, n + 1);
        check_holds_numbers(constructed, n);
    }
}

namespace {
    using counted_pair = std::pair<counter::obj, counter::obj>;

    /// a map with the keys `first` to `first + n - 1`, each mapped to `key + 1000`, with the allocator `alloc`
    template<typename Map>
    Map make_counted_with(
        counter &counts,
        std::size_t first,
        std::size_t n,
        typename Map::allocator_type const &alloc
    ) {
        Map map(alloc);
        for (std::size_t key = first; key < first + n; ++key) {
            map.try_emplace(counter::obj{key, counts}, counter::obj{key + 1000, counts});
        }
        return map;
    }

    /**
     * Checks that every live element was constructed by the allocator of the map that holds it: per allocator id, the
     * live elements that `recording_id_allocator` recorded are as many as the maps with that allocator hold. And that
     * no allocator destroyed an element that another allocator constructed.
     */
    template<typename Map>
    void check_owners(std::initializer_list<Map const *> maps) {
        std::map<int, std::size_t> expected;
        for (Map const *map : maps) {
            if (!map->empty()) {
                expected[map->get_allocator().id] += map->size();
            }
        }
        std::map<int, std::size_t> recorded;
        for (auto const &[address, id] : constructed_by) {
            ++recorded[id];
        }
        CHECK(recorded == expected);
        CHECK_FALSE(construct_mismatch);
    }
} // namespace

// The elements are not trivially copyable, so the inline elements are constructed and destroyed one by one through
// the allocator, never copied as bytes.
TEST_CASE_TEMPLATE(
    "an allocator that is not equal to the source constructs the elements of its map one by one, inline or not",
    map_t,
    counted_map<3, recording_pmr_like_allocator<counted_pair>>,
    counted_map<3, recording_pocca_allocator<counted_pair>>,
    counted_map<3, recording_pocma_allocator<counted_pair>>,
    counted_map<3, recording_pocs_allocator<counted_pair>>
) {
    using alloc_t = typename map_t::allocator_type;
    using traits = std::allocator_traits<alloc_t>;
    constexpr bool pocca = traits::propagate_on_container_copy_assignment::value;
    constexpr bool pocma = traits::propagate_on_container_move_assignment::value;
    constexpr bool pocs = traits::propagate_on_container_swap::value;
    int const copy_id = traits::select_on_container_copy_construction(alloc_t{1}).id;

    std::size_t const capacity = inline_capacity_of<map_t>();
    std::vector<std::size_t> const sizes{0, 1, capacity, capacity + 1};
    auto counts = counter{};
    for (auto const a : sizes) {
        for (auto const b : sizes) {
            CAPTURE(a);
            CAPTURE(b);
            construct_mismatch = false;
            {
                auto first = make_counted_with<map_t>(counts, 0, a, alloc_t{1});
                auto second = make_counted_with<map_t>(counts, 100, b, alloc_t{2});
                check_owners({&first, &second});

                {
                    auto copy = first;
                    CHECK(copy.get_allocator().id == copy_id);
                    CHECK(counted_keys(copy) == numbers(0, a));
                    check_owners({&first, &second, &copy});
                }
                {
                    auto copy = map_t(first, alloc_t{3});
                    CHECK(copy.get_allocator().id == 3);
                    CHECK(counted_keys(copy) == numbers(0, a));
                    check_owners({&first, &second, &copy});
                }
                {
                    auto target = make_counted_with<map_t>(counts, 200, b, alloc_t{3});
                    target = first;
                    CHECK(target.get_allocator().id == (pocca ? 1 : 3));
                    CHECK(counted_keys(target) == numbers(0, a));
                    check_owners({&first, &second, &target});
                }
                {
                    auto source = map_t(first, alloc_t{1});
                    auto moved = std::move(source);
                    CHECK(moved.get_allocator().id == 1);
                    CHECK(counted_keys(moved) == numbers(0, a));
                    CHECK(source.empty()); // NOLINT(bugprone-use-after-move)
                    check_owners({&first, &second, &source, &moved});
                }
                {
                    // moves the elements one by one: no element is copied
                    auto source = map_t(first, alloc_t{1});
                    auto const copies = counts.copy_ctor();
                    auto moved = map_t(std::move(source), alloc_t{3});
                    CHECK(counts.copy_ctor() == copies);
                    CHECK(moved.get_allocator().id == 3);
                    CHECK(counted_keys(moved) == numbers(0, a));
                    CHECK(source.empty()); // NOLINT(bugprone-use-after-move)
                    check_owners({&first, &second, &source, &moved});
                }
                {
                    // without propagation the allocators differ, so the elements move one by one: no element is copied
                    auto source = map_t(first, alloc_t{1});
                    auto target = make_counted_with<map_t>(counts, 200, b, alloc_t{3});
                    auto const copies = counts.copy_ctor();
                    target = std::move(source);
                    CHECK(counts.copy_ctor() == copies);
                    CHECK(target.get_allocator().id == (pocma ? 1 : 3));
                    CHECK(counted_keys(target) == numbers(0, a));
                    CHECK(source.empty()); // NOLINT(bugprone-use-after-move)
                    check_owners({&first, &second, &source, &target});
                }
                if constexpr (pocs) {
                    first.swap(second);
                    CHECK(first.get_allocator().id == 2);
                    CHECK(second.get_allocator().id == 1);
                    CHECK(counted_keys(first) == numbers(100, b));
                    CHECK(counted_keys(second) == numbers(0, a));
                    check_owners({&first, &second});
                    swap(first, second);
                    CHECK(first.get_allocator().id == 1);
                    CHECK(second.get_allocator().id == 2);
                    check_owners({&first, &second});
                }

                // the elements of both maps survive an insertion after all of it
                first.try_emplace(counter::obj{50, counts}, counter::obj{1050, counts});
                second.try_emplace(counter::obj{150, counts}, counter::obj{1150, counts});
                auto expected_first = numbers(0, a);
                expected_first.push_back(50);
                std::ranges::sort(expected_first);
                auto expected_second = numbers(100, b);
                expected_second.push_back(150);
                std::ranges::sort(expected_second);
                CHECK(counted_keys(first) == expected_first);
                CHECK(counted_keys(second) == expected_second);
                check_owners({&first, &second});
            }
            CHECK(constructed_by.empty());
            CHECK_FALSE(construct_mismatch);
            CHECK(alive(counts) == 0);
        }
    }
}

namespace {
    /// calls of `throwing_hash` that are left before it throws, -1 never throws
    int hash_calls_until_throw = -1; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    struct hash_bomb_after {
        explicit hash_bomb_after(int n) noexcept {
            hash_calls_until_throw = n;
        }
        hash_bomb_after(hash_bomb_after const &) = delete;
        hash_bomb_after(hash_bomb_after &&) = delete;
        hash_bomb_after &operator=(hash_bomb_after const &) = delete;
        hash_bomb_after &operator=(hash_bomb_after &&) = delete;
        ~hash_bomb_after() {
            hash_calls_until_throw = -1;
        }
    };

    /**
     * Hash function that throws `std::runtime_error` on the n-th call after a `hash_bomb_after(n)`.
     */
    struct throwing_hash {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(counter::obj const &key) const {
            if (hash_calls_until_throw >= 0 && 0 == hash_calls_until_throw--) {
                throw std::runtime_error{"throwing_hash"};
            }
            return test_hash<counter::obj>{}(key);
        }
    };

    /// three `counter::obj -> counter::obj` elements inline. A failed allocation throws.
    using failing_map = map_with<
        counter::obj,
        counter::obj,
        3,
        leak_checking_allocator<std::pair<counter::obj, counter::obj>>,
        throwing_hash,
        sh::allocation_failure::throwing
    >;

    void insert_counted(failing_map &map, counter &counts, std::size_t key) {
        map.try_emplace(counter::obj{key, counts}, counter::obj{key + 1000, counts});
    }
} // namespace

// The move of the inline group into an allocated group allocates the bucket array and the values array before any
// element moves, so an allocation that fails leaves the map unchanged.
TEST_CASE(
    "an insertion that moves the inline elements into an allocated group and fails to allocate leaves them unchanged"
) {
    auto counts = counter{};
    std::size_t unchanged = 0;
    bool completed = false;
    for (int budget = 0; budget < 100 && !completed; ++budget) {
        CAPTURE(budget);
        {
            auto map = failing_map{};
            for (std::size_t key = 0; key < 3; ++key) {
                insert_counted(map, counts, key);
            }
            REQUIRE(live_blocks == 0);
            try {
                auto const bomb = bomb_after{budget};
                insert_counted(map, counts, 3);
                completed = true;
            } catch (std::bad_alloc const &) {
                CHECK(counted_keys(map) == numbers(0, 3));
                CHECK(map.contains(counter::obj{2, counts}));
                CHECK(live_blocks == 0); // still inline
                ++unchanged;
            }
            if (completed) {
                CHECK(counted_keys(map) == numbers(0, 4));
                CHECK(map.bucket_count() == 64);
            }
            insert_counted(map, counts, 10);
            CHECK(map.contains(counter::obj{10, counts}));
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
    CHECK(unchanged == 2); // the bucket array and the values array
    CHECK(completed);
}

// An inline insertion hashes the new key, the move into an allocated group hashes nothing. So a hash that throws
// leaves the map unchanged.
TEST_CASE(
    "an insertion that moves the inline elements into an allocated group and whose hash throws leaves them unchanged"
) {
    auto counts = counter{};
    for (int budget = 0; budget < 3; ++budget) {
        CAPTURE(budget);
        {
            auto map = failing_map{};
            for (std::size_t key = 0; key < 3; ++key) {
                insert_counted(map, counts, key);
            }
            REQUIRE(map.size() == 3);
            bool threw = false;
            {
                auto const bomb = hash_bomb_after{budget};
                try {
                    insert_counted(map, counts, 3);
                } catch (std::runtime_error const &) {
                    threw = true;
                }
            }
            if (budget == 0) {
                CHECK(threw);
                CHECK(counted_keys(map) == numbers(0, 3));
                CHECK(live_blocks == 0);
            } else {
                CHECK_FALSE(threw);
                CHECK(counted_keys(map) == numbers(0, 4));
            }
        }
        CHECK(alive(counts) == 0);
        CHECK(live_blocks == 0);
    }
}

// A rehash of an inline map into more than 64 buckets moves the elements into a new table. If the hash throws while
// they are moved, the map is empty, as after a rehash of a table whose elements cannot throw on a move.
TEST_CASE("a rehash of inline elements whose hash throws leaves an empty map") {
    auto counts = counter{};
    {
        auto map = failing_map{};
        for (std::size_t key = 0; key < 3; ++key) {
            insert_counted(map, counts, key);
        }
        {
            auto const bomb = hash_bomb_after{1};
            CHECK_THROWS_AS(map.rehash(256), std::runtime_error);
        }
        CHECK(map.empty());
        CHECK(map.bucket_count() == 0);
        CHECK(live_blocks == 0);
        insert_counted(map, counts, 7);
        CHECK(counted_keys(map) == numbers(7, 1));
    }
    CHECK(alive(counts) == 0);
    CHECK(live_blocks == 0);
}

namespace {
    /// copies of `throwing_copy` that are left before one throws, -1 never throws
    int copies_until_throw = -1; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// objects of `throwing_copy` that are alive
    int alive_copies = 0; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * A mapped type whose copy constructor throws on the n-th copy after `copies_until_throw` was set to n. Its move
     * constructor cannot throw.
     */
    struct throwing_copy {
        int value = 0;

        explicit throwing_copy(int v) : value(v) {
            ++alive_copies;
        }

        throwing_copy(throwing_copy const &other) : value(other.value) {
            if (copies_until_throw >= 0 && 0 == copies_until_throw--) {
                throw std::runtime_error{"throwing_copy"};
            }
            ++alive_copies;
        }

        throwing_copy(throwing_copy &&other) noexcept : value(other.value) {
            ++alive_copies;
        }

        throwing_copy &operator=(throwing_copy const &) = delete;
        throwing_copy &operator=(throwing_copy &&) = delete;

        ~throwing_copy() {
            --alive_copies;
        }
    };
} // namespace

// A copy of an inline map copies the elements one by one. If a copy throws, the copied elements are destroyed and the
// target is empty.
TEST_CASE("a copy of inline elements that throws leaves the target empty") {
    using map_t = map_with<int, throwing_copy, 4>;
    {
        auto source = map_t{};
        for (int key = 0; key < 4; ++key) {
            source.try_emplace(key, key * 10);
        }
        int const before = alive_copies;
        for (int budget = 0; budget < 4; ++budget) {
            CAPTURE(budget);
            copies_until_throw = budget;
            CHECK_THROWS_AS(static_cast<void>(map_t(source)), std::runtime_error);
            CHECK(alive_copies == before);

            auto target = map_t{};
            target.try_emplace(9, 90);
            copies_until_throw = budget;
            CHECK_THROWS_AS(target = source, std::runtime_error);
            copies_until_throw = -1;
            CHECK(target.empty());
            CHECK(target.begin() == target.end());
            target.try_emplace(8, 80);
            CHECK(target.at(8).value == 80);
        }
        copies_until_throw = -1;
        auto const copy = source;
        CHECK(copy.size() == 4);
        CHECK(copy.at(3).value == 30);
    }
    CHECK(alive_copies == 0);
}

TEST_CASE("merge moves elements between inline and bucket storage") {
    using map_t = map_with<int, int, 5>;

    auto target = map_t{{0, 0}, {1, 10}};
    auto source = map_t{{1, 99}, {2, 20}, {3, 30}};
    target.merge(source);
    CHECK(target.bucket_count() == 64);
    check_holds_numbers(target, 4);
    CHECK(source.size() == 1);
    CHECK(source.at(1) == 99);

    // the merge fills the inline storage and moves the elements into buckets in the middle
    auto more = map_t{{4, 40}, {5, 50}, {6, 60}, {7, 70}};
    target.merge(more);
    CHECK(target.bucket_count() > 0);
    check_holds_numbers(target, 8);
    CHECK(more.empty());

    // from buckets into an inline map
    auto big = map_t{};
    for (int key = 0; key < 20; ++key) {
        big.try_emplace(key, key * 10);
    }
    auto small = map_t{{0, 0}};
    small.merge(big);
    check_holds_numbers(small, 20);
    CHECK(big.size() == 1);
    CHECK(big.at(0) == 0);
}

TEST_CASE("merge works between maps with different inline capacities") {
    using default_t = sparse_map<int, int>;
    using five_t = map_with<int, int, 5>;
    using large_t = map_with<int, int, 15>;

    auto target = five_t{{0, 0}};
    auto not_inline = default_t{{1, 10}, {2, 20}};
    auto large = large_t{{3, 30}, {4, 40}, {5, 50}, {6, 60}, {7, 70}, {8, 80}, {9, 90}};
    CHECK(not_inline.bucket_count() > 0);
    CHECK(large.bucket_count() == 64);
    target.merge(not_inline);
    target.merge(large);
    check_holds_numbers(target, 10);
    CHECK(not_inline.empty());
    CHECK(large.empty());

    auto back = large_t{};
    back.merge(target);
    check_holds_numbers(back, 10);
    CHECK(back.bucket_count() == 64);
    CHECK(target.empty());

    auto set = set_with<int, 4>{1, 2};
    auto other_set = sparse_set<int>{2, 3};
    set.merge(other_set);
    CHECK(set.size() == 3);
    CHECK(other_set.size() == 1);
}

namespace {
    /// a node of a tree: its children are in a map of its own type, which is incomplete where the map is a member
    struct tree_node {
        int value = 0;
        sparse_map<int, tree_node> children;
    };
} // namespace

TEST_CASE("a map with the inline capacity 0 can be a member of its own mapped type") {
    tree_node root;
    root.children[1].value = 1;
    root.children[1].children[2].value = 2;
    root.children[3].value = 3;
    CHECK(root.children.size() == 2);
    CHECK(root.children.at(1).children.at(2).value == 2);
    CHECK(root.children.at(3).value == 3);
}

namespace constant_evaluation {
    using namespace dice::sparse_map;

    struct constexpr_hash {
        using is_avalanching = void;

        [[nodiscard]] constexpr std::size_t operator()(int key) const noexcept {
            return static_cast<std::size_t>(key) * std::size_t{0x9E3779B97F4A7C15};
        }
    };

    using map_t = sparse_map<
        int,
        int,
        constexpr_hash,
        std::equal_to<int>,
        std::allocator<std::pair<int, int>>,
        sh::sparsity::medium,
        sh::allocation_failure::terminating,
        4
    >;

    /**
     * In a constant expression a map with an inline capacity keeps no element inline: it has a bucket array after its
     * first insertion. At run time the element is inline. In both cases the map has 64 buckets.
     */
    constexpr bool small_maps_in_a_constant_expression() {
        auto first = map_t{};
        first.try_emplace(1, 10);
        bool const buckets_right = first.bucket_count() == 64;
        auto second = map_t{};
        second.swap(first);
        auto third = second;
        auto fourth = std::move(third);
        fourth.erase(1);
        second.try_emplace(2, 20);
        second.erase(second.find(2));
        return buckets_right
            && first.empty()
            && first.bucket_count() == 0
            && second.at(1) == 10
            && second.size() == 1
            && fourth.empty()
            && third.empty(); // NOLINT(bugprone-use-after-move)
    }

    static_assert(small_maps_in_a_constant_expression());

    /// elements that the groups copy as bytes
    using set_t = sparse_set<
        int,
        constexpr_hash,
        std::equal_to<int>,
        std::allocator<int>,
        sh::sparsity::medium,
        sh::allocation_failure::terminating,
        4
    >;

    /**
     * Swaps two containers without buckets. In a constant expression both are inline and their inline groups are
     * empty.
     */
    template<typename Container>
    constexpr bool swap_two_without_buckets() {
        auto first = Container{};
        auto second = Container{};
        first.swap(second);
        swap(first, second);
        return first.empty() && second.empty() && first.bucket_count() == 0 && second.bucket_count() == 0;
    }

    static_assert(swap_two_without_buckets<map_t>());
    static_assert(swap_two_without_buckets<set_t>());
} // namespace constant_evaluation

TEST_CASE("small maps with an inline capacity in a constant expression and at run time") {
    CHECK(constant_evaluation::small_maps_in_a_constant_expression());
    CHECK(constant_evaluation::swap_two_without_buckets<constant_evaluation::map_t>());
    CHECK(constant_evaluation::swap_two_without_buckets<constant_evaluation::set_t>());
}
