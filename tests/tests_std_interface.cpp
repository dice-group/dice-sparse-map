#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include "fixtures/allocators.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

using namespace std::literals;
using namespace dice::sparse_map;

namespace {

    /**
     * Transparent string hash: it accepts `std::string`, `std::string_view` and `char const *` and hashes them with
     * `tests::test_hash`. It is marked as avalanching, like `tests::test_hash`.
     */
    struct string_hash {
        using is_transparent = void;
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(std::string_view str) const noexcept {
            return tests::test_hash<std::string_view>{}(str);
        }
    };

    /**
     * Key equality that is not transparent, for a merge source with another `KeyEqual`.
     */
    struct string_equal {
        [[nodiscard]] bool operator()(std::string const &lhs, std::string const &rhs) const noexcept {
            return lhs == rhs;
        }
    };

    using string_map = sparse_map<std::string, int, string_hash, std::equal_to<>>;
    using string_set = sparse_set<std::string, string_hash, std::equal_to<>>;

    template<typename Map, typename K>
    concept HasHeterogeneousTryEmplace = requires (Map map, K key) { map.try_emplace(key, 1); };

}  // namespace

TEST_SUITE("ranges") {
    TEST_CASE("insert_range after the max load factor was lowered below the load") {
        auto map = sparse_map<int, int>{};
        for (int i = 0; i < 100; ++i) {
            map[i] = i;
        }
        map.max_load_factor(0.1f);
        REQUIRE(map.load_factor() > map.max_load_factor());

        // the range records the bucket count when an element is read, so the test sees that insert_range makes room
        // for the whole range before the first element
        auto const more = std::vector<std::pair<int, int>>{{1000, 1}, {1001, 2}};
        auto bucket_counts = std::vector<std::size_t>{};
        map.insert_range(more | std::views::transform([&](std::pair<int, int> const &value) {
                             bucket_counts.push_back(map.bucket_count());
                             return value;
                         }));
        CHECK(map.size() == 102);
        CHECK(map.load_factor() <= map.max_load_factor());
        CHECK(bucket_counts == std::vector<std::size_t>(2, map.bucket_count()));
        CHECK(map.at(1001) == 2);
    }

    TEST_CASE("insert_range keeps the first element of each key") {
        auto map = sparse_map<int, int>{{1, 100}};
        auto const input = std::vector<std::pair<int, int>>{{1, 1}, {2, 2}, {2, 3}, {3, 4}};
        map.insert_range(input);
        CHECK(map.size() == 3);
        CHECK(map.at(1) == 100);
        CHECK(map.at(2) == 2);
        CHECK(map.at(3) == 4);
    }

    TEST_CASE("insert_range from a view and from another map") {
        auto source = sparse_map<int, int>{{1, 1}, {2, 4}};
        auto map = sparse_map<int, int>{};
        map.insert_range(source);
        CHECK(map == source);

        auto squares = sparse_map<int, int>{};
        squares.insert_range(std::views::iota(0, 100) | std::views::transform([](int i) {
                                 return std::pair{i, i * i};
                             }));
        CHECK(squares.size() == 100);
        CHECK(squares.at(9) == 81);

        auto set = sparse_set<int>{};
        set.insert_range(std::views::iota(0, 10));
        CHECK(set.size() == 10);
    }

    TEST_CASE("from_range constructor and ranges::to") {
        auto const input = std::vector<std::pair<std::string, int>>{{"a", 1}, {"b", 2}};
        auto const map = sparse_map<std::string, int>(std::from_range, input);
        CHECK(map.size() == 2);
        CHECK(map.at("b") == 2);

        auto const map2 = std::ranges::to<sparse_map<std::string, int>>(input);
        CHECK(map2 == map);

        auto const set = std::ranges::to<sparse_set<int>>(std::views::iota(0, 5));
        CHECK(set.size() == 5);
        CHECK(set.contains(4));
    }
}

TEST_SUITE("deduction guides") {
    TEST_CASE("class template argument deduction") {
        auto const input = std::vector<std::pair<int, std::string>>{{1, "a"}, {2, "b"}};

        sparse_map from_iterators(input.begin(), input.end());
        static_assert(std::is_same_v<decltype(from_iterators), sparse_map<int, std::string>>);
        CHECK(from_iterators.size() == 2);

        sparse_map from_list{std::pair{1, 2.5}, std::pair{2, 3.5}};
        static_assert(std::is_same_v<decltype(from_list), sparse_map<int, double>>);

        sparse_map from_range(std::from_range, input);
        static_assert(std::is_same_v<decltype(from_range), sparse_map<int, std::string>>);

        sparse_set set_from_list{1, 2, 3};
        static_assert(std::is_same_v<decltype(set_from_list), sparse_set<int>>);

        auto const keys = std::vector<std::string>{"x", "y"};
        sparse_set set_from_iterators(keys.begin(), keys.end());
        static_assert(std::is_same_v<decltype(set_from_iterators), sparse_set<std::string>>);
        CHECK(set_from_iterators.contains("y"));

        sparse_set set_from_range(std::from_range, keys);
        static_assert(std::is_same_v<decltype(set_from_range), sparse_set<std::string>>);
        CHECK(set_from_range.contains("x"));
    }

    TEST_CASE("a deduction guide without a hash function takes the default hash function") {
        using map_alloc_t = tests::id_allocator<std::pair<int, std::string>>;
        using set_alloc_t = tests::id_allocator<std::string>;
        auto const input = std::vector<std::pair<int, std::string>>{{1, "a"}, {2, "b"}};
        auto const keys = std::vector<std::string>{"x", "y"};

        sparse_map map_from_iterators(input.begin(), input.end(), 4, map_alloc_t(1));
        static_assert(std::is_same_v<decltype(map_from_iterators), sparse_map<int, std::string, detail_sparse_hash::default_hash<int>, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_iterators.get_allocator().id == 1);

        sparse_map map_from_range(std::from_range, input, map_alloc_t(2));
        static_assert(std::is_same_v<decltype(map_from_range), sparse_map<int, std::string, detail_sparse_hash::default_hash<int>, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_range.at(2) == "b");

        sparse_map map_from_list({std::pair{1, std::string{"a"}}}, 4, map_alloc_t(3));
        static_assert(std::is_same_v<decltype(map_from_list), sparse_map<int, std::string, detail_sparse_hash::default_hash<int>, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_list.size() == 1);

        sparse_set set_from_iterators(keys.begin(), keys.end(), set_alloc_t(4));
        static_assert(std::is_same_v<decltype(set_from_iterators), sparse_set<std::string, detail_sparse_hash::default_hash<std::string>, std::equal_to<std::string>, set_alloc_t>>);
        CHECK(set_from_iterators.contains("x"));

        sparse_set set_from_range(std::from_range, keys, 4, set_alloc_t(5));
        static_assert(std::is_same_v<decltype(set_from_range), sparse_set<std::string, detail_sparse_hash::default_hash<std::string>, std::equal_to<std::string>, set_alloc_t>>);
        CHECK(set_from_range.size() == 2);

        sparse_set set_from_list({1, 2, 3}, set_alloc_t::rebind<int>::other(6));
        static_assert(std::is_same_v<decltype(set_from_list), sparse_set<int, detail_sparse_hash::default_hash<int>, std::equal_to<int>, tests::id_allocator<int>>>);
        CHECK(set_from_list.size() == 3);

        sparse_map map_from_iterators_alloc(input.begin(), input.end(), map_alloc_t(7));
        static_assert(std::is_same_v<decltype(map_from_iterators_alloc), sparse_map<int, std::string, detail_sparse_hash::default_hash<int>, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_iterators_alloc.at(1) == "a");

        sparse_map map_from_range_count(std::from_range, input, 4, map_alloc_t(8));
        static_assert(std::is_same_v<decltype(map_from_range_count), sparse_map<int, std::string, detail_sparse_hash::default_hash<int>, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_range_count.at(1) == "a");
        CHECK(map_from_range_count.get_allocator().id == 8);

        sparse_map map_from_list_alloc({std::pair{1, std::string{"a"}}}, map_alloc_t(9));
        static_assert(std::is_same_v<decltype(map_from_list_alloc), sparse_map<int, std::string, detail_sparse_hash::default_hash<int>, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_list_alloc.at(1) == "a");

        sparse_set set_from_iterators_count(keys.begin(), keys.end(), 4, set_alloc_t(10));
        static_assert(std::is_same_v<decltype(set_from_iterators_count), sparse_set<std::string, detail_sparse_hash::default_hash<std::string>, std::equal_to<std::string>, set_alloc_t>>);
        CHECK(set_from_iterators_count.contains("y"));

        sparse_set set_from_range_alloc(std::from_range, keys, set_alloc_t(11));
        static_assert(std::is_same_v<decltype(set_from_range_alloc), sparse_set<std::string, detail_sparse_hash::default_hash<std::string>, std::equal_to<std::string>, set_alloc_t>>);
        CHECK(set_from_range_alloc.contains("y"));
        CHECK(set_from_range_alloc.get_allocator().id == 11);

        sparse_set set_from_list_count({1, 2, 3}, 4, tests::id_allocator<int>(12));
        static_assert(std::is_same_v<decltype(set_from_list_count), sparse_set<int, detail_sparse_hash::default_hash<int>, std::equal_to<int>, tests::id_allocator<int>>>);
        CHECK(set_from_list_count.contains(3));
    }

    TEST_CASE("a deduction guide with a hash function takes it") {
        using int_hash_t = tests::test_hash<int>;
        using string_hash_t = tests::test_hash<std::string>;
        using map_alloc_t = tests::id_allocator<std::pair<int, std::string>>;
        using set_alloc_t = tests::id_allocator<std::string>;
        auto const input = std::vector<std::pair<int, std::string>>{{1, "a"}, {2, "b"}};
        auto const keys = std::vector<std::string>{"x", "y"};

        sparse_map map_from_iterators(input.begin(), input.end(), 4, int_hash_t{});
        static_assert(std::is_same_v<decltype(map_from_iterators), sparse_map<int, std::string, int_hash_t>>);
        CHECK(map_from_iterators.at(1) == "a");

        sparse_map map_from_range(std::from_range, input, 4, int_hash_t{});
        static_assert(std::is_same_v<decltype(map_from_range), sparse_map<int, std::string, int_hash_t>>);
        CHECK(map_from_range.at(2) == "b");

        sparse_map map_from_list({std::pair{1, 2.5}}, 4, int_hash_t{});
        static_assert(std::is_same_v<decltype(map_from_list), sparse_map<int, double, int_hash_t>>);
        CHECK(map_from_list.at(1) == 2.5);

        sparse_map map_from_iterators_alloc(input.begin(), input.end(), 4, int_hash_t{}, map_alloc_t(1));
        static_assert(std::is_same_v<decltype(map_from_iterators_alloc), sparse_map<int, std::string, int_hash_t, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_iterators_alloc.at(1) == "a");

        sparse_map map_from_range_alloc(std::from_range, input, 4, int_hash_t{}, map_alloc_t(2));
        static_assert(std::is_same_v<decltype(map_from_range_alloc), sparse_map<int, std::string, int_hash_t, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_range_alloc.at(2) == "b");
        CHECK(map_from_range_alloc.get_allocator().id == 2);

        sparse_map map_from_list_alloc({std::pair{1, std::string{"a"}}}, 4, int_hash_t{}, map_alloc_t(3));
        static_assert(std::is_same_v<decltype(map_from_list_alloc), sparse_map<int, std::string, int_hash_t, std::equal_to<int>, map_alloc_t>>);
        CHECK(map_from_list_alloc.at(1) == "a");

        sparse_set set_from_iterators(keys.begin(), keys.end(), 4, string_hash_t{});
        static_assert(std::is_same_v<decltype(set_from_iterators), sparse_set<std::string, string_hash_t>>);
        CHECK(set_from_iterators.contains("x"));

        sparse_set set_from_range(std::from_range, keys, 4, string_hash_t{});
        static_assert(std::is_same_v<decltype(set_from_range), sparse_set<std::string, string_hash_t>>);
        CHECK(set_from_range.contains("y"));

        sparse_set set_from_list({1, 2, 3}, 4, int_hash_t{});
        static_assert(std::is_same_v<decltype(set_from_list), sparse_set<int, int_hash_t>>);
        CHECK(set_from_list.contains(3));

        sparse_set set_from_iterators_alloc(keys.begin(), keys.end(), 4, string_hash_t{}, set_alloc_t(4));
        static_assert(std::is_same_v<decltype(set_from_iterators_alloc), sparse_set<std::string, string_hash_t, std::equal_to<std::string>, set_alloc_t>>);
        CHECK(set_from_iterators_alloc.contains("x"));

        sparse_set set_from_range_alloc(std::from_range, keys, 4, string_hash_t{}, set_alloc_t(5));
        static_assert(std::is_same_v<decltype(set_from_range_alloc), sparse_set<std::string, string_hash_t, std::equal_to<std::string>, set_alloc_t>>);
        CHECK(set_from_range_alloc.contains("y"));
        CHECK(set_from_range_alloc.get_allocator().id == 5);

        sparse_set set_from_list_alloc({1, 2, 3}, 4, int_hash_t{}, tests::id_allocator<int>(6));
        static_assert(std::is_same_v<decltype(set_from_list_alloc), sparse_set<int, int_hash_t, std::equal_to<int>, tests::id_allocator<int>>>);
        CHECK(set_from_list_alloc.contains(3));
    }
}

TEST_SUITE("erase_if and merge") {
    TEST_CASE_MAP("erase_if on a map", int, int) {
        auto map = map_t{};
        for (int i = 0; i < 1000; ++i) {
            map[i] = i * 3;
        }

        auto const erased = erase_if(map, [](auto const &element) {
            return element.second % 2 == 0;
        });
        CHECK(erased == 500);
        CHECK(map.size() == 500);
        for (int i = 0; i < 1000; ++i) {
            CHECK(map.contains(i) == (i % 2 == 1));
        }
    }

    TEST_CASE_SET("erase_if on a set", int) {
        auto set = set_t{};
        for (int i = 0; i < 1000; ++i) {
            set.insert(i);
        }
        CHECK(erase_if(set, [](int key) {
                  return key < 100;
              })
              == 100);
        CHECK(set.size() == 900);
        CHECK(!set.contains(99));
        CHECK(set.contains(100));
    }

    TEST_CASE("merge moves the elements whose keys are missing") {
        auto target = sparse_map<std::string, int>{{"a", 1}, {"b", 2}};
        // another hash function, key equality, sparsity and allocation failure mode
        auto source = sparse_map<std::string,
                                 int,
                                 tests::test_hash<std::string>,
                                 string_equal,
                                 std::allocator<std::pair<std::string, int>>,
                                 sh::sparsity::high,
                                 sh::allocation_failure::throwing>{{"b", 20}, {"c", 30}, {"d", 40}};

        target.merge(source);

        CHECK(target.size() == 4);
        CHECK(target.at("b") == 2);  // the element of the target stays
        CHECK(target.at("c") == 30);
        CHECK(target.at("d") == 40);
        CHECK(source.size() == 1);  // the duplicate stays in the source
        CHECK(source.at("b") == 20);

        auto other = sparse_map<std::string, int>{{"e", 50}};
        target.merge(std::move(other));
        CHECK(target.at("e") == 50);
    }

    TEST_CASE("merge of sets") {
        auto target = sparse_set<int>{1, 2, 3};
        auto source = sparse_set<int>{3, 4};
        target.merge(source);
        CHECK(target.size() == 4);
        CHECK(source.size() == 1);
        CHECK(source.contains(3));

        target.merge(sparse_set<int>{5});
        CHECK(target.contains(5));
    }

    TEST_CASE("merge of sets moves the elements whose keys are missing from a set of another type") {
        auto target = sparse_set<std::string>{"a", "b"};
        // another hash function, key equality, sparsity and allocation failure mode
        auto source = sparse_set<std::string,
                                 tests::test_hash<std::string>,
                                 string_equal,
                                 std::allocator<std::string>,
                                 sh::sparsity::high,
                                 sh::allocation_failure::throwing>{"b", "c", "d"};

        target.merge(source);

        CHECK(target.size() == 4);
        CHECK(target.contains("c"));
        CHECK(target.contains("d"));
        CHECK(source.size() == 1);  // the duplicate stays in the source
        CHECK(source.contains("b"));
    }

    TEST_CASE("merge of a container into itself changes nothing, also when the next insertion would rehash") {
        // with the default max load factor, 1, 2, 4, 8 and so on elements fill a table up to its threshold
        for (int size : {1, 2, 4, 8, 16, 5}) {
            CAPTURE(size);
            auto map = sparse_map<std::string, int>{};
            auto set = sparse_set<int>{};
            for (int i = 0; i < size; ++i) {
                map.try_emplace(std::to_string(i), i);
                set.insert(i);
            }
            auto const bucket_count = map.bucket_count();

            map.merge(map);
            set.merge(set);

            CHECK(map.size() == static_cast<std::size_t>(size));
            CHECK(set.size() == static_cast<std::size_t>(size));
            CHECK(map.bucket_count() == bucket_count);
            for (int i = 0; i < size; ++i) {
                CHECK(map.at(std::to_string(i)) == i);
                CHECK(set.contains(i));
            }
        }
    }
}

TEST_SUITE("heterogeneous insertion") {
    static_assert(HasHeterogeneousTryEmplace<string_map, std::string_view>);
    static_assert(!HasHeterogeneousTryEmplace<sparse_map<std::string, int>, std::string_view>);

    TEST_CASE("try_emplace, insert_or_assign, operator[] and erase with a string_view") {
        auto map = string_map{};

        auto const [it, inserted] = map.try_emplace("key"sv, 1);
        CHECK(inserted);
        CHECK(it->first == "key");
        CHECK(!map.try_emplace("key"sv, 2).second);
        CHECK(map.at("key"sv) == 1);

        CHECK(!map.insert_or_assign("key"sv, 3).second);
        CHECK(map.at("key") == 3);
        CHECK(map.insert_or_assign("other"sv, 4).second);

        map["third"sv] = 5;
        CHECK(map.at("third") == 5);
        CHECK(map.size() == 3);

        CHECK(map.try_emplace(map.cbegin(), "fourth"sv, 6)->second == 6);
        CHECK(map.erase("fourth"sv) == 1);
        CHECK(map.erase("missing"sv) == 0);
        CHECK(map.size() == 3);
    }

    TEST_CASE("insert into a set with a string_view") {
        auto set = string_set{};
        CHECK(set.insert("key"sv).second);
        CHECK(!set.insert("key"sv).second);
        CHECK(set.contains("key"sv));
        CHECK(*set.insert(set.cbegin(), "key"sv) == "key");
        CHECK(set.size() == 1);
    }
}

TEST_SUITE("allocator-extended constructors") {
    using map_t = sparse_map<int, int, tests::test_hash<int>, std::equal_to<int>, tests::id_allocator<std::pair<int, int>>>;
    using set_t = sparse_set<int, tests::test_hash<int>, std::equal_to<int>, tests::id_allocator<int>>;

    /**
     * Inserts the keys 0 to 99. A map maps each key to itself.
     */
    template<typename Container>
    void insert_keys(Container & container) {
        for (int i = 0; i < 100; ++i) {
            if constexpr (std::is_same_v<Container, map_t>) {
                container[i] = i;
            } else {
                container.insert(i);
            }
        }
    }

    template<typename Container>
    bool holds_42(Container const &container) {
        if constexpr (std::is_same_v<Container, map_t>) {
            return container.at(42) == 42;
        } else {
            return container.contains(42);
        }
    }

    TEST_CASE_TEMPLATE("copy with another allocator", Container, map_t, set_t) {
        using alloc_t = typename Container::allocator_type;
        auto counts = tests::alloc_counts{};
        auto source = Container(alloc_t(1));
        insert_keys(source);

        auto const copy = Container(source, alloc_t(9, &counts));
        CHECK(copy.get_allocator().id == 9);
        CHECK(copy == source);
        CHECK(counts.allocations > 0);
    }

    TEST_CASE_TEMPLATE("move with an equal allocator takes the storage", Container, map_t, set_t) {
        using alloc_t = typename Container::allocator_type;
        auto counts = tests::alloc_counts{};
        auto source = Container(alloc_t(1, &counts));
        insert_keys(source);
        auto const allocations = counts.allocations;

        auto const moved = Container(std::move(source), alloc_t(1, &counts));
        CHECK(counts.allocations == allocations);
        CHECK(moved.size() == 100);
        CHECK(source.empty());  // NOLINT(bugprone-use-after-move)
    }

    TEST_CASE_TEMPLATE("move with an unequal allocator moves the elements", Container, map_t, set_t) {
        using alloc_t = typename Container::allocator_type;
        auto counts = tests::alloc_counts{};
        auto source = Container(alloc_t(1));
        insert_keys(source);

        auto const moved = Container(std::move(source), alloc_t(2, &counts));
        CHECK(moved.get_allocator().id == 2);
        CHECK(counts.allocations > 0);
        CHECK(moved.size() == 100);
        CHECK(holds_42(moved));
        CHECK(source.empty());  // NOLINT(bugprone-use-after-move)
    }
}

namespace constant_evaluation {
    /**
     * Neither `std::hash` nor the default hash function `dice::hash::DiceHash` is constexpr, so the containers need
     * another hash function in a constant expression. This one mixes the key with one multiplication, like
     * `tests::test_hash`, and is marked as avalanching like it.
     */
    struct constexpr_hash {
        using is_avalanching = void;

        [[nodiscard]] constexpr std::size_t operator()(int key) const noexcept {
            return static_cast<std::size_t>(tests::multiply_fold(static_cast<std::uint64_t>(key), UINT64_C(0x9E3779B97F4A7C15)));
        }
    };

    /**
     * A rehash moves the elements of this map and frees each old group right after its elements are moved: their move
     * constructor cannot throw.
     */
    constexpr bool map_in_a_constant_expression() {
        auto map = sparse_map<int, int, constexpr_hash>{};
        for (int i = 0; i < 300; ++i) {
            map[i] = i * i;
        }
        map.erase(10);
        map.try_emplace(1000, 1);
        map.insert_or_assign(1000, 2);

        int sum = 0;
        for (auto &&[key, value] : map) {
            value += 1;
            sum += key;
        }

        auto copy = map;
        copy.erase(20);
        auto moved = std::move(copy);
        moved.rehash(4096);

        auto copy_assigned = sparse_map<int, int, constexpr_hash>{};
        copy_assigned = moved;
        auto move_assigned = sparse_map<int, int, constexpr_hash>{};
        move_assigned = std::move(copy_assigned);
        bool const assigned = move_assigned == moved;

        return map.size() == 300 && map.at(7) == 50 && !map.contains(10) && map.at(1000) == 3
               && sum == (299 * 300 / 2) - 10 + 1000 && moved.size() == 299 && !moved.contains(20) && assigned
               && erase_if(moved, [](auto const &element) {
                      return element.first < 100;
                  }) == 98;
    }

    constexpr bool set_in_a_constant_expression() {
        auto set = sparse_set<int, constexpr_hash>{1, 2, 3};
        set.insert_range(std::views::iota(10, 20));
        auto other = sparse_set<int, constexpr_hash>{3, 4};
        set.merge(other);
        return set.size() == 14 && set.contains(4) && other.size() == 1;
    }

    /**
     * Like `map_in_a_constant_expression`, with elements that are not trivial: a rehash moves them and destroys the
     * moved-from ones.
     */
    constexpr bool map_of_strings_in_a_constant_expression() {
        static_assert(std::is_nothrow_move_constructible_v<detail_sparse_hash::map_slot<int, std::string>>
                      && !std::is_trivially_destructible_v<detail_sparse_hash::map_slot<int, std::string>>);
        auto map = sparse_map<int, std::string, constexpr_hash>{};
        for (int i = 0; i < 100; ++i) {
            map.try_emplace(i, std::string(20, static_cast<char>('a' + (i % 26))));
        }
        map.rehash(1024);

        bool all_found = true;
        for (int i = 0; i < 100; ++i) {
            all_found = all_found && map.at(i) == std::string(20, static_cast<char>('a' + (i % 26)));
        }
        return map.size() == 100 && map.bucket_count() >= 1024 && all_found;
    }

    /**
     * A value whose move constructor can throw, so a rehash copies it.
     */
    struct copied_value {
        int value = 0;

        constexpr explicit copied_value(int v) noexcept
            : value(v) {
        }

        constexpr copied_value(copied_value const &other) noexcept
            : value(other.value) {
        }

        constexpr copied_value(copied_value &&other) noexcept(false)  // NOLINT(performance-noexcept-move-constructor)
            : value(other.value) {
        }

        constexpr copied_value &operator=(copied_value const &) = default;
        constexpr copied_value &operator=(copied_value &&) = default;
        constexpr ~copied_value() = default;
    };

    /**
     * A rehash copies the elements of this map and frees the old groups at the end: their move constructor can throw.
     */
    constexpr bool map_of_copied_values_in_a_constant_expression() {
        static_assert(!std::is_nothrow_move_constructible_v<detail_sparse_hash::map_slot<int, copied_value>>);
        auto map = sparse_map<int, copied_value, constexpr_hash>{};
        for (int i = 0; i < 100; ++i) {
            map.try_emplace(i, i);
        }
        map.rehash(1024);

        bool all_found = true;
        for (int i = 0; i < 100; ++i) {
            all_found = all_found && map.at(i).value == i;
        }
        return map.size() == 100 && map.bucket_count() >= 1024 && all_found;
    }

    /**
     * The members that the functions above do not call: the other constructors, lookups, the insertions with a hint
     * or of a list, emplace, erase of a range, swap, clear, reserve, the max load factor, the precalculated hash, and
     * the set's rvalue `merge` and `erase_if`.
     */
    constexpr bool more_members_in_a_constant_expression() {
        using map_t = sparse_map<int, int, constexpr_hash>;
        auto map = map_t(std::from_range, std::views::iota(1, 4) | std::views::transform([](int i) {
                                              return std::pair{i, i * 10};
                                          }));
        map.insert(std::pair{4, 40});
        map.insert(map.cbegin(), std::pair{5, 50});
        map.insert({std::pair{6, 60}, std::pair{7, 70}});
        map.emplace(8, 80);
        map.emplace_hint(map.cend(), 9, 90);
        map.try_emplace(map.cend(), 10, 100);
        map.insert_or_assign(map.cend(), 10, 101);
        map.max_load_factor(0.5f);
        map.reserve(64);
        bool const found = map.find(4)->second == 40 && map.count(5) == 1 && map.equal_range(6).first->second == 60
                           && map.find(7, constexpr_hash{}(7))->second == 70 && map.contains(10, constexpr_hash{}(10))
                           && !map.empty() && map.max_load_factor() == 0.5f;

        auto copy = map_t(map, map.get_allocator());
        auto moved = map_t(std::move(copy), map.get_allocator());
        map.erase(map.find(1));
        map.erase(map.cbegin(), std::next(map.cbegin()));
        moved.swap(map);
        swap(moved, map);
        bool const sizes = map.size() == 8 && moved.size() == 10 && moved.at(10) == 101;
        moved.clear();

        auto set = sparse_set<int, constexpr_hash>(std::from_range, std::views::iota(0, 10));
        set.merge(sparse_set<int, constexpr_hash>{20, 21});
        auto const erased = erase_if(set, [](int key) {
            return key % 2 == 1;
        });
        return found && sizes && moved.empty() && erased == 6 && set.size() == 6 && set.contains(20);
    }

    static_assert(map_in_a_constant_expression());
    static_assert(set_in_a_constant_expression());
    static_assert(map_of_strings_in_a_constant_expression());
    static_assert(map_of_copied_values_in_a_constant_expression());
    static_assert(more_members_in_a_constant_expression());
}  // namespace constant_evaluation

TEST_CASE("containers in constant expressions") {
    CHECK(constant_evaluation::map_in_a_constant_expression());
    CHECK(constant_evaluation::set_in_a_constant_expression());
    CHECK(constant_evaluation::map_of_strings_in_a_constant_expression());
    CHECK(constant_evaluation::map_of_copied_values_in_a_constant_expression());
    CHECK(constant_evaluation::more_members_in_a_constant_expression());
}
