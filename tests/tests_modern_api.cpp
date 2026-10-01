#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include "fixtures/allocators.hpp"
#include "fixtures/offset_ptr_allocator.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>
#include <metall/metall.hpp>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <map>
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
     * Transparent and avalanching string hash: it accepts `std::string`, `std::string_view` and `char const *`.
     */
    struct string_hash {
        using is_transparent = void;

        [[nodiscard]] std::size_t operator()(std::string_view str) const noexcept {
            return std::hash<std::string_view>{}(str);
        }
    };

    using string_map = sparse_map<std::string, int, string_hash, std::equal_to<>>;
    using string_set = sparse_set<std::string, string_hash, std::equal_to<>>;

    /**
     * Hash that returns the key and is marked avalanching, so that a key picks its bucket directly.
     */
    struct identity_hash {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(int key) const noexcept {
            return static_cast<std::size_t>(key);
        }
    };

    template<typename Map>
    concept HasValueAccessor = requires (typename Map::iterator it) { it.value(); };

    template<typename Map>
    concept HasLookup = requires (Map map, typename Map::key_type key) { map.lookup(key); };

    /// `ref.emplace(arg)` compiles for an argument of type `Arg`
    template<typename Ref, typename Arg>
    concept CanEmplace = requires (Ref ref, Arg &&arg) { ref.emplace(std::forward<Arg>(arg)); };

    template<typename Map, typename K>
    concept HasHeterogeneousTryEmplace = requires (Map map, K key) { map.try_emplace(key, 1); };

}  // namespace

TEST_SUITE("iterators") {
    using map_t = sparse_map<std::string, int>;
    using set_t = sparse_set<std::string>;

    static_assert(std::forward_iterator<map_t::iterator>);
    static_assert(std::forward_iterator<map_t::const_iterator>);
    static_assert(std::forward_iterator<set_t::iterator>);
    static_assert(std::forward_iterator<set_t::const_iterator>);
    static_assert(std::ranges::forward_range<map_t>);
    static_assert(std::ranges::forward_range<map_t const>);
    static_assert(std::ranges::forward_range<set_t>);
    static_assert(std::is_same_v<std::iter_reference_t<map_t::iterator>, std::pair<std::string const &, int &>>);
    static_assert(std::is_same_v<std::iter_reference_t<map_t::const_iterator>, std::pair<std::string const &, int const &>>);
    static_assert(std::is_same_v<std::iter_value_t<map_t::iterator>, std::pair<std::string, int>>);
    static_assert(std::is_same_v<std::iter_reference_t<set_t::iterator>, std::string const &>);
    static_assert(!HasValueAccessor<map_t>);
    static_assert(std::is_same_v<decltype((std::declval<map_t::iterator>()->first)), std::string const &>);
    static_assert(std::is_same_v<decltype((std::declval<map_t::iterator>()->second)), int &>);
    static_assert(std::is_same_v<decltype((std::declval<map_t::const_iterator>()->second)), int const &>);
    static_assert(std::is_convertible_v<map_t::iterator, map_t::const_iterator>);
    static_assert(!std::is_convertible_v<map_t::const_iterator, map_t::iterator>);

    TEST_CASE("the mapped value is mutable through the iterator") {
        auto map = map_t{{"a", 1}, {"b", 2}, {"c", 3}};

        map.find("a")->second = 10;
        (*map.find("b")).second += 18;
        for (auto &&[key, value] : map) {
            value *= 2;
        }

        CHECK(map.at("a") == 20);
        CHECK(map.at("b") == 40);
        CHECK(map.at("c") == 6);
    }

    TEST_CASE("value-initialized iterators compare equal") {
        CHECK(map_t::iterator{} == map_t::iterator{});
        CHECK(map_t::const_iterator{} == map_t::const_iterator{});
        CHECK(set_t::iterator{} == set_t::iterator{});
    }

    TEST_CASE("const_iterator is comparable with iterator") {
        auto map = map_t{{"a", 1}};
        map_t::const_iterator const cit = map.begin();
        CHECK(cit == map.begin());
        CHECK(map.begin() == cit);
        CHECK(cit != map.end());
        CHECK(std::next(cit) == map.cend());
        CHECK(map.mutable_iterator(cit) == map.begin());
    }

    TEST_CASE("views::keys and views::values work, and values are mutable") {
        auto map = map_t{{"a", 1}, {"bb", 2}, {"ccc", 3}};

        std::size_t key_length = 0;
        for (auto const &key : map | std::views::keys) {
            key_length += key.size();
        }
        CHECK(key_length == 6);

        for (auto &value : map | std::views::values) {
            value += 100;
        }
        CHECK(map.at("bb") == 102);

        auto sorted_keys = std::vector<std::string>(std::views::keys(map).begin(), std::views::keys(map).end());
        std::ranges::sort(sorted_keys);
        CHECK(sorted_keys == std::vector<std::string>{"a", "bb", "ccc"});
    }

    TEST_CASE("the elements convert to value_type") {
        auto const map = map_t{{"a", 1}, {"b", 2}};
        auto values = std::vector<map_t::value_type>(map.begin(), map.end());
        std::ranges::sort(values);
        CHECK(values == std::vector<map_t::value_type>{{"a", 1}, {"b", 2}});

        auto const ordered = std::ranges::to<std::map<std::string, int>>(map);
        CHECK(ordered.size() == 2);
        CHECK(ordered.at("b") == 2);
    }
}

TEST_SUITE("lookup") {
    static_assert(HasLookup<sparse_map<int, int>>);
    static_assert(!HasLookup<sparse_set<int>>);

    // like std::optional<T &>, optional_ref does not bind a temporary
    static_assert(std::is_constructible_v<optional_ref<int const>, int &>);
    static_assert(std::is_constructible_v<optional_ref<int const>, int const &>);
    static_assert(!std::is_constructible_v<optional_ref<int const>, int>);
    static_assert(!std::is_constructible_v<optional_ref<int const>, long>);
    static_assert(!std::is_convertible_v<int, optional_ref<int const>>);
    static_assert(!std::is_constructible_v<optional_ref<int>, int>);
    static_assert(!std::is_constructible_v<optional_ref<std::string const>, std::string const>);
    static_assert(!std::is_constructible_v<optional_ref<std::string const>, std::string>);
    static_assert(CanEmplace<optional_ref<int const>, int const &>);
    static_assert(!CanEmplace<optional_ref<int const>, int>);
    static_assert(!CanEmplace<optional_ref<std::string const>, std::string const>);

    TEST_CASE("lookup returns a reference to the mapped value or nothing") {
        auto map = sparse_map<int, std::string>{{1, "one"}, {2, "two"}};

        auto found = map.lookup(1);
        REQUIRE(found.has_value());
        CHECK(*found == "one");
        *found = "uno";
        CHECK(map.at(1) == "uno");

        auto missing = map.lookup(3);
        CHECK(!missing.has_value());
        CHECK(missing == std::nullopt);
        CHECK(map.size() == 2);  // lookup never inserts

        CHECK(map.lookup(2).value_or("none") == "two");
        CHECK(map.lookup(3).value_or("none") == "none");
        CHECK(map.lookup(2).transform([](std::string const &s) {
            return s.size();
        }) == std::optional<std::size_t>{3});
        CHECK(!map.lookup(3).transform([](std::string const &s) {
                                return s.size();
                            })
                   .has_value());
        CHECK_THROWS_AS(static_cast<void>(map.lookup(3).value()), std::bad_optional_access);

        std::size_t visited = 0;
        for (auto &value : map.lookup(2)) {
            CHECK(value == "two");
            ++visited;
        }
        CHECK(visited == 1);
    }

    TEST_CASE("lookup on a const map returns a const reference") {
        auto const map = sparse_map<int, int>{{1, 10}};
        auto found = map.lookup(1);
        static_assert(std::is_same_v<decltype(*found), int const &>);
        CHECK(*found == 10);
    }

    TEST_CASE("heterogeneous lookup") {
        auto map = string_map{{"key", 1}};
        CHECK(map.lookup("key"sv).value_or(0) == 1);
        CHECK(!map.lookup("other"sv).has_value());
    }
}

TEST_SUITE("ranges") {
    TEST_CASE("insert_range after the max load factor was lowered below the load") {
        auto map = sparse_map<int, int>{};
        for (int i = 0; i < 100; ++i) {
            map[i] = i;
        }
        map.max_load_factor(0.1f);
        REQUIRE(map.load_factor() > map.max_load_factor());

        map.insert_range(std::vector<std::pair<int, int>>{{1000, 1}, {1001, 2}});
        CHECK(map.size() == 102);
        CHECK(map.load_factor() <= map.max_load_factor());
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
        auto source = sparse_map<std::string, int, std::hash<std::string>, std::equal_to<std::string>, std::allocator<std::pair<std::string, int>>, sh::exception_safety::basic, sh::sparsity::high>{{"b", 20}, {"c", 30}, {"d", 40}};

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
    using alloc_t = tests::id_allocator<std::pair<int, int>>;
    using map_t = sparse_map<int, int, std::hash<int>, std::equal_to<int>, alloc_t>;

    TEST_CASE("copy with another allocator") {
        auto counts = tests::alloc_counts{};
        auto source = map_t(alloc_t(1));
        for (int i = 0; i < 100; ++i) {
            source[i] = i;
        }

        auto const copy = map_t(source, alloc_t(9, &counts));
        CHECK(copy.get_allocator().id == 9);
        CHECK(copy == source);
        CHECK(counts.allocations > 0);
    }

    TEST_CASE("move with an equal allocator takes the storage") {
        auto counts = tests::alloc_counts{};
        auto source = map_t(alloc_t(1, &counts));
        for (int i = 0; i < 100; ++i) {
            source[i] = i;
        }
        auto const allocations = counts.allocations;

        auto const moved = map_t(std::move(source), alloc_t(1, &counts));
        CHECK(counts.allocations == allocations);
        CHECK(moved.size() == 100);
        CHECK(source.empty());  // NOLINT(bugprone-use-after-move)
    }

    TEST_CASE("move with an unequal allocator moves the elements") {
        auto counts = tests::alloc_counts{};
        auto source = map_t(alloc_t(1));
        for (int i = 0; i < 100; ++i) {
            source[i] = i;
        }

        auto const moved = map_t(std::move(source), alloc_t(2, &counts));
        CHECK(moved.get_allocator().id == 2);
        CHECK(counts.allocations > 0);
        CHECK(moved.size() == 100);
        CHECK(moved.at(42) == 42);
        CHECK(source.empty());  // NOLINT(bugprone-use-after-move)
    }
}

TEST_SUITE("hashing") {
    static_assert(!hash_is_avalanching_v<std::hash<int>>);
    static_assert(hash_is_avalanching_v<identity_hash>);

    TEST_CASE_MAP("a precalculated hash is the hash of the hash function", std::uint64_t, int) {
        auto map = map_t{};
        for (std::uint64_t i = 0; i < 1000; ++i) {
            map[i * 1024] = static_cast<int>(i);
        }

        auto const hash = map.hash_function();
        for (std::uint64_t i = 0; i < 1000; ++i) {
            auto const key = i * 1024;
            auto const it = map.find(key, hash(key));
            REQUIRE(it != map.end());
            CHECK(it->second == static_cast<int>(i));
            CHECK(map.contains(key, hash(key)));
            CHECK(map.count(key, hash(key)) == 1);
        }
        CHECK(!map.contains(1, hash(1)));
        CHECK(map.erase(1024, hash(1024)) == 1);
        CHECK(map.size() == 999);
    }
}

TEST_SUITE("layout") {
    using metall_manager = metall::manager;

    template<typename T>
    using metall_allocator = metall_manager::allocator_type<T>;

    static_assert(std::is_standard_layout_v<detail_sparse_hash::map_slot<int, std::uint64_t>>);
    static_assert(std::is_standard_layout_v<sparse_map<int, int>>);
    static_assert(std::is_standard_layout_v<sparse_set<int>>);
    static_assert(std::is_standard_layout_v<sparse_map<int, int, std::hash<int>, std::equal_to<int>, tests::offset_ptr_allocator<std::pair<int, int>>>>);
    static_assert(std::is_standard_layout_v<sparse_set<int, std::hash<int>, std::equal_to<int>, tests::offset_ptr_allocator<int>>>);
    static_assert(std::is_standard_layout_v<sparse_map<int, int, std::hash<int>, std::equal_to<int>, metall_allocator<std::pair<int, int>>, sh::exception_safety::basic, sh::sparsity::high>>);
    static_assert(std::is_standard_layout_v<sparse_set<int, std::hash<int>, std::equal_to<int>, metall_allocator<int>, sh::exception_safety::basic, sh::sparsity::high>>);

    TEST_CASE("sizes") {
        MESSAGE("sizeof(sparse_map<int, int>) = " << sizeof(sparse_map<int, int>));
        MESSAGE("sizeof(sparse_set<int>) = " << sizeof(sparse_set<int>));
        CHECK(sizeof(sparse_map<int, int>) == 64);
    }
}

namespace noexcept_contract {
    using map_t = sparse_map<std::string, int>;
    static_assert(std::is_nothrow_move_constructible_v<map_t>);
    static_assert(std::is_nothrow_move_assignable_v<map_t>);
    static_assert(std::is_nothrow_swappable_v<map_t>);

    using pmr_like_map = sparse_map<int, int, std::hash<int>, std::equal_to<int>, tests::pmr_like_allocator<std::pair<int, int>>>;
    static_assert(!std::is_nothrow_move_assignable_v<pmr_like_map>);  // moves element by element with unequal allocators
}  // namespace noexcept_contract

TEST_SUITE("aliasing") {
    TEST_CASE("try_emplace with a mapped value that refers to an element of the same group") {
        auto map = sparse_map<int, std::string, identity_hash>{};
        map.reserve(16);
        auto const original = std::string(100, 'x');
        map.try_emplace(1, original);

        // key 0 is placed before key 1 in the same group, so the value of key 1 moves one place
        map.try_emplace(0, map.at(1));

        CHECK(map.at(1) == original);
        CHECK(map.at(0) == original);
    }

    TEST_CASE("try_emplace with a mapped value that refers to an element, when the insertion grows the table") {
        auto map = sparse_map<int, std::string, identity_hash>{};
        auto const original = std::string(100, 'x');
        map.try_emplace(1, original);
        REQUIRE(map.bucket_count() == 2);  // the threshold is 1, so the next insertion grows the table

        map.try_emplace(5, map.at(1));

        CHECK(map.bucket_count() == 4);
        CHECK(map.at(5) == original);
        CHECK(map.at(1) == original);
    }

    TEST_CASE("operator[] with a key that refers to an element of the same group") {
        auto map = sparse_map<int, int, identity_hash>{};
        map.reserve(16);
        map[2] = 1;  // the mapped value of key 2 is the key of the next element

        map[map.at(2)] = 7;  // key 1 is placed before key 2

        CHECK(map.at(1) == 7);
        CHECK(map.at(2) == 1);
    }
}

namespace constant_evaluation {
    /**
     * `std::hash` is not constexpr, so the containers need another hash function in a constant expression.
     */
    struct constexpr_hash {
        using is_avalanching = void;

        [[nodiscard]] constexpr std::size_t operator()(int key) const noexcept {
            return static_cast<std::size_t>(key) * std::size_t{0x9E3779B97F4A7C15};
        }
    };

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

        return map.size() == 300 && map.at(7) == 50 && !map.contains(10) && map.lookup(1000).value_or(0) == 3
               && sum == (299 * 300 / 2) - 10 + 1000 && moved.size() == 299 && !moved.contains(20)
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

    static_assert(map_in_a_constant_expression());
    static_assert(set_in_a_constant_expression());
}  // namespace constant_evaluation

TEST_CASE("containers in constant expressions") {
    CHECK(constant_evaluation::map_in_a_constant_expression());
    CHECK(constant_evaluation::set_in_a_constant_expression());
}
