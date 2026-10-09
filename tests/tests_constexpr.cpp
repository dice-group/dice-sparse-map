#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <ranges>
#include <string>
#include <type_traits>
#include <utility>

using namespace dice::sparse_map;

namespace constant_evaluation {
    /**
     * Neither `std::hash` nor the default hash function `dice::hash::DiceHash` is constexpr, so the containers need
     * another hash function in a constant expression. `tests::test_hash` is not `constexpr` because `std::hash` is
     * not. This one mixes the key with one multiplication, like `tests::test_hash`, and is marked as avalanching
     * like it.
     */
    struct constexpr_hash {
        using is_avalanching = void;

        [[nodiscard]] constexpr std::size_t operator()(int key) const noexcept {
            return static_cast<std::size_t>(
                tests::multiply_fold(static_cast<std::uint64_t>(key), UINT64_C(0x9E3779B97F4A7C15))
            );
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

        return map.size() == 300
            && map.at(7) == 50
            && !map.contains(10)
            && map.at(1000) == 3
            && sum == (299 * 300 / 2) - 10 + 1000
            && moved.size() == 299
            && !moved.contains(20)
            && assigned
            && erase_if(moved, [](auto const &element) { return element.first < 100; }) == 98;
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
        static_assert(
            std::is_nothrow_move_constructible_v<detail_sparse_hash::map_slot<int, std::string>>
            && !std::is_trivially_destructible_v<detail_sparse_hash::map_slot<int, std::string>>
        );
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

    /// a value whose move constructor can throw, so a rehash copies it
    using copied_value = tests::copied_value<int>;

    /**
     * A rehash copies the elements of this map and frees the old groups at the end: their move constructor can throw.
     * An erase destroys an element in place and leaves a hole, a slot without an object. A constant expression that
     * reads a hole as an element does not compile.
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
        bool const rehashed = map.size() == 100 && map.bucket_count() >= 1024 && all_found;

        // holes in the middle of the groups: 34 erased, 66 left
        for (int i = 0; i < 100; i += 3) {
            map.erase(i);
        }
        int sum = 0;
        int count = 0;
        for (auto &&[key, value] : map) {
            sum += value.value;
            count += key == value.value ? 1 : 0;
        }
        bool const holes_skipped = map.size() == 66
            && count == 66
            && sum == 4950 - 1683
            && !map.contains(3)
            && map.contains(4);

        // the insertions of 0, 3, ..., 27 fill holes
        for (int i = 0; i < 30; i += 3) {
            map.try_emplace(i, i);
        }
        bool const holes_filled = map.size() == 76 && map.at(27).value == 27 && !map.contains(30);

        // a copy has no holes, a rehash neither
        auto copy = map;
        copy.erase(1);
        copy.rehash(2048);
        auto other = sparse_map<int, copied_value, constexpr_hash>{};
        other.try_emplace(1000, 1000);
        other.try_emplace(2, -1);
        copy.merge(other);
        bool const merged = copy.size() == 76
            && copy.at(1000).value == 1000
            && other.size() == 1
            && other.at(2).value == -1;

        // the even keys: 33 below 100 that are not a multiple of 3, 0, 6, 12, 18, 24 and 1000
        auto const erased = erase_if(copy, [](auto const &element) { return element.first % 2 == 0; });
        bool const erased_right = erased == 39 && copy.size() == 37 && !copy.contains(1000) && copy.contains(5);

        map.clear();
        return rehashed
            && holes_skipped
            && holes_filled
            && merged
            && erased_right
            && map.empty()
            && map.begin() == map.end();
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
        bool const found = map.find(4)->second == 40
            && map.count(5) == 1
            && map.equal_range(6).first->second == 60
            && map.find(7, constexpr_hash{}(7))->second == 70
            && map.contains(10, constexpr_hash{}(10))
            && !map.empty()
            && map.max_load_factor() == 0.5f;

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
        auto const erased = erase_if(set, [](int key) { return key % 2 == 1; });
        return found && sizes && moved.empty() && erased == 6 && set.size() == 6 && set.contains(20);
    }

    /**
     * `emplace` and `emplace_hint` construct the new element with the allocator of the container before they insert it.
     */
    constexpr bool emplace_in_a_constant_expression() {
        auto map = sparse_map<int, std::string, constexpr_hash>{};
        map.emplace(1, std::string(20, 'a'));
        map.emplace(std::pair{2, std::string(20, 'b')});
        map.emplace_hint(map.end(), 3, std::string(20, 'c'));
        map.emplace(1, std::string(20, 'x'));
        auto set = sparse_set<int, constexpr_hash>{};
        set.emplace(7);
        set.emplace_hint(set.end(), 8);
        return map.size() == 3
            && map.at(1) == std::string(20, 'a')
            && map.at(3) == std::string(20, 'c')
            && set.size() == 2
            && set.contains(8);
    }

    static_assert(map_in_a_constant_expression());
    static_assert(set_in_a_constant_expression());
    static_assert(map_of_strings_in_a_constant_expression());
    static_assert(map_of_copied_values_in_a_constant_expression());
    static_assert(more_members_in_a_constant_expression());
    static_assert(emplace_in_a_constant_expression());
} // namespace constant_evaluation

TEST_CASE("containers in constant expressions") {
    CHECK(constant_evaluation::map_in_a_constant_expression());
    CHECK(constant_evaluation::set_in_a_constant_expression());
    CHECK(constant_evaluation::map_of_strings_in_a_constant_expression());
    CHECK(constant_evaluation::map_of_copied_values_in_a_constant_expression());
    CHECK(constant_evaluation::more_members_in_a_constant_expression());
    CHECK(constant_evaluation::emplace_in_a_constant_expression());
}
