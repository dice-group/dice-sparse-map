// The examples of README.md and doc/usage.md, so that they keep compiling and doing what the docs say. Not here:
// the sketch of a hash function in doc/usage.md (it names no real hash) and the metall alias (it needs metall).

#include <dice/hash/DiceHash.hpp>
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <functional>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace {

    struct string_hash {
        using is_transparent = void;
        using is_avalanching = void;

        std::size_t operator()(std::string_view str) const noexcept {
            return dice::hash::DiceHash<std::string_view, dice::hash::Policies::wyhash>{}(str);
        }
    };

}  // namespace

TEST_CASE("readme example") {
    dice::sparse_map::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    map["c"] = 3;
    map.erase("a");

    dice::sparse_map::sparse_set const wanted(std::from_range, std::views::single(2));
    static_assert(std::is_same_v<decltype(wanted), dice::sparse_map::sparse_set<int> const>);

    std::vector<std::string> printed;
    for (auto const &[key, value] : map) {
        if (wanted.contains(value)) {
            printed.push_back(key);
        }
    }
    CHECK(printed == std::vector<std::string>{"b"});
}

TEST_CASE("example") {
    dice::sparse_map::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    map["c"] = 3;
    map["d"] = 4;

    map.insert({"e", 5});
    map.erase("b");

    for (auto &&[key, value] : map) {
        value += 2;
    }

    CHECK(map == dice::sparse_map::sparse_map<std::string, int>{{"a", 3}, {"c", 5}, {"d", 6}, {"e", 7}});
    CHECK(map.contains("a"));

    std::size_t const precalculated_hash = map.hash_function()("a");
    CHECK(map.find("a", precalculated_hash) != map.end());

    erase_if(map, [](auto const &element) {
        return element.second > 6;
    });
    CHECK(map.size() == 3);

    dice::sparse_map::sparse_set<int> set;
    set.insert({1, 9, 0});
    set.insert({2, -1, 9});
    CHECK(set == dice::sparse_map::sparse_set<int>{0, 1, 2, 9, -1});
}

TEST_CASE("binding the elements") {
    dice::sparse_map::sparse_map<int, int> map = {{1, 1}, {2, 1}, {3, 1}};
    for (auto &&[key, value] : map) {
        value = 2;
    }
    map.find(1)->second = 3;
    CHECK(map == dice::sparse_map::sparse_map<int, int>{{1, 3}, {2, 2}, {3, 2}});
}

TEST_CASE("heterogeneous lookup") {
    dice::sparse_map::sparse_map<std::string, int, string_hash, std::equal_to<>> map;

    map.try_emplace(std::string_view{"John Doe"}, 2001);
    map[std::string_view{"Jane Doe"}] = 2002;

    CHECK(map.at(std::string_view{"John Doe"}) == 2001);

    map.erase(std::string_view{"Jane Doe"});
    CHECK(map.size() == 1);
}
