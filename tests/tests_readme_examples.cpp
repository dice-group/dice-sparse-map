// The examples of README.md and doc/usage.md, so that they keep compiling and doing what the docs say.

#include <dice/unordered_sparse.hpp>

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

        std::size_t operator()(std::string_view str) const noexcept {
            return std::hash<std::string_view>{}(str);
        }
    };

}  // namespace

TEST_CASE("readme example") {
    dice::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    map["c"] = 3;
    map.erase("a");

    dice::sparse_set const wanted(std::from_range, std::views::single(2));
    static_assert(std::is_same_v<decltype(wanted), dice::sparse_set<int> const>);

    std::vector<std::string> printed;
    for (auto const &[key, value] : map) {
        if (wanted.contains(value)) {
            printed.push_back(key);
        }
    }
    CHECK(printed == std::vector<std::string>{"b"});
}

TEST_CASE("example") {
    dice::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    map["c"] = 3;
    map["d"] = 4;

    map.insert({"e", 5});
    map.erase("b");

    for (auto &&[key, value] : map) {
        value += 2;
    }

    CHECK(map == dice::sparse_map<std::string, int>{{"a", 3}, {"c", 5}, {"d", 6}, {"e", 7}});
    CHECK(map.contains("a"));
    CHECK(map.lookup("z").value_or(0) == 0);

    std::size_t const precalculated_hash = map.hash_function()("a");
    CHECK(map.find("a", precalculated_hash) != map.end());

    erase_if(map, [](auto const &element) {
        return element.second > 6;
    });
    CHECK(map.size() == 3);

    dice::sparse_set<int> set;
    set.insert({1, 9, 0});
    set.insert({2, -1, 9});
    CHECK(set == dice::sparse_set<int>{0, 1, 2, 9, -1});
}

TEST_CASE("for_each") {
    dice::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    int sum = 0;
    map.for_each([&sum](auto &&element) {
        element.second += 10;
        sum += element.second;
    });
    CHECK(sum == 23);
    CHECK(map == dice::sparse_map<std::string, int>{{"a", 11}, {"b", 12}});
}

TEST_CASE("for_each_while") {
    dice::sparse_map<std::string, int> map = {{"a", 1}, {"b", -2}, {"c", 3}};
    std::string first_negative;
    bool const all_positive = map.for_each_while([&first_negative](auto const &element) {
        if (element.second < 0) {
            first_negative = element.first;
            return false;
        }
        return true;
    });
    CHECK_FALSE(all_positive);
    CHECK(first_negative == "b");
}

TEST_CASE("heterogeneous lookup") {
    dice::sparse_map<std::string, int, string_hash, std::equal_to<>> map;

    map.try_emplace(std::string_view{"John Doe"}, 2001);
    map[std::string_view{"Jane Doe"}] = 2002;

    CHECK(map.at(std::string_view{"John Doe"}) == 2001);

    map.erase(std::string_view{"Jane Doe"});
    CHECK(map.size() == 1);
}
