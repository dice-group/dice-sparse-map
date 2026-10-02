#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>

#include <algorithm>
#include <cstddef>
#include <iterator>
#include <map>
#include <ranges>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

using namespace dice::unordered_sparse;

namespace {

    template<typename Map>
    concept HasValueAccessor = requires (typename Map::iterator it) { it.value(); };

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
