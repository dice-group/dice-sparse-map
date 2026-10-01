// The examples of README.md, so that they keep compiling and doing what the README says.

#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <functional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

namespace {

    struct string_hash {
        using is_transparent = void;

        std::size_t operator()(std::string_view str) const noexcept {
            return std::hash<std::string_view>{}(str);
        }
    };

    struct serializer {
        explicit serializer(char const *file_name) {
            ostream_.exceptions(ostream_.badbit | ostream_.failbit);
            ostream_.open(file_name, std::ios::binary);
        }

        template<typename T>
        requires std::is_arithmetic_v<T>
        void operator()(T const &value) {
            ostream_.write(reinterpret_cast<char const *>(&value), sizeof(T));
        }

        template<typename K, typename V>
        void operator()(std::pair<K, V> const &value) {
            (*this)(value.first);
            (*this)(value.second);
        }

    private:
        std::ofstream ostream_;
    };

    struct deserializer {
        explicit deserializer(char const *file_name) {
            istream_.exceptions(istream_.badbit | istream_.failbit | istream_.eofbit);
            istream_.open(file_name, std::ios::binary);
        }

        template<typename T>
        T operator()() {
            T value;
            deserialize(value);
            return value;
        }

    private:
        template<typename T>
        requires std::is_arithmetic_v<T>
        void deserialize(T &value) {
            istream_.read(reinterpret_cast<char *>(&value), sizeof(T));
        }

        template<typename K, typename V>
        void deserialize(std::pair<K, V> &value) {
            deserialize(value.first);
            deserialize(value.second);
        }

        std::ifstream istream_;
    };

}  // namespace

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
    CHECK(map.lookup("z").value_or(0) == 0);

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

TEST_CASE("heterogeneous lookup") {
    dice::sparse_map::sparse_map<std::string, int, string_hash, std::equal_to<>> map;

    map.try_emplace(std::string_view{"John Doe"}, 2001);
    map[std::string_view{"Jane Doe"}] = 2002;

    CHECK(map.at(std::string_view{"John Doe"}) == 2001);

    map.erase(std::string_view{"Jane Doe"});
    CHECK(map.size() == 1);
}

TEST_CASE("serialization") {
    dice::sparse_map::sparse_map<std::int64_t, std::int64_t> const map = {{1, -1}, {2, -2}, {3, -3}, {4, -4}};

    auto const path = std::filesystem::temp_directory_path() / "dice_sparse_map_readme_example.data";
    auto const file_name = path.string();
    {
        serializer serial(file_name.c_str());
        map.serialize(serial);
    }

    {
        deserializer dserial(file_name.c_str());
        auto const map_deserialized = dice::sparse_map::sparse_map<std::int64_t, std::int64_t>::deserialize(dserial);
        CHECK(map == map_deserialized);
    }

    {
        deserializer dserial(file_name.c_str());
        bool const hash_compatible = true;
        auto const map_deserialized = dice::sparse_map::sparse_map<std::int64_t, std::int64_t>::deserialize(dserial, hash_compatible);
        CHECK(map == map_deserialized);
    }

    std::filesystem::remove(path);
}
