// The default hash function of `sparse_map` and `sparse_set`, and the trait `sh::hash_is_avalanching` that their
// `static_assert` reads. The cases that must not compile are in `tests/compile`.
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <type_traits>
#include <utility>

namespace {

    /**
     * A key type with its own `dice_hash_overload`. The overload declares no `is_avalanching`.
     */
    struct overload_key {
        std::uint64_t value{};  // NOLINT

        bool operator==(overload_key const &other) const = default;
    };

}  // namespace

namespace dice::hash {
    template<typename Policy>
    struct dice_hash_overload<Policy, overload_key> {
        static std::size_t dice_hash(overload_key const &key) noexcept {
            return dice_hash_templates<Policy>::dice_hash(key.value);
        }
    };
}  // namespace dice::hash

namespace {

    namespace sh = dice::sparse_map::sh;

    using wyhash = dice::hash::Policies::wyhash;

    using int_string = std::pair<int, std::string>;

    struct marked_void {
        using is_avalanching = void;
    };

    struct marked_true {
        using is_avalanching = std::true_type;
    };

    struct marked_false {
        using is_avalanching = std::false_type;
    };

    struct unmarked {};

    /**
     * Not marked itself. A specialization of `sh::hash_is_avalanching` below marks it.
     */
    struct specialized_hash {
        [[nodiscard]] std::size_t operator()(int key) const noexcept {
            return dice::hash::DiceHash<int, wyhash>{}(key);
        }
    };

    template<typename Key>
    Key make_key(int i) {
        if constexpr (std::is_same_v<Key, std::string>) {
            return "key " + std::to_string(i);
        } else if constexpr (std::is_same_v<Key, int_string>) {
            return {i, std::to_string(i)};
        } else if constexpr (std::is_same_v<Key, overload_key>) {
            return overload_key{static_cast<std::uint64_t>(i)};
        } else {
            return static_cast<Key>(i);
        }
    }

    /**
     * Inserts 1000 keys into a map and a set with the default hash function, finds them, and erases every second.
     */
    template<typename Key>
    void check_default_hash() {
        dice::sparse_map::sparse_map<Key, int> map;
        dice::sparse_map::sparse_set<Key> set;
        for (int i = 0; i < 1000; ++i) {
            map.emplace(make_key<Key>(i), i);
            set.insert(make_key<Key>(i));
        }
        REQUIRE(map.size() == 1000);
        REQUIRE(set.size() == 1000);
        for (int i = 0; i < 1000; ++i) {
            auto const it = map.find(make_key<Key>(i));
            REQUIRE(it != map.end());
            CHECK(it->second == i);
            CHECK(set.contains(make_key<Key>(i)));
        }
        for (int i = 0; i < 1000; i += 2) {
            CHECK(map.erase(make_key<Key>(i)) == 1);
            CHECK(set.erase(make_key<Key>(i)) == 1);
        }
        CHECK(map.size() == 500);
        CHECK(set.size() == 500);
        CHECK(!map.contains(make_key<Key>(0)));
        CHECK(map.contains(make_key<Key>(1)));
    }

}  // namespace

template<>
struct dice::sparse_map::sh::hash_is_avalanching<specialized_hash> : std::true_type {};

TEST_CASE("the default hash function is DiceHash with the policy wyhash") {
    CHECK(std::is_same_v<dice::sparse_map::sparse_map<int, int>::hasher, dice::hash::DiceHash<int, wyhash>>);
    CHECK(std::is_same_v<dice::sparse_map::sparse_set<std::string>::hasher, dice::hash::DiceHash<std::string, wyhash>>);
}

TEST_CASE("hash_is_avalanching reads the member type is_avalanching") {
    CHECK(sh::hash_is_avalanching_v<marked_void>);
    CHECK(sh::hash_is_avalanching_v<marked_true>);
    CHECK(!sh::hash_is_avalanching_v<marked_false>);
    CHECK(!sh::hash_is_avalanching_v<unmarked>);
    CHECK(!sh::hash_is_avalanching_v<std::hash<int>>);
}

TEST_CASE("DiceHash is avalanching for the policies wyhash, xxh3 and rapidhash and for every key type") {
    using dice::hash::DiceHash;
    namespace policies = dice::hash::Policies;
    CHECK(sh::hash_is_avalanching_v<DiceHash<int, wyhash>>);
    CHECK(sh::hash_is_avalanching_v<DiceHash<int, policies::xxh3>>);
    CHECK(sh::hash_is_avalanching_v<DiceHash<int, policies::rapidhash>>);
    CHECK(!sh::hash_is_avalanching_v<DiceHash<int, policies::Martinus>>);
    CHECK(sh::hash_is_avalanching_v<DiceHash<overload_key, wyhash>>);
    CHECK(sh::hash_is_avalanching_v<DiceHash<overload_key, policies::xxh3>>);
    CHECK(sh::hash_is_avalanching_v<DiceHash<overload_key, policies::rapidhash>>);
    CHECK(!sh::hash_is_avalanching_v<DiceHash<overload_key, policies::Martinus>>);
}

TEST_CASE("a specialization of hash_is_avalanching marks a hash function") {
    CHECK(sh::hash_is_avalanching_v<specialized_hash>);

    dice::sparse_map::sparse_map<int, int, specialized_hash> map;
    for (int i = 0; i < 1000; ++i) {
        map[i] = i;
    }
    REQUIRE(map.size() == 1000);
    for (int i = 0; i < 1000; ++i) {
        CHECK(map.at(i) == i);
    }
}

TYPE_TO_STRING_AS("int_string", int_string);

TEST_CASE_TEMPLATE("maps and sets with the default hash function", Key, int, std::uint64_t, std::string, int_string) {
    check_default_hash<Key>();
}

TEST_CASE("a key type with its own dice_hash_overload works with the default hash function") {
    CHECK(sh::hash_is_avalanching_v<dice::sparse_map::sparse_map<overload_key, int>::hasher>);
    check_default_hash<overload_key>();
}
