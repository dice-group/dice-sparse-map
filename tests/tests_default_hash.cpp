// The default hash function of `sparse_map` and `sparse_set`, and the trait `sh::hash_is_avalanching` that their
// `static_assert` reads. The cases that must not compile are in `tests/compile`.
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <type_traits>
#include <utility>

namespace {

    /**
     * A key type with its own `dice_hash_overload`. The overload declares `is_avalanching`.
     */
    struct marked_key {
        std::uint64_t value{};  // NOLINT

        bool operator==(marked_key const &other) const = default;
    };

}  // namespace

namespace dice::hash {
    template<typename Policy>
    struct dice_hash_overload<Policy, marked_key> {
        using is_avalanching = avalanching_like<std::uint64_t, Policy>;

        static std::size_t dice_hash(marked_key const &key) noexcept {
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
        } else if constexpr (std::is_same_v<Key, marked_key>) {
            return marked_key{static_cast<std::uint64_t>(i)};
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
    CHECK(sh::hash_is_avalanching_v<dice::hash::DiceHash<int, wyhash>>);
    CHECK(!sh::hash_is_avalanching_v<dice::hash::DiceHash<int, dice::hash::Policies::Martinus>>);
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

TEST_CASE("a key type whose dice_hash_overload declares is_avalanching works with the default hash function") {
    CHECK(sh::hash_is_avalanching_v<dice::sparse_map::sparse_map<marked_key, int>::hasher>);
    check_default_hash<marked_key>();
}

TEST_CASE_TEMPLATE("0.0 and -0.0 are one key with the default hash function", Key, float, double, long double) {
    using map_t = dice::sparse_map::sparse_map<Key, int>;
    using set_t = dice::sparse_map::sparse_set<Key>;
    static_assert(sh::hash_is_avalanching_v<typename map_t::hasher>);

    Key const zero{};
    Key const negative_zero = -zero;
    REQUIRE(std::signbit(negative_zero));
    REQUIRE(!std::signbit(zero));
    REQUIRE(zero == negative_zero);
    CHECK(typename map_t::hasher{}(zero) == typename map_t::hasher{}(negative_zero));

    // 1024 buckets, so that two different hashes of the zeros most likely pick different buckets.
    map_t map(1024);
    map[zero] = 1;
    CHECK(map.contains(negative_zero));
    map[negative_zero] = 2;
    CHECK(map.size() == 1);
    auto const it = map.find(negative_zero);
    REQUIRE(it != map.end());
    CHECK(it->second == 2);
    CHECK(!std::signbit(it->first));

    set_t set(1024);
    set.insert(zero);
    CHECK(set.contains(negative_zero));
    CHECK(!set.insert(negative_zero).second);
    CHECK(set.size() == 1);
    auto const set_it = set.find(negative_zero);
    REQUIRE(set_it != set.end());
    CHECK(!std::signbit(*set_it));
}
