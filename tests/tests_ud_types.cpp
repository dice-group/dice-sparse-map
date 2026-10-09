// Ported from the unit tests not_copyable, not_moveable, unique_ptr, explicit, diamond, array_of_maps, maps_of_maps, vectorofmaps, set, unordered_set, set_or_map_types and custom_hash of ankerl::unordered_dense (MIT license).
#include "fixtures/checksum.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace dice::sparse_map::tests;

// not_copyable

namespace {

    /**
     * Not copyable, but movable.
     */
    struct no_copy {
        no_copy() noexcept = default;

        explicit no_copy(std::size_t d) noexcept : data_(d) {}

        ~no_copy() = default;
        no_copy(no_copy const &) = delete;
        no_copy &operator=(no_copy const &) = delete;
        no_copy(no_copy &&) = default;
        no_copy &operator=(no_copy &&) = default;

        [[nodiscard]] std::size_t data() const {
            return data_;
        }

    private:
        std::size_t data_{};
    };

} // namespace

TEST_CASE_MAP("not_copyable", std::size_t, no_copy) {
    map_t m;
    for (std::size_t i = 0; i < 100; ++i) {
        m[i];
        m.emplace(std::piecewise_construct, std::forward_as_tuple(i * 100), std::forward_as_tuple(i));
    }
    REQUIRE(m.size() == 199);
    REQUIRE(m.at(300).data() == 3);

    map_t m2 = std::move(m);
    REQUIRE(m2.size() == 199);
    m = map_t{};
    REQUIRE(m.size() == 0);
}

// not_moveable

namespace {

    /**
     * Copyable, but not movable.
     */
    struct no_move {
        no_move() noexcept = default;

        explicit no_move(std::size_t d) noexcept : data_(d) {}

        ~no_move() = default;
        no_move(no_move const &) = default;
        no_move &operator=(no_move const &) = default;
        no_move(no_move &&) = delete;
        no_move &operator=(no_move &&) = delete;

        [[nodiscard]] std::size_t data() const {
            return data_;
        }

    private:
        std::size_t data_{};
    };

} // namespace

TEST_CASE_MAP("not_moveable", std::size_t, no_move) {
    auto m = map_t();
    for (std::size_t i = 0; i < 100; ++i) {
        m[i];
        m.emplace(std::piecewise_construct, std::forward_as_tuple(i * 100), std::forward_as_tuple(i));
    }
    REQUIRE(m.size() == 199);
    REQUIRE(m.at(300).data() == 3);

    m.clear();
    REQUIRE(m.size() == 0);
}

// unique_ptr

TEST_CASE_MAP("unique_ptr", std::size_t, std::unique_ptr<int>) {
    map_t m;
    REQUIRE(m.end() == m.find(123));
    REQUIRE(m.end() == m.begin());
    m[static_cast<std::size_t>(32)] = std::make_unique<int>(123);
    REQUIRE(m.end() != m.begin());
    REQUIRE(m.end() == m.find(123));
    REQUIRE(m.end() != m.find(32));

    for (auto const &kv : m) {
        REQUIRE(kv.first == 32);
        REQUIRE(kv.second != nullptr);
        REQUIRE(*kv.second == 123);
    }

    m = map_t();
    REQUIRE(m.end() == m.begin());
    REQUIRE(m.end() == m.find(123));
    REQUIRE(m.end() == m.find(32));

    map_t empty;
    map_t m3(std::move(empty));
    REQUIRE(m3.end() == m3.begin());
    REQUIRE(m3.end() == m3.find(123));
    REQUIRE(m3.end() == m3.find(32));
    m3[static_cast<std::size_t>(32)];
    REQUIRE(m3.end() != m3.begin());
    REQUIRE(m3.end() == m3.find(123));
    REQUIRE(m3.end() != m3.find(32));

    empty = map_t{}; // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
    map_t m4(std::move(empty));
    REQUIRE(m4.count(123) == 0);
    REQUIRE(m4.end() == m4.begin());
    REQUIRE(m4.end() == m4.find(123));
    REQUIRE(m4.end() == m4.find(32));
}

// emplace() builds the element before it looks up the key, so the second emplace of a key owns the pointer and
// destroys it. Nothing leaks, which the leak checker of the sanitizer build checks.
TEST_CASE_MAP("unique_ptr_fill", std::size_t, std::unique_ptr<int>) {
    map_t m;
    for (int i = 0; i < 1000; ++i) {
        m.emplace(static_cast<std::size_t>(i), new int(i)); // NOLINT(cppcoreguidelines-owning-memory)
        m.emplace(static_cast<std::size_t>(i), new int(i)); // NOLINT(cppcoreguidelines-owning-memory)
    }
    REQUIRE(m.size() == 1000U);
    REQUIRE(*m.at(999) == 999);
}

// explicit
//
// A map member of an aggregate that is initialized with `{}` needs a default constructor that is not explicit.

namespace {

    struct texture {
        int width;
        int height;
        void *data;
    };

    struct per_image {
        std::unordered_map<void *, texture *> texture_index;
    };

    template<typename Map>
    struct scene {
        std::vector<per_image> per_images;
        Map textures_per_key;
    };

    template<typename Map>
    struct app_state {
        scene<Map> current_scene;
    };

} // namespace

TEST_CASE_MAP("create_app_state_with_a_map_member", void *, texture *) {
    app_state<map_t> const state{};
    REQUIRE(state.current_scene.textures_per_key.empty());
}

// diamond
//
// The same type as hash and as key equality.

namespace {

    /**
     * Hash and key equality in one class. The hash returns the key itself. It is not avalanching, but it is marked as
     * avalanching, so that the containers accept it.
     */
    struct hash_with_equal {
        using is_avalanching = void;

        std::size_t operator()(int x) const {
            return static_cast<std::size_t>(x);
        }

        bool operator()(int a, int b) const {
            return a == b;
        }
    };

} // namespace

TEST_CASE_MAP("diamond_problem", int, int, hash_with_equal, hash_with_equal) {
    auto map = map_t();
    map[1] = 2;
    REQUIRE(map.size() == 1);
    REQUIRE(map.find(1) != map.end());
}

// array_of_maps

TEST_CASE_MAP("array_of_maps", int, int) {
    map_t maps_ary2[2];
    map_t maps_ary1[1];
    std::array<map_t, 2> maps;

    maps_ary2[1][1] = 2;
    maps_ary1[0][3] = 4;
    maps[1][5] = 6;
    REQUIRE(maps_ary2[0].empty());
    REQUIRE(maps_ary2[1].at(1) == 2);
    REQUIRE(maps_ary1[0].at(3) == 4);
    REQUIRE(maps[0].empty());
    REQUIRE(maps[1].at(5) == 6);
}

// maps_of_maps

namespace {

    template<typename Map>
    void check_maps_of_maps() {
        auto counts = counter();
        INFO(counts);

        auto rng = ankerl::nanobench::Rng(1234);
        for (std::size_t trial = 0; trial < 4; ++trial) {
            {
                counts("start");
                auto maps = Map();
                for (std::size_t i = 0; i < 100; ++i) {
                    auto a = rng.bounded(20);
                    auto b = rng.bounded(20);
                    auto x = static_cast<std::size_t>(rng());
                    maps[counter::obj(a, counts)][counter::obj(b, counts)] = counter::obj(x, counts);
                }
                counts("filled");

                Map maps_copied;
                maps_copied = maps;
                REQUIRE(checksum::mapmap(maps_copied) == checksum::mapmap(maps));
                REQUIRE(maps_copied == maps);
                counts("copied");

                Map maps_moved;
                maps_moved = std::move(maps_copied);
                counts("moved");

                // move
                REQUIRE(checksum::mapmap(maps_moved) == checksum::mapmap(maps));
                REQUIRE(maps_copied.size() == 0); // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
                maps_copied = std::move(maps_moved);
                counts("moved back");

                // move back
                REQUIRE(checksum::mapmap(maps_copied) == checksum::mapmap(maps));
                REQUIRE(maps_moved.size() == 0); // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
                counts("done");
            }
            counts("all destructed");
            REQUIRE(
                counts.dtor() + counter::static_dtor
                == counts.ctor()
                    + counter::static_ctor
                    + counts.copy_ctor()
                    + counts.default_ctor()
                    + counts.move_ctor()
            );
        }
    }

    using inner_map = dice::sparse_map::sparse_map<counter::obj, counter::obj, test_hash<counter::obj>>;

} // namespace

TEST_CASE_MAP("mapmap_map", counter::obj, inner_map) {
    check_maps_of_maps<map_t>();
}

// vectorofmaps

namespace {

    template<typename Map>
    void fill_randomly(counter &counts, Map &map, ankerl::nanobench::Rng &rng) {
        auto n = rng.bounded(20);
        for (std::size_t i = 0; i < n; ++i) {
            auto a = rng.bounded(20);
            auto b = rng.bounded(20);
            map.try_emplace({a, counts}, b, counts);
        }
    }

} // namespace

TEST_CASE_MAP("vectormap", counter::obj, counter::obj) {
    auto counts = counter();
    INFO(counts);

    auto rng = ankerl::nanobench::Rng(32154);
    {
        counts("begin");
        std::vector<map_t> maps;

        // copies
        for (std::size_t i = 0; i < 10; ++i) {
            map_t m;
            fill_randomly(counts, m, rng);
            maps.push_back(m);
        }
        counts("copies");

        // move
        for (std::size_t i = 0; i < 10; ++i) {
            map_t m;
            fill_randomly(counts, m, rng);
            maps.push_back(std::move(m));
        }
        counts("move");

        // emplace
        for (std::size_t i = 0; i < 10; ++i) {
            maps.emplace_back();
            fill_randomly(counts, maps.back(), rng);
        }
        counts("emplace");
        REQUIRE(maps.size() == 30U);
    }
    counts("dtor");
    REQUIRE(
        counts.dtor() + counter::static_dtor
        == counts.ctor() + counter::static_ctor + counts.copy_ctor() + counts.default_ctor() + counts.move_ctor()
    );
}

// set

TEST_CASE_SET("set", std::string) {
    auto set = set_t();

    set.insert("asdf");
    REQUIRE(set.size() == 1);
    auto it = set.find("asdf");
    REQUIRE(it != set.end());
    REQUIRE(*it == "asdf");
}

// unordered_set

TEST_CASE_SET("unordered_set_asserts", std::uint64_t) {
    static_assert(std::is_same_v<typename set_t::key_type, std::uint64_t>, "key_type same");
    static_assert(std::is_same_v<typename set_t::value_type, std::uint64_t>, "value_type same");
}

TEST_CASE_SET("unordered_set", std::uint64_t) {
    set_t set;
    set.emplace(UINT64_C(123));
    REQUIRE(set.size() == 1U);

    set.insert(UINT64_C(333));
    REQUIRE(set.size() == 2U);

    set.erase(UINT64_C(222));
    REQUIRE(set.size() == 2U);

    set.erase(UINT64_C(123));
    REQUIRE(set.size() == 1U);
}

TEST_CASE_SET("unordered_set_string", std::string) {
    set_t set;
    REQUIRE(set.begin() == set.end());

    set.emplace(static_cast<std::size_t>(2000), 'a');
    REQUIRE(set.size() == 1);

    REQUIRE(set.begin() != set.end());
    std::string const &str = *set.begin();
    REQUIRE(str == std::string(static_cast<std::size_t>(2000), 'a'));

    auto it = set.begin();
    REQUIRE(++it == set.end());
}

TEST_CASE_SET("unordered_set_eq", std::string) {
    set_t set1;
    set_t set2;
    REQUIRE(set1.size() == set2.size());
    REQUIRE(set1 == set2);
    REQUIRE(set2 == set1);

    set1.emplace("asdf");
    // (asdf) == ()
    REQUIRE(set1.size() != set2.size());
    REQUIRE(set1 != set2);
    REQUIRE(set2 != set1);

    set2.emplace("huh");
    // (asdf) == (huh)
    REQUIRE(set1.size() == set2.size());
    REQUIRE(set1 != set2);
    REQUIRE(set2 != set1);

    set1.emplace("huh");
    // (asdf, huh) == (huh)
    REQUIRE(set1.size() != set2.size());
    REQUIRE(set1 != set2);
    REQUIRE(set2 != set1);

    set2.emplace("asdf");
    // (asdf, huh) == (asdf, huh)
    REQUIRE(set1.size() == set2.size());
    REQUIRE(set1 == set2);
    REQUIRE(set2 == set1);

    set1.erase("asdf");
    // (huh) == (asdf, huh)
    REQUIRE(set1.size() != set2.size());
    REQUIRE(set1 != set2);
    REQUIRE(set2 != set1);

    set2.erase("asdf");
    // (huh) == (huh)
    REQUIRE(set1.size() == set2.size());
    REQUIRE(set1 == set2);
    REQUIRE(set2 == set1);

    set1.clear();
    // () == (huh)
    REQUIRE(set1.size() != set2.size());
    REQUIRE(set1 != set2);
    REQUIRE(set2 != set1);

    set2.erase("huh");
    // () == ()
    REQUIRE(set1.size() == set2.size());
    REQUIRE(set1 == set2);
    REQUIRE(set2 == set1);
}

// set_or_map_types
//
// A map has a `mapped_type`, a set does not. The static_asserts are the test.

namespace {

    template<typename T>
    concept HasMappedType = requires { typename T::mapped_type; };

} // namespace

TEST_CASE_MAP("set_or_map_types_map", int, double) {
    static_assert(std::is_same_v<double, typename map_t::mapped_type>);
    static_assert(HasMappedType<map_t>);
}

TEST_CASE_SET("set_or_map_types_set", int) {
    static_assert(!HasMappedType<set_t>);
}

// custom_hash

namespace {

    struct id {
        std::uint64_t value{}; // NOLINT

        bool operator==(id const &other) const {
            return value == other.value;
        }
    };

    /**
     * Returns the value itself. It is not avalanching, but it is marked as avalanching, so that the containers accept
     * it.
     */
    struct custom_hash_simple {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(id const &x) const noexcept {
            return x.value;
        }
    };

    struct custom_hash_mixing {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(id const &x) const noexcept {
            return checksum::mix(x.value);
        }
    };

    struct point {
        int x{}; // NOLINT
        int y{}; // NOLINT

        bool operator==(point const &other) const {
            return x == other.x && y == other.y;
        }
    };

    /**
     * Hashes the object representation of a `point`.
     */
    struct custom_hash_unique_object_representation {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(point const &f) const noexcept {
            static_assert(std::has_unique_object_representations_v<point>);
            static_assert(sizeof(point) == sizeof(std::uint64_t));
            return test_hash<std::uint64_t>{}(std::bit_cast<std::uint64_t>(f));
        }
    };

} // namespace

template<>
struct std::hash<id> {
    [[nodiscard]] std::size_t operator()(id const &x) const noexcept {
        return std::hash<std::uint64_t>{}(x.value);
    }
};

TEST_CASE_SET("custom_hash_simple", id, custom_hash_simple) {
    auto set = set_t();
    set.insert(id{124});
    REQUIRE(set.size() == 1U);
    REQUIRE(set.contains(id{124}));
    REQUIRE(!set.contains(id{125}));
}

TEST_CASE_SET("custom_hash_mixing", id, custom_hash_mixing) {
    auto set = set_t();
    set.insert(id{124});
    REQUIRE(set.size() == 1U);
    REQUIRE(set.contains(id{124}));
    REQUIRE(!set.contains(id{125}));
}

TEST_CASE_SET("custom_hash_unique", point, custom_hash_unique_object_representation) {
    auto set = set_t();
    set.insert(point{123, 321});
    REQUIRE(set.size() == 1U);
    REQUIRE(set.contains(point{123, 321}));
    REQUIRE(!set.contains(point{321, 123}));
}

TEST_CASE_SET("custom_hash_default", id) {
    auto set = set_t();
    set.insert(id{124});
    REQUIRE(set.size() == 1U);
    REQUIRE(set.contains(id{124}));
    REQUIRE(!set.contains(id{125}));
}
