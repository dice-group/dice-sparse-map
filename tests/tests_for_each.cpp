#include "fixtures/allocators.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * `for_each` calls a function with every element, in the order of the iterators, as the iterators give it. The tests
 * compare the elements that `for_each` visits, in their order, with the elements of a loop over the iterators.
 */
namespace {
    using namespace dice::unordered_sparse;
    using namespace dice::unordered_sparse::tests;

    /**
     * A number whose move constructor can throw. The groups of a container with it as key or mapped value have
     * holes: an erase destroys the element in place and keeps its slot. A literal type, for the constant evaluation.
     */
    struct throwing_move_number {
        std::size_t value = 0;

        throwing_move_number() = default;

        constexpr explicit throwing_move_number(std::size_t v) noexcept
            : value(v) {
        }

        throwing_move_number(throwing_move_number const &) = default;

        // NOLINTNEXTLINE(performance-noexcept-move-constructor)
        constexpr throwing_move_number(throwing_move_number &&other) noexcept(false)
            : value(other.value) {
        }

        throwing_move_number &operator=(throwing_move_number const &) = default;

        // NOLINTNEXTLINE(performance-noexcept-move-constructor)
        constexpr throwing_move_number &operator=(throwing_move_number &&other) noexcept(false) {
            value = other.value;
            return *this;
        }

        ~throwing_move_number() = default;

        friend bool operator==(throwing_move_number const &, throwing_move_number const &) = default;
    };

    static_assert(!std::is_nothrow_move_constructible_v<throwing_move_number>);

    struct throwing_move_number_hash {
        [[nodiscard]] std::size_t operator()(throwing_move_number const &number) const noexcept {
            return std::hash<std::size_t>{}(number.value);
        }
    };

    [[nodiscard]] std::size_t number_of(std::size_t value) {
        return value;
    }

    [[nodiscard]] std::size_t number_of(std::string const &value) {
        return std::stoull(value);
    }

    [[nodiscard]] std::size_t number_of(throwing_move_number const &value) {
        return value.value;
    }

    /// the mapped value of the key `n`
    template<typename T>
    [[nodiscard]] T value_for(std::size_t n) {
        if constexpr (std::is_same_v<T, std::string>) {
            return std::to_string(n * 3);
        } else {
            return T(n * 3);
        }
    }

    template<typename Container>
    constexpr bool is_map_v = requires { typename Container::mapped_type; };

    /// a key and a mapped value as numbers, or a key and 0 for a set
    using element = std::pair<std::size_t, std::size_t>;

    template<typename Element>
    [[nodiscard]] element element_of(Element const &e) {
        if constexpr (requires { e.second; }) {
            return {number_of(e.first), number_of(e.second)};
        } else {
            return {number_of(e), 0};
        }
    }

    /// the elements of `container` in the order of a loop over the iterators
    template<typename Container>
    [[nodiscard]] std::vector<element> elements_by_iterators(Container const &container) {
        std::vector<element> result;
        for (auto it = container.begin(); it != container.end(); ++it) {
            result.push_back(element_of(*it));
        }
        return result;
    }

    /// the elements of `container` in the order of `for_each` of the const container
    template<typename Container>
    [[nodiscard]] std::vector<element> elements_by_for_each(Container const &container) {
        std::vector<element> result;
        container.for_each([&result](auto const &e) {
            result.push_back(element_of(e));
        });
        return result;
    }

    /// the elements of `container` in the order of `for_each` of the container that is not const
    template<typename Container>
    [[nodiscard]] std::vector<element> elements_by_mutable_for_each(Container &container) {
        std::vector<element> result;
        container.for_each([&result](auto &&e) {
            result.push_back(element_of(e));
        });
        return result;
    }

    /// the number of calls of `f` by `for_each`, for the const container and the container that is not const
    template<typename Container>
    [[nodiscard]] std::pair<std::size_t, std::size_t> nb_calls(Container &container) {
        std::size_t const_calls = 0;
        std::as_const(container).for_each([&const_calls](auto const &) {
            ++const_calls;
        });
        std::size_t calls = 0;
        container.for_each([&calls](auto &&) {
            ++calls;
        });
        return {const_calls, calls};
    }

    template<typename Container>
    void insert_number(Container &container, std::size_t n) {
        using key_type = typename Container::key_type;
        if constexpr (is_map_v<Container>) {
            container.try_emplace(key_type(n), value_for<typename Container::mapped_type>(n));
        } else {
            container.insert(key_type(n));
        }
    }

    template<typename Container>
    void erase_number(Container &container, std::size_t n) {
        using key_type = typename Container::key_type;
        CHECK(container.erase(key_type(n)) == 1);
    }

    /// checks that both `for_each` visit the elements of `container` in the order of the iterators
    template<typename Container>
    void check_order(Container &container) {
        auto const expected = elements_by_iterators(container);
        CHECK(expected.size() == container.size());
        CHECK(elements_by_for_each(container) == expected);
        CHECK(elements_by_mutable_for_each(container) == expected);
    }

    /**
     * `for_each` of an empty container without buckets, of an empty container with buckets, of a container with one
     * group, with many groups, with empty groups between the others, and after `clear`.
     */
    template<typename Container>
    void check_for_each_order() {
        SUBCASE("an empty container without buckets") {
            auto container = Container{};
            REQUIRE(container.bucket_count() == 0);
            CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{0, 0});
        }

        SUBCASE("an empty container with buckets") {
            auto container = Container{};
            container.reserve(1000);
            REQUIRE(container.bucket_count() > 64);
            CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{0, 0});
        }

        SUBCASE("a container with one group") {
            auto container = Container{};
            for (std::size_t n = 0; n < 20; ++n) {
                insert_number(container, n * 7919);
            }
            REQUIRE(container.bucket_count() <= 64);
            check_order(container);
            CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{20, 20});
        }

        SUBCASE("a container with many groups, after erasures and after clear") {
            auto container = Container{};
            for (std::size_t n = 0; n < 5000; ++n) {
                insert_number(container, n * 7919);
            }
            REQUIRE(container.bucket_count() >= 64 * 64);
            check_order(container);

            // erasures in the middle, at the start and at the end of groups, and holes if the elements can have them
            for (std::size_t n = 0; n < 5000; n += 3) {
                erase_number(container, n * 7919);
            }
            check_order(container);

            // empty groups between the others, the first groups probably empty too
            for (std::size_t n = 1; n < 5000; n += 3) {
                if (n % 97 != 1) {
                    erase_number(container, n * 7919);
                }
            }
            for (std::size_t n = 2; n < 5000; n += 3) {
                if (n % 89 != 2) {
                    erase_number(container, n * 7919);
                }
            }
            REQUIRE(container.size() < container.bucket_count() / 64);
            check_order(container);

            container.clear();
            CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{0, 0});
        }
    }

}  // namespace

TEST_CASE_MAP("for_each of a map visits the elements in the order of the iterators", std::size_t, std::string) {
    check_for_each_order<map_t>();
}

TEST_CASE_MAP("for_each of a map with holes visits the elements in the order of the iterators", std::size_t, throwing_move_number) {
    check_for_each_order<map_t>();
}

TEST_CASE_SET("for_each of a set visits the keys in the order of the iterators", std::size_t) {
    check_for_each_order<set_t>();
}

TEST_CASE_SET("for_each of a set with holes visits the keys in the order of the iterators", throwing_move_number, throwing_move_number_hash) {
    check_for_each_order<set_t>();
}

TEST_CASE_MAP("for_each gives the reference types of the iterators and changes mapped values", std::size_t, std::size_t) {
    auto map = map_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        map.try_emplace(n, n);
    }
    static_assert(std::is_same_v<decltype(*map.begin()), typename map_t::reference>);
    static_assert(std::is_same_v<decltype(*std::as_const(map).begin()), typename map_t::const_reference>);

    map.for_each([](auto &&e) {
        static_assert(std::is_same_v<decltype(e), typename map_t::reference &&>);
        static_assert(std::is_same_v<decltype(e.second), std::size_t &>);
        e.second += 1;
    });
    bool all_changed = true;
    for (std::size_t n = 0; n < 1000; ++n) {
        all_changed = all_changed && map.at(n) == n + 1;
    }
    CHECK(all_changed);

    std::size_t sum = 0;
    std::as_const(map).for_each([&sum](auto &&e) {
        static_assert(std::is_same_v<decltype(e), typename map_t::const_reference &&>);
        sum += e.second;
    });
    CHECK(sum == 1000 * 1001 / 2);
}

TEST_CASE_SET("for_each of a set gives the reference type of the iterators", std::size_t) {
    auto set = set_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        set.insert(n);
    }
    static_assert(std::is_same_v<decltype(*set.begin()), std::size_t const &>);
    std::size_t sum = 0;
    set.for_each([&sum](auto &&key) {
        static_assert(std::is_same_v<decltype(key), std::size_t const &>);
        sum += key;
    });
    CHECK(sum == 999 * 1000 / 2);
}

TEST_CASE_MAP("an exception from f leaves for_each after the elements before it", std::size_t, std::size_t) {
    auto map = map_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        map.try_emplace(n * 7919, 0);
    }
    auto const order = elements_by_iterators(map);
    REQUIRE(order.size() == 1000);

    // f changes every element it gets and throws at the 500th
    std::size_t calls = 0;
    CHECK_THROWS_AS(map.for_each([&calls](auto &&e) {
        e.second = 1;
        ++calls;
        if (calls == 500) {
            throw std::runtime_error("f");
        }
    }),
                    std::runtime_error);
    CHECK(calls == 500);

    // the first 500 elements in the order of the iterators are changed, the others are not
    CHECK(map.size() == 1000);
    std::vector<element> expected = order;
    for (std::size_t i = 0; i < 500; ++i) {
        expected[i].second = 1;
    }
    CHECK(elements_by_iterators(map) == expected);
    CHECK(elements_by_for_each(map) == expected);

    // the map works on
    map.try_emplace(1, 2);
    CHECK(map.erase(0) == 1);
    CHECK(map.size() == 1000);
    check_order(map);
}

/*
 * Groups with holes at the first slot, at a middle slot and at the last slot. `bucket_hash` puts key `k` into bucket
 * `k`, so the keys 0 to 63 are in group 0, 64 to 127 in group 1 and so on. `counter::obj` aborts the program when a
 * destroyed object is copied, so a hole that `for_each` takes for an element ends the test.
 */
namespace {
    struct bucket_hash {
        using is_avalanching = void;

        std::size_t operator()(counter::obj const &key) const {
            return key.get_for_hash();
        }
    };

    using holes_map = sparse_map<copied_obj, copied_obj, bucket_hash, std::equal_to<counter::obj>, leak_checking_allocator<std::pair<copied_obj, copied_obj>>>;
    using holes_set = sparse_set<copied_obj, bucket_hash, std::equal_to<counter::obj>, leak_checking_allocator<copied_obj>>;

    std::vector<std::size_t> range(std::size_t first, std::size_t last) {
        std::vector<std::size_t> numbers;
        for (std::size_t n = first; n < last; ++n) {
            numbers.push_back(n);
        }
        return numbers;
    }

    std::vector<std::size_t> concat(std::vector<std::vector<std::size_t>> const &parts) {
        std::vector<std::size_t> all;
        for (auto const &part : parts) {
            all.insert(all.end(), part.begin(), part.end());
        }
        return all;
    }
}  // namespace

TEST_CASE("for_each skips the holes at the first, a middle and the last slot of a group") {
    auto counts = counter{};
    {
        auto map = holes_map{};
        map.reserve(100);
        REQUIRE(map.bucket_count() == 256);
        for (auto const n : concat({range(0, 10), range(64, 76), range(128, 138), {192, 193}})) {
            map.try_emplace(copied_obj{n, counts}, copied_obj{n, counts});
        }
        // group 1 has holes at its first slot, at two neighbouring slots and at its last slot, group 3 has a hole
        // after its only element, group 0 is empty
        for (std::size_t const n : {64, 67, 68, 75, 193}) {
            CHECK(map.erase(copied_obj{n, counts}) == 1);
        }
        for (std::size_t n = 0; n < 10; ++n) {
            CHECK(map.erase(copied_obj{n, counts}) == 1);
        }
        auto const expected = concat({{65, 66, 69, 70, 71, 72, 73, 74}, range(128, 138), {192}});

        std::vector<std::size_t> iterated;
        for (auto const &e : map) {
            iterated.push_back(e.first.get());
        }
        CHECK(iterated == expected);

        // `for_each` copies every key and mapped value it gets, so a hole would abort
        std::vector<std::size_t> visited;
        bool values_right = true;
        std::as_const(map).for_each([&visited, &values_right](auto const &e) {
            copied_obj const key = e.first;
            copied_obj const value = e.second;
            visited.push_back(key.get());
            values_right = values_right && value.get() == key.get();
        });
        CHECK(visited == expected);
        CHECK(values_right);

        map.for_each([](auto &&e) {
            e.second.get() += 1000;
        });
        bool all_changed = true;
        for (auto const n : expected) {
            auto const it = map.find(copied_obj{n, counts});
            all_changed = all_changed && it != map.end() && it->second.get() == n + 1000;
        }
        CHECK(all_changed);

        auto set = holes_set{};
        set.reserve(100);
        for (auto const n : range(64, 76)) {
            set.insert(copied_obj{n, counts});
        }
        for (std::size_t const n : {64, 67, 68, 75}) {
            CHECK(set.erase(copied_obj{n, counts}) == 1);
        }
        std::vector<std::size_t> visited_keys;
        set.for_each([&visited_keys](auto const &key) {
            copied_obj const copy = key;
            visited_keys.push_back(copy.get());
        });
        CHECK(visited_keys == std::vector<std::size_t>{65, 66, 69, 70, 71, 72, 73, 74});
    }
    CHECK(live_blocks == 0);
}

namespace {

    /**
     * `std::hash` is not constexpr.
     */
    struct constexpr_hash {
        using is_avalanching = void;

        [[nodiscard]] constexpr std::size_t operator()(int key) const noexcept {
            return static_cast<std::size_t>(key) * UINT64_C(0x9E3779B97F4A7C15);
        }
    };

    /**
     * A map of 300 elements (16 groups) with erasures in a constant evaluation: `for_each` that is not const adds 1
     * to every mapped value, then the const `for_each` and the iterators sum the mapped values. 1 if both sums are
     * right.
     */
    template<typename T>
    constexpr int for_each_sums() {
        auto map = dice::sparse_map<int, T, constexpr_hash>();
        for (int i = 0; i < 300; ++i) {
            map.try_emplace(i, T(static_cast<std::size_t>(i)));
        }
        for (int i = 0; i < 300; i += 7) {
            map.erase(i);
        }
        map.for_each([](auto &&e) {
            if constexpr (std::is_same_v<T, throwing_move_number>) {
                e.second.value += 1;
            } else {
                e.second += 1;
            }
        });

        std::size_t for_each_sum = 0;
        std::as_const(map).for_each([&for_each_sum](auto const &e) {
            if constexpr (std::is_same_v<T, throwing_move_number>) {
                for_each_sum += e.second.value;
            } else {
                for_each_sum += e.second;
            }
        });
        std::size_t iterator_sum = 0;
        for (auto const &e : map) {
            if constexpr (std::is_same_v<T, throwing_move_number>) {
                iterator_sum += e.second.value;
            } else {
                iterator_sum += e.second;
            }
        }

        // the sum of i + 1 for the i below 300 that are not multiples of 7
        std::size_t expected = 0;
        for (std::size_t i = 0; i < 300; ++i) {
            if (i % 7 != 0) {
                expected += i + 1;
            }
        }
        return (map.bucket_count() == 1024 && for_each_sum == expected && iterator_sum == expected) ? 1 : 0;
    }

    constexpr int for_each_set_sum() {
        auto set = dice::sparse_set<int, constexpr_hash>();
        for (int i = 0; i < 300; ++i) {
            set.insert(i);
        }
        int sum = 0;
        set.for_each([&sum](int const &key) {
            sum += key;
        });
        return sum;
    }

}  // namespace

TEST_CASE("for_each in a constant expression") {
    static_assert(for_each_sums<int>() == 1);
    static_assert(for_each_sums<std::size_t>() == 1);
    static_assert(for_each_sums<throwing_move_number>() == 1);
    static_assert(for_each_set_sum() == 299 * 300 / 2);
    CHECK(for_each_sums<int>() == 1);
    CHECK(for_each_sums<throwing_move_number>() == 1);
    CHECK(for_each_set_sum() == 299 * 300 / 2);
}

namespace {

    /**
     * Puts the key `k` into bucket `k` of a table with more than `k` buckets, so that a test knows the group of a key.
     */
    struct identity_hash {
        using is_avalanching = void;

        [[nodiscard]] constexpr std::size_t operator()(std::size_t key) const noexcept {
            return key;
        }
    };

    /**
     * Calls `for_each` with an `f` that records every element it gets, sets the mapped value of a map element to 1,
     * and throws at the element with the key `throw_key`. Checks that the exception leaves `for_each` there: `f` got
     * the elements in the order of the iterators up to `throw_key` and no other, only these elements are changed, the
     * size is the same, and the container works on.
     */
    template<typename Container>
    void check_exception_from_f(Container &container, std::size_t throw_key) {
        auto const order = elements_by_iterators(container);
        // the number of elements up to and including the one with `throw_key`
        std::size_t nb_before = 0;
        while (nb_before < order.size() && order[nb_before].first != throw_key) {
            ++nb_before;
        }
        ++nb_before;
        REQUIRE(nb_before < order.size());

        std::vector<element> got;
        CHECK_THROWS_AS(container.for_each([&got, throw_key](auto &&e) {
            got.push_back(element_of(e));
            if constexpr (is_map_v<Container>) {
                e.second = typename Container::mapped_type(1);
            }
            if (got.back().first == throw_key) {
                throw std::runtime_error("f");
            }
        }),
                        std::runtime_error);
        CHECK(got == std::vector<element>(order.begin(), order.begin() + static_cast<std::ptrdiff_t>(nb_before)));

        std::vector<element> expected = order;
        if constexpr (is_map_v<Container>) {
            for (std::size_t i = 0; i < nb_before; ++i) {
                expected[i].second = 1;
            }
        }
        CHECK(container.size() == order.size());
        CHECK(elements_by_iterators(container) == expected);
        CHECK(elements_by_for_each(container) == expected);

        // the container works on
        insert_number(container, 1000003);
        erase_number(container, 1000003);
        check_order(container);
    }

}  // namespace

TEST_CASE_SET("an exception from f leaves for_each of a set after the keys before it", std::size_t) {
    auto set = set_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        set.insert(n * 7919);
    }
    auto const order = elements_by_iterators(set);
    check_exception_from_f(set, order[499].first);
}

TEST_CASE_MAP("an exception from f in a group with holes leaves for_each after the elements before it", std::size_t, throwing_move_number, identity_hash) {
    auto map = map_t{};
    for (std::size_t n = 0; n < 120; ++n) {
        map.try_emplace(n, throwing_move_number(n * 3));
    }
    REQUIRE(map.bucket_count() >= 128);
    // group 1 (the keys 64 to 127) gets holes at its first slot and before the key 80, where `f` throws
    for (std::size_t const n : {64, 70, 71, 90}) {
        CHECK(map.erase(n) == 1);
    }
    check_exception_from_f(map, 80);
}

namespace {

    /**
     * A function object that can be neither copied nor moved. It sums the keys it gets. Its call operator takes only
     * an lvalue object.
     */
    struct pinned_key_sum {
        std::size_t sum = 0;

        pinned_key_sum() = default;
        pinned_key_sum(pinned_key_sum const &) = delete;
        pinned_key_sum(pinned_key_sum &&) = delete;
        pinned_key_sum &operator=(pinned_key_sum const &) = delete;
        pinned_key_sum &operator=(pinned_key_sum &&) = delete;
        ~pinned_key_sum() = default;

        template<typename Element>
        void operator()(Element const &e) & {
            sum += element_of(e).first;
        }
    };

    /// `for_each` of a container and of a const container, with `f` as an lvalue and as a prvalue, `f` is called as an lvalue and never copied or moved
    template<typename Container>
    void check_f_not_copied() {
        auto container = Container{};
        for (std::size_t n = 0; n < 1000; ++n) {
            insert_number(container, n);
        }
        pinned_key_sum f;
        container.for_each(f);
        CHECK(f.sum == 999 * 1000 / 2);
        std::as_const(container).for_each(f);
        CHECK(f.sum == 999 * 1000);
        container.for_each(pinned_key_sum{});
        std::as_const(container).for_each(pinned_key_sum{});
    }

}  // namespace

TEST_CASE_MAP("for_each calls f as an lvalue and never copies or moves it", std::size_t, std::size_t) {
    check_f_not_copied<map_t>();
}

TEST_CASE_SET("for_each of a set calls f as an lvalue and never copies or moves it", std::size_t) {
    check_f_not_copied<set_t>();
}

TEST_CASE_MAP("f of for_each may read the map with find", std::size_t, std::size_t) {
    auto map = map_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        map.try_emplace(n, n);
    }

    // f looks up the next key and a key that is not in the map, and adds 1 to the value it gets
    std::size_t calls = 0;
    bool all_found = true;
    map.for_each([&](auto &&e) {
        auto const next = (e.first + 1) % 1000;
        auto const it = map.find(next);
        all_found = all_found && it != map.end() && it->first == next && (it->second == next || it->second == next + 1);
        all_found = all_found && map.find(e.first + 1000) == map.end();
        e.second += 1;
        ++calls;
    });
    CHECK(calls == 1000);
    CHECK(all_found);

    // after the first pass every value is its key plus 1
    calls = 0;
    all_found = true;
    std::as_const(map).for_each([&](auto &&e) {
        auto const next = (e.first + 1) % 1000;
        auto const it = std::as_const(map).find(next);
        all_found = all_found && it != map.cend() && it->first == next && it->second == next + 1 && e.second == e.first + 1;
        ++calls;
    });
    CHECK(calls == 1000);
    CHECK(all_found);
}

namespace {

    /// `for_each` with an `f` that returns `false` calls `f` with every element, of a container and of a const container
    template<typename Container>
    void check_result_ignored() {
        auto container = Container{};
        for (std::size_t n = 0; n < 1000; ++n) {
            insert_number(container, n);
        }
        std::size_t calls = 0;
        auto returns_false = [&calls](auto const &) {
            ++calls;
            return false;
        };
        container.for_each(returns_false);
        CHECK(calls == 1000);
        std::as_const(container).for_each(returns_false);
        CHECK(calls == 2000);
    }

}  // namespace

TEST_CASE_MAP("for_each ignores the result of f and calls f with every element", std::size_t, std::size_t) {
    check_result_ignored<map_t>();
}

TEST_CASE_SET("for_each of a set ignores the result of f and calls f with every element", std::size_t) {
    check_result_ignored<set_t>();
}

namespace {

    /// true if `container.for_each(f)` is a valid call
    template<typename Container, typename F>
    concept CanForEach = requires (Container &container, F &f) { container.for_each(f); };

    /// true if `container.for_each(F{})` is a valid call, with `f` as a prvalue
    template<typename Container, typename F>
    concept CanForEachWithPrvalue = requires (Container &container) { container.for_each(F{}); };

    /// true if `std::move(container).for_each(f)` is a valid call, with the container as an rvalue
    template<typename Container, typename F>
    concept CanForEachOnRvalue = requires (Container &container, F &f) { std::move(container).for_each(f); };

    /// a function object that can be called only with the reference type of a map that is not const, with a result
    struct mutable_element_function {
        template<typename Element>
        requires (!std::is_const_v<std::remove_reference_t<typename Element::second_type>>)
        bool operator()(Element const &) const {
            return true;
        }
    };

    /// a function object that can be called only with the reference type of a const map, with a result
    struct const_element_function {
        template<typename Element>
        requires std::is_const_v<std::remove_reference_t<typename Element::second_type>>
        bool operator()(Element const &) const {
            return true;
        }
    };

    /// a function object that can be called only as an rvalue, with a result
    struct rvalue_only_function {
        template<typename Element>
        bool operator()(Element const &) && {
            return true;
        }
    };

    /// a function object that can be called as an lvalue and as an rvalue, with a result
    struct any_value_function {
        template<typename Element>
        bool operator()(Element const &) const {
            return true;
        }
    };

    /// a map type that adds nothing to `sparse_map`
    struct derived_map : sparse_map<std::size_t, std::size_t> {};

    /// a map type with its own member `ht_`, which hides the member of `sparse_map` with this name
    struct map_with_own_ht : sparse_map<std::size_t, std::size_t> {
        int ht_ = 0;
    };

    /// a type with the map as a private base, which calls `for_each` through a `static_cast` to the map type
    struct map_as_private_base : private sparse_map<std::size_t, std::size_t> {
        using sparse_map::at;
        using sparse_map::try_emplace;

        void add_one() {
            static_cast<sparse_map &>(*this).for_each([](auto &&e) {
                e.second += 1;
            });
        }

        [[nodiscard]] std::size_t sum() const {
            std::size_t sum = 0;
            static_cast<sparse_map const &>(*this).for_each([&sum](auto const &e) {
                sum += e.second;
            });
            return sum;
        }
    };

    /// a type with the map as a protected base, which calls `for_each` through a `static_cast` to the map type
    struct map_as_protected_base : protected sparse_map<std::size_t, std::size_t> {
        using sparse_map::at;
        using sparse_map::try_emplace;

        void add_one() {
            static_cast<sparse_map &>(*this).for_each([](auto &&e) {
                e.second += 1;
            });
        }

        [[nodiscard]] std::size_t sum() const {
            std::size_t sum = 0;
            static_cast<sparse_map const &>(*this).for_each([&sum](auto const &e) {
                sum += e.second;
            });
            return sum;
        }
    };

}  // namespace

TEST_CASE("for_each takes an f that can be called with the reference type of the call, and no other f, also on a derived map and on an rvalue map") {
    using map_type = sparse_map<std::size_t, std::size_t>;
    using set_type = sparse_set<std::size_t>;

    using takes_any_element = decltype([](auto const &) {
    });
    using takes_a_number = decltype([](std::size_t) {
    });
    auto writes = [](auto &&e) {
        e.second += 1;
    };

    static_assert(CanForEach<map_type, takes_any_element>);
    static_assert(CanForEach<map_type const, takes_any_element>);
    static_assert(CanForEach<set_type, takes_any_element>);
    static_assert(CanForEach<set_type const, takes_any_element>);
    // the constraint for a map that is not const checks only the reference type that is not const
    static_assert(CanForEach<map_type, decltype(writes)>);
    static_assert(CanForEach<set_type, takes_a_number>);
    static_assert(!CanForEach<map_type, takes_a_number>);

    // a map that is not const gives `reference`, a const map gives `const_reference`
    static_assert(CanForEach<map_type, mutable_element_function>);
    static_assert(!CanForEach<map_type const, mutable_element_function>);
    static_assert(CanForEach<map_type const, const_element_function>);
    static_assert(!CanForEach<map_type, const_element_function>);
    // the same for an rvalue map and a const rvalue map
    static_assert(CanForEachOnRvalue<map_type, mutable_element_function>);
    static_assert(!CanForEachOnRvalue<map_type, const_element_function>);
    static_assert(CanForEachOnRvalue<map_type const, const_element_function>);
    static_assert(!CanForEachOnRvalue<map_type const, mutable_element_function>);

    // a type derived from the map, and a map that is an rvalue
    derived_map map;
    map.try_emplace(1, 2);
    map.for_each(writes);
    CHECK(map.at(1) == 3);
    std::size_t calls = 0;
    map_type{{1, 2}, {3, 4}}.for_each([&calls](auto &&e) {
        e.second += 1;
        ++calls;
    });
    CHECK(calls == 2);
}

TEST_CASE("for_each rejects an f that can be called only as an rvalue") {
    using map_type = sparse_map<std::size_t, std::size_t>;
    using set_type = sparse_set<std::size_t>;

    // `for_each` calls `f` as an lvalue, also when the caller passes a prvalue
    static_assert(!CanForEachWithPrvalue<map_type, rvalue_only_function>);
    static_assert(!CanForEachWithPrvalue<map_type const, rvalue_only_function>);
    static_assert(!CanForEachWithPrvalue<set_type, rvalue_only_function>);
    static_assert(!CanForEachWithPrvalue<set_type const, rvalue_only_function>);

    static_assert(CanForEachWithPrvalue<map_type, any_value_function>);
    static_assert(CanForEachWithPrvalue<map_type const, any_value_function>);
    static_assert(CanForEachWithPrvalue<set_type, any_value_function>);
    static_assert(CanForEachWithPrvalue<set_type const, any_value_function>);
}

TEST_CASE("for_each of a type with the map as a private or protected base works through a static_cast to the map type") {
    map_as_private_base private_map;
    private_map.try_emplace(1, 2);
    private_map.try_emplace(2, 3);
    private_map.add_one();
    CHECK(private_map.at(1) == 3);
    CHECK(private_map.at(2) == 4);
    CHECK(private_map.sum() == 7);

    map_as_protected_base protected_map;
    protected_map.try_emplace(1, 2);
    protected_map.try_emplace(2, 3);
    protected_map.add_one();
    CHECK(protected_map.at(1) == 3);
    CHECK(protected_map.at(2) == 4);
    CHECK(protected_map.sum() == 7);
}

TEST_CASE("for_each of a type derived from the map visits the elements of the map, also if the type has a member ht_") {
    map_with_own_ht map;
    map.try_emplace(1, 2);
    map.try_emplace(2, 3);

    std::size_t sum = 0;
    map.for_each([&sum](auto &&e) {
        e.second += 1;
        sum += e.second;
    });
    CHECK(sum == 7);
    CHECK(map.at(1) == 3);
    CHECK(map.at(2) == 4);

    std::size_t calls = 0;
    std::as_const(map).for_each([&calls](auto const &) {
        ++calls;
    });
    CHECK(calls == 2);
    CHECK(map.ht_ == 0);
}

/*
 * A container with an inline capacity keeps its first elements in the container object, in the inline group: group 0
 * of a table of 64 buckets, with the elements densely in bucket order. `identity_hash` puts key `k` into bucket
 * `k % 64` there, so that the tests know the buckets, the probe sequences and the order of the elements.
 */
namespace {
    template<typename Allocator, sparsity Sparsity, std::size_t capacity>
    using identity_inline_map = sparse_map<std::size_t, std::size_t, identity_hash, std::equal_to<std::size_t>, Allocator, Sparsity, capacity>;

    template<typename Allocator, sparsity Sparsity, std::size_t capacity>
    using identity_inline_set = sparse_set<std::size_t, identity_hash, std::equal_to<std::size_t>, Allocator, Sparsity, capacity>;

    using inline_map_4 = identity_inline_map<std::allocator<std::pair<std::size_t, std::size_t>>, sparsity::medium, 4>;
    using inline_map_4_offset_ptr = identity_inline_map<offset_ptr_allocator<std::pair<std::size_t, std::size_t>>, sparsity::high, 4>;
    using inline_map_32 = identity_inline_map<std::allocator<std::pair<std::size_t, std::size_t>>, sparsity::low, 32>;
    using inline_set_4 = identity_inline_set<std::allocator<std::size_t>, sparsity::medium, 4>;
    using inline_set_4_offset_ptr = identity_inline_set<offset_ptr_allocator<std::size_t>, sparsity::high, 4>;
    using inline_set_32 = identity_inline_set<std::allocator<std::size_t>, sparsity::low, 32>;

    /// the address of the key of an element as `*it` gives it
    template<typename Element>
    [[nodiscard]] void const *key_address(Element const &e) {
        if constexpr (requires { e.second; }) {
            return std::addressof(e.first);
        } else {
            return std::addressof(e);
        }
    }

    /// true if `container` has elements and every element lies in the container object, so in the inline group
    template<typename Container>
    [[nodiscard]] bool elements_in_object(Container const &container) {
        auto const *const first = static_cast<void const *>(std::addressof(container));
        auto const *const last = static_cast<void const *>(std::addressof(container) + 1);
        bool all_inside = !container.empty();
        for (auto const &e : container) {
            void const *const key = key_address(e);
            all_inside = all_inside && std::less_equal<>{}(first, key) && std::less<>{}(key, last);
        }
        return all_inside;
    }

    /// true if `container` has elements and no element lies in the container object, so all are in allocated groups
    template<typename Container>
    [[nodiscard]] bool elements_in_groups(Container const &container) {
        auto const *const first = static_cast<void const *>(std::addressof(container));
        auto const *const last = static_cast<void const *>(std::addressof(container) + 1);
        bool all_outside = !container.empty();
        for (auto const &e : container) {
            void const *const key = key_address(e);
            all_outside = all_outside && (std::less<>{}(key, first) || std::less_equal<>{}(last, key));
        }
        return all_outside;
    }

    /// the keys of `container` in the order of a loop over the iterators
    template<typename Container>
    [[nodiscard]] std::vector<std::size_t> keys_by_iterators(Container const &container) {
        std::vector<std::size_t> keys;
        for (auto const &e : elements_by_iterators(container)) {
            keys.push_back(e.first);
        }
        return keys;
    }

    /**
     * `5` in bucket 5, `1` in bucket 1, `65` and `129` with the home bucket 1 in the buckets 2 and 4 (quadratic probing:
     * 1, 1 + 1, 1 + 3). In bucket order: 1, 65, 129, 5.
     */
    constexpr std::array<std::size_t, 4> colliding_keys{5, 1, 65, 129};

    /**
     * `capacity` keys that fill the inline group, the first four `colliding_keys`, then keys with the home bucket 1 or
     * 3, so that many elements are outside their home bucket.
     */
    [[nodiscard]] std::vector<std::size_t> filling_keys(std::size_t capacity) {
        std::vector<std::size_t> keys(colliding_keys.begin(), colliding_keys.end());
        for (std::size_t n = 3; keys.size() < capacity; ++n) {
            keys.push_back(64 * n + 1 + 2 * (n % 2));
        }
        return keys;
    }

    /// checks the elements that `for_each` visits, as `check_order` does, and that they are in the container object
    template<typename Container>
    void check_inline_order(Container &container) {
        REQUIRE(elements_in_object(container));
        check_order(container);
    }

    /// `for_each` that is not const adds 1 to every mapped value of a map. Checks the values with `at`.
    template<typename Container>
    void check_inline_writes(Container &container) {
        if constexpr (is_map_v<Container>) {
            auto const before = elements_by_iterators(container);
            container.for_each([](auto &&e) {
                e.second += 1;
            });
            for (auto const &[key, value] : before) {
                CHECK(container.at(key) == value + 1);
            }
        }
    }
}  // namespace

TEST_CASE_TEMPLATE("for_each of an inline container visits the inline elements in the order of the iterators",
                   container_t,
                   inline_map_4,
                   inline_map_4_offset_ptr,
                   inline_map_32,
                   inline_set_4,
                   inline_set_4_offset_ptr,
                   inline_set_32) {
    constexpr std::size_t capacity = inline_capacity_of_v<container_t>;

    SUBCASE("an empty inline container") {
        auto container = container_t{};
        REQUIRE(container.bucket_count() == 0);
        CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{0, 0});
    }

    SUBCASE("a full inline group") {
        auto container = container_t{};
        for (auto const key : filling_keys(capacity)) {
            insert_number(container, key);
        }
        REQUIRE(container.size() == capacity);
        check_inline_order(container);
        CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{capacity, capacity});
        if constexpr (capacity == 4) {
            CHECK(keys_by_iterators(container) == std::vector<std::size_t>{1, 65, 129, 5});
        }
        check_inline_writes(container);
        check_inline_order(container);
    }

    SUBCASE("an inline group with deleted buckets") {
        auto container = container_t{};
        for (auto const key : colliding_keys) {
            insert_number(container, key);
        }
        // 65 is outside its home bucket 1, so the erasure leaves bucket 1 deleted
        erase_number(container, 1);
        check_inline_order(container);
        CHECK(keys_by_iterators(container) == std::vector<std::size_t>{65, 129, 5});

        // 129 is behind the deleted buckets 1 and 2
        container.erase(container.find(65));
        check_inline_order(container);
        CHECK(keys_by_iterators(container) == std::vector<std::size_t>{129, 5});

        // 193 has the home bucket 1 and takes the deleted bucket 1, the first on its probe sequence
        insert_number(container, 193);
        check_inline_order(container);
        CHECK(keys_by_iterators(container) == std::vector<std::size_t>{193, 129, 5});
        check_inline_writes(container);
        CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{3, 3});
    }

    SUBCASE("a container that moved out of the inline group") {
        auto container = container_t{};
        auto const keys = filling_keys(capacity);
        for (auto const key : keys) {
            insert_number(container, key);
        }
        REQUIRE(elements_in_object(container));

        // the next insertion moves the elements into allocated groups
        insert_number(container, 3);
        REQUIRE(elements_in_groups(container));
        check_order(container);
        CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{capacity + 1, capacity + 1});

        // fewer elements than the inline capacity, still in the allocated group
        for (std::size_t i = 2; i < keys.size(); ++i) {
            erase_number(container, keys[i]);
        }
        REQUIRE(container.size() == 3);
        REQUIRE(elements_in_groups(container));
        check_order(container);
        check_inline_writes(container);
        CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{3, 3});

        // a moved-from container is an empty inline container
        auto moved = std::move(container);
        REQUIRE(elements_in_groups(moved));
        check_order(moved);
        CHECK(nb_calls(container) == std::pair<std::size_t, std::size_t>{0, 0});  // NOLINT(bugprone-use-after-move)

        // `rehash(0)` of an empty container gives it the inline group again
        moved.clear();
        moved.rehash(0);
        REQUIRE(moved.bucket_count() == 0);
        insert_number(moved, 7);
        insert_number(moved, 71);
        check_inline_order(moved);
        CHECK(keys_by_iterators(moved) == std::vector<std::size_t>{7, 71});
    }
}

namespace {
    /**
     * An inline map in a constant evaluation: the inline group holds no element there, `for_each` of the empty map
     * calls nothing, and the first insertion allocates the group of 64 buckets. 1 if `for_each` visits the elements
     * before and after the insertions as expected. At run time (the `CHECK`), the 3 elements stay in the inline group,
     * which also has 64 buckets, and `for_each` visits them there.
     */
    constexpr int for_each_inline_constant() {
        auto map = identity_inline_map<std::allocator<std::pair<std::size_t, std::size_t>>, sparsity::medium, 4>();
        std::size_t calls = 0;
        map.for_each([&calls](auto &&) {
            ++calls;
        });
        std::as_const(map).for_each([&calls](auto const &) {
            ++calls;
        });
        for (std::size_t key = 0; key < 3; ++key) {
            map.try_emplace(key, key + 1);
        }
        std::size_t sum = 0;
        std::as_const(map).for_each([&sum](auto const &e) {
            sum += e.second;
        });
        return (calls == 0 && sum == 6 && map.bucket_count() == 64) ? 1 : 0;
    }
}  // namespace

TEST_CASE("for_each of an inline map in a constant expression") {
    static_assert(for_each_inline_constant() == 1);
    CHECK(for_each_inline_constant() == 1);
}
