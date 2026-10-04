#include "fixtures/allocators.hpp"
#include "fixtures/counter.hpp"
#include "fixtures/inline_containers.hpp"
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

    /// a type with the map as a private base, which calls `for_each` and `for_each_while` through a `static_cast`
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

        /// adds 1 to each mapped value, in the order of the iterators, until a mapped value reaches `limit`
        bool add_one_while_below(std::size_t limit) {
            return static_cast<sparse_map &>(*this).for_each_while([limit](auto &&e) {
                e.second += 1;
                return e.second < limit;
            });
        }

        /// true if every mapped value is below `limit`
        [[nodiscard]] bool all_below(std::size_t limit) const {
            return static_cast<sparse_map const &>(*this).for_each_while([limit](auto const &e) {
                return e.second < limit;
            });
        }
    };

    /// a type with the map as a protected base, which calls `for_each` and `for_each_while` through a `static_cast`
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

        /// adds 1 to each mapped value, in the order of the iterators, until a mapped value reaches `limit`
        bool add_one_while_below(std::size_t limit) {
            return static_cast<sparse_map &>(*this).for_each_while([limit](auto &&e) {
                e.second += 1;
                return e.second < limit;
            });
        }

        /// true if every mapped value is below `limit`
        [[nodiscard]] bool all_below(std::size_t limit) const {
            return static_cast<sparse_map const &>(*this).for_each_while([limit](auto const &e) {
                return e.second < limit;
            });
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

/*
 * `for_each_while` calls `f` as `for_each` does, until `f` returns `false`. The tests compare the elements that it
 * visits, in their order, with a loop over the iterators that breaks at the same element.
 */
namespace {

    /// a key that no test inserts, so an `f` that stops at it never stops
    constexpr std::size_t never_stop = SIZE_MAX;

    /// the elements that a pass gives to its `f`, and the result of the pass
    struct pass_result {
        std::vector<element> visited;
        bool ran_to_end = false;
    };

    /**
     * The elements of a loop over the iterators of `container` that breaks after the element with the key `stop_key`.
     * `ran_to_end` is true if the loop did not break.
     */
    template<typename Container>
    [[nodiscard]] pass_result elements_by_iterators_until(Container const &container, std::size_t stop_key) {
        pass_result result;
        for (auto it = container.begin(); it != container.end(); ++it) {
            result.visited.push_back(element_of(*it));
            if (result.visited.back().first == stop_key) {
                return result;
            }
        }
        result.ran_to_end = true;
        return result;
    }

    /// the elements that `for_each_while` of the const container gives to an `f` that returns `false` at `stop_key`
    template<typename Container>
    [[nodiscard]] pass_result elements_by_for_each_while(Container const &container, std::size_t stop_key) {
        pass_result result;
        result.ran_to_end = container.for_each_while([&result, stop_key](auto const &e) {
            result.visited.push_back(element_of(e));
            return result.visited.back().first != stop_key;
        });
        return result;
    }

    /// the same with `for_each_while` of the container that is not const
    template<typename Container>
    [[nodiscard]] pass_result elements_by_mutable_for_each_while(Container &container, std::size_t stop_key) {
        pass_result result;
        result.ran_to_end = container.for_each_while([&result, stop_key](auto &&e) {
            result.visited.push_back(element_of(e));
            return result.visited.back().first != stop_key;
        });
        return result;
    }

    /**
     * Checks that both `for_each_while`, with an `f` that returns `false` at the key `stop_key`, visit the elements of
     * a loop over the iterators that breaks there, and that they return `false` exactly if the loop breaks.
     */
    template<typename Container>
    void check_stop(Container &container, std::size_t stop_key) {
        auto const expected = elements_by_iterators_until(container, stop_key);
        auto const by_const = elements_by_for_each_while(container, stop_key);
        CHECK(by_const.visited == expected.visited);
        CHECK(by_const.ran_to_end == expected.ran_to_end);
        auto const by_mutable = elements_by_mutable_for_each_while(container, stop_key);
        CHECK(by_mutable.visited == expected.visited);
        CHECK(by_mutable.ran_to_end == expected.ran_to_end);
    }

    /// `check_stop` at the key of every element of `container`, and with a key that is not in it
    template<typename Container>
    void check_stop_everywhere(Container &container) {
        for (auto const &e : elements_by_iterators(container)) {
            check_stop(container, e.first);
        }
        check_stop(container, never_stop);
    }

    /// checks that `for_each_while` of both containers calls no `f` and returns `true`
    template<typename Container>
    void check_no_call(Container &container) {
        std::size_t calls = 0;
        CHECK(std::as_const(container).for_each_while([&calls](auto const &) {
            ++calls;
            return false;
        }));
        CHECK(container.for_each_while([&calls](auto &&) {
            ++calls;
            return false;
        }));
        CHECK(calls == 0);
    }

    /**
     * `for_each_while` of an empty container, of a container with one group, with a few groups, with many groups and
     * with empty groups between the others, against a loop over the iterators that breaks at the same element.
     */
    template<typename Container>
    void check_for_each_while_stops() {
        SUBCASE("an empty container without buckets") {
            auto container = Container{};
            REQUIRE(container.bucket_count() == 0);
            check_no_call(container);
            check_stop(container, never_stop);
        }

        SUBCASE("an empty container with buckets") {
            auto container = Container{};
            container.reserve(1000);
            REQUIRE(container.bucket_count() > 64);
            check_no_call(container);
        }

        SUBCASE("a container with one group, a stop at every element") {
            auto container = Container{};
            for (std::size_t n = 0; n < 20; ++n) {
                insert_number(container, n * 7919);
            }
            REQUIRE(container.bucket_count() <= 64);
            check_stop_everywhere(container);
        }

        SUBCASE("a container with a few groups after erasures, a stop at every element") {
            auto container = Container{};
            for (std::size_t n = 0; n < 100; ++n) {
                insert_number(container, n * 7919);
            }
            // erasures at the start, in the middle and at the end of groups, and holes if the elements can have them
            for (std::size_t n = 0; n < 100; n += 3) {
                erase_number(container, n * 7919);
            }
            REQUIRE(container.bucket_count() > 64);
            check_stop_everywhere(container);
        }

        SUBCASE("a container with many groups, and with empty groups between the others") {
            auto container = Container{};
            for (std::size_t n = 0; n < 5000; ++n) {
                insert_number(container, n * 7919);
            }
            REQUIRE(container.bucket_count() >= 64 * 64);
            // the first, the second, every 97th and the last two elements
            auto const order = elements_by_iterators(container);
            for (std::size_t i = 0; i < order.size(); i += 97) {
                check_stop(container, order[i].first);
            }
            for (std::size_t const i : {std::size_t{1}, order.size() - 2, order.size() - 1}) {
                check_stop(container, order[i].first);
            }
            check_stop(container, never_stop);

            for (std::size_t n = 0; n < 5000; n += 3) {
                erase_number(container, n * 7919);
            }
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
            check_stop_everywhere(container);

            container.clear();
            check_no_call(container);
        }
    }

}  // namespace

TEST_CASE_MAP("for_each_while of a map stops where a loop over the iterators breaks", std::size_t, std::string) {
    check_for_each_while_stops<map_t>();
}

TEST_CASE_MAP("for_each_while of a map with holes stops where a loop over the iterators breaks", std::size_t, throwing_move_number) {
    check_for_each_while_stops<map_t>();
}

TEST_CASE_SET("for_each_while of a set stops where a loop over the iterators breaks", std::size_t) {
    check_for_each_while_stops<set_t>();
}

TEST_CASE_SET("for_each_while of a set with holes stops where a loop over the iterators breaks", throwing_move_number, throwing_move_number_hash) {
    check_for_each_while_stops<set_t>();
}

TEST_CASE_MAP("for_each_while gives the reference types of the iterators and changes the mapped values that f got", std::size_t, std::size_t) {
    auto map = map_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        map.try_emplace(n * 7919, n);
    }
    auto const order = elements_by_iterators(map);

    // a generic `f` that writes through the reference, on a map that is not const
    std::size_t calls = 0;
    CHECK_FALSE(map.for_each_while([&calls](auto &&e) {
        static_assert(std::is_same_v<decltype(e), typename map_t::reference &&>);
        static_assert(std::is_same_v<decltype(e.second), std::size_t &>);
        e.second += 1000;
        ++calls;
        return calls != 500;
    }));
    CHECK(calls == 500);

    // the first 500 elements in the order of the iterators are changed, the others are not
    std::vector<element> expected = order;
    for (std::size_t i = 0; i < 500; ++i) {
        expected[i].second += 1000;
    }
    CHECK(elements_by_iterators(map) == expected);

    std::size_t sum = 0;
    CHECK(std::as_const(map).for_each_while([&sum](auto &&e) {
        static_assert(std::is_same_v<decltype(e), typename map_t::const_reference &&>);
        sum += e.second;
        return true;
    }));
    CHECK(sum == 999 * 1000 / 2 + 500 * 1000);
}

TEST_CASE_SET("for_each_while of a set gives the reference type of the iterators", std::size_t) {
    auto set = set_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        set.insert(n);
    }
    std::size_t sum = 0;
    CHECK(set.for_each_while([&sum](auto &&key) {
        static_assert(std::is_same_v<decltype(key), std::size_t const &>);
        sum += key;
        return true;
    }));
    CHECK(sum == 999 * 1000 / 2);
}

namespace {

    /// true if `container.for_each_while(f)` is a valid call
    template<typename Container, typename F>
    concept CanForEachWhile = requires (Container &container, F &f) { container.for_each_while(f); };

    /// true if `std::move(container).for_each_while(f)` is a valid call, with the container as an rvalue
    template<typename Container, typename F>
    concept CanForEachWhileOnRvalue = requires (Container &container, F &f) { std::move(container).for_each_while(f); };

    struct bool_reference_function {
        bool result = true;

        template<typename Element>
        bool const &operator()(Element const &) {
            return result;
        }
    };

    /// a result that converts to `bool` only explicitly
    struct explicit_bool {
        bool value = true;

        explicit operator bool() const {
            return value;
        }
    };

    /// a result that converts to `bool`, but whose `!` is deleted, so it is not boolean-testable
    struct untestable_bool {
        bool value = true;

        operator bool() const {
            return value;
        }

        void operator!() const = delete;
    };

}  // namespace

TEST_CASE("for_each_while takes an f whose result converts to bool and is boolean-testable, and no other f, also on a derived map and on an rvalue map") {
    using map_type = sparse_map<std::size_t, std::size_t>;
    using set_type = sparse_set<std::size_t>;

    using returns_bool = decltype([](auto const &) {
        return true;
    });
    using returns_int = decltype([](auto const &) {
        return 1;
    });
    using returns_pointer = decltype([](auto const &e) {
        return &e;
    });
    using returns_nothing = decltype([](auto const &) {
    });
    using returns_explicit_bool = decltype([](auto const &) {
        return explicit_bool{};
    });
    using returns_untestable_bool = decltype([](auto const &) {
        return untestable_bool{};
    });
    using takes_a_number = decltype([](std::size_t) {
        return true;
    });
    auto writes = [](auto &&e) {
        e.second += 1;
        return true;
    };

    static_assert(CanForEachWhile<map_type, returns_bool>);
    static_assert(CanForEachWhile<map_type const, returns_bool>);
    static_assert(CanForEachWhile<set_type, returns_bool>);
    static_assert(CanForEachWhile<set_type const, returns_bool>);
    static_assert(CanForEachWhile<map_type, bool_reference_function>);
    static_assert(CanForEachWhile<set_type, bool_reference_function>);
    // the constraint for a map that is not const checks only the reference type that is not const
    static_assert(CanForEachWhile<map_type, decltype(writes)>);
    static_assert(CanForEachWhile<set_type, takes_a_number>);

    // `f` is a `std::predicate`: a result that converts to `bool` and is boolean-testable
    static_assert(CanForEachWhile<map_type, returns_int>);
    static_assert(CanForEachWhile<map_type const, returns_int>);
    static_assert(CanForEachWhile<set_type, returns_int>);
    static_assert(CanForEachWhile<map_type, returns_pointer>);
    static_assert(CanForEachWhile<set_type, returns_pointer>);

    static_assert(!CanForEachWhile<map_type, returns_nothing>);
    static_assert(!CanForEachWhile<set_type, returns_nothing>);
    static_assert(!CanForEachWhile<map_type, returns_explicit_bool>);
    static_assert(!CanForEachWhile<set_type, returns_explicit_bool>);
    static_assert(!CanForEachWhile<map_type, returns_untestable_bool>);
    static_assert(!CanForEachWhile<set_type, returns_untestable_bool>);
    static_assert(!CanForEachWhile<map_type, takes_a_number>);

    // a map that is not const gives `reference`, a const map gives `const_reference`, also as an rvalue
    static_assert(CanForEachWhile<map_type, mutable_element_function>);
    static_assert(!CanForEachWhile<map_type const, mutable_element_function>);
    static_assert(CanForEachWhile<map_type const, const_element_function>);
    static_assert(!CanForEachWhile<map_type, const_element_function>);
    static_assert(CanForEachWhileOnRvalue<map_type, mutable_element_function>);
    static_assert(!CanForEachWhileOnRvalue<map_type, const_element_function>);
    static_assert(CanForEachWhileOnRvalue<map_type const, const_element_function>);
    static_assert(!CanForEachWhileOnRvalue<map_type const, mutable_element_function>);

    // a type derived from the map, and a map that is an rvalue
    derived_map map;
    map.try_emplace(1, 2);
    CHECK(map.for_each_while(writes));
    CHECK(map.at(1) == 3);
    bool const rvalue_result = map_type{{1, 2}}.for_each_while(writes);
    CHECK(rvalue_result);

    // an `f` that returns `int` stops at 0, an `f` that returns a pointer to its argument goes on
    std::size_t calls = 0;
    auto stops_at_second_call = [&calls](auto const &) {
        ++calls;
        return calls == 2 ? 0 : 1;
    };
    CHECK_FALSE(map_type{{1, 2}, {3, 4}, {5, 6}}.for_each_while(stops_at_second_call));
    CHECK(calls == 2);
    calls = 0;
    set_type set{1, 2, 3};
    CHECK_FALSE(set.for_each_while(stops_at_second_call));
    CHECK(calls == 2);
    CHECK(map.for_each_while(returns_pointer{}));
    CHECK(std::as_const(map).for_each_while(returns_pointer{}));
    CHECK(set.for_each_while(returns_pointer{}));
    calls = 0;
    CHECK(set.for_each_while([&calls](auto const &e) {
        ++calls;
        return &e;
    }));
    CHECK(calls == 3);
}

TEST_CASE("for_each_while stops at every element of groups with holes") {
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
        auto const keys = concat({{65, 66, 69, 70, 71, 72, 73, 74}, range(128, 138), {192}});

        // the keys of a loop over the iterators that breaks at `stop`, and of `for_each_while` with an `f` that returns
        // `false` at `stop`. `f` copies every key and mapped value it gets, so a hole would abort.
        auto iterated_until = [&map](std::size_t stop) {
            std::vector<std::size_t> iterated;
            for (auto const &e : map) {
                iterated.push_back(e.first.get());
                if (iterated.back() == stop) {
                    break;
                }
            }
            return iterated;
        };
        auto visited_until = [&map](std::size_t stop, bool &values_right) {
            std::vector<std::size_t> visited;
            bool const ran_to_end = std::as_const(map).for_each_while([&visited, &values_right, stop](auto const &e) {
                copied_obj const key = e.first;
                copied_obj const value = e.second;
                visited.push_back(key.get());
                values_right = values_right && value.get() == key.get();
                return key.get() != stop;
            });
            return std::pair{visited, ran_to_end};
        };
        REQUIRE(iterated_until(never_stop) == keys);
        bool values_right = true;
        for (auto const stop : keys) {
            CHECK(visited_until(stop, values_right) == std::pair{iterated_until(stop), false});
        }
        CHECK(visited_until(never_stop, values_right) == std::pair{keys, true});
        CHECK(values_right);

        // `f` changes the mapped values of the elements it gets, up to the key 70
        CHECK_FALSE(map.for_each_while([](auto &&e) {
            e.second.get() += 1000;
            return e.first.get() != 70;
        }));
        bool only_these_changed = true;
        for (auto const n : keys) {
            auto const it = map.find(copied_obj{n, counts});
            std::size_t const added = (n <= 70) ? 1000 : 0;
            only_these_changed = only_these_changed && it != map.end() && it->second.get() == n + added;
        }
        CHECK(only_these_changed);

        auto set = holes_set{};
        set.reserve(100);
        for (auto const n : range(64, 76)) {
            set.insert(copied_obj{n, counts});
        }
        for (std::size_t const n : {64, 67, 68, 75}) {
            CHECK(set.erase(copied_obj{n, counts}) == 1);
        }
        std::vector<std::size_t> const set_keys{65, 66, 69, 70, 71, 72, 73, 74};
        for (std::size_t i = 0; i <= set_keys.size(); ++i) {
            auto const stop = (i < set_keys.size()) ? set_keys[i] : never_stop;
            std::vector<std::size_t> visited_keys;
            bool const ran_to_end = set.for_each_while([&visited_keys, stop](auto const &key) {
                copied_obj const copy = key;
                visited_keys.push_back(copy.get());
                return copy.get() != stop;
            });
            auto const nb_visited = static_cast<std::ptrdiff_t>((i < set_keys.size()) ? i + 1 : set_keys.size());
            CHECK(visited_keys == std::vector<std::size_t>(set_keys.begin(), set_keys.begin() + nb_visited));
            CHECK(ran_to_end == (i == set_keys.size()));
        }
    }
    CHECK(live_blocks == 0);
}

TEST_CASE_MAP("for_each_while stops behind empty groups and at the holes of a group", std::size_t, throwing_move_number, identity_hash) {
    auto map = map_t{};
    map.rehash(1024);
    REQUIRE(map.bucket_count() == 1024);
    // the groups 0 to 2, 4 to 6 and 8 to 11 are empty, and so are the last groups, 13 to 15. Group 3 (the keys 192 to
    // 255) gets holes at its first and its last slot.
    for (std::size_t const n : {198, 200, 201, 205, 206, 470, 800, 830}) {
        map.try_emplace(n, throwing_move_number(n * 3));
    }
    CHECK(map.erase(198) == 1);
    CHECK(map.erase(206) == 1);
    REQUIRE(map.bucket_count() == 1024);
    auto const order = elements_by_iterators(map);
    std::vector<std::size_t> keys;
    for (auto const &e : order) {
        keys.push_back(e.first);
    }
    REQUIRE(keys == std::vector<std::size_t>{200, 201, 205, 470, 800, 830});
    check_stop_everywhere(map);
}

namespace {

    /**
     * Calls `for_each_while` with an `f` that records every element it gets, sets the mapped value of a map element to
     * 1, and throws at the element with the key `throw_key`. Checks that the exception leaves `for_each_while` there:
     * `f` got the elements in the order of the iterators up to `throw_key` and no other, only these elements are
     * changed, the size is the same, and the container works on.
     */
    template<typename Container>
    void check_exception_from_f_of_for_each_while(Container &container, std::size_t throw_key) {
        auto const order = elements_by_iterators(container);
        auto const nb_before = elements_by_iterators_until(container, throw_key).visited.size();
        REQUIRE(nb_before < order.size());

        std::vector<element> got;
        CHECK_THROWS_AS(container.for_each_while([&got, throw_key](auto &&e) {
            got.push_back(element_of(e));
            if constexpr (is_map_v<Container>) {
                e.second = typename Container::mapped_type(1);
            }
            if (got.back().first == throw_key) {
                throw std::runtime_error("f");
            }
            return true;
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

        // the container works on
        insert_number(container, 1000003);
        erase_number(container, 1000003);
        check_stop(container, order[nb_before].first);
    }

}  // namespace

TEST_CASE_MAP("an exception from f leaves for_each_while after the elements before it", std::size_t, std::size_t) {
    auto map = map_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        map.try_emplace(n * 7919, 0);
    }
    auto const order = elements_by_iterators(map);
    check_exception_from_f_of_for_each_while(map, order[499].first);
}

TEST_CASE_SET("an exception from f leaves for_each_while of a set after the keys before it", std::size_t) {
    auto set = set_t{};
    for (std::size_t n = 0; n < 1000; ++n) {
        set.insert(n * 7919);
    }
    auto const order = elements_by_iterators(set);
    check_exception_from_f_of_for_each_while(set, order[499].first);
}

TEST_CASE_MAP("an exception from f in a group with holes leaves for_each_while after the elements before it", std::size_t, throwing_move_number, identity_hash) {
    auto map = map_t{};
    for (std::size_t n = 0; n < 120; ++n) {
        map.try_emplace(n, throwing_move_number(n * 3));
    }
    REQUIRE(map.bucket_count() >= 128);
    // group 1 (the keys 64 to 127) gets holes at its first slot and before the key 80, where `f` throws
    for (std::size_t const n : {64, 70, 71, 90}) {
        CHECK(map.erase(n) == 1);
    }
    check_exception_from_f_of_for_each_while(map, 80);
}

namespace {

    /**
     * A map of 300 elements (16 groups) with erasures in a constant evaluation: `for_each_while` that is not const adds
     * 1 to every mapped value and never stops, then the const `for_each_while` sums the mapped values up to the 100th
     * element, and a loop over the iterators sums them up to the 100th element too. 1 if the results and the sums
     * are right.
     */
    template<typename T>
    constexpr int for_each_while_sums() {
        auto map = dice::sparse_map<int, T, constexpr_hash>();
        for (int i = 0; i < 300; ++i) {
            map.try_emplace(i, T(static_cast<std::size_t>(i)));
        }
        for (int i = 0; i < 300; i += 7) {
            map.erase(i);
        }
        auto number = [](auto const &value) -> std::size_t {
            if constexpr (std::is_same_v<T, throwing_move_number>) {
                return value.value;
            } else {
                return static_cast<std::size_t>(value);
            }
        };
        bool const full_pass = map.for_each_while([](auto &&e) {
            if constexpr (std::is_same_v<T, throwing_move_number>) {
                e.second.value += 1;
            } else {
                e.second += 1;
            }
            return true;
        });

        std::size_t calls = 0;
        std::size_t while_sum = 0;
        bool const stopped_pass = std::as_const(map).for_each_while([&](auto const &e) {
            while_sum += number(e.second);
            ++calls;
            return calls != 100;
        });
        std::size_t iterator_sum = 0;
        std::size_t iterated = 0;
        std::size_t full_sum = 0;
        for (auto const &e : map) {
            if (iterated < 100) {
                iterator_sum += number(e.second);
                ++iterated;
            }
            full_sum += number(e.second);
        }

        // the sum of i + 1 for the i below 300 that are not multiples of 7
        std::size_t expected = 0;
        for (std::size_t i = 0; i < 300; ++i) {
            if (i % 7 != 0) {
                expected += i + 1;
            }
        }
        bool const right = map.bucket_count() == 1024 && full_pass && !stopped_pass && calls == 100 && while_sum == iterator_sum && full_sum == expected;
        return right ? 1 : 0;
    }

    /// the sum of the keys of a set of 300 keys up to the key 150, which `f` stops at, and 1 if it stopped
    constexpr std::pair<int, int> for_each_while_set_sum() {
        auto set = dice::sparse_set<int, constexpr_hash>();
        for (int i = 0; i < 300; ++i) {
            set.insert(i);
        }
        int while_sum = 0;
        bool const ran_to_end = set.for_each_while([&while_sum](int const &key) {
            while_sum += key;
            return key != 150;
        });
        int iterator_sum = 0;
        for (int const key : set) {
            iterator_sum += key;
            if (key == 150) {
                break;
            }
        }
        return {while_sum == iterator_sum ? 1 : 0, ran_to_end ? 0 : 1};
    }

}  // namespace

TEST_CASE("for_each_while in a constant expression") {
    static_assert(for_each_while_sums<int>() == 1);
    static_assert(for_each_while_sums<std::size_t>() == 1);
    static_assert(for_each_while_sums<throwing_move_number>() == 1);
    static_assert(for_each_while_set_sum() == std::pair{1, 1});
    CHECK(for_each_while_sums<int>() == 1);
    CHECK(for_each_while_sums<throwing_move_number>() == 1);
    CHECK(for_each_while_set_sum() == std::pair{1, 1});
}

namespace {

    /**
     * A function object that can be neither copied nor moved. It counts its calls and returns `false` at the call
     * `stop_at`. Its call operator takes only an lvalue object.
     */
    struct pinned_counter {
        std::size_t calls = 0;
        std::size_t stop_at = 0;

        explicit pinned_counter(std::size_t stop) noexcept
            : stop_at(stop) {
        }

        pinned_counter(pinned_counter const &) = delete;
        pinned_counter(pinned_counter &&) = delete;
        pinned_counter &operator=(pinned_counter const &) = delete;
        pinned_counter &operator=(pinned_counter &&) = delete;
        ~pinned_counter() = default;

        template<typename Element>
        bool operator()(Element const &) & {
            ++calls;
            return calls != stop_at;
        }
    };

    /// `for_each_while` with `f` as an lvalue and as a prvalue, `f` is called as an lvalue and never copied or moved
    template<typename Container>
    void check_f_of_for_each_while_not_copied() {
        auto container = Container{};
        for (std::size_t n = 0; n < 1000; ++n) {
            insert_number(container, n);
        }
        pinned_counter f{300};
        CHECK_FALSE(container.for_each_while(f));
        CHECK(f.calls == 300);
        f.calls = 0;
        CHECK_FALSE(std::as_const(container).for_each_while(f));
        CHECK(f.calls == 300);
        CHECK(container.for_each_while(pinned_counter{2000}));
        CHECK(std::as_const(container).for_each_while(pinned_counter{2000}));
    }

}  // namespace

TEST_CASE_MAP("for_each_while calls f as an lvalue and never copies or moves it", std::size_t, std::size_t) {
    check_f_of_for_each_while_not_copied<map_t>();
}

TEST_CASE_SET("for_each_while of a set calls f as an lvalue and never copies or moves it", std::size_t) {
    check_f_of_for_each_while_not_copied<set_t>();
}

namespace {

    /// true if `container.for_each_while(F{})` is a valid call, with `f` as a prvalue
    template<typename Container, typename F>
    concept CanForEachWhileWithPrvalue = requires (Container &container) { container.for_each_while(F{}); };

}  // namespace

TEST_CASE("for_each_while rejects an f that can be called only as an rvalue") {
    using map_type = sparse_map<std::size_t, std::size_t>;
    using set_type = sparse_set<std::size_t>;

    // `for_each_while` calls `f` as an lvalue, also when the caller passes a prvalue
    static_assert(!CanForEachWhileWithPrvalue<map_type, rvalue_only_function>);
    static_assert(!CanForEachWhileWithPrvalue<map_type const, rvalue_only_function>);
    static_assert(!CanForEachWhileWithPrvalue<set_type, rvalue_only_function>);
    static_assert(!CanForEachWhileWithPrvalue<set_type const, rvalue_only_function>);

    static_assert(CanForEachWhileWithPrvalue<map_type, any_value_function>);
    static_assert(CanForEachWhileWithPrvalue<map_type const, any_value_function>);
    static_assert(CanForEachWhileWithPrvalue<set_type, any_value_function>);
    static_assert(CanForEachWhileWithPrvalue<set_type const, any_value_function>);
}

TEST_CASE("for_each_while of a type derived from the map visits the elements of the map, also if the type has a member ht_") {
    map_with_own_ht map;
    map.try_emplace(1, 2);
    map.try_emplace(2, 3);

    std::size_t sum = 0;
    CHECK(map.for_each_while([&sum](auto &&e) {
        e.second += 1;
        sum += e.second;
        return true;
    }));
    CHECK(sum == 7);
    CHECK(map.at(1) == 3);
    CHECK(map.at(2) == 4);

    std::size_t calls = 0;
    CHECK_FALSE(std::as_const(map).for_each_while([&calls](auto const &) {
        ++calls;
        return false;
    }));
    CHECK(calls == 1);
    CHECK(map.ht_ == 0);
}

TEST_CASE("for_each_while of a type with the map as a private or protected base works through a static_cast to the map type") {
    map_as_private_base private_map;
    private_map.try_emplace(1, 2);
    private_map.try_emplace(2, 3);
    CHECK(private_map.all_below(4));
    CHECK_FALSE(private_map.all_below(3));
    CHECK(private_map.add_one_while_below(10));
    CHECK(private_map.at(1) == 3);
    CHECK(private_map.at(2) == 4);

    map_as_protected_base protected_map;
    protected_map.try_emplace(1, 2);
    protected_map.try_emplace(2, 3);
    CHECK(protected_map.all_below(4));
    CHECK_FALSE(protected_map.all_below(3));
    CHECK(protected_map.add_one_while_below(10));
    CHECK(protected_map.at(1) == 3);
    CHECK(protected_map.at(2) == 4);
}

namespace {
    /**
     * `for_each_while` that is not const adds 1 to the mapped value of every element of a map up to the second element
     * of the order of the iterators, and stops there. Checks that exactly these elements changed.
     */
    template<typename Container>
    void check_inline_writes_until_second(Container &container) {
        if constexpr (is_map_v<Container>) {
            auto const before = elements_by_iterators(container);
            REQUIRE(before.size() >= 2);
            std::size_t const stop_key = before[1].first;
            CHECK_FALSE(container.for_each_while([stop_key](auto &&e) {
                e.second += 1;
                return e.first != stop_key;
            }));
            for (std::size_t i = 0; i < before.size(); ++i) {
                CHECK(container.at(before[i].first) == before[i].second + (i < 2 ? 1 : 0));
            }
        }
    }
}  // namespace

TEST_CASE_TEMPLATE("for_each_while of an inline container stops where a loop over the iterators breaks",
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
        check_no_call(container);
        check_stop(container, never_stop);
    }

    SUBCASE("a full inline group, a stop at every element") {
        auto container = container_t{};
        for (auto const key : filling_keys(capacity)) {
            insert_number(container, key);
        }
        REQUIRE(container.size() == capacity);
        REQUIRE(elements_in_object(container));
        check_stop_everywhere(container);
        check_inline_writes_until_second(container);
        check_stop_everywhere(container);
    }

    SUBCASE("an inline group with deleted buckets, a stop at every element") {
        auto container = container_t{};
        for (auto const key : colliding_keys) {
            insert_number(container, key);
        }
        // the deleted buckets 1 and 2, then 193 in bucket 1
        erase_number(container, 1);
        REQUIRE(elements_in_object(container));
        check_stop_everywhere(container);
        container.erase(container.find(65));
        REQUIRE(elements_in_object(container));
        check_stop_everywhere(container);
        insert_number(container, 193);
        REQUIRE(elements_in_object(container));
        REQUIRE(keys_by_iterators(container) == std::vector<std::size_t>{193, 129, 5});
        check_stop_everywhere(container);
        check_inline_writes_until_second(container);
    }

    SUBCASE("a container that moved out of the inline group, a stop at every element") {
        auto container = container_t{};
        auto const keys = filling_keys(capacity);
        for (auto const key : keys) {
            insert_number(container, key);
        }
        insert_number(container, 3);
        REQUIRE(elements_in_groups(container));
        check_stop_everywhere(container);

        for (std::size_t i = 2; i < keys.size(); ++i) {
            erase_number(container, keys[i]);
        }
        REQUIRE(container.size() == 3);
        REQUIRE(elements_in_groups(container));
        check_stop_everywhere(container);

        auto moved = std::move(container);
        check_no_call(container);  // NOLINT(bugprone-use-after-move)
        check_stop_everywhere(moved);

        moved.clear();
        moved.rehash(0);
        insert_number(moved, 7);
        insert_number(moved, 71);
        REQUIRE(elements_in_object(moved));
        check_stop_everywhere(moved);
    }
}

namespace {
    /**
     * An inline map in a constant evaluation: `for_each_while` of the empty map calls nothing and returns `true`, and
     * after the insertions, which allocate the group of 64 buckets, it stops at the key 1. 1 if both passes are right.
     * At run time (the `CHECK`), the 3 elements stay in the inline group, and `for_each_while` stops there.
     */
    constexpr int for_each_while_inline_constant() {
        auto map = identity_inline_map<std::allocator<std::pair<std::size_t, std::size_t>>, sparsity::medium, 4>();
        std::size_t calls = 0;
        bool const empty_ran_to_end = std::as_const(map).for_each_while([&calls](auto const &) {
            ++calls;
            return false;
        });
        for (std::size_t key = 0; key < 3; ++key) {
            map.try_emplace(key, key + 1);
        }
        std::size_t sum = 0;
        bool const ran_to_end = map.for_each_while([&sum](auto &&e) {
            sum += e.second;
            return e.first != 1;
        });
        return (empty_ran_to_end && calls == 0 && !ran_to_end && sum == 3) ? 1 : 0;
    }
}  // namespace

TEST_CASE("for_each_while of an inline map in a constant expression") {
    static_assert(for_each_while_inline_constant() == 1);
    CHECK(for_each_while_inline_constant() == 1);
}
