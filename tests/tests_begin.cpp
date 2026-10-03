#include "fixtures/allocators.hpp"

#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>
#include <nanobench.h>

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <functional>
#include <optional>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * `begin()` is constant time. The table keeps the index of its first group that holds an element, and every
 * operation that changes the table keeps it exact. A wrong index shows in two ways: one that is too low points to an
 * empty group and trips the debug check of `begin()` (the tests define `DICE_UNORDERED_SPARSE_DEBUG`), one that is
 * too high skips elements, which `check_begin` sees.
 *
 * `bucket_hash` puts key `k` into bucket `k` of a table with more than `k` buckets, so key `64 * g` is in group `g`.
 * The maps hold `std::size_t`, whose move constructor cannot throw, and `copied_number`, whose move constructor can
 * throw, so that the groups have holes.
 */
namespace {
    using namespace dice::unordered_sparse;
    using namespace dice::unordered_sparse::tests;

    /**
     * A number whose move constructor can throw.
     */
    struct copied_number {
        std::size_t value = 0;

        copied_number(std::size_t v) noexcept  // NOLINT(google-explicit-constructor)
            : value(v) {
        }

        copied_number(copied_number const &) = default;

        copied_number(copied_number &&other) noexcept(false)  // NOLINT(performance-noexcept-move-constructor)
            : value(other.value) {
        }

        copied_number &operator=(copied_number const &) = default;

        copied_number &operator=(copied_number &&other) noexcept(false) {  // NOLINT(performance-noexcept-move-constructor)
            value = other.value;
            return *this;
        }

        ~copied_number() = default;

        friend bool operator==(copied_number const &, copied_number const &) = default;
    };

    std::size_t number(std::size_t n) {
        return n;
    }

    std::size_t number(copied_number const &n) {
        return n.value;
    }

    /// calls of `bucket_hash` that are left before it throws, -1 never throws
    int hash_calls_until_throw = -1;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /// the move assignment of `bucket_hash` throws while this is true
    bool hash_moves_throw = false;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * The hash of a key is its number, and the table uses it without mixing. It throws `std::runtime_error` on the
     * n-th call after `hash_calls_until_throw` was set to n, and its move assignment throws while `hash_moves_throw`
     * is true.
     */
    struct bucket_hash {
        using is_avalanching = void;

        bucket_hash() = default;
        bucket_hash(bucket_hash const &) = default;
        bucket_hash(bucket_hash &&) = default;
        bucket_hash &operator=(bucket_hash const &) = default;
        ~bucket_hash() = default;

        bucket_hash &operator=(bucket_hash && /*other*/) noexcept(false) {
            if (hash_moves_throw) {
                throw std::runtime_error("bucket_hash move assignment");
            }
            return *this;
        }

        std::size_t operator()(std::size_t key) const {
            count_down();
            return key;
        }

        std::size_t operator()(copied_number const &key) const {
            count_down();
            return key.value;
        }

    private:
        static void count_down() {
            if (hash_calls_until_throw >= 0 && 0 == hash_calls_until_throw--) {
                throw std::runtime_error("bucket_hash");
            }
        }
    };

    template<typename T, typename Allocator = std::allocator<std::pair<T, T>>>
    using map_of = sparse_map<T, T, bucket_hash, std::equal_to<T>, Allocator>;

    static_assert(std::is_nothrow_move_constructible_v<detail::map_slot<std::size_t, std::size_t>>);
    static_assert(!std::is_nothrow_move_constructible_v<detail::map_slot<copied_number, copied_number>>);

    template<typename Map>
    void insert(Map &map, std::size_t n) {
        map.try_emplace(n, n);
    }

    /**
     * Checks that `begin()` of `map`, also of the const map, is the element with the key `first`, or `end()`, and that
     * iteration from `begin()` visits all elements, each mapped to its key.
     */
    template<typename Map>
    void check_begin(Map const &map, std::optional<std::size_t> first) {
        if (first.has_value()) {
            REQUIRE(map.begin() != map.end());
            CHECK(number(map.begin()->first) == *first);
            CHECK(number(map.cbegin()->first) == *first);
            CHECK(number(const_cast<Map &>(map).begin()->first) == *first);
        } else {
            CHECK(map.begin() == map.end());
            CHECK(map.cbegin() == map.cend());
            CHECK(const_cast<Map &>(map).begin() == const_cast<Map &>(map).end());
        }
        std::size_t nb_iterated = 0;
        bool values_right = true;
        for (auto const &[key, value] : map) {
            values_right = values_right && number(key) == number(value);
            ++nb_iterated;
        }
        CHECK(nb_iterated == map.size());
        CHECK(values_right);
    }

    /// a map with 2048 buckets: the groups 0 to 31, and no insertion below 1024 elements rehashes it
    template<typename Map>
    Map map_with_32_groups(typename Map::allocator_type const &alloc = typename Map::allocator_type{}) {
        auto map = Map{alloc};
        map.reserve(1000);
        REQUIRE(map.bucket_count() == 2048);
        return map;
    }

}  // namespace

TYPE_TO_STRING_AS("std::size_t", std::size_t);
TYPE_TO_STRING_AS("copied_number", copied_number);

TEST_CASE_TEMPLATE("begin follows the first group that holds an element", T, std::size_t, copied_number) {
    using map_t = map_of<T>;
    auto map = map_with_32_groups<map_t>();
    check_begin(map, std::nullopt);

    // elements in the groups 0, 5 and 30
    insert(map, 0);
    insert(map, 320);
    insert(map, 1920);
    check_begin(map, 0);

    CHECK(map.erase(T{0}) == 1);
    check_begin(map, 320);

    // an insertion into an earlier group
    insert(map, 128);
    check_begin(map, 128);

    auto it = map.erase(map.begin());
    REQUIRE(it != map.end());
    CHECK(number(it->first) == 320);
    check_begin(map, 320);

    CHECK(map.erase(T{320}) == 1);
    check_begin(map, 1920);

    it = map.erase(map.begin());
    CHECK(it == map.end());
    CHECK(map.empty());
    check_begin(map, std::nullopt);

    // an erase that keeps elements in the first group, and one after the first group
    insert(map, 70);
    insert(map, 71);
    insert(map, 700);
    CHECK(map.erase(T{71}) == 1);
    check_begin(map, 70);
    CHECK(map.erase(T{700}) == 1);
    check_begin(map, 70);
    it = map.erase(map.find(T{70}));
    CHECK(it == map.end());
    check_begin(map, std::nullopt);
}

TEST_CASE_TEMPLATE("every operation that changes a map keeps begin right", T, std::size_t, copied_number) {
    using map_t = map_of<T>;

    SUBCASE("constructors") {
        auto const no_buckets = map_t{};
        check_begin(no_buckets, std::nullopt);
        auto const with_buckets = map_t(1000);
        check_begin(with_buckets, std::nullopt);
    }

    SUBCASE("rehash") {
        auto map = map_with_32_groups<map_t>();
        insert(map, 640);
        insert(map, 1300);
        map.rehash(4096);
        CHECK(map.bucket_count() == 4096);
        check_begin(map, 640);
        // a rehash to the same bucket count after an erase, which leaves a deleted bucket
        CHECK(map.erase(T{640}) == 1);
        map.rehash(map.bucket_count());
        check_begin(map, 1300);
    }

    SUBCASE("clear") {
        auto map = map_with_32_groups<map_t>();
        insert(map, 640);
        map.clear();
        check_begin(map, std::nullopt);
        insert(map, 700);
        check_begin(map, 700);
    }

    SUBCASE("copy construction and copy assignment") {
        auto source = map_with_32_groups<map_t>();
        insert(source, 320);
        insert(source, 1300);
        auto const copy = source;
        check_begin(copy, 320);

        auto target = map_with_32_groups<map_t>();
        insert(target, 0);
        target = source;
        check_begin(target, 320);
    }

    SUBCASE("move construction and move assignment") {
        auto source = map_with_32_groups<map_t>();
        insert(source, 320);
        auto moved = std::move(source);
        check_begin(moved, 320);
        check_begin(source, std::nullopt);  // NOLINT(bugprone-use-after-move)
        insert(source, 64);                 // NOLINT(bugprone-use-after-move)
        check_begin(source, 64);

        auto target = map_with_32_groups<map_t>();
        insert(target, 0);
        target = std::move(moved);
        check_begin(target, 320);
        check_begin(moved, std::nullopt);  // NOLINT(bugprone-use-after-move)
    }

    SUBCASE("swap") {
        auto map_1 = map_with_32_groups<map_t>();
        insert(map_1, 320);
        auto map_2 = map_with_32_groups<map_t>();
        insert(map_2, 1920);
        map_1.swap(map_2);
        check_begin(map_1, 1920);
        check_begin(map_2, 320);
    }

    SUBCASE("merge, erase_if and erase of a range") {
        auto target = map_with_32_groups<map_t>();
        insert(target, 1920);
        auto source = map_with_32_groups<map_t>();
        insert(source, 320);
        insert(source, 1920);
        target.merge(source);
        check_begin(target, 320);
        check_begin(source, 1920);

        insert(target, 700);
        CHECK(erase_if(target, [](auto const &element) {
                  return number(element.first) < 1000;
              })
              == 2);
        check_begin(target, 1920);
        target.erase(target.cbegin(), target.cend());
        check_begin(target, std::nullopt);
    }

    SUBCASE("a rehash whose hash function throws") {
        auto map = map_with_32_groups<map_t>();
        insert(map, 320);
        insert(map, 1300);
        hash_calls_until_throw = 1;
        CHECK_THROWS_AS(map.rehash(4096), std::runtime_error);
        hash_calls_until_throw = -1;
        if constexpr (std::is_same_v<T, std::size_t>) {
            // a rehash that moves leaves the map empty
            CHECK(map.empty());
            check_begin(map, std::nullopt);
        } else {
            // a rehash that copies leaves the map unchanged
            CHECK(map.size() == 2);
            check_begin(map, 320);
        }
        insert(map, 640);
        check_begin(map, std::is_same_v<T, std::size_t> ? 640 : 320);
    }
}

TEST_CASE_TEMPLATE("moves with an unequal allocator keep begin right", T, std::size_t, copied_number) {
    using allocator_t = id_allocator<std::pair<T, T>>;
    using map_t = map_of<T, allocator_t>;

    SUBCASE("allocator-extended move construction") {
        auto source = map_with_32_groups<map_t>(allocator_t{1});
        insert(source, 320);
        auto const same = map_t(std::move(source), allocator_t{1});
        check_begin(same, 320);
        check_begin(source, std::nullopt);  // NOLINT(bugprone-use-after-move)

        insert(source, 640);  // NOLINT(bugprone-use-after-move)
        auto const other = map_t(std::move(source), allocator_t{2});
        check_begin(other, 640);
        check_begin(source, std::nullopt);  // NOLINT(bugprone-use-after-move)
    }

    SUBCASE("move assignment") {
        auto source = map_with_32_groups<map_t>(allocator_t{1});
        insert(source, 320);
        auto target = map_with_32_groups<map_t>(allocator_t{2});
        insert(target, 0);
        target = std::move(source);
        check_begin(target, 320);
        check_begin(source, std::nullopt);  // NOLINT(bugprone-use-after-move)
    }

    SUBCASE("move assignment whose hash function throws") {
        auto source = map_with_32_groups<map_t>(allocator_t{1});
        insert(source, 320);
        auto target = map_with_32_groups<map_t>(allocator_t{2});
        insert(target, 0);
        hash_moves_throw = true;
        CHECK_THROWS_AS(target = std::move(source), std::runtime_error);
        hash_moves_throw = false;
        check_begin(target, std::nullopt);
        check_begin(source, 320);  // NOLINT(bugprone-use-after-move)
        insert(target, 640);
        check_begin(target, 640);
    }
}

namespace {
    using clock_type = std::chrono::steady_clock;

    double seconds_since(clock_type::time_point start) {
        return std::chrono::duration<double>(clock_type::now() - start).count();
    }

    /// 2^22 groups of 64 buckets, 128 MiB of group headers
    constexpr std::size_t timing_bucket_count = std::size_t{1} << 28U;
    constexpr int timing_begin_calls = 40000;
    constexpr std::size_t timing_drain_elements = 65536;
    constexpr double timing_bound_seconds = 4.0;
}  // namespace

/*
 * A timing test, because the cost of `begin()` cannot be seen through the interface otherwise. A table that searches
 * for its first group reads the headers of all empty groups before it.
 *
 * (a) One element in the last of 2^22 groups, then 40000 calls of `begin()` on the const map: 1.7e11 header reads
 *     with a search, 40000 reads of `first_nonempty_group_` without.
 * (b) 65536 random keys in 2^22 groups, then `erase(begin())` until the map is empty: about 1.4e11 header reads with
 *     a search (each `begin()` reads up to the group of the next element), with `first_nonempty_group_` each header
 *     once, 2^22 in total.
 *
 * Each part must take less than 4 s. CI builds Debug, also with ASan+UBSan, and runs two test binaries at a time, so
 * the bound is far above the slowest build. Measured on thistle (AMD EPYC 9684X, at most 3.7 GHz, shared with other
 * jobs), in seconds, with the search (the table before `first_nonempty_group_`, one run) and with
 * `first_nonempty_group_` (the slowest of 3 runs):
 *
 *     build                          search (a)   search (b)   first group (a)   first group (b)
 *     clang 20 Release                    131           78           0.00007            0.0039
 *     gcc 14 Release                      126           75           0.00007            0.0041
 *     clang 20 RelWithDebInfo             126           70           0.00005            0.0042
 *     gcc 14 RelWithDebInfo               123           67           0.00007            0.0039
 *     clang 20 ASan+UBSan                 284          153           0.0004             0.0081
 *     gcc 14 ASan+UBSan                   245          188           0.0004             0.0099
 *     clang 20 Debug                      381          271           0.0019             0.019
 *     gcc 14 Debug                        304          237           0.0017             0.018
 *     clang 20 Debug, ASan+UBSan (CI)                                0.0032             0.038
 *     gcc 14 Debug, ASan+UBSan (CI)                                  0.0028             0.034
 *
 * The times of the search vary by about 15 % between runs. In Release they were at least 68 s on thistle, 17 times
 * the bound. The 128 MiB of group headers are larger than the L3 cache of one core on thistle (96 MiB) and on an
 * i9-14900K (36 MiB), so every pass reads main memory. The i9 runs at most at 6.0 GHz, 1.62 times the clock of
 * thistle. If the search ran 1.62 times faster there, it would take 42 s, 10 times the bound (derived, not
 * measured). With `first_nonempty_group_` the slowest build takes 0.038 s, 1/105 of the bound.
 */
TEST_CASE("begin is constant time, also while a map is drained") {
    using map_t = map_of<std::size_t>;

    {
        auto map = map_t{};
        map.rehash(timing_bucket_count);
        REQUIRE(map.bucket_count() == timing_bucket_count);
        insert(map, timing_bucket_count - 1);

        auto const start = clock_type::now();
        for (int i = 0; i < timing_begin_calls; ++i) {
            auto const it = std::as_const(map).begin();
            // the compiler must assume that the map changed, so it calls `begin()` again
            ankerl::nanobench::doNotOptimizeAway(it);
            ankerl::nanobench::doNotOptimizeAway(map);
        }
        auto const seconds = seconds_since(start);
        MESSAGE("(a) " << timing_begin_calls << " calls of begin(): " << seconds << " s");
        CHECK(seconds < timing_bound_seconds);
        CHECK(number(map.begin()->first) == timing_bucket_count - 1);
    }

    {
        auto map = map_t{};
        map.rehash(timing_bucket_count);
        REQUIRE(map.bucket_count() == timing_bucket_count);
        ankerl::nanobench::Rng rng(4711);
        while (map.size() < timing_drain_elements) {
            insert(map, static_cast<std::size_t>(rng()) & (timing_bucket_count - 1));
        }

        auto const start = clock_type::now();
        std::size_t nb_erased = 0;
        while (!map.empty()) {
            map.erase(map.begin());
            ++nb_erased;
        }
        auto const seconds = seconds_since(start);
        MESSAGE("(b) drain of " << timing_drain_elements << " elements: " << seconds << " s");
        CHECK(seconds < timing_bound_seconds);
        CHECK(nb_erased == timing_drain_elements);
    }
}
