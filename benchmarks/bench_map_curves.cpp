#include "common.hpp"
#include "readme/disk_usage.hpp"
#include "tracking_allocator.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

// metall calls std::destroy_at on its pointer to the segment header and reads the pointer after that
// (kernel/segment_storage.hpp), and GCC warns about the read where it inlines the code.
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
#include <metall/metall.hpp>
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <format>
#include <functional>
#include <iostream>
#include <limits>
#include <memory>
#include <numeric>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>
#include <vector>

/*
 * Curves over the number of elements per map. For each element count n in `curve_sizes`, `num_maps` maps with exactly
 * n elements each, and the cost per map (or per find) of: a build by insertions in a random order, in ascending and in
 * descending key order, a build from a range in one call (the `std::from_range` constructor and `insert_range`, random
 * and ascending order), finds that hit and that miss (maps in a random order, the keys of a map in another random order
 * than their insertion), the erasure of all elements one by one, an iteration, the destruction, and churn at a fixed
 * size. A separate pass with `tracking_allocator` counts the bytes and allocations of a map. Each time is the median of
 * `rounds` rounds.
 *
 * Churn: the first `churn_maps` maps each run `churn_cycles` cycles of one erase and one insertion of a new key, so
 * the size stays n. An erase leaves a deleted bucket, and when the elements and the deleted buckets reach the clean-up
 * threshold (48 of a group of 64 buckets at the default maximum load factor), an insertion rebuilds the buckets. The
 * time is per cycle. For `sparse_map` and `sparse_set`, a separate pass with a hash function that counts its calls
 * gives the hash calls per map beyond the two of each cycle (`churn_rehash_hashes`): a rebuild hashes every element,
 * so this is n times the number of rebuilds.
 *
 * Types: `uint64_t -> uint64_t` maps, `uint64_t` sets and `std::string -> uint64_t` maps (keys of 12 bytes, inside the
 * small string buffer of libstdc++), each as `sparse_map`, `ankerl::unordered_dense` and `std::unordered_map`, and
 * `sparse_map<uint64_t, uint64_t>` with metall. For metall, a separate pass builds the maps (map objects included) in a
 * fresh datastore and gives the disk bytes per map (`disk_bytes`, the blocks of the datastore files after the build
 * minus before, see `readme/disk_usage.hpp`).
 *
 * The output starts with `sizeof` of each container. Every result is a line `CSV,...`.
 */

namespace {
    using namespace dice::sparse_map::bench;
    namespace sh = dice::sparse_map::sh;

    inline constexpr std::array<std::size_t, 22>
        curve_sizes{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 20, 24, 32, 48, 64};

    /// churn cycles per map: more than the 48 - n cycles after which a map of n elements in 64 buckets cleans up
    inline constexpr std::size_t churn_cycles = 64;

    struct curves_full {
        static constexpr std::size_t num_maps = 100000;
        static constexpr std::size_t churn_maps = 10000;
        static constexpr std::size_t rounds = 3;
    };

    struct curves_quick {
        static constexpr std::size_t num_maps = 300;
        static constexpr std::size_t churn_maps = 100;
        static constexpr std::size_t rounds = 1;
    };

    /// calls of `counting_hash` since the last reset
    std::size_t hash_calls = 0; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    /**
     * `table_hash` that counts its calls in `hash_calls`.
     */
    template<typename Key>
    struct counting_hash {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(Key const &key) const {
            ++hash_calls;
            return table_hash<Key>{}(key);
        }
    };

    template<
        typename Key,
        typename T,
        template<typename> typename Alloc = std::allocator,
        typename Hash = table_hash<Key>
    >
    using curve_map = ::dice::sparse_map::sparse_map<
        Key,
        T,
        Hash,
        std::equal_to<Key>,
        Alloc<std::pair<Key, T>>,
        sh::sparsity::medium
    >;

    template<typename Key, template<typename> typename Alloc = std::allocator, typename Hash = table_hash<Key>>
    using curve_set = ::dice::sparse_map::sparse_set<Key, Hash, std::equal_to<Key>, Alloc<Key>, sh::sparsity::medium>;

    template<typename Container>
    inline constexpr bool is_map = requires { typename Container::mapped_type; };

    /**
     * Makes the key of a random value. A `std::string` key has 12 bytes: the 8 bytes of the value and a fixed tail.
     */
    template<typename Key>
    struct key_maker;

    template<>
    struct key_maker<std::uint64_t> {
        [[nodiscard]] static std::uint64_t make(std::uint64_t v) {
            return v;
        }
    };

    template<>
    struct key_maker<std::string> {
        [[nodiscard]] static std::string make(std::uint64_t v) {
            std::string key(12, 'k');
            std::memcpy(key.data(), &v, sizeof(v));
            return key;
        }
    };

    /**
     * The keys of all maps with `size` elements. Map `i` holds the keys `keys[i * size]` to `keys[(i + 1) * size - 1]`,
     * distinct within the map, in a random order. The value of a key is its index in `keys`. The orders hold, for each
     * map, the indices of its keys in ascending and descending key order (`std::less`) and in another random order.
     *
     * The miss key of a key is made from its value with the lowest bit flipped. For integer keys it is next to the key
     * in the key order, so the miss keys fall at random places in the key order of a map. No map holds the miss key of
     * one of its keys.
     *
     * The churn keys of map `i` (for `i < churn_maps`) are `churn_keys[i * churn_cycles]` to
     * `churn_keys[(i + 1) * churn_cycles - 1]`, distinct and made from values with the top bit set
     * (`never_inserted`), so that they differ from every key and every miss key. Cycle `c` erases the key that cycle
     * `c - size` inserted, or the original key at `random_order[i * size + c]` while `c < size`, and inserts
     * `churn_keys[i * churn_cycles + c]`.
     */
    template<typename Key>
    struct curve_input {
        std::size_t size = 0;
        std::size_t num_maps = 0;
        std::size_t churn_maps = 0;
        std::vector<Key> keys;
        std::vector<Key> miss_keys; ///< per element its miss key, which its map does not hold
        std::vector<Key> churn_keys;
        std::vector<std::uint32_t> ascending;
        std::vector<std::uint32_t> descending;
        std::vector<std::uint32_t> random_order; ///< for the finds that hit and for the erasure
        std::vector<std::size_t> lookup_order; ///< the maps in a random order
        std::uint64_t value_sum = 0; ///< the sum of all values
        std::uint64_t key_sum = 0; ///< the sum of all keys (integer keys only), modulo 2^64

        curve_input(std::size_t num_maps, std::size_t churn_maps, std::size_t size) :
            size(size),
            num_maps(num_maps),
            churn_maps(size == 0 ? 0 : std::min(churn_maps, num_maps)) {
            ankerl::nanobench::Rng rng(4242 + size);
            std::vector<std::uint64_t> values;
            values.reserve(num_maps * size);
            for (std::size_t i = 0; i < num_maps; ++i) {
                auto const begin = values.size();
                while (values.size() - begin < size) {
                    auto const v = rng() & ~never_inserted;
                    auto const first = values.begin() + static_cast<std::ptrdiff_t>(begin);
                    if (std::find(first, values.end(), v) == values.end()
                        && std::find(first, values.end(), v ^ 1) == values.end()) {
                        values.push_back(v);
                    }
                }
            }
            keys.reserve(values.size());
            miss_keys.reserve(values.size());
            for (auto const v : values) {
                keys.push_back(key_maker<Key>::make(v));
                miss_keys.push_back(key_maker<Key>::make(v ^ 1));
                key_sum += v;
            }
            ascending.resize(values.size());
            std::iota(ascending.begin(), ascending.end(), std::uint32_t{0});
            random_order = ascending;
            for (std::size_t i = 0; i < num_maps; ++i) {
                auto const first = ascending.begin() + static_cast<std::ptrdiff_t>(i * size);
                std::sort(first, first + static_cast<std::ptrdiff_t>(size), [this](std::uint32_t a, std::uint32_t b) {
                    return std::less<Key>{}(keys[a], keys[b]);
                });
                auto random_span = std::span(random_order).subspan(i * size, size);
                rng.shuffle(random_span);
            }
            descending = ascending;
            for (std::size_t i = 0; i < num_maps; ++i) {
                auto const first = descending.begin() + static_cast<std::ptrdiff_t>(i * size);
                std::reverse(first, first + static_cast<std::ptrdiff_t>(size));
            }
            lookup_order.resize(num_maps);
            std::iota(lookup_order.begin(), lookup_order.end(), std::size_t{0});
            rng.shuffle(lookup_order);
            for (std::size_t k = 0; k < keys.size(); ++k) {
                value_sum += k;
            }

            std::vector<std::uint64_t> churn_values;
            churn_values.reserve(this->churn_maps * churn_cycles);
            for (std::size_t i = 0; i < this->churn_maps; ++i) {
                auto const begin = churn_values.size();
                while (churn_values.size() - begin < churn_cycles) {
                    auto const v = rng() | never_inserted;
                    if (std::find(churn_values.begin() + static_cast<std::ptrdiff_t>(begin), churn_values.end(), v)
                        == churn_values.end()) {
                        churn_values.push_back(v);
                    }
                }
            }
            churn_keys.reserve(churn_values.size());
            for (auto const v : churn_values) {
                churn_keys.push_back(key_maker<Key>::make(v));
            }
        }

        /**
         * The key that churn cycle `cycle` of map `i` erases.
         */
        [[nodiscard]] Key const &churn_erased(std::size_t i, std::size_t cycle) const {
            return cycle < size ? keys[random_order[i * size + cycle]] : churn_keys[i * churn_cycles + cycle - size];
        }

        /**
         * The key that churn cycle `cycle` of map `i` inserts.
         */
        [[nodiscard]] Key const &churn_inserted(std::size_t i, std::size_t cycle) const {
            return churn_keys[i * churn_cycles + cycle];
        }
    };

    template<typename Container, typename Key>
    void insert_element(Container &container, Key const &key, std::size_t index) {
        if constexpr (is_map<Container>) {
            container.try_emplace(key, index);
        } else {
            container.insert(key);
        }
    }

    /**
     * Runs the churn cycles of the first `in.churn_maps` maps. Returns the number of erased elements.
     */
    template<typename Container, typename Key>
    std::size_t churn(Container *containers, curve_input<Key> const &in) {
        std::size_t erased = 0;
        for (std::size_t i = 0; i < in.churn_maps; ++i) {
            Container &container = containers[i];
            for (std::size_t cycle = 0; cycle < churn_cycles; ++cycle) {
                erased += container.erase(in.churn_erased(i, cycle));
                insert_element(container, in.churn_inserted(i, cycle), cycle);
            }
        }
        return erased;
    }

    /**
     * True if the first `in.churn_maps` maps hold the keys that the churn cycles left, and no erased key.
     */
    template<typename Container, typename Key>
    [[nodiscard]] bool churn_left_right_keys(Container const *containers, curve_input<Key> const &in) {
        bool right = true;
        for (std::size_t i = 0; i < in.churn_maps; ++i) {
            Container const &container = containers[i];
            right = right && container.size() == in.size;
            right = right && container.contains(in.churn_inserted(i, churn_cycles - 1));
            right = right && !container.contains(in.churn_erased(i, churn_cycles - 1));
        }
        return right;
    }

    /**
     * What an element adds to the checksum: the value for a map, the key for a set of integers.
     */
    template<typename Container, typename Iterator>
    [[nodiscard]] std::uint64_t checksum_of(Iterator const &it) {
        if constexpr (is_map<Container>) {
            return it->second;
        } else {
            return *it;
        }
    }

    template<typename Container, typename Key>
    [[nodiscard]] std::uint64_t expected_sum(curve_input<Key> const &in) {
        if constexpr (is_map<Container>) {
            return in.value_sum;
        } else {
            return in.key_sum;
        }
    }

    /**
     * The elements of all maps as `value_type`, in the order of `order` (nullptr: the order of `in.keys`).
     */
    template<typename Container, typename Key>
    [[nodiscard]] std::vector<typename Container::value_type> range_values(
        curve_input<Key> const &in,
        std::vector<std::uint32_t> const *order
    ) {
        std::vector<typename Container::value_type> result;
        result.reserve(in.keys.size());
        for (std::size_t k = 0; k < in.keys.size(); ++k) {
            std::size_t const index = order == nullptr ? k : (*order)[k];
            if constexpr (is_map<Container>) {
                result.emplace_back(in.keys[index], index);
            } else {
                result.push_back(in.keys[index]);
            }
        }
        return result;
    }

    /**
     * Storage for `n` containers from `ArrayAlloc`, constructed and destroyed by hand.
     */
    template<typename Container, typename ArrayAlloc>
    struct container_array {
        using traits = std::allocator_traits<ArrayAlloc>;

        ArrayAlloc alloc;
        std::size_t n;
        typename traits::pointer storage;

        container_array(ArrayAlloc const &alloc, std::size_t n) :
            alloc(alloc),
            n(n),
            storage(traits::allocate(this->alloc, std::max<std::size_t>(n, 1))) {}

        container_array(container_array const &) = delete;
        container_array(container_array &&) = delete;
        container_array &operator=(container_array const &) = delete;
        container_array &operator=(container_array &&) = delete;

        ~container_array() {
            traits::deallocate(alloc, storage, std::max<std::size_t>(n, 1));
        }

        [[nodiscard]] Container *data() noexcept {
            return std::to_address(storage);
        }
    };

    /**
     * The metrics of one container and one element count. Times are ns per map, `hit` and `miss` ns per find, `churn`
     * ns per cycle.
     */
    struct curve_point {
        double build_random = 0;
        double build_ascending = 0;
        double build_descending = 0;
        double from_range_random = 0;
        double from_range_ascending = 0;
        double insert_range_random = 0;
        double insert_range_ascending = 0;
        double hit = 0;
        double miss = 0;
        double erase = 0;
        double iterate = 0;
        double destroy = 0;
        double churn = 0;
    };

    struct curve_counts {
        double heap_bytes = 0; ///< bytes per map after a build by insertions
        double live_allocations = 0; ///< allocations per map after a build by insertions
        double allocate_calls = 0; ///< calls of `allocate` per map during a build by insertions
        double heap_bytes_range = 0; ///< bytes per map after a build from a range
        double allocate_calls_range = 0; ///< calls of `allocate` per map during a build from a range
    };

    /**
     * The columns that only some containers have, NaN where a container does not have them.
     */
    struct curve_extras {
        double churn_rehash_hashes = std::numeric_limits<
            double
        >::quiet_NaN(); ///< hash calls per churned map beyond two per cycle
        double disk_bytes = std::numeric_limits<double>::quiet_NaN(); ///< datastore bytes per map (metall)
    };

    [[nodiscard]] double median_ns_per(std::vector<std::chrono::nanoseconds> times, std::size_t units) {
        if (units == 0 || times.empty()) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        std::ranges::sort(times);
        auto const median = times[times.size() / 2];
        return static_cast<double>(median.count()) / static_cast<double>(units);
    }

    template<typename F>
    [[nodiscard]] std::chrono::nanoseconds timed(F &&f) {
        auto const start = std::chrono::steady_clock::now();
        std::forward<F>(f)();
        return std::chrono::steady_clock::now() - start;
    }

    /**
     * Constructs a container at `p` from the elements `elements`, with `std::from_range` if the container has that
     * constructor, otherwise from the iterator pair.
     */
    template<typename Container, typename Value, typename... Alloc>
    Container &construct_from_range(Container *p, std::span<Value const> elements, Alloc const &...alloc) {
        if constexpr (requires { Container(std::from_range, elements, alloc...); }) {
            return *std::construct_at(p, std::from_range, elements, alloc...);
        } else if constexpr (sizeof...(Alloc) == 0) {
            return *std::construct_at(p, elements.begin(), elements.end());
        } else {
            return *std::construct_at(p, elements.begin(), elements.end(), 0, alloc...);
        }
    }

    /**
     * Inserts `elements` with `insert_range` if the container has it, otherwise with `insert(first, last)`.
     */
    template<typename Container, typename Value>
    void insert_all(Container &container, std::span<Value const> elements) {
        if constexpr (requires { container.insert_range(elements); }) {
            container.insert_range(elements);
        } else {
            container.insert(elements.begin(), elements.end());
        }
    }

    /**
     * Times all phases for `in.num_maps` containers. `make_empty(p)` constructs an empty container at `p`,
     * `make_from_range(p, elements)` one from a range.
     */
    template<typename Container, typename Key, typename ArrayAlloc, typename MakeEmpty, typename MakeFromRange>
    [[nodiscard]] curve_point time_curve(
        curve_input<Key> const &in,
        std::size_t rounds,
        ArrayAlloc const &array_alloc,
        MakeEmpty const &make_empty,
        MakeFromRange const &make_from_range
    ) {
        using value_type = typename Container::value_type;
        std::size_t const n = in.size;
        std::size_t const num_maps = in.num_maps;
        container_array<Container, ArrayAlloc> array(array_alloc, num_maps);
        Container *const containers = array.data();
        auto const random_values = range_values<Container>(in, nullptr);
        auto const ascending_values = range_values<Container>(in, &in.ascending);
        std::uint64_t const expected = expected_sum<Container>(in);

        auto const span_of = [n](std::vector<value_type> const &values, std::size_t i) {
            return std::span<value_type const>(values.data() + i * n, n);
        };
        auto const build_in_order = [&](std::vector<std::uint32_t> const *order) {
            for (std::size_t i = 0; i < num_maps; ++i) {
                Container &container = make_empty(containers + i);
                for (std::size_t k = i * n; k < (i + 1) * n; ++k) {
                    std::size_t const index = order == nullptr ? k : (*order)[k];
                    insert_element(container, in.keys[index], index);
                }
            }
        };
        auto const destroy_all = [&] {
            for (std::size_t i = 0; i < num_maps; ++i) {
                std::destroy_at(containers + i);
            }
        };
        auto const iterate_sum = [&] {
            std::uint64_t sum = 0;
            for (std::size_t i = 0; i < num_maps; ++i) {
                for (auto it = containers[i].begin(); it != containers[i].end(); ++it) {
                    sum += checksum_of<Container>(it);
                }
            }
            return sum;
        };
        auto const total_size = [&] {
            std::size_t total = 0;
            for (std::size_t i = 0; i < num_maps; ++i) {
                total += containers[i].size();
            }
            return total;
        };
        /// checks the elements after a build and destroys the containers, untimed
        auto const check_and_destroy = [&] {
            CHECK(total_size() == num_maps * n);
            CHECK(iterate_sum() == expected);
            destroy_all();
        };

        std::vector<std::chrono::nanoseconds> build_random;
        std::vector<std::chrono::nanoseconds> build_ascending;
        std::vector<std::chrono::nanoseconds> build_descending;
        std::vector<std::chrono::nanoseconds> from_range_random;
        std::vector<std::chrono::nanoseconds> from_range_ascending;
        std::vector<std::chrono::nanoseconds> insert_range_random;
        std::vector<std::chrono::nanoseconds> insert_range_ascending;
        std::vector<std::chrono::nanoseconds> hit;
        std::vector<std::chrono::nanoseconds> miss;
        std::vector<std::chrono::nanoseconds> erase;
        std::vector<std::chrono::nanoseconds> iterate;
        std::vector<std::chrono::nanoseconds> destroy;
        std::vector<std::chrono::nanoseconds> churn_times;

        for (std::size_t round = 0; round < rounds; ++round) {
            // random insertion order, then the finds, the iteration and the destruction
            build_random.push_back(timed([&] { build_in_order(nullptr); }));
            CHECK(total_size() == num_maps * n);

            std::uint64_t hit_sum = 0;
            hit.push_back(timed([&] {
                for (auto const i : in.lookup_order) {
                    Container const &container = containers[i];
                    for (std::size_t k = i * n; k < (i + 1) * n; ++k) {
                        auto it = container.find(in.keys[in.random_order[k]]);
                        if (it != container.end()) {
                            hit_sum += checksum_of<Container>(it);
                        }
                    }
                }
            }));
            CHECK(hit_sum == expected);

            std::size_t false_hits = 0;
            miss.push_back(timed([&] {
                for (auto const i : in.lookup_order) {
                    Container const &container = containers[i];
                    for (std::size_t k = i * n; k < (i + 1) * n; ++k) {
                        if (container.find(in.miss_keys[k]) != container.end()) {
                            ++false_hits;
                        }
                    }
                }
            }));
            CHECK(false_hits == 0);

            std::uint64_t sum = 0;
            iterate.push_back(timed([&] { sum = iterate_sum(); }));
            CHECK(sum == expected);

            destroy.push_back(timed(destroy_all));

            build_ascending.push_back(timed([&] { build_in_order(&in.ascending); }));
            check_and_destroy();

            build_descending.push_back(timed([&] { build_in_order(&in.descending); }));
            check_and_destroy();

            from_range_random.push_back(timed([&] {
                for (std::size_t i = 0; i < num_maps; ++i) {
                    make_from_range(containers + i, span_of(random_values, i));
                }
            }));
            check_and_destroy();

            from_range_ascending.push_back(timed([&] {
                for (std::size_t i = 0; i < num_maps; ++i) {
                    make_from_range(containers + i, span_of(ascending_values, i));
                }
            }));
            check_and_destroy();

            insert_range_random.push_back(timed([&] {
                for (std::size_t i = 0; i < num_maps; ++i) {
                    insert_all(make_empty(containers + i), span_of(random_values, i));
                }
            }));
            check_and_destroy();

            insert_range_ascending.push_back(timed([&] {
                for (std::size_t i = 0; i < num_maps; ++i) {
                    insert_all(make_empty(containers + i), span_of(ascending_values, i));
                }
            }));
            check_and_destroy();

            // all elements erased one by one, each map in its own random order
            build_in_order(nullptr);
            std::size_t erased = 0;
            erase.push_back(timed([&] {
                for (auto const i : in.lookup_order) {
                    Container &container = containers[i];
                    for (std::size_t k = i * n; k < (i + 1) * n; ++k) {
                        erased += container.erase(in.keys[in.random_order[k]]);
                    }
                }
            }));
            CHECK(erased == num_maps * n);
            CHECK(total_size() == 0);
            destroy_all();

            // churn at a fixed size
            if (in.churn_maps > 0) {
                build_in_order(nullptr);
                std::size_t churned = 0;
                churn_times.push_back(timed([&] { churned = churn(containers, in); }));
                CHECK(churned == in.churn_maps * churn_cycles);
                CHECK(total_size() == num_maps * n);
                CHECK(churn_left_right_keys(containers, in));
                destroy_all();
            }
        }

        return curve_point{
            .build_random = median_ns_per(build_random, num_maps),
            .build_ascending = median_ns_per(build_ascending, num_maps),
            .build_descending = median_ns_per(build_descending, num_maps),
            .from_range_random = median_ns_per(from_range_random, num_maps),
            .from_range_ascending = median_ns_per(from_range_ascending, num_maps),
            .insert_range_random = median_ns_per(insert_range_random, num_maps),
            .insert_range_ascending = median_ns_per(insert_range_ascending, num_maps),
            .hit = median_ns_per(hit, num_maps * n),
            .miss = median_ns_per(miss, num_maps * n),
            .erase = median_ns_per(erase, num_maps),
            .iterate = median_ns_per(iterate, num_maps),
            .destroy = median_ns_per(destroy, num_maps),
            .churn = median_ns_per(churn_times, in.churn_maps * churn_cycles)
        };
    }

    /**
     * Builds the containers once by insertions and once from ranges, with `tracking_allocator`, and counts what they
     * request.
     */
    template<typename Counted, typename Key>
    [[nodiscard]] curve_counts count_curve(curve_input<Key> const &in) {
        using value_type = typename Counted::value_type;
        std::size_t const n = in.size;
        auto const n_maps = static_cast<double>(in.num_maps);
        curve_counts result;
        {
            allocation_stats stats;
            std::vector<Counted> containers;
            containers.reserve(in.num_maps);
            for (std::size_t i = 0; i < in.num_maps; ++i) {
                auto &container = containers.emplace_back(typename Counted::allocator_type(&stats));
                for (std::size_t k = i * n; k < (i + 1) * n; ++k) {
                    insert_element(container, in.keys[k], k);
                }
            }
            result.heap_bytes = static_cast<double>(stats.current) / n_maps;
            result.live_allocations = static_cast<double>(stats.live_allocations) / n_maps;
            result.allocate_calls = static_cast<double>(stats.allocations) / n_maps;
            containers.clear();
            CHECK(stats.current == 0);
            CHECK(stats.live_allocations == 0);
        }
        {
            auto const values = range_values<Counted>(in, nullptr);
            allocation_stats stats;
            std::vector<std::optional<Counted>> containers(in.num_maps);
            for (std::size_t i = 0; i < in.num_maps; ++i) {
                auto const elements = std::span<value_type const>(values.data() + i * n, n);
                typename Counted::allocator_type const alloc(&stats);
                if constexpr (requires { Counted(std::from_range, elements, alloc); }) {
                    containers[i].emplace(std::from_range, elements, alloc);
                } else {
                    containers[i].emplace(elements.begin(), elements.end(), 0, alloc);
                }
            }
            result.heap_bytes_range = static_cast<double>(stats.current) / n_maps;
            result.allocate_calls_range = static_cast<double>(stats.allocations) / n_maps;
            containers.clear();
            CHECK(stats.current == 0);
        }
        return result;
    }

    /**
     * Builds the first `in.churn_maps` maps of `Hashed`, a container with `counting_hash`, runs their churn cycles and
     * returns the hash calls per map beyond the two of each cycle.
     */
    template<typename Hashed, typename Key>
    [[nodiscard]] double count_churn_hashes(curve_input<Key> const &in) {
        if (in.churn_maps == 0) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        std::vector<Hashed> containers(in.churn_maps);
        for (std::size_t i = 0; i < in.churn_maps; ++i) {
            for (std::size_t k = i * in.size; k < (i + 1) * in.size; ++k) {
                insert_element(containers[i], in.keys[k], k);
            }
        }
        hash_calls = 0;
        std::size_t const erased = churn(containers.data(), in);
        std::size_t const calls = hash_calls;
        CHECK(erased == in.churn_maps * churn_cycles);
        CHECK(churn_left_right_keys(containers.data(), in));
        std::size_t const cycle_calls = 2 * churn_cycles * in.churn_maps;
        return static_cast<double>(calls - cycle_calls) / static_cast<double>(in.churn_maps);
    }

    void print_csv_header() {
        std::cout
            << "CSV,container,n,sizeof,build_random,build_ascending,build_descending,from_range_random,"
               "from_range_ascending,insert_range_random,insert_range_ascending,hit,miss,erase,iterate,destroy,churn,"
               "heap_bytes,live_allocations,allocate_calls,heap_bytes_range,allocate_calls_range,churn_rehash_hashes,"
               "disk_bytes\n";
    }

    /**
     * Formats `value` with `digits` decimals, or empty for NaN.
     */
    [[nodiscard]] std::string optional_number(double value, int digits) {
        if (std::isnan(value)) {
            return {};
        }
        return std::format("{:.{}f}", value, digits);
    }

    void print_csv_row(
        std::string_view container,
        std::size_t n,
        std::size_t size_of,
        curve_point const &p,
        curve_counts const *c,
        curve_extras const &extras
    ) {
        std::cout << std::format(
            "CSV,{},{},{},{:.2f},{:.2f},{:.2f},{:.2f},{:.2f},{:.2f},{:.2f},{:.3f},{:.3f},{:.2f},{:.2f},{:.2f},{}",
            container,
            n,
            size_of,
            p.build_random,
            p.build_ascending,
            p.build_descending,
            p.from_range_random,
            p.from_range_ascending,
            p.insert_range_random,
            p.insert_range_ascending,
            p.hit,
            p.miss,
            p.erase,
            p.iterate,
            p.destroy,
            optional_number(p.churn, 3)
        );
        if (c != nullptr) {
            std::cout << std::format(
                ",{:.2f},{:.3f},{:.3f},{:.2f},{:.3f}",
                c->heap_bytes,
                c->live_allocations,
                c->allocate_calls,
                c->heap_bytes_range,
                c->allocate_calls_range
            );
        } else {
            std::cout << ",,,,,";
        }
        std::cout
            << std::format(
                   ",{},{}\n",
                   optional_number(extras.churn_rehash_hashes, 2),
                   optional_number(extras.disk_bytes, 2)
               )
            << std::flush;
    }

    /**
     * One row for `Container` (with `std::allocator`), its counting twin `Counted` (the same container with
     * `tracking_allocator`) and, if it is not `void`, `Hashed` (the same container with `counting_hash`).
     */
    template<typename Container, typename Counted, typename Hashed, typename Key>
    void run_std_curve(std::string_view name, curve_input<Key> const &in, std::size_t rounds) {
        using value_type = typename Container::value_type;
        auto const p = time_curve<Container>(
            in,
            rounds,
            std::allocator<Container>{},
            [](Container *ptr) -> Container & { return *std::construct_at(ptr); },
            [](Container *ptr, std::span<value_type const> elements) -> Container & {
                return construct_from_range(ptr, elements);
            }
        );
        auto const c = count_curve<Counted>(in);
        curve_extras extras;
        if constexpr (!std::is_void_v<Hashed>) {
            extras.churn_rehash_hashes = count_churn_hashes<Hashed>(in);
        }
        print_csv_row(name, in.size, sizeof(Container), p, &c, extras);
    }

    template<typename T>
    using tracked = tracking_allocator<T>;

    template<typename Sizes>
    void run_std_curves() {
        for (auto const n : curve_sizes) {
            std::cout << std::format("# n = {} at {:%FT%T}\n", n, std::chrono::system_clock::now()) << std::flush;
            {
                curve_input<std::uint64_t> const in(Sizes::num_maps, Sizes::churn_maps, n);
                run_std_curve<
                    curve_map<std::uint64_t, std::uint64_t>,
                    curve_map<std::uint64_t, std::uint64_t, tracked>,
                    curve_map<std::uint64_t, std::uint64_t, std::allocator, counting_hash<std::uint64_t>>
                >("sparse_map u64", in, Sizes::rounds);
                run_std_curve<
                    curve_set<std::uint64_t>,
                    curve_set<std::uint64_t, tracked>,
                    curve_set<std::uint64_t, std::allocator, counting_hash<std::uint64_t>>
                >("sparse_set u64", in, Sizes::rounds);
                run_std_curve<
                    unordered_dense_family<>::map<std::uint64_t, std::uint64_t>,
                    unordered_dense_family<tracked>::map<std::uint64_t, std::uint64_t>,
                    void
                >("unordered_dense map u64", in, Sizes::rounds);
                run_std_curve<
                    unordered_dense_family<>::set<std::uint64_t>,
                    unordered_dense_family<tracked>::set<std::uint64_t>,
                    void
                >("unordered_dense set u64", in, Sizes::rounds);
                run_std_curve<
                    std_family<>::map<std::uint64_t, std::uint64_t>,
                    std_family<tracked>::map<std::uint64_t, std::uint64_t>,
                    void
                >("std::unordered_map u64", in, Sizes::rounds);
                run_std_curve<std_family<>::set<std::uint64_t>, std_family<tracked>::set<std::uint64_t>, void>(
                    "std::unordered_set u64",
                    in,
                    Sizes::rounds
                );
            }
            {
                curve_input<std::string> const in(Sizes::num_maps, Sizes::churn_maps, n);
                run_std_curve<
                    curve_map<std::string, std::uint64_t>,
                    curve_map<std::string, std::uint64_t, tracked>,
                    curve_map<std::string, std::uint64_t, std::allocator, counting_hash<std::string>>
                >("sparse_map string", in, Sizes::rounds);
                run_std_curve<
                    unordered_dense_family<>::map<std::string, std::uint64_t>,
                    unordered_dense_family<tracked>::map<std::string, std::uint64_t>,
                    void
                >("unordered_dense map string", in, Sizes::rounds);
                run_std_curve<
                    std_family<>::map<std::string, std::uint64_t>,
                    std_family<tracked>::map<std::string, std::uint64_t>,
                    void
                >("std::unordered_map string", in, Sizes::rounds);
            }
        }
    }

    /**
     * The directory for the metall datastores: the environment variable `DICE_SPARSE_MAP_BENCH_METALL_DIR`, or
     * `/tmp/dice-sparse-map-bench-metall`.
     */
    [[nodiscard]] std::filesystem::path curves_metall_dir() {
        char const *dir = std::getenv("DICE_SPARSE_MAP_BENCH_METALL_DIR");
        if (dir != nullptr && *dir != '\0') {
            return dir;
        }
        return "/tmp/dice-sparse-map-bench-metall";
    }

    void remove_curves_datastore(std::filesystem::path const &path) {
        if (metall::manager::remove(path)) {
            std::error_code error;
            std::filesystem::remove_all(path, error);
        }
    }

    /**
     * A fresh metall datastore in the subdirectory `name` of `curves_metall_dir()`, removed again at the end of its
     * scope. Only the subdirectory is removed, never anything else in `curves_metall_dir()`.
     */
    struct curves_datastore {
        std::filesystem::path path;
        std::optional<metall::manager> manager;

        explicit curves_datastore(std::string_view name) : path(curves_metall_dir() / name) {
            remove_curves_datastore(path);
            std::filesystem::create_directories(path.parent_path());
            manager.emplace(metall::create_only, path);
        }

        curves_datastore(curves_datastore const &) = delete;
        curves_datastore(curves_datastore &&) = delete;
        curves_datastore &operator=(curves_datastore const &) = delete;
        curves_datastore &operator=(curves_datastore &&) = delete;

        ~curves_datastore() {
            manager.reset();
            remove_curves_datastore(path);
        }
    };

    using metall_curve_map = curve_map<std::uint64_t, std::uint64_t, metall::manager::allocator_type>;

    /**
     * Builds `in.num_maps` maps by insertions in a fresh datastore, with the map objects in the datastore too, and
     * returns the bytes of the blocks of the datastore files per map after the build minus before.
     */
    [[nodiscard]] double probe_disk_bytes(curve_input<std::uint64_t> const &in) {
        using container_t = metall_curve_map;
        using element_alloc = typename container_t::allocator_type;
        namespace disk = dice::sparse_map::bench::readme::disk_usage;
        curves_datastore datastore("curves-disk");
        REQUIRE(datastore.manager.has_value());
        metall::manager &manager = *datastore.manager;
        std::uint64_t const before = disk::allocated_bytes(datastore.path);
        std::uint64_t after = 0;
        {
            element_alloc const alloc(manager.get_allocator<typename element_alloc::value_type>());
            container_array<container_t, metall::manager::allocator_type<container_t>> array(
                manager.get_allocator<container_t>(),
                in.num_maps
            );
            container_t *const containers = array.data();
            for (std::size_t i = 0; i < in.num_maps; ++i) {
                container_t &container = *std::construct_at(containers + i, alloc);
                for (std::size_t k = i * in.size; k < (i + 1) * in.size; ++k) {
                    insert_element(container, in.keys[k], k);
                }
            }
            after = disk::allocated_bytes(datastore.path);
            for (std::size_t i = 0; i < in.num_maps; ++i) {
                std::destroy_at(containers + i);
            }
        }
        return static_cast<double>(after - std::min(after, before)) / static_cast<double>(in.num_maps);
    }

    template<typename Sizes>
    void run_metall_curves() {
        using container_t = metall_curve_map;
        using value_type = typename container_t::value_type;
        using element_alloc = typename container_t::allocator_type;
        curves_datastore datastore("curves");
        REQUIRE(datastore.manager.has_value());
        metall::manager &manager = *datastore.manager;
        for (auto const n : curve_sizes) {
            std::cout << std::format("# n = {} at {:%FT%T}\n", n, std::chrono::system_clock::now()) << std::flush;
            curve_input<std::uint64_t> const in(Sizes::num_maps, Sizes::churn_maps, n);
            element_alloc const alloc(manager.get_allocator<typename element_alloc::value_type>());
            auto const p = time_curve<container_t>(
                in,
                Sizes::rounds,
                manager.get_allocator<container_t>(),
                [&alloc](container_t *ptr) -> container_t & { return *std::construct_at(ptr, alloc); },
                [&alloc](container_t *ptr, std::span<value_type const> elements) -> container_t & {
                    return construct_from_range(ptr, elements, alloc);
                }
            );
            curve_extras extras;
            extras.disk_bytes = probe_disk_bytes(in);
            print_csv_row("metall sparse_map u64", n, sizeof(container_t), p, nullptr, extras);
        }
    }

    void print_config() {
        auto const line = [](std::string_view name, std::size_t size_of) {
            std::cout << std::format("CONFIG,{},{}\n", name, size_of);
        };
        std::cout << "\nmap curves\n";
        line("sparse_map u64", sizeof(curve_map<std::uint64_t, std::uint64_t>));
        line("sparse_set u64", sizeof(curve_set<std::uint64_t>));
        line("sparse_map string", sizeof(curve_map<std::string, std::uint64_t>));
        line("metall sparse_map u64", sizeof(metall_curve_map));
        std::cout << std::flush;
    }

} // namespace

TEST_CASE("map_curves") {
    tame_allocator();
    print_config();
    with_sizes<curves_full, curves_quick>([]<typename Sizes>() {
        std::cout << std::format(
            "map curves, std::allocator: {} maps per element count, {} of them churned, median of {} rounds\n",
            Sizes::num_maps,
            Sizes::churn_maps,
            Sizes::rounds
        );
        print_csv_header();
        run_std_curves<Sizes>();
    });
}

TEST_CASE("map_curves metall") {
    tame_allocator();
    print_config();
    with_sizes<curves_full, curves_quick>([]<typename Sizes>() {
        std::cout << std::format(
            "map curves, metall: {} maps per element count, {} of them churned, median of {} rounds\n",
            Sizes::num_maps,
            Sizes::churn_maps,
            Sizes::rounds
        );
        print_csv_header();
        run_metall_curves<Sizes>();
    });
}
