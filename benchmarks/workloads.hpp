#ifndef DICE_SPARSE_MAP_BENCHMARKS_WORKLOADS_HPP
#define DICE_SPARSE_MAP_BENCHMARKS_WORKLOADS_HPP

#include <nanobench.h>

#include <array>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#if defined(__GLIBC__)
#include <malloc.h>  // for mallopt
#endif

/**
 * The workloads of the quick overall benchmark, as functions of the map type that return what the
 * benchmark checks. The checksums depend only on the contents of the map, so every correct map
 * gives the same ones.
 *
 * Every workload takes a factory that makes an empty map, so that a map which needs its allocator
 * at construction (metall) runs them too. Mapped values are read with `it->second` and written with
 * `map[key] = value`.
 *
 * Ported from ankerl::unordered_dense (test/bench/workloads.h, MIT license). The comments keep the
 * reasons upstream gives for the shape of each workload.
 */
namespace dice::sparse_map::bench {

    /**
     * Makes an empty `Map` with `Map{}`.
     */
    template<typename Map>
    struct default_factory {
        [[nodiscard]] Map operator()() const {
            return Map{};
        }
    };

    /**
     * Keeps glibc from giving large blocks back to the operating system.
     *
     * A workload that builds a map from empty asks for megabytes and frees them again. glibc returns
     * every block above its mmap threshold to the operating system, so every repetition faults in
     * the same fresh pages again. That cost is not the map. It also depends on what ran before in the
     * process: freeing a large block raises the threshold, and then the same workload runs in the
     * arena. Upstream measured `build` with `uint64_t` keys at 38% kernel cycles without this.
     *
     * Raising both thresholds keeps the blocks in the arena, so the pages are faulted once per
     * process and not once per repetition. A program that builds one map in a fresh process pays
     * this cost too, but only once. A benchmark that repeats the build would pay it every time.
     *
     * glibc only. Elsewhere this does nothing.
     */
    inline void tame_allocator() {
#if defined(__GLIBC__)
        static auto const done = [] {
            // above any block these workloads ask for, including the doubling of the largest
            constexpr int limit = 64 * 1024 * 1024;
            mallopt(M_MMAP_THRESHOLD, limit);
            mallopt(M_TRIM_THRESHOLD, limit);
            return true;
        }();
        static_cast<void>(done);
#endif
    }

    /**
     * How long a string key is.
     *
     * Real string keys are identifiers, field names or paths. Most of them are short, a few are
     * long, and they do not all have the same length. Keys of one fixed length would hide two
     * things: the length dispatch of the hash would be perfectly predicted, and with long keys every
     * key would live on the heap.
     *
     * The lengths run from 8 to 135 bytes, skewed towards short: about a quarter are 16 bytes or
     * less, a third are 17 to 48, a quarter are 49 to 96 and a sixth are longer. That covers every
     * path of the hash, and about a quarter of the keys fit into the small string buffer of
     * `std::string`.
     */
    inline constexpr std::size_t min_key_len = 8;  // room for the whole value, so keys stay distinct
    inline constexpr std::size_t max_key_len = 135;

    /**
     * What a string key is made of.
     *
     * Only the length changes what a lookup costs: the hash and the comparison cost the same for
     * every byte value. So the filler is a fixed string, and the value goes into the first 8 bytes.
     * Two different values give two different keys, which every workload relies on.
     */
    inline constexpr std::string_view key_filler = "service.name/attribute.count/http.status_code/db.query.duration_ms/net.peer.address.family/"
                                                   "process.runtime.description/log.record.uid/k8s.pod.namespace";

    /**
     * The key that stands for the value `v`.
     *
     * For an integer key this is not the value itself. insert_erase and iterate draw their values
     * from a small range, and a hash can spread small sequential integers so evenly that nothing
     * ever collides. Then the probing is never exercised. A real integer key is as often an id from
     * somewhere else as a counter, so the value is scrambled: an xor-shift between two
     * multiplications is a bijection on 64 bits. Every value stays distinct, and every checksum
     * stays the same.
     */
    template<typename K>
    struct key_source {
        [[nodiscard]] static K get(std::uint64_t v) {
            v *= UINT64_C(0x9E3779B97F4A7C15);
            v ^= v >> 29U;
            v *= UINT64_C(0xBF58476D1CE4E5B9);
            return static_cast<K>(v);
        }
    };

    /**
     * For a string, the key is a reference to one of a set of buffers prepared once, one per
     * length, and making it costs a copy of the 8 bytes of the value. Building a new string for
     * every key costs more than the lookup in the map: upstream measured a third of a string find
     * workload in `std::string::assign`.
     *
     * The buffers are shared, so a key is only valid until the next call for the same length. Every
     * workload uses a key and is done with it.
     */
    template<>
    struct key_source<std::string> {
        [[nodiscard]] static std::string const &get(std::uint64_t v) {
            static_assert(key_filler.size() >= max_key_len);
            static auto buffers = [] {
                std::vector<std::string> prepared;
                prepared.reserve(max_key_len - min_key_len + 1);
                for (std::size_t n = min_key_len; n <= max_key_len; ++n) {
                    prepared.emplace_back(key_filler.data(), n);
                }
                return prepared;
            }();

            // square a byte of a mixed value: uniform in, skewed towards short out
            auto const spread = (v * UINT64_C(0x9E3779B97F4A7C15)) >> 56U;
            auto &key = buffers[static_cast<std::size_t>((spread * spread) >> 9U)];
            std::memcpy(key.data(), &v, sizeof(v));
            return key;
        }
    };

    /**
     * The top bit. No workload inserts a value with this bit set, so setting it names a key that
     * cannot be in the map.
     */
    inline constexpr auto never_inserted = std::uint64_t{1} << 63U;

    template<typename Map>
    [[nodiscard]] decltype(auto) key_for(std::uint64_t v) {
        return key_source<typename Map::key_type>::get(v);
    }

    /**
     * The key for a random value in [0, n), with `n` limited to 32 bits.
     */
    template<typename Map>
    [[nodiscard]] decltype(auto) random_key(ankerl::nanobench::Rng *rng, int n) {
        auto const limited = (((*rng)() >> 32U) * static_cast<std::uint64_t>(n)) >> 32U;
        return key_for<Map>(limited);
    }

    /**
     * A mapped type of the size a real one has.
     *
     * With eight bytes of payload, the property that separates the table designs stays hidden.
     * Every cost of a flat table grows with `sizeof(value_type)`, because it writes the whole value
     * into a hash-scattered slot. A dense map writes the payload in order. A sparse map moves the
     * values of a bucket group when it inserts into the group. `map<Key, SomeStruct>` is at least as
     * common as `map<Key, size_t>`, and 64 bytes is an ordinary struct.
     *
     * It is trivially copyable on purpose: the variable under test is the size of the value, and a
     * non-trivial move or an owned allocation would mix it up with the heap. The padding is
     * value-initialized, so that no copy reads an indeterminate byte.
     */
    struct big_value {
        // Implicit in both directions, so that every workload reads and writes it like a size_t and
        // the checksums are exactly the ones of the small-value maps.
        big_value() = default;
        // NOLINTNEXTLINE(google-explicit-constructor,hicpp-explicit-conversions)
        big_value(std::size_t v)
            : value(v) {
        }
        // NOLINTNEXTLINE(google-explicit-constructor,hicpp-explicit-conversions)
        operator std::size_t() const {
            return value;
        }

        std::size_t value{};
        // sized from size_t, so that the type is 64 bytes on 32 bit and on 64 bit platforms
        std::array<char, 64 - sizeof(std::size_t)> padding{};
    };
    static_assert(sizeof(big_value) == 64);
    static_assert(std::is_trivially_copyable_v<big_value>);

    /**
     * A mapped type with a `std::size_t` payload whose move constructor can throw.
     *
     * A user type gets this when its move constructor is not marked `noexcept`. A container cannot undo a move that
     * throws halfway, so it copies such a value where it would move another one: `sparse_map` when it inserts into a
     * group and when it rehashes, `std::vector` (and so `ankerl::unordered_dense`) when it grows. The copy and the
     * move cost what the copy of a `size_t` costs, so the difference to the `size_t` rows is what the container does
     * with such a type.
     *
     * Implicit in both directions, like `big_value`, so that the workloads and their checksums are the ones of the
     * `size_t` maps. Standard layout, so that it can live in a metall datastore.
     */
    struct throwing_move_value {
        throwing_move_value() = default;
        // NOLINTNEXTLINE(google-explicit-constructor,hicpp-explicit-conversions)
        throwing_move_value(std::size_t v)
            : value(v) {
        }
        throwing_move_value(throwing_move_value const &) = default;
        // NOLINTNEXTLINE(performance-noexcept-move-constructor)
        throwing_move_value(throwing_move_value &&other) noexcept(false)
            : value(other.value) {
        }
        throwing_move_value &operator=(throwing_move_value const &) = default;
        // NOLINTNEXTLINE(performance-noexcept-move-constructor)
        throwing_move_value &operator=(throwing_move_value &&other) noexcept(false) {
            value = other.value;
            return *this;
        }
        ~throwing_move_value() = default;

        // NOLINTNEXTLINE(google-explicit-constructor,hicpp-explicit-conversions)
        operator std::size_t() const {
            return value;
        }

        std::size_t value{};
    };
    static_assert(!std::is_nothrow_move_constructible_v<throwing_move_value>);
    static_assert(std::is_standard_layout_v<throwing_move_value>);

    struct insert_erase_result {
        std::size_t erased;
        std::size_t size;
    };

    /**
     * Random insert and erase, about `range / 2` entries live.
     *
     * The sizes of all workloads are template parameters, as upstream has them, so that the
     * compiler knows them.
     */
    template<typename Map, std::size_t range = 20000, typename Factory = default_factory<Map>>
    insert_erase_result insert_erase(Factory const &make_map = Factory{}) {
        tame_allocator();
        ankerl::nanobench::Rng rng(123);
        std::size_t erased{};
        Map map = make_map();
        for (int n = 1; n < static_cast<int>(range); ++n) {
            for (int i = 0; i < 200; ++i) {
                map[random_key<Map>(&rng, n)];
                erased += map.erase(random_key<Map>(&rng, n));
            }
        }
        return {erased, map.size()};
    }

    /**
     * Iterate over the whole map after every insert, then after every erase.
     */
    template<typename Map, std::size_t num_elements = 5000, typename Factory = default_factory<Map>>
    std::size_t iterate(Factory const &make_map = Factory{}) {
        tame_allocator();
        ankerl::nanobench::Rng rng(555);
        Map map = make_map();
        std::size_t result = 0;
        for (std::size_t n = 0; n < num_elements; ++n) {
            map[random_key<Map>(&rng, 1000000)] = n;
            for (auto const &key_val : map) {
                result += key_val.second;
            }
        }

        rng = ankerl::nanobench::Rng(555);
        do {
            map.erase(random_key<Map>(&rng, 1000000));
            for (auto const &key_val : map) {
                result += key_val.second;
            }
        } while (!map.empty());
        return result;
    }

    /**
     * Random finds against a map that grows to about `steps / 2` entries while it is searched.
     *
     * Half of the lookups look for a key that is in the map, chosen at random among those. The
     * other half look for a key that cannot be there. A separate rng decides which kind each lookup
     * is, so nothing repeats. A sequence that replays the insertions lets a branch predictor with a
     * long history learn the outcomes, and then a change that trades instructions for
     * mispredictions looks worse than it is.
     */
    template<typename Map, std::size_t steps = 100000, typename Factory = default_factory<Map>>
    std::size_t find_50(Factory const &make_map = Factory{}) {
        tame_allocator();
        ankerl::nanobench::Rng insert_rng(123123);
        ankerl::nanobench::Rng search_rng(987654321);

        std::vector<std::uint64_t> inserted;
        inserted.reserve(steps);
        std::size_t checksum = 0;
        Map map = make_map();
        for (std::size_t i = 0; i < steps; ++i) {
            // half of the candidates go in, the first one always, so there is something to find
            auto const candidate = insert_rng() & ~never_inserted;
            if (inserted.empty() || (insert_rng() & 1U) != 0) {
                if (map.emplace(key_for<Map>(candidate), i).second) {
                    inserted.push_back(candidate);
                }
            }

            // search 100 entries in the map
            for (std::size_t search = 0; search < 100; ++search) {
                auto const r = search_rng();
                auto const &key = (r & 1U) != 0
                                      ? key_for<Map>(inserted[((r >> 32U) * inserted.size()) >> 32U])
                                      : key_for<Map>(r | never_inserted);
                auto it = map.find(key);
                if (it != map.end()) {
                    checksum += it->second;
                }
            }
        }
        return checksum;
    }

    /**
     * A map of a fixed size and the keys that are in it, for the lookup-only workloads.
     *
     * It is built apart from the loop that searches it, so that a timed run measures the lookups
     * and not a fill. The rng is part of the table, so successive timed runs draw fresh keys instead
     * of replaying one sequence from a seed, which a branch predictor learns.
     */
    template<typename Map>
    struct lookup_table {
        Map map;
        std::vector<std::uint64_t> keys{};
        ankerl::nanobench::Rng rng{999};

        template<typename Factory = default_factory<Map>>
        explicit lookup_table(std::size_t num_elements = 50000, Factory const &make_map = Factory{})
            : map(make_map()) {
            tame_allocator();
            keys.reserve(num_elements);
            while (map.size() < num_elements) {
                auto const v = rng() & ~never_inserted;
                if (map.emplace(key_for<Map>(v), keys.size()).second) {
                    keys.push_back(v);
                }
            }
        }
    };

    /**
     * `num_lookups` lookups in a `lookup_table` that all hit (a random key that is there) or all
     * miss (a key that cannot be there): the two ends of what the hit rate does to the probe.
     *
     * The rng is copied into a local and written back at the end, the keys are read through their
     * own pointer, and the end iterator is taken once. If the rng stayed in the table, its state
     * update would be a store that the compiler must assume can alias the members of the map, and
     * it would reload them on every lookup.
     */
    template<bool hits, std::size_t num_lookups = 1000000, typename Map>
    std::size_t find_all(lookup_table<Map> *t) {
        auto rng = t->rng.copy();
        auto const *keys = t->keys.data();
        auto const num_keys = t->keys.size();
        auto const &map = t->map;
        auto const end = map.end();
        std::size_t checksum = 0;
        for (std::size_t i = 0; i < num_lookups; ++i) {
            auto const r = rng();
            auto const &key = key_for<Map>(hits ? keys[((r >> 32U) * num_keys) >> 32U] : (r | never_inserted));
            auto it = map.find(key);
            if (it != end) {
                checksum += it->second;
            }
        }
        t->rng = std::move(rng);
        return checksum;
    }

    /**
     * The keys of the hash-only workload, built once and shared by every caller. Ten thousand keys
     * are more than a branch predictor can remember and still few enough to stay in the cache.
     */
    inline std::vector<std::string> const &hash_keys() {
        static auto const keys = [] {
            std::vector<std::string> built;
            built.reserve(10000);
            ankerl::nanobench::Rng rng(4711);
            for (std::size_t i = 0; i < 10000; ++i) {
                built.push_back(key_source<std::string>::get(rng()));
            }
            return built;
        }();
        return keys;
    }

    /**
     * Build a map from empty, the half of inserting that insert_erase does not show.
     *
     * insert_erase keeps the map at a few thousand entries, so growth is a rounding error there. It
     * is not one in general: upstream measured that building a million entries without a reserve
     * costs 52% more for `uint64_t` keys and 31% more for strings, all of it rehashing.
     */
    template<typename Map, std::size_t num_elements = 200000, typename Factory = default_factory<Map>>
    std::size_t build(Factory const &make_map = Factory{}) {
        tame_allocator();
        ankerl::nanobench::Rng rng(777);
        Map map = make_map();
        for (std::size_t i = 0; i < num_elements; ++i) {
            map[key_for<Map>(rng())] = i;
        }
        return map.size();
    }

    /**
     * Sustained churn at a fixed size, which every other workload resets before it accumulates.
     *
     * The map grows once and never again: fill it to `num_elements`, reserve the room, then erase
     * one key and insert one key, so that the size stays the same. A table that frees a slot without
     * undoing what probed past it (`sparse_hash` marks an erased bucket as deleted) gets longer probe
     * sequences over time and repairs them with a rehash. The growing workloads cannot show this,
     * because a rehash for growth repairs the damage for free.
     *
     * The lookups are interleaved, so that a table which has degraded pays for it while it is
     * degraded, as a real churning cache does.
     */
    template<typename Map, std::size_t num_elements = 50000, typename Factory = default_factory<Map>>
    std::size_t churn(Factory const &make_map = Factory{}) {
        tame_allocator();
        constexpr std::size_t num_rounds = 4;

        ankerl::nanobench::Rng rng(31337);
        Map map = make_map();
        map.reserve(num_elements);

        // The keys are a counter, so every insert below brings a key the map has never held and
        // every erase names a key it holds. live[] is the set of those, always num_elements long.
        std::vector<std::uint64_t> live;
        live.reserve(num_elements);
        std::uint64_t next = 0;
        while (live.size() < num_elements) {
            map[key_for<Map>(next)] = live.size();
            live.push_back(next);
            ++next;
        }

        std::size_t checksum = 0;
        for (std::size_t i = 0; i < num_rounds * num_elements; ++i) {
            auto const slot = static_cast<std::size_t>(((rng() >> 32U) * live.size()) >> 32U);
            map.erase(key_for<Map>(live[slot]));
            map[key_for<Map>(next)] = i;
            live[slot] = next;
            ++next;

            for (std::size_t search = 0; search < 2; ++search) {
                auto const r = rng();
                auto const &key = (r & 1U) != 0 ? key_for<Map>(live[((r >> 32U) * live.size()) >> 32U])
                                                : key_for<Map>(r | never_inserted);
                auto it = map.find(key);
                if (it != map.end()) {
                    checksum += it->second;
                }
            }
        }
        return checksum + map.size();
    }

    /**
     * Erases one element after the other with `map.erase(map.begin())` until the map is empty, the pattern of a
     * worklist. Returns the sum of the mapped values. Not from upstream.
     *
     * The elements at the front of the iteration order go first, so the buckets at the front empty first. A table
     * whose `begin()` searches its buckets for the first element searches a longer way in every step.
     */
    template<typename Map>
    std::size_t drain(Map &map) {
        std::size_t sum = 0;
        while (!map.empty()) {
            auto const it = map.begin();
            sum += it->second;
            map.erase(it);
        }
        return sum;
    }

    /**
     * Hashing alone, over the keys of the string workloads.
     *
     * A whole lookup is many more instructions than the hash, so a change to the hash is easier to
     * see here. The keys are built before the loop, so the loop times hashing and not the making of
     * a key. A loop this tight is sensitive to code layout, so only a large difference here means
     * something.
     */
    template<typename Map>
    std::uint64_t hash_strings() {
        typename Map::hasher const hash{};
        std::uint64_t checksum = 0;  // the hash is 64 bits wide whatever size_t is
        for (auto const &key : hash_keys()) {
            checksum += hash(key);
        }
        return checksum;
    }

    /*
     * Set versions of build, find_50 and churn, for integer keys. They are not from upstream. A set
     * has no mapped value, so the checksums add up the keys that are found instead of the values.
     * These sums are other numbers than the ones of the map workloads, but they also depend only on
     * the contents.
     */

    template<typename Set, std::size_t num_elements = 200000, typename Factory = default_factory<Set>>
    std::size_t build_set(Factory const &make_set = Factory{}) {
        tame_allocator();
        ankerl::nanobench::Rng rng(777);
        Set set = make_set();
        for (std::size_t i = 0; i < num_elements; ++i) {
            set.insert(key_for<Set>(rng()));
        }
        return set.size();
    }

    template<typename Set, std::size_t steps = 100000, typename Factory = default_factory<Set>>
    std::size_t find_50_set(Factory const &make_set = Factory{}) {
        static_assert(std::is_integral_v<typename Set::key_type>);
        tame_allocator();
        ankerl::nanobench::Rng insert_rng(123123);
        ankerl::nanobench::Rng search_rng(987654321);

        std::vector<std::uint64_t> inserted;
        inserted.reserve(steps);
        std::size_t checksum = 0;
        Set set = make_set();
        for (std::size_t i = 0; i < steps; ++i) {
            auto const candidate = insert_rng() & ~never_inserted;
            if (inserted.empty() || (insert_rng() & 1U) != 0) {
                if (set.insert(key_for<Set>(candidate)).second) {
                    inserted.push_back(candidate);
                }
            }

            for (std::size_t search = 0; search < 100; ++search) {
                auto const r = search_rng();
                auto const key = (r & 1U) != 0 ? key_for<Set>(inserted[((r >> 32U) * inserted.size()) >> 32U])
                                               : key_for<Set>(r | never_inserted);
                auto it = set.find(key);
                if (it != set.end()) {
                    checksum += static_cast<std::size_t>(*it);
                }
            }
        }
        return checksum;
    }

    template<typename Set, std::size_t num_elements = 50000, typename Factory = default_factory<Set>>
    std::size_t churn_set(Factory const &make_set = Factory{}) {
        static_assert(std::is_integral_v<typename Set::key_type>);
        tame_allocator();
        constexpr std::size_t num_rounds = 4;

        ankerl::nanobench::Rng rng(31337);
        Set set = make_set();
        set.reserve(num_elements);

        std::vector<std::uint64_t> live;
        live.reserve(num_elements);
        std::uint64_t next = 0;
        while (live.size() < num_elements) {
            set.insert(key_for<Set>(next));
            live.push_back(next);
            ++next;
        }

        std::size_t checksum = 0;
        for (std::size_t i = 0; i < num_rounds * num_elements; ++i) {
            auto const slot = static_cast<std::size_t>(((rng() >> 32U) * live.size()) >> 32U);
            set.erase(key_for<Set>(live[slot]));
            set.insert(key_for<Set>(next));
            live[slot] = next;
            ++next;

            for (std::size_t search = 0; search < 2; ++search) {
                auto const r = rng();
                auto const key = (r & 1U) != 0 ? key_for<Set>(live[((r >> 32U) * live.size()) >> 32U])
                                               : key_for<Set>(r | never_inserted);
                auto it = set.find(key);
                if (it != set.end()) {
                    checksum += static_cast<std::size_t>(*it);
                }
            }
        }
        return checksum + set.size();
    }

}  // namespace dice::sparse_map::bench

#endif  // DICE_SPARSE_MAP_BENCHMARKS_WORKLOADS_HPP
