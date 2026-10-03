/*
 * The README benchmark: one map, one key type and one allocator per binary, measured on one panel
 * per process.
 *
 *   dsm_readme_<variant>_<key>_<map> <build|buildfree|find|churn|iterate|rss|memory|disk> <base>
 *   dsm_readme_timeline_std_<key>_<map>[_plain] timeline <n> [<csv>]
 *
 * The map is chosen at compile time (`DSM_README_MAP`, a struct of maps.hpp), the key type with
 * `DSM_README_STRING_KEYS` and the allocator with `DSM_README_METALL`. A binary that holds many maps
 * has a code layout that moves the numbers by more than the differences between the maps, and a
 * map in a process with other maps shares their heap. So every map gets its own binary, and every
 * panel its own process.
 *
 * Every panel is measured at five sizes that span one octave from `base` (`base * 2^(i/5)`). The
 * program prints one line: allocator, key type, map, panel, base, the five values and their
 * geometric mean. The load factor of a table is a
 * sawtooth between two doublings, and two maps do not double at the same size, so a ratio at one
 * size compares two random points of two cycles.
 *
 * Panels, per entry of the map:
 *  - `build`: one insert into an empty map, nothing reserved. The destructor is not timed.
 *  - `buildfree`: the same with the destructor timed (the "build + destroy" panel).
 *  - `find`: one lookup, half of them for keys in the map, in random order.
 *  - `churn`: one erase plus one insert of a key that is not in the map, at a fixed size.
 *  - `iterate`: one element of a full pass that sums the mapped values.
 *  - `rss`: peak resident set while the map is built, baseline subtracted, in bytes.
 *  - `memory`: peak bytes requested from the heap while the map is built. Only the binaries built
 *    with `DSM_README_COUNT_ALLOC` can measure it.
 *  - `disk`: blocks of the files of the metall datastore, empty datastore subtracted, in bytes. Only
 *    the metall binaries built with `DSM_README_DISK_USAGE` can measure it (see disk_usage.hpp). It
 *    prints one line per quantity:
 *     - `disk`: after the build, with the map alive.
 *     - `diskpeak`: the peak during the build, sampled right before every hole that metall punches,
 *       every 1/256 of the inserts and right after every insert that changed `bucket_count()`.
 *     - `diskflushed`: after the build, with the map alive, after metall's object cache was
       emptied (`profile` does that). `diskclear`: after `clear()`. `diskfreed`: after the map is
       destroyed. `diskempty`: after that and after the object cache was emptied again.
 *     - `metallsegment`, `metallchunks`, `metallobjects`: metall's own numbers after the build,
 *       read from `metall::manager::profile`: the chunks up to the last one in use, the chunks in
 *       use, and the live objects, each rounded up to its size class. The numbers of the empty
 *       datastore are subtracted.
 *    The summary of a `disk` line is the arithmetic mean if one of the five values is not positive.
 *  - `timeline`: not a panel. Builds one map of `n` entries (the second argument is `n`, not a
 *    base) and destroys it, and records the bytes allocated from the heap after every allocation
 *    and every free (see alloc_timeline.hpp). Only the binaries built with
 *    `DSM_README_ALLOC_TIMELINE` record, the `_plain` binaries only measure the time. The CSV of
 *    the timeline goes to the third argument. `DSM_README_TIMELINE_EVERY=<k>` records only every
 *    k-th event, `DSM_README_TIMELINE_BINS` sets the time bins of the CSV (default 2000). It prints
 *    one line: allocator, key type, map, `timeline`, `n`, then pairs of name and value.
 *
 * Every timed panel does about 10 million operations per size (`DSM_README_OPS` changes that), after
 * one run that is not timed.
 * The workloads check their results and the program exits with code 3 if a check fails.
 *
 * The workloads, the keys and the sizes follow the README benchmark of ankerl::unordered_dense
 * (scripts/ab/bench_readme.cpp and scripts/ab/maps.h, MIT license).
 */

#include "alloc_timeline.hpp"
#include "count_alloc.hpp"
#include "disk_usage.hpp"
#include "max_rss.hpp"
#include "maps.hpp"

#include <array>
#include <bit>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#if defined(__GLIBC__)
#include <malloc.h>
#endif

#include <unistd.h>

#ifndef DSM_README_MAP
#error "DSM_README_MAP must name a map struct of maps.hpp"
#endif

#if defined(DSM_README_DISK_USAGE) && !defined(DSM_README_METALL)
#error "DSM_README_DISK_USAGE measures a metall datastore and needs DSM_README_METALL"
#endif

namespace {

    using namespace dice::sparse_map::bench::readme;

#if defined(DSM_README_STRING_KEYS)
    using key_type = std::string;
    constexpr char const *key_name = "str";
#else
    using key_type = std::uint64_t;
    constexpr char const *key_name = "u64";
#endif

#if defined(DSM_README_METALL)
    constexpr char const *variant_name = "metall";
#else
    constexpr char const *variant_name = "std";
#endif

    using map_desc = DSM_README_MAP;
    using map_t = map_desc::type<key_type>;

    using clock_type = std::chrono::steady_clock;

    /**
     * Operations per timed region, independent of the size of the map: 10 million, or the value of
     * the environment variable `DSM_README_OPS` (for smoke tests).
     */
    std::size_t const target_ops = [] {
        char const *ops = std::getenv("DSM_README_OPS");
        if (ops != nullptr && *ops != '\0') {
            return static_cast<std::size_t>(std::strtoull(ops, nullptr, 10));
        }
        return std::size_t{10'000'000};
    }();

    std::string_view current_work;

    /// reports a failed check and ends the process
    [[noreturn]] void fail(char const *what) {
        std::fflush(stdout);
        std::fprintf(stderr, "FAILED %s %s %s %.*s: %s\n", variant_name, key_name, map_desc::name,
                     static_cast<int>(current_work.size()), current_work.data(), what);
        std::fflush(stderr);
        std::_Exit(3);
    }

    /// keeps the compiler from removing the computation of `value`
    template<typename T>
    void keep(T const &value) {
        asm volatile("" : : "r,m"(value) : "memory");
    }

    /// tells the compiler that any memory may have changed
    void clobber() {
        asm volatile("" : : : "memory");
    }

    /**
     * Keeps every block these workloads free in glibc's heap instead of giving it back with
     * `munmap`, as the harness of ankerl::unordered_dense does. 64 MiB is above most blocks here.
     */
    void tame_allocator() {
#if defined(__GLIBC__)
        constexpr int limit = 64 * 1024 * 1024;
        mallopt(M_MMAP_THRESHOLD, limit);
        mallopt(M_TRIM_THRESHOLD, limit);
#endif
    }

    /// romu duo jr, seeded with splitmix64
    struct rng {
    private:
        std::uint64_t x_ = 0;
        std::uint64_t y_ = 0;

    public:
        explicit rng(std::uint64_t seed) noexcept {
            auto next = [&seed] {
                seed += 0x9E3779B97F4A7C15ULL;
                std::uint64_t z = seed;
                z = (z ^ (z >> 30U)) * 0xBF58476D1CE4E5B9ULL;
                z = (z ^ (z >> 27U)) * 0x94D049BB133111EBULL;
                return z ^ (z >> 31U);
            };
            x_ = next();
            y_ = next();
        }

        std::uint64_t operator()() noexcept {
            std::uint64_t const x = x_;
            x_ = 15241094284759029579ULL * y_;
            y_ = std::rotl(y_ - x, 27);
            return x;
        }

        /// uniform in `[0, n)` for `n < 2^32`, without a division
        std::size_t below(std::size_t n) noexcept {
            return static_cast<std::size_t>(((operator()() >> 32U) * n) >> 32U);
        }
    };

    /*
     * String keys are 8 to 135 bytes long, skewed towards short: about a quarter have 16 bytes or
     * less and fit into the `std::string` itself, a third have 17 to 48, a quarter 49 to 96 and a
     * sixth more. The first 8 bytes hold the value, so two values give two keys, and the rest is a
     * fixed filler.
     */
    constexpr std::size_t min_key_length = 8;
    constexpr std::size_t max_key_length = 135;
    constexpr std::string_view key_filler =
            "service.name/attribute.count/http.status_code/db.query.duration_ms/net.peer.address.family/"
            "process.runtime.description/log.record.uid/k8s.pod.namespace";
    static_assert(key_filler.size() >= max_key_length);

    template<typename Key>
    Key make_key(std::uint64_t value);

    template<>
    [[maybe_unused]] std::uint64_t make_key<std::uint64_t>(std::uint64_t value) {
        return value;
    }

    template<>
    [[maybe_unused]] std::string make_key<std::string>(std::uint64_t value) {
        std::uint64_t const spread = (value * 0x9E3779B97F4A7C15ULL) >> 56U;
        std::size_t const length = min_key_length + static_cast<std::size_t>((spread * spread) >> 9U);
        std::string key{key_filler.substr(0, length)};
        std::memcpy(key.data(), &value, sizeof(value));
        return key;
    }

    /**
     * Three disjoint pools of `n` random keys: the keys in the map, keys that are never in the map
     * (the misses of `find`) and the keys that `churn` inserts. The top two bits of the value
     * select the pool.
     */
    struct pools {
        std::vector<key_type> present;
        std::vector<key_type> absent;
        std::vector<key_type> spare;

        explicit pools(std::size_t n)
            : present(make_keys(0, n, 1)),
              absent(make_keys(std::uint64_t{1} << 63U, n, 2)),
              spare(make_keys(std::uint64_t{1} << 62U, n, 3)) {
        }

        static std::vector<key_type> make_keys(std::uint64_t pool, std::size_t n, std::uint64_t seed) {
            std::vector<key_type> keys;
            keys.reserve(n);
            rng random{seed};
            for (std::size_t i = 0; i < n; ++i) {
                keys.push_back(make_key<key_type>((random() >> 2U) | pool));
            }
            return keys;
        }
    };

#if defined(DSM_README_METALL)
    /// directory of the metall datastores: `DSM_README_METALL_DIR` or `/tmp/dsm-readme-metall`
    std::filesystem::path metall_dir() {
        char const *dir = std::getenv("DSM_README_METALL_DIR");
        if (dir != nullptr && *dir != '\0') {
            return dir;
        }
        return "/tmp/dsm-readme-metall";
    }

    /// removes the datastore at `path`, also one that was not closed
    void remove_datastore(std::filesystem::path const &path) {
        static_cast<void>(metall::manager::remove(path.c_str()));
        std::error_code error;
        std::filesystem::remove_all(path, error);
    }

    /// a path for a datastore that no other process and no earlier call uses
    std::filesystem::path fresh_datastore_path() {
        static std::size_t counter = 0;
        return metall_dir() / ("ds-" + std::to_string(::getpid()) + "-" + std::to_string(counter++));
    }

    /**
     * A fresh metall datastore at `path`, removed again at the end of its scope. Every map is made
     * with an allocator of its manager.
     */
    struct context {
    private:
        std::filesystem::path path_;
        std::optional<metall::manager> manager_;

    public:
        explicit context(std::filesystem::path path)
            : path_(std::move(path)) {
            remove_datastore(path_);
            std::filesystem::create_directories(path_.parent_path());
            manager_.emplace(metall::create_only, path_.c_str());
            if (!manager_->check_sanity()) {
                fail("metall manager");
            }
        }

        context(context const &) = delete;
        context(context &&) = delete;
        context &operator=(context const &) = delete;
        context &operator=(context &&) = delete;

        ~context() {
            manager_.reset();
            remove_datastore(path_);
        }

        template<typename Map>
        [[nodiscard]] Map make() {
            return Map(typename Map::allocator_type(manager_->get_allocator()));
        }

        [[nodiscard]] std::filesystem::path const &path() const noexcept {
            return path_;
        }

        [[nodiscard]] metall::manager &manager() noexcept {
            return *manager_;
        }
    };

#if defined(DSM_README_DISK_USAGE)
    /// metall's own numbers, read from `metall::manager::profile`, in bytes
    struct metall_numbers {
        double segment = 0.0;  ///< the chunks up to the last one in use
        double chunks = 0.0;   ///< the chunks in use
        double objects = 0.0;  ///< the live objects, each rounded up to its size class
        /// per object size: the chunks in use and the live objects
        std::map<std::uint64_t, std::pair<double, double>> classes;
    };

    /**
     * Reads metall's profile. It has one line per chunk up to the last one in use: the chunk
     * number, the object size (0 for a free chunk) and the share of its slots in use, in percent
     * with two decimals. A chunk of a large object shows the size of the object and 100. `profile`
     * first gives the objects in metall's object cache back to their chunks, which can free
     * chunks, so it changes the datastore.
     */
    metall_numbers read_profile(metall::manager &manager) {
        std::stringstream out;
        manager.profile(&out);
        constexpr auto chunk = static_cast<double>(metall::manager::chunk_size());
        metall_numbers numbers;
        std::string line;
        bool in_chunks = false;
        while (std::getline(out, line)) {
            if (line.starts_with("[chunk no]")) {
                in_chunks = true;
                continue;
            }
            if (!in_chunks) {
                continue;
            }
            std::istringstream fields{line};
            std::uint64_t chunk_no = 0;
            double object_size = 0.0;
            double occupancy = 0.0;
            if (!(fields >> chunk_no >> object_size >> occupancy)) {
                break;
            }
            numbers.segment += chunk;
            if (object_size == 0.0) {
                continue;
            }
            numbers.chunks += chunk;
            double objects = chunk;
            if (object_size <= chunk / 2.0) {
                double const slots = std::floor(chunk / object_size);
                objects = std::round(occupancy / 100.0 * slots) * object_size;
            }
            numbers.objects += objects;
            auto &of_class = numbers.classes[static_cast<std::uint64_t>(object_size)];
            of_class.first += chunk;
            of_class.second += objects;
        }
        return numbers;
    }
#endif
#else
    std::filesystem::path fresh_datastore_path() {
        return {};
    }

    void remove_datastore(std::filesystem::path const &) {
    }

    /// makes every map with `Map()`
    struct context {
        explicit context(std::filesystem::path const &) {
        }

        template<typename Map>
        [[nodiscard]] Map make() {
            return Map();
        }
    };
#endif

    template<typename Map>
    void fill(Map &map, std::vector<key_type> const &keys) {
        for (auto const &key : keys) {
            map.try_emplace(key, 1);
        }
    }

    template<typename Map>
    std::size_t sum(Map const &map) {
        std::size_t total = 0;
        for (auto const &element : map) {
            total += element.second;
        }
        return total;
    }

    /// the i-th of five sizes that span one octave from `base`
    std::size_t octave_size(std::size_t base, std::size_t i) {
        return static_cast<std::size_t>(static_cast<double>(base) * std::pow(2.0, static_cast<double>(i) / 5.0));
    }

    double nanoseconds(clock_type::duration duration) {
        return static_cast<double>(std::chrono::duration_cast<std::chrono::nanoseconds>(duration).count());
    }

#if defined(DSM_README_DISK_USAGE)
    /// the quantities of the `disk` panel, one line each, in this order
    constexpr std::array<char const *, 9> disk_quantities{"disk", "diskpeak", "diskflushed", "diskclear", "diskfreed", "diskempty", "metallsegment", "metallchunks", "metallobjects"};

    /**
     * `fill` with samples of the disk usage: every 1/256 of the inserts, and right after every
     * insert that changed `bucket_count()`. The samples right before a hole punch come from the
     * replaced `madvise`.
     */
    template<typename Map>
    void fill_sampled(Map &map, std::vector<key_type> const &keys, disk_usage::sampler &sampler) {
        std::size_t const every = keys.size() / 256 > 0 ? keys.size() / 256 : 1;
        auto buckets = map.bucket_count();
        std::size_t inserted = 0;
        for (auto const &key : keys) {
            map.try_emplace(key, 1);
            ++inserted;
            if (map.bucket_count() != buckets) {
                buckets = map.bucket_count();
                sampler.sample(disk_usage::trigger::growth);
            }
            if (inserted % every == 0) {
                sampler.sample(disk_usage::trigger::periodic);
            }
        }
    }

    /**
     * The quantities of `disk_quantities` at size `n`, in bytes per entry. Prints the details of
     * the sampling and metall's chunks per object size to stderr.
     */
    std::array<double, disk_quantities.size()> disk_one_size(std::size_t n) {
        pools p{n};
        context ctx{fresh_datastore_path()};
        std::uint64_t const empty = disk_usage::allocated_bytes(ctx.path());
        metall_numbers const empty_metall = read_profile(ctx.manager());

        disk_usage::sampler sampler{ctx.path()};
        std::optional<map_t> map;
        disk_usage::punch_sampler = &sampler;
        auto const t0 = clock_type::now();
        map.emplace(ctx.make<map_t>());
        fill_sampled(*map, p.present, sampler);
        auto const t1 = clock_type::now();
        std::uint64_t const built = sampler.now();
        disk_usage::punch_sampler = nullptr;
        if (map->size() != n) {
            fail("size after fill");
        }
        std::size_t const buckets = map->bucket_count();

        metall_numbers const metall = read_profile(ctx.manager());
        std::uint64_t const flushed = disk_usage::allocated_bytes(ctx.path());
        map->clear();
        std::uint64_t const cleared = disk_usage::allocated_bytes(ctx.path());
        map.reset();
        std::uint64_t const freed = disk_usage::allocated_bytes(ctx.path());
        static_cast<void>(read_profile(ctx.manager()));
        std::size_t files = 0;
        std::uint64_t const emptied = disk_usage::allocated_bytes(ctx.path(), &files);

        auto const entries = static_cast<double>(n);
        auto per_entry = [&](std::uint64_t bytes) {
            return (static_cast<double>(bytes) - static_cast<double>(empty)) / entries;
        };
        auto const p_index = static_cast<std::size_t>(disk_usage::trigger::periodic);
        auto const g_index = static_cast<std::size_t>(disk_usage::trigger::growth);
        auto const h_index = static_cast<std::size_t>(disk_usage::trigger::punch);
        std::fprintf(stderr,
                     "disk-detail %s %s %s n %zu buckets %zu empty_bytes %llu built_bytes %llu peak_bytes %llu "
                     "samples periodic %zu growth %zu punch %zu largest_per_entry periodic %.4f growth %.4f "
                     "punch %.4f punches %zu punched_bytes %llu sample_us %.1f fill_ms %.1f "
                     "files %zu empty_metall segment %.0f chunks %.0f objects %.0f\n",
                     variant_name,
                     key_name,
                     map_desc::name,
                     n,
                     buckets,
                     static_cast<unsigned long long>(empty),
                     static_cast<unsigned long long>(built),
                     static_cast<unsigned long long>(sampler.peak),
                     sampler.samples[p_index],
                     sampler.samples[g_index],
                     sampler.samples[h_index],
                     sampler.samples[p_index] > 0 ? per_entry(sampler.largest[p_index]) : 0.0,
                     sampler.samples[g_index] > 0 ? per_entry(sampler.largest[g_index]) : 0.0,
                     sampler.samples[h_index] > 0 ? per_entry(sampler.largest[h_index]) : 0.0,
                     sampler.punches,
                     static_cast<unsigned long long>(sampler.punched_bytes),
                     sampler.total_samples() > 0
                         ? nanoseconds(sampler.spent) / 1000.0 / static_cast<double>(sampler.total_samples())
                         : 0.0,
                     nanoseconds(t1 - t0) / 1e6,
                     files,
                     empty_metall.segment,
                     empty_metall.chunks,
                     empty_metall.objects);
        std::fprintf(stderr, "disk-classes %s %s %s n %zu", variant_name, key_name, map_desc::name, n);
        for (auto const &[object_size, of_class] : metall.classes) {
            std::fprintf(stderr, " %llu:%.0f:%.0f", static_cast<unsigned long long>(object_size), of_class.first, of_class.second);
        }
        std::fprintf(stderr, "\n");

        return {per_entry(built),
                per_entry(sampler.peak),
                per_entry(flushed),
                per_entry(cleared),
                per_entry(freed),
                per_entry(emptied),
                (metall.segment - empty_metall.segment) / entries,
                (metall.chunks - empty_metall.chunks) / entries,
                (metall.objects - empty_metall.objects) / entries};
    }

    /// runs the `disk` panel at the five sizes and prints one line per quantity
    void print_disk(std::size_t base) {
        std::array<std::array<double, disk_quantities.size()>, 5> values{};
        for (std::size_t i = 0; i < 5; ++i) {
            values[i] = disk_one_size(octave_size(base, i));
        }
        for (std::size_t q = 0; q < disk_quantities.size(); ++q) {
            std::printf("%s %s %s %s %zu", variant_name, key_name, map_desc::name, disk_quantities[q], base);
            bool all_positive = true;
            double log_sum = 0.0;
            double sum = 0.0;
            for (std::size_t i = 0; i < 5; ++i) {
                double const value = values[i][q];
                all_positive = all_positive && value > 0.0;
                log_sum += std::log(value > 0.0 ? value : 1e-9);
                sum += value;
                std::printf(" %.4f", value);
            }
            std::printf(" geomean %.4f\n", all_positive ? std::exp(log_sum / 5.0) : sum / 5.0);
        }
        std::fflush(stdout);
    }
#endif

    /// a positive number from the environment variable `name`, or `fallback`
    std::size_t env_size(char const *name, std::size_t fallback) {
        char const *value = std::getenv(name);
        if (value != nullptr && *value != '\0') {
            return static_cast<std::size_t>(std::strtoull(value, nullptr, 10));
        }
        return fallback;
    }

    /**
     * The `timeline` workload: an empty map, `n` inserts one by one (nothing reserved), the
     * destructor. The keys are made before the recording starts. Writes the timeline to `csv` if
     * this binary records and `csv` is not null, and prints one line.
     */
    [[maybe_unused]] void print_timeline(std::size_t n, char const *csv) {
        std::vector<key_type> const keys = pools::make_keys(0, n, 1);
        std::size_t const every = env_size("DSM_README_TIMELINE_EVERY", 1);
        std::size_t const bins = env_size("DSM_README_TIMELINE_BINS", 2000);
        // Up to 2.8 events per insert (`sparse_map` with sparsity high, two for a node map). Mapped and
        // faulted in now.
        std::size_t const capacity = env_size("DSM_README_TIMELINE_CAPACITY", 3 * n + 1'000'000) / (every > 0 ? every : 1) + 16;
        alloc_timeline::recorder recorder{capacity, every};
        context ctx{fresh_datastore_path()};

        std::size_t size = 0;
        std::uint64_t inserted_at = 0;
        std::size_t inserted_bytes = 0;
        recorder.start();
        {
            auto map = ctx.make<map_t>();
            fill(map, keys);
            size = map.size();
            keep(size);
            inserted_at = alloc_timeline::ticks();
            inserted_bytes = alloc_timeline::live;
        }
        recorder.stop();

        if (size != n) {
            fail("size after fill");
        }
        if (alloc_timeline::available() && (alloc_timeline::live != 0 || alloc_timeline::unmatched != 0)) {
            fail("bytes left after the destructor, or freed bytes that were not allocated");
        }
        if (alloc_timeline::overflow) {
            fail("timeline buffer too small (DSM_README_TIMELINE_CAPACITY)");
        }
        double const total = recorder.seconds();
        double const insert = recorder.seconds_at(inserted_at);
        std::printf("%s %s %s timeline %zu insert_s %.6f destroy_s %.6f total_s %.6f every %zu events %zu recorded %zu "
                    "peak_bytes %zu inserted_bytes %zu\n",
                    variant_name,
                    key_name,
                    map_desc::name,
                    n,
                    insert,
                    total - insert,
                    total,
                    alloc_timeline::available() ? every : 0,
                    alloc_timeline::seen,
                    alloc_timeline::recorded,
                    alloc_timeline::peak,
                    inserted_bytes);
        std::fflush(stdout);
        if (alloc_timeline::available() && csv != nullptr && !recorder.write(csv, bins)) {
            fail("cannot write the timeline CSV");
        }
    }

    /// nanoseconds per operation at size `n`, or bytes per entry for `rss` and `memory`
    double one_size(std::string_view work, std::size_t n) {
        pools p{n};

        if (work == "memory") {
            double const peak = count_alloc::peak_of([&] {
                context ctx{fresh_datastore_path()};
                auto map = ctx.make<map_t>();
                fill(map, p.present);
                if (map.size() != n) {
                    fail("size after fill");
                }
            });
            if (peak < 0.0) {
                fail("memory counting");
            }
            return peak / static_cast<double>(n);
        }

        if (work == "rss") {
            auto const path = fresh_datastore_path();
            std::fflush(stdout);
            double const bytes = max_rss::of(
                    [&](max_rss::probe &probe) {
                        context ctx{path};
                        probe.start();
                        auto map = ctx.make<map_t>();
                        fill(map, p.present);
                        keep(map.size());
                        probe.stop();
                        if (map.size() != n) {
                            fail("size after fill");
                        }
                    },
                    [&](max_rss::probe &probe) {
                        context ctx{path};
                        probe.start();
                        probe.stop();
                    },
                    [&] { remove_datastore(path); });
            if (bytes < 0.0) {
                fail("peak resident set (is /proc/self/clear_refs writable?)");
            }
            return bytes / static_cast<double>(n);
        }

        context ctx{fresh_datastore_path()};

        auto once = [&]() -> double {
            if (work == "build" || work == "buildfree") {
                std::size_t const builds = target_ops / n + 1;
                std::size_t entries = 0;
                clock_type::duration total{};
                if (work == "build") {
                    // the destructor runs after `t1`, outside the timed region
                    for (std::size_t i = 0; i < builds; ++i) {
                        auto const t0 = clock_type::now();
                        auto map = ctx.make<map_t>();
                        fill(map, p.present);
                        auto const t1 = clock_type::now();
                        total += t1 - t0;
                        entries += map.size();
                    }
                } else {
                    auto const t0 = clock_type::now();
                    for (std::size_t i = 0; i < builds; ++i) {
                        auto map = ctx.make<map_t>();
                        fill(map, p.present);
                        entries += map.size();
                    }
                    total = clock_type::now() - t0;
                }
                keep(entries);
                if (entries != builds * n) {
                    fail("size after fill");
                }
                return nanoseconds(total) / static_cast<double>(builds * n);
            }

            // The other panels measure a map that exists, so the fill is not timed.
            auto map = ctx.make<map_t>();
            fill(map, p.present);
            if (map.size() != n) {
                fail("size after fill");
            }

            if (work == "find") {
                rng pick{5};
                rng coin{99};
                std::size_t found = 0;
                std::size_t asked_for_present = 0;
                auto const t0 = clock_type::now();
                for (std::size_t i = 0; i < target_ops; ++i) {
                    std::size_t const at = pick.below(n);
                    bool const present = (coin() & 1U) != 0;
                    asked_for_present += present ? 1 : 0;
                    found += map.count(present ? p.present[at] : p.absent[at]);
                }
                auto const t1 = clock_type::now();
                keep(found);
                if (found != asked_for_present) {
                    fail("find hits");
                }
                return nanoseconds(t1 - t0) / static_cast<double>(target_ops);
            }

            if (work == "churn") {
                // The pools are copied, because churn swaps keys between them.
                auto present = p.present;
                auto spare = p.spare;
                rng pick{7};
                std::size_t next = 0;
                std::size_t erased = 0;
                auto const t0 = clock_type::now();
                for (std::size_t i = 0; i < target_ops; ++i) {
                    std::size_t const at = pick.below(n);
                    erased += map.erase(present[at]);
                    map.try_emplace(spare[next], 1);
                    std::swap(present[at], spare[next]);
                    next = next + 1 == n ? 0 : next + 1;
                }
                auto const t1 = clock_type::now();
                keep(erased);
                if (erased != target_ops || map.size() != n) {
                    fail("churn");
                }
                return nanoseconds(t1 - t0) / static_cast<double>(target_ops);
            }

            // iterate: one operation is one element visited
            std::size_t const sweeps = target_ops / n + 1;
            std::size_t total = 0;
            auto const t0 = clock_type::now();
            for (std::size_t i = 0; i < sweeps; ++i) {
                total += sum(map);
                clobber();
            }
            auto const t1 = clock_type::now();
            keep(total);
            if (total != sweeps * n) {
                fail("iterate sum");
            }
            return nanoseconds(t1 - t0) / static_cast<double>(sweeps * n);
        };

        static_cast<void>(once());  // warmup, not reported
        return once();
    }

}  // namespace

int main(int argc, char **argv) {
    tame_allocator();
    if (argc < 3) {
        std::fprintf(stderr, "usage: %s <build|buildfree|find|churn|iterate|rss|memory|disk> <base>\n", argv[0]);
        std::fprintf(stderr, "       %s timeline <n> [<csv>]\n", argv[0]);
        return 1;
    }
    std::string_view const work{argv[1]};
    current_work = work;
    std::size_t const base = std::strtoull(argv[2], nullptr, 10);
    static constexpr std::array<std::string_view, 9> known{"build", "buildfree", "find", "churn", "iterate", "rss", "memory", "disk", "timeline"};
    bool is_known = false;
    for (auto const &name : known) {
        is_known = is_known || name == work;
    }
    if (!is_known || base < 1000) {
        std::fprintf(stderr, "unknown workload %s or base below 1000\n", argv[1]);
        return 2;
    }
    if (work == "memory" && !count_alloc::available()) {
        std::fprintf(stderr, "this binary was built without DSM_README_COUNT_ALLOC and cannot count bytes\n");
        return 2;
    }
    if (work == "timeline") {
#if defined(DSM_README_METALL)
        std::fprintf(stderr, "timeline counts the heap and runs with std::allocator only\n");
        return 2;
#else
        print_timeline(base, argc > 3 ? argv[3] : nullptr);
        return 0;
#endif
    }
    if (work == "disk") {
#if defined(DSM_README_DISK_USAGE)
        print_disk(base);
        return 0;
#else
        std::fprintf(stderr, "this binary was built without DSM_README_DISK_USAGE and cannot measure the disk\n");
        return 2;
#endif
    }

    double log_sum = 0.0;
    std::printf("%s %s %s %s %zu", variant_name, key_name, map_desc::name, argv[1], base);
    std::fflush(stdout);
    for (std::size_t i = 0; i < 5; ++i) {
        double const value = one_size(work, octave_size(base, i));
        log_sum += std::log(value > 0.0 ? value : 1e-9);
        std::printf(" %.4f", value);
        std::fflush(stdout);
    }
    std::printf(" geomean %.4f\n", std::exp(log_sum / 5.0));
    return 0;
}
