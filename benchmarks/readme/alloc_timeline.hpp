#ifndef DICE_SPARSE_MAP_BENCHMARKS_README_ALLOC_TIMELINE_HPP
#define DICE_SPARSE_MAP_BENCHMARKS_README_ALLOC_TIMELINE_HPP

/**
 * @file
 * A timeline of the bytes allocated from the heap: one event (time, allocated bytes after the
 * event) per allocation and per free. The replaced `malloc` and its relatives of `count_alloc.hpp`
 * call `grow` and `shrink`, so a binary with `DSM_README_ALLOC_TIMELINE` also needs
 * `DSM_README_COUNT_ALLOC`.
 *
 * A block counts with its usable size (`malloc_usable_size`), without glibc's chunk header. The
 * `memory` panel counts the header too, that is 8 bytes more per live block. A block that an
 * allocator maps itself (`mmap`) counts with its length.
 *
 * The events go to a buffer that is mapped with `mmap` and faulted in before the recording starts,
 * so the recording does not allocate from the heap it counts. The time of an event is the time
 * stamp counter of the CPU (`rdtsc` on x86-64, `cntvct_el0` on AArch64), converted to seconds with
 * `std::chrono::steady_clock` at the start and the end of the recording. The buffer is written
 * out after the recording, reduced to the first, the last, the smallest and the largest value of
 * every time bin, so the peak is exact.
 *
 * Without `DSM_README_ALLOC_TIMELINE`, `recorder` only measures the time.
 */

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>

#if defined(DSM_README_ALLOC_TIMELINE)
#include <sys/mman.h>
#if defined(__x86_64__)
#include <x86intrin.h>
#endif
#endif

namespace dice::sparse_map::bench::readme::alloc_timeline {

    /// true if this binary records the allocated bytes
    inline constexpr bool available() noexcept {
#if defined(DSM_README_ALLOC_TIMELINE)
        return true;
#else
        return false;
#endif
    }

    struct event {
        std::uint64_t ticks; ///< the time stamp counter
        std::uint64_t bytes; ///< the allocated bytes after the event
    };

    using clock_type = std::chrono::steady_clock;

    /// the time stamp counter of the CPU, or the nanoseconds of `steady_clock` on other CPUs
    inline std::uint64_t ticks() noexcept {
#if defined(DSM_README_ALLOC_TIMELINE) && defined(__x86_64__)
        return __rdtsc();
#elif defined(DSM_README_ALLOC_TIMELINE) && defined(__aarch64__)
        std::uint64_t value = 0;
        asm volatile("mrs %0, cntvct_el0" : "=r"(value));
        return value;
#else
        return static_cast<std::uint64_t>(
            std::chrono::duration_cast<std::chrono::nanoseconds>(clock_type::now().time_since_epoch()).count()
        );
#endif
    }

    // The state of the recording. The replaced `malloc` reaches it from everywhere, so it is global.
    inline bool recording = false;
    inline event *events = nullptr;
    inline std::size_t capacity = 0; ///< events that fit into `events`
    inline std::size_t recorded = 0; ///< events in `events`
    inline std::size_t seen = 0; ///< events, also those that were not recorded
    inline std::size_t every = 1; ///< only every `every`-th event is recorded
    inline std::size_t countdown = 1; ///< events until the next recorded one
    inline std::size_t live = 0; ///< allocated bytes
    inline std::size_t peak = 0; ///< largest value of `live`
    inline std::size_t unmatched = 0; ///< bytes freed that were allocated before the recording
    inline bool overflow = false; ///< true if an event did not fit into `events`

    inline void record() noexcept {
        ++seen;
        if (--countdown != 0) {
            return;
        }
        countdown = every;
        if (recorded == capacity) {
            overflow = true;
            return;
        }
        events[recorded] = event{ticks(), live};
        ++recorded;
    }

    /// counts an allocation of `bytes`, called by the replaced `malloc` and its relatives
    inline void grow(std::size_t bytes) noexcept {
        if (!recording || bytes == 0) {
            return;
        }
        live += bytes;
        if (live > peak) {
            peak = live;
        }
        record();
    }

    /// counts a free of `bytes`
    inline void shrink(std::size_t bytes) noexcept {
        if (!recording || bytes == 0) {
            return;
        }
        if (bytes > live) {
            unmatched += bytes - live;
            bytes = live;
        }
        live -= bytes;
        record();
    }

    /**
     * Records the allocated bytes between `start` and `stop`. The buffer holds `capacity` events
     * and is mapped and faulted in by the constructor. Only one recorder may exist at a time.
     */
    struct recorder {
    private:
        clock_type::time_point steady_start_{};
        clock_type::time_point steady_stop_{};
        std::uint64_t ticks_start_ = 0;
        std::uint64_t ticks_stop_ = 0;

    public:
        explicit recorder(std::size_t capacity_events, std::size_t record_every) {
#if defined(DSM_README_ALLOC_TIMELINE)
            std::size_t const length = capacity_events * sizeof(event);
            void
                *p = ::mmap(nullptr, length, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE, -1, 0);
            if (p == MAP_FAILED) {
                std::fprintf(stderr, "alloc_timeline: cannot map %zu bytes\n", length);
                std::exit(4);
            }
            events = static_cast<event *>(p);
            capacity = capacity_events;
            every = record_every > 0 ? record_every : 1;
#else
            static_cast<void>(capacity_events);
            static_cast<void>(record_every);
#endif
        }

        recorder(recorder const &) = delete;
        recorder(recorder &&) = delete;
        recorder &operator=(recorder const &) = delete;
        recorder &operator=(recorder &&) = delete;

        ~recorder() {
#if defined(DSM_README_ALLOC_TIMELINE)
            recording = false;
            ::munmap(events, capacity * sizeof(event));
            events = nullptr;
            capacity = 0;
#endif
        }

        void start() noexcept {
            recorded = 0;
            seen = 0;
            countdown = every;
            live = 0;
            peak = 0;
            unmatched = 0;
            overflow = false;
            steady_start_ = clock_type::now();
            ticks_start_ = ticks();
            recording = available();
        }

        void stop() noexcept {
            recording = false;
            ticks_stop_ = ticks();
            steady_stop_ = clock_type::now();
        }

        /// seconds of `steady_clock` from `start` to `stop`
        [[nodiscard]] double seconds() const noexcept {
            return std::chrono::duration<double>(steady_stop_ - steady_start_).count();
        }

        /// seconds from `start` to the time stamp `t`
        [[nodiscard]] double seconds_at(std::uint64_t t) const noexcept {
            if (ticks_stop_ == ticks_start_) {
                return 0.0;
            }
            return static_cast<double>(t - ticks_start_) / static_cast<double>(ticks_stop_ - ticks_start_) * seconds();
        }

        /**
         * Writes the timeline as CSV (`seconds,bytes`) to `path`: a point at 0, then per time bin the
         * first, the last, the smallest and the largest event in the order of time, then a point at
         * the stop. Returns false if the file cannot be written.
         */
        [[nodiscard]] bool write(char const *path, std::size_t bins) const {
            std::FILE *out = std::fopen(path, "w");
            if (out == nullptr) {
                return false;
            }
            std::fprintf(out, "seconds,bytes\n0,0\n");
            double const span = static_cast<double>(ticks_stop_ - ticks_start_);
            auto bin_of = [&](std::size_t i) {
                double const at = static_cast<double>(events[i].ticks - ticks_start_)
                    / span
                    * static_cast<double>(bins);
                return std::min(static_cast<std::size_t>(at), bins - 1);
            };
            std::size_t i = 0;
            while (i < recorded) {
                std::size_t const bin = bin_of(i);
                std::size_t lowest = i;
                std::size_t highest = i;
                std::size_t j = i;
                while (j < recorded && bin_of(j) == bin) {
                    if (events[j].bytes < events[lowest].bytes) {
                        lowest = j;
                    }
                    if (events[j].bytes > events[highest].bytes) {
                        highest = j;
                    }
                    ++j;
                }
                std::size_t keep[4] = {i, lowest, highest, j - 1};
                std::sort(keep, keep + 4);
                for (std::size_t k = 0; k < 4; ++k) {
                    if (k > 0 && keep[k] == keep[k - 1]) {
                        continue;
                    }
                    event const &e = events[keep[k]];
                    std::fprintf(out, "%.9f,%llu\n", seconds_at(e.ticks), static_cast<unsigned long long>(e.bytes));
                }
                i = j;
            }
            std::fprintf(out, "%.9f,%llu\n", seconds(), static_cast<unsigned long long>(live));
            return std::fclose(out) == 0;
        }
    };

} // namespace dice::sparse_map::bench::readme::alloc_timeline

#endif // DICE_SPARSE_MAP_BENCHMARKS_README_ALLOC_TIMELINE_HPP
