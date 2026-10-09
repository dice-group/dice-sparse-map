#ifndef DICE_SPARSE_MAP_BENCHMARKS_README_MAX_RSS_HPP
#define DICE_SPARSE_MAP_BENCHMARKS_README_MAX_RSS_HPP

/**
 * @file
 * Peak resident set of one piece of work: what the kernel backed, including the slack of the
 * allocator and pages that were freed but stay resident, and without memory that was requested but
 * never touched. Pages of a file mapping (a metall datastore) count as well.
 *
 * The work runs in a forked child. glibc does not give a grown heap back, so a second build in the
 * same process would reuse resident pages. In the child, the state of the parent (the key pools) is
 * part of the baseline and is never charged to the work. The peak (`VmHWM`) is reset through
 * `/proc/self/clear_refs` right before the work starts.
 *
 * Ported from ankerl::unordered_dense (scripts/ab/max_rss.h, MIT license). Here the work marks its
 * own start, so that a metall datastore is created before the baseline is read.
 */

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <utility>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

namespace dice::sparse_map::bench::readme::max_rss {

    /// a field of `/proc/self/status` in KiB, or -1 if it cannot be read
    inline long status_kb(char const *field) {
        auto *file = std::fopen("/proc/self/status", "r");
        if (file == nullptr) {
            return -1;
        }
        char line[256];
        auto const length = std::strlen(field);
        long out = -1;
        while (std::fgets(line, sizeof(line), file) != nullptr) {
            if (std::strncmp(line, field, length) == 0 && line[length] == ':') {
                out = std::strtol(line + length + 1, nullptr, 10);
                break;
            }
        }
        std::fclose(file);
        return out;
    }

    /// sets `VmHWM` back to the current resident set (`CLEAR_REFS_MM_HIWATER_RSS`), false if that fails
    inline bool reset_peak() {
        auto *file = std::fopen("/proc/self/clear_refs", "w");
        if (file == nullptr) {
            return false;
        }
        bool const written = std::fputs("5\n", file) >= 0;
        return std::fclose(file) == 0 && written;
    }

    /**
     * Passed to the work. The work calls `start()` when everything that is not its subject exists,
     * and `stop()` when the subject is complete and before anything is torn down.
     */
    struct probe {
        long before_kb = -1;
        long peak_kb = -1;
        bool reset = false;

        void start() {
            before_kb = status_kb("VmRSS");
            reset = reset_peak();
        }

        void stop() {
            peak_kb = status_kb("VmHWM");
        }
    };

    /**
     * Bytes of peak resident set between `probe.start()` and `probe.stop()` of `work(probe)`, in a
     * forked child. `cleanup()` runs in the parent after the child has exited, also if it crashed.
     * Returns a negative value if the child failed or could not reset the peak.
     */
    template<typename Work, typename Cleanup>
    double gross(Work &&work, Cleanup &&cleanup) {
        int fd[2];
        if (::pipe(fd) != 0) {
            return -1.0;
        }
        pid_t const pid = ::fork();
        if (pid == 0) {
            ::close(fd[0]);
            probe p;
            work(p);
            double value = -1.0;
            if (p.reset && p.before_kb >= 0 && p.peak_kb >= 0) {
                value = static_cast<double>(p.peak_kb - p.before_kb) * 1024.0;
            }
            auto const ignored = ::write(fd[1], &value, sizeof(value));
            static_cast<void>(ignored);
            ::_exit(0);
        }
        ::close(fd[1]);
        double value = -1.0;
        auto const got = ::read(fd[0], &value, sizeof(value));
        ::close(fd[0]);
        int status = 0;
        ::waitpid(pid, &status, 0);
        std::forward<Cleanup>(cleanup)();
        if (got != static_cast<ssize_t>(sizeof(value)) || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            return -1.0;
        }
        return value;
    }

    /**
     * Bytes of peak resident set that `work` is responsible for: `gross(work)` minus `gross(empty)`.
     *
     * A child that does nothing does not read zero. Forking, writing `/proc/self/clear_refs` and
     * reading `/proc/self/status` fault in about 128 KiB of their own. `empty` does the same setup as
     * `work` without the subject, and its result is subtracted. What remains is good for a map of a
     * few megabytes and useless for a map of a few kilobytes.
     */
    template<typename Work, typename Empty, typename Cleanup>
    double of(Work &&work, Empty &&empty, Cleanup &&cleanup) {
        double const loaded = gross(std::forward<Work>(work), cleanup);
        double const floor = gross(std::forward<Empty>(empty), cleanup);
        if (loaded < 0.0 || floor < 0.0) {
            return -1.0;
        }
        return loaded > floor ? loaded - floor : 0.0;
    }

} // namespace dice::sparse_map::bench::readme::max_rss

#endif // DICE_SPARSE_MAP_BENCHMARKS_README_MAX_RSS_HPP
