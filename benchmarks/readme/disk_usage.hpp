#ifndef DICE_SPARSE_MAP_BENCHMARKS_README_DISK_USAGE_HPP
#define DICE_SPARSE_MAP_BENCHMARKS_README_DISK_USAGE_HPP

/**
 * @file
 * Disk usage of a metall datastore: the bytes of the blocks that its files occupy
 * (`st_blocks * 512`), summed over every file and directory below the datastore, as `du` counts
 * them. metall creates its segment files with `ftruncate`, so a page that was never written costs
 * no block. When a chunk of 2 MiB becomes free, metall punches a hole into the file
 * (`madvise(MADV_REMOVE)`), and the blocks of the hole are free again. The directory is scanned
 * again for every sample, because metall adds a segment file for every 256 MiB.
 *
 * A block is only freed by a hole punch. So between two hole punches the disk usage can only grow,
 * and its peak is right before a hole punch or at the end. The binaries built with
 * `DSM_README_DISK_USAGE` replace `madvise` and take a sample right before every `MADV_REMOVE`
 * while a sampler is active. The other samples (periodic and after a growth of the map) do not
 * change the peak, and they show what sampling without the hook would see.
 *
 * A sample costs one `lstat` per file and directory of the datastore: seven for a datastore with one
 * segment file, one more for every further segment file.
 *
 * The file system allocates blocks for a whole page cache folio when a page of it is written
 * through the mapping, and a folio can be larger than a page. On xfs with Linux 6.8, one written
 * byte allocated 4 KiB to 64 KiB, depending on the access pattern. So the disk usage can be a
 * little larger than the pages that were written.
 *
 * Only the `disk` binaries define `DSM_README_DISK_USAGE`. The timed binaries do not have the
 * replaced `madvise`.
 */

#include <array>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <system_error>
#include <utility>

#include <sys/stat.h>

#if defined(DSM_README_DISK_USAGE)
#include <sys/mman.h>
#include <sys/syscall.h>
#include <unistd.h>
#endif

namespace dice::sparse_map::bench::readme::disk_usage {

    /**
     * Bytes of the blocks of `path` and, for a directory, of everything below it. If `entries` is
     * not null, it is set to the number of files and directories that were counted.
     */
    inline std::uint64_t allocated_bytes(std::filesystem::path const &path, std::size_t *entries = nullptr) {
        std::uint64_t total = 0;
        std::size_t counted = 0;
        struct stat status{};
        if (::lstat(path.c_str(), &status) == 0) {
            total += static_cast<std::uint64_t>(status.st_blocks) * 512U;
            ++counted;
        }
        std::error_code error;
        std::filesystem::recursive_directory_iterator it{path, error};
        std::filesystem::recursive_directory_iterator const end;
        while (!error && it != end) {
            if (::lstat(it->path().c_str(), &status) == 0) {
                total += static_cast<std::uint64_t>(status.st_blocks) * 512U;
                ++counted;
            }
            it.increment(error);
        }
        if (entries != nullptr) {
            *entries = counted;
        }
        return total;
    }

    /// why a sample was taken
    enum struct trigger : std::size_t {
        periodic = 0, ///< every 1/256 of the inserts
        growth = 1, ///< right after an insert that changed `bucket_count()`
        punch = 2, ///< right before metall punches a hole (`MADV_REMOVE`)
    };

    inline constexpr std::size_t trigger_count = 3;

    /// samples the disk usage of one directory and keeps the largest sample
    struct sampler {
        using clock_type = std::chrono::steady_clock;

        std::filesystem::path dir;
        std::array<std::size_t, trigger_count> samples{};
        std::array<std::uint64_t, trigger_count> largest{};
        std::uint64_t peak = 0;
        std::size_t punches = 0;
        std::uint64_t punched_bytes = 0;
        clock_type::duration spent{};

        explicit sampler(std::filesystem::path path) : dir(std::move(path)) {}

        /// the disk usage now, also counted as a sample of kind `why`
        std::uint64_t sample(trigger why) {
            auto const t0 = clock_type::now();
            std::uint64_t const bytes = allocated_bytes(dir);
            spent += clock_type::now() - t0;
            auto const i = static_cast<std::size_t>(why);
            ++samples[i];
            largest[i] = bytes > largest[i] ? bytes : largest[i];
            peak = bytes > peak ? bytes : peak;
            return bytes;
        }

        /// the disk usage now, counted towards the peak but not as a sample of any kind
        std::uint64_t now() {
            std::uint64_t const bytes = allocated_bytes(dir);
            peak = bytes > peak ? bytes : peak;
            return bytes;
        }

        [[nodiscard]] std::size_t total_samples() const noexcept {
            std::size_t total = 0;
            for (auto const count : samples) {
                total += count;
            }
            return total;
        }
    };

    /// the sampler that the replaced `madvise` calls right before a hole punch, or null
    inline sampler *punch_sampler = nullptr;

    /// true if this binary samples right before every hole punch
    inline constexpr bool available() noexcept {
#if defined(DSM_README_DISK_USAGE)
        return true;
#else
        return false;
#endif
    }

} // namespace dice::sparse_map::bench::readme::disk_usage

#if defined(DSM_README_DISK_USAGE)
extern "C" {

    // metall frees the file space of a free chunk with `madvise(MADV_REMOVE)`. This takes a sample
    // before the hole is punched and then makes the system call itself.
    int madvise(void *addr, std::size_t length, int advice) noexcept {
        namespace du = dice::sparse_map::bench::readme::disk_usage;
        if (advice == MADV_REMOVE && du::punch_sampler != nullptr) {
            du::punch_sampler->sample(du::trigger::punch);
            ++du::punch_sampler->punches;
            du::punch_sampler->punched_bytes += length;
        }
        return static_cast<int>(::syscall(SYS_madvise, addr, length, advice));
    }

} // extern "C"
#endif // DSM_README_DISK_USAGE

#endif // DICE_SPARSE_MAP_BENCHMARKS_README_DISK_USAGE_HPP
