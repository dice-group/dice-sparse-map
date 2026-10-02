#ifndef DICE_SPARSE_MAP_BENCHMARKS_README_COUNT_ALLOC_HPP
#define DICE_SPARSE_MAP_BENCHMARKS_README_COUNT_ALLOC_HPP

/**
 * @file
 * Bytes a map requests from the heap, counted in `malloc`, `calloc`, `realloc`, `free`,
 * `aligned_alloc`, `posix_memalign`, `mmap` and `munmap`. This is the companion of `max_rss.hpp`,
 * which counts what the kernel backed. The two answer different questions: a block that a doubling
 * map freed is no longer counted here, but it can stay resident in glibc's heap.
 *
 * A block is charged what glibc really gives: `malloc_usable_size` plus the 8 byte chunk header.
 *
 * Interposing `malloc` costs time per allocation, so it costs a node map more than a flat map. Only
 * the `memory` binaries define `DSM_README_COUNT_ALLOC`. The timed binaries do not have any of this
 * compiled in. Memory from a metall datastore does not go through `malloc` and is not counted.
 *
 * Ported from ankerl::unordered_dense (scripts/ab/count_alloc.h, MIT license).
 */

#include <cstddef>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#if defined(DSM_README_COUNT_ALLOC)
#include <dlfcn.h>
#include <malloc.h>
#include <sys/mman.h>

namespace dice::sparse_map::bench::readme::count_alloc {

    inline std::size_t live_bytes = 0;
    inline std::size_t peak_bytes = 0;
    inline bool counting = false;

    /// what the block at `p` costs: its usable size plus the chunk header
    inline std::size_t charged(void *p) {
        return p == nullptr ? 0 : malloc_usable_size(p) + sizeof(std::size_t);
    }

    inline void add(std::size_t bytes) {
        if (counting) {
            live_bytes += bytes;
            if (live_bytes > peak_bytes) {
                peak_bytes = live_bytes;
            }
        }
    }

    inline void remove(std::size_t bytes) {
        if (counting) {
            live_bytes -= bytes < live_bytes ? bytes : live_bytes;
        }
    }

}  // namespace dice::sparse_map::bench::readme::count_alloc

extern "C" {

// glibc's own functions, so that the functions below do not call themselves
void *__libc_malloc(std::size_t n);
void *__libc_calloc(std::size_t count, std::size_t size);
void *__libc_realloc(void *p, std::size_t n);
void __libc_free(void *p);

void *malloc(std::size_t n) noexcept {
    namespace ca = dice::sparse_map::bench::readme::count_alloc;
    void *p = __libc_malloc(n);
    ca::add(ca::charged(p));
    return p;
}

void *calloc(std::size_t count, std::size_t size) noexcept {
    namespace ca = dice::sparse_map::bench::readme::count_alloc;
    void *p = __libc_calloc(count, size);
    ca::add(ca::charged(p));
    return p;
}

void free(void *p) noexcept {
    namespace ca = dice::sparse_map::bench::readme::count_alloc;
    ca::remove(ca::charged(p));
    __libc_free(p);
}

void *realloc(void *p, std::size_t n) noexcept {
    namespace ca = dice::sparse_map::bench::readme::count_alloc;
    ca::remove(ca::charged(p));
    void *fresh = __libc_realloc(p, n);
    ca::add(ca::charged(fresh));
    return fresh;
}

// over-aligned blocks, for example from an over-aligned `operator new`
void *aligned_alloc(std::size_t alignment, std::size_t n) noexcept {
    namespace ca = dice::sparse_map::bench::readme::count_alloc;
    static auto *real = reinterpret_cast<void *(*) (std::size_t, std::size_t)>(dlsym(RTLD_NEXT, "aligned_alloc"));
    void *p = real(alignment, n);
    ca::add(ca::charged(p));
    return p;
}

int posix_memalign(void **out, std::size_t alignment, std::size_t n) noexcept {
    namespace ca = dice::sparse_map::bench::readme::count_alloc;
    static auto *real = reinterpret_cast<int (*)(void **, std::size_t, std::size_t)>(dlsym(RTLD_NEXT, "posix_memalign"));
    int const rc = real(out, alignment, n);
    if (rc == 0) {
        ca::add(ca::charged(*out));
    }
    return rc;
}

// blocks that an allocator maps itself and that never touch the heap
void *mmap(void *addr, std::size_t length, int prot, int flags, int fd, off_t offset) noexcept {
    namespace ca = dice::sparse_map::bench::readme::count_alloc;
    static auto *real = reinterpret_cast<void *(*) (void *, std::size_t, int, int, int, off_t)>(dlsym(RTLD_NEXT, "mmap"));
    void *p = real(addr, length, prot, flags, fd, offset);
    if (p != MAP_FAILED) {
        ca::add(length);
    }
    return p;
}

int munmap(void *addr, std::size_t length) noexcept {
    namespace ca = dice::sparse_map::bench::readme::count_alloc;
    static auto *real = reinterpret_cast<int (*)(void *, std::size_t)>(dlsym(RTLD_NEXT, "munmap"));
    ca::remove(length);
    return real(addr, length);
}

}  // extern "C"
#endif  // DSM_README_COUNT_ALLOC

namespace dice::sparse_map::bench::readme::count_alloc {

    /// true if this binary counts allocations
    inline constexpr bool available() noexcept {
#if defined(DSM_README_COUNT_ALLOC)
        return true;
#else
        return false;
#endif
    }

    /**
     * Peak bytes that `work` requested, counted in a forked child, or a negative value if the child
     * failed. The fork keeps the heap that the work grows out of the parent, so that a later
     * measurement does not reuse it.
     */
    template<typename Work>
    double peak_of(Work &&work) {
#if defined(DSM_README_COUNT_ALLOC)
        int fd[2];
        if (::pipe(fd) != 0) {
            return -1.0;
        }
        pid_t const pid = ::fork();
        if (pid == 0) {
            ::close(fd[0]);
            live_bytes = 0;
            peak_bytes = 0;
            counting = true;
            work();
            counting = false;
            double const value = static_cast<double>(peak_bytes);
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
        if (got != static_cast<ssize_t>(sizeof(value)) || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            return -1.0;
        }
        return value;
#else
        static_cast<void>(work);
        return -1.0;
#endif
    }

}  // namespace dice::sparse_map::bench::readme::count_alloc

#endif  // DICE_SPARSE_MAP_BENCHMARKS_README_COUNT_ALLOC_HPP
