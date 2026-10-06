/*
 * Checks which maps of the README benchmark are relocatable with metall's allocator: a map is
 * built in a metall datastore, the datastore is closed and opened again at another address, and
 * the map must still find every key and accept new ones.
 *
 *   dsm_readme_relocate create <dir>   builds one datastore per map in <dir>
 *   dsm_readme_relocate check <dir>    opens every datastore again and checks its maps
 *
 * The two steps are two processes, so the program and the heap are at other addresses as well
 * (address space layout randomization). Before a datastore is opened again, the address range of
 * its first mapping is blocked, so that metall maps it at another address. Each map is checked in
 * its own child process, so a map that crashes does not stop the check of the others.
 *
 * Every map is checked twice: filled with `num_keys` keys, and empty. Keys are `std::uint64_t`. A
 * `std::string` key keeps raw pointers (to its own buffer or to the heap), so no map with
 * `std::string` keys is relocatable.
 */

#include "maps.hpp"

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <tuple>

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef DSM_README_METALL
#error "relocate.cpp needs DSM_README_METALL"
#endif

namespace {

    using namespace dice::sparse_map::bench::readme;

    constexpr std::uint64_t num_keys = 200000;

    /// an object in the data segment of the program, whose address shows where the program is mapped
    int program_marker = 0;

    std::uint64_t key_of(std::uint64_t i) noexcept {
        return i * 0x9E3779B97F4A7C15ULL + 12345;
    }

    std::uint64_t value_of(std::uint64_t key) noexcept {
        return (key >> 7U) + 1;
    }

    /**
     * Keeps an address range mapped without access rights while it is in scope, so that a datastore
     * that was mapped there is mapped at another address.
     */
    struct address_blocker {
    private:
        void *block_ = nullptr;
        std::size_t size_;

    public:
        address_blocker(void *address, std::size_t size) noexcept
            : size_(size) {
            void *const mapped = ::mmap(address, size_, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE | MAP_FIXED_NOREPLACE, -1, 0);
            if (mapped == address) {
                block_ = mapped;
            } else if (mapped != MAP_FAILED) {
                ::munmap(mapped, size_);
            }
        }

        address_blocker(address_blocker const &) = delete;
        address_blocker(address_blocker &&) = delete;
        address_blocker &operator=(address_blocker const &) = delete;
        address_blocker &operator=(address_blocker &&) = delete;

        ~address_blocker() {
            if (block_ != nullptr) {
                ::munmap(block_, size_);
            }
        }

        [[nodiscard]] bool blocks() const noexcept {
            return block_ != nullptr;
        }
    };

    template<typename Desc>
    using map_of = typename Desc::template type<std::uint64_t>;

    template<typename Desc>
    void create(std::filesystem::path const &dir) {
        auto const path = dir / Desc::name;
        static_cast<void>(metall::manager::remove(path.c_str()));
        metall::manager manager{metall::create_only, path.c_str()};
        using map_t = map_of<Desc>;
        auto *filled = manager.construct<map_t>("filled")(typename map_t::allocator_type(manager.get_allocator()));
        for (std::uint64_t i = 0; i < num_keys; ++i) {
            auto const key = key_of(i);
            filled->try_emplace(key, value_of(key));
        }
        static_cast<void>(manager.construct<map_t>("empty")(typename map_t::allocator_type(manager.get_allocator())));
        std::ofstream{dir / (std::string{Desc::name} + ".address")} << reinterpret_cast<std::uintptr_t>(manager.get_address()) << ' '
                                                                    << manager.get_size() << '\n';
        std::printf("created %-14s at %p, program at %p\n", Desc::name, manager.get_address(), static_cast<void *>(&program_marker));
    }

    /**
     * Opens the datastore of `Desc` at another address and checks its maps. Returns an empty
     * string if both maps are right, or what is wrong.
     */
    template<typename Desc>
    std::string check_in_this_process(std::filesystem::path const &dir) {
        auto const path = dir / Desc::name;
        std::uintptr_t old_address = 0;
        std::size_t old_size = 0;
        std::ifstream{dir / (std::string{Desc::name} + ".address")} >> old_address >> old_size;
        if (old_address == 0 || old_size == 0) {
            return "no address of the first mapping";
        }
        address_blocker const blocker{reinterpret_cast<void *>(old_address), old_size};
        if (!blocker.blocks()) {
            return "could not block the first mapping";
        }
        metall::manager manager{metall::open_only, path.c_str()};
        if (!manager.check_sanity()) {
            return "datastore does not open";
        }
        std::printf("  opened %-14s at %p (was %p), program at %p\n", Desc::name, manager.get_address(), reinterpret_cast<void *>(old_address), static_cast<void *>(&program_marker));
        if (reinterpret_cast<std::uintptr_t>(manager.get_address()) == old_address) {
            return "datastore mapped at the same address";
        }
        std::fflush(stdout);

        using map_t = map_of<Desc>;
        auto *filled = std::get<0>(manager.find<map_t>("filled"));
        auto *empty = std::get<0>(manager.find<map_t>("empty"));
        if (filled == nullptr || empty == nullptr) {
            return "maps not found";
        }
        if (filled->size() != num_keys) {
            return "filled map: wrong size";
        }
        for (std::uint64_t i = 0; i < num_keys; ++i) {
            auto const key = key_of(i);
            auto const it = filled->find(key);
            if (it == filled->end() || it->second != value_of(key)) {
                return "filled map: a key is missing or has a wrong value";
            }
        }
        if (filled->contains(key_of(num_keys))) {
            return "filled map: finds a key that was never inserted";
        }
        std::size_t sum = 0;
        for (auto const &element : *filled) {
            sum += element.second == value_of(element.first) ? 1 : 0;
        }
        if (sum != num_keys) {
            return "filled map: iteration";
        }
        for (std::uint64_t i = num_keys; i < num_keys + num_keys / 2; ++i) {
            auto const key = key_of(i);
            filled->try_emplace(key, value_of(key));
        }
        for (std::uint64_t i = 0; i < num_keys + num_keys / 2; i += 7) {
            auto const key = key_of(i);
            auto const it = filled->find(key);
            if (it == filled->end() || it->second != value_of(key)) {
                return "filled map: wrong after inserts";
            }
        }

        if (!empty->empty() || empty->contains(key_of(0)) || empty->begin() != empty->end()) {
            return "empty map: not empty";
        }
        for (std::uint64_t i = 0; i < 1000; ++i) {
            auto const key = key_of(i);
            empty->try_emplace(key, value_of(key));
        }
        for (std::uint64_t i = 0; i < 1000; ++i) {
            auto const key = key_of(i);
            auto const it = empty->find(key);
            if (it == empty->end() || it->second != value_of(key)) {
                return "empty map: wrong after inserts";
            }
        }
        return {};
    }

    /// runs the check of `Desc` in a child process and prints the result
    template<typename Desc>
    bool check(std::filesystem::path const &dir) {
        std::fflush(stdout);
        pid_t const pid = ::fork();
        if (pid == 0) {
            auto const problem = check_in_this_process<Desc>(dir);
            if (!problem.empty()) {
                std::printf("  %s\n", problem.c_str());
                std::fflush(stdout);
                std::_Exit(1);
            }
            std::fflush(stdout);
            std::_Exit(0);
        }
        int status = 0;
        ::waitpid(pid, &status, 0);
        bool const ok = WIFEXITED(status) && WEXITSTATUS(status) == 0;
        if (ok) {
            std::printf("relocatable %s: yes\n", Desc::name);
        } else if (WIFSIGNALED(status)) {
            std::printf("relocatable %s: no (crashed with signal %d)\n", Desc::name, WTERMSIG(status));
        } else {
            std::printf("relocatable %s: no (wrong results)\n", Desc::name);
        }
        std::fflush(stdout);
        return ok;
    }

    template<typename... Descs>
    struct maps {
        static void create_all(std::filesystem::path const &dir) {
            (create<Descs>(dir), ...);
        }

        static void check_all(std::filesystem::path const &dir) {
            (static_cast<void>(check<Descs>(dir)), ...);
        }
    };

    using metall_maps = maps<sparse_medium, sparse_high, sparse_low, unordered_dense, boost_flat, std_unordered_map>;

}  // namespace

int main(int argc, char **argv) {
    if (argc < 3) {
        std::fprintf(stderr, "usage: %s <create|check> <dir>\n", argv[0]);
        return 1;
    }
    std::string_view const mode{argv[1]};
    std::filesystem::path const dir{argv[2]};
    if (mode == "create") {
        std::filesystem::create_directories(dir);
        metall_maps::create_all(dir);
        return 0;
    }
    if (mode == "check") {
        metall_maps::check_all(dir);
        return 0;
    }
    std::fprintf(stderr, "unknown mode %s\n", argv[1]);
    return 1;
}
