#include "common.hpp"
#include "map_benchmarks.hpp"

#include "fixtures/offset_ptr_allocator.hpp"

#include <doctest/doctest.h>

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

#include <cstdlib>
#include <filesystem>
#include <optional>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>

using namespace dice::unordered_sparse::bench;
using dice::unordered_sparse::sparsity;

/**
 * The `construct` of metall's allocator is placement new, so the maps copy trivially copyable elements as bytes (see
 * `allocator_constructs_in_place`).
 */
template<typename T, typename Kernel>
struct dice::unordered_sparse::allocator_constructs_in_place<metall::stl_allocator<T, Kernel>> : std::true_type {};

/*
 * The map benchmarks with fancy pointers. With the metall allocator, the maps live in a metall
 * datastore. With `offset_ptr_allocator` (from the tests), the maps use
 * `boost::interprocess::offset_ptr` but the memory comes from the heap, which separates the cost of
 * the fancy pointer from the cost of the metall allocator. The keys of the `std::string` maps keep
 * their characters on the heap in both cases.
 *
 * There is no metall baseline with `ankerl::unordered_dense::map`. Version 5.2.0 does not compile
 * with metall's allocator: its `operator[]` calls `operator->` on the iterator of its value vector,
 * and with libstdc++ that iterator cannot return an `offset_ptr` as a raw pointer.
 */

namespace {

    /**
     * The directory for the metall datastores: the environment variable
     * `DICE_UNORDERED_SPARSE_BENCH_METALL_DIR`, or `/tmp/dice-unordered-sparse-bench-metall`.
     */
    [[nodiscard]] std::filesystem::path metall_dir() {
        char const *dir = std::getenv("DICE_UNORDERED_SPARSE_BENCH_METALL_DIR");
        if (dir != nullptr && *dir != '\0') {
            return dir;
        }
        return "/tmp/dice-unordered-sparse-bench-metall";
    }

    /**
     * Removes the metall datastore at `path`. metall keeps its lock file, so the rest of the
     * directory goes too, but only if metall removed the datastore: it refuses to remove a
     * datastore that is open elsewhere.
     */
    void remove_datastore(std::filesystem::path const &path) {
        if (metall::manager::remove(path)) {
            std::error_code error;
            std::filesystem::remove_all(path, error);
        }
    }

    /**
     * A fresh metall datastore in the subdirectory `datastore` of `metall_dir()`, removed again at
     * the end of its scope. Only the subdirectory is removed, never anything else in `metall_dir()`.
     */
    struct metall_datastore {
        std::filesystem::path path = metall_dir() / "datastore";
        std::optional<metall::manager> manager;

        metall_datastore() {
            remove_datastore(path);
            std::filesystem::create_directories(path.parent_path());
            manager.emplace(metall::create_only, path);
        }

        metall_datastore(metall_datastore const &) = delete;
        metall_datastore(metall_datastore &&) = delete;
        metall_datastore &operator=(metall_datastore const &) = delete;
        metall_datastore &operator=(metall_datastore &&) = delete;

        ~metall_datastore() {
            manager.reset();
            remove_datastore(path);
        }
    };

    /**
     * Makes every map with an allocator of a metall manager.
     */
    struct metall_source {
        metall::manager *manager;

        template<typename Map>
        [[nodiscard]] Map make() const {
            return Map(manager->get_allocator<typename Map::allocator_type::value_type>());
        }
    };

    /**
     * Calls `f` with a `metall_source` of a fresh metall datastore.
     */
    template<typename F>
    void with_datastore(F &&f) {
        metall_datastore datastore;
        REQUIRE(datastore.manager.has_value());
        REQUIRE(datastore.manager->check_sanity());
        std::forward<F>(f)(metall_source{&*datastore.manager});
    }

    template<sparsity Sparsity>
    using metall_sparse = sparse_family<Sparsity, metall::manager::allocator_type>;

    template<sparsity Sparsity>
    using offset_ptr_sparse = sparse_family<Sparsity, dice::unordered_sparse::tests::offset_ptr_allocator>;

    template<typename Family>
    void quick_overall_metall(std::string_view container) {
        with_datastore([&](metall_source const &source) {
            quick_overall<Family>(container, source);
        });
    }

    template<typename Family>
    void find_all_metall(std::string_view container) {
        with_datastore([&](metall_source const &source) {
            find_all_hits_or_misses<Family>(container, source);
        });
    }

}  // namespace

TEST_CASE("quick_overall metall sparse_map high") {
    quick_overall_metall<metall_sparse<sparsity::high>>("metall sparse_map high");
}

TEST_CASE("quick_overall metall sparse_map medium") {
    quick_overall_metall<metall_sparse<sparsity::medium>>("metall sparse_map medium");
}

TEST_CASE("quick_overall metall sparse_map low") {
    quick_overall_metall<metall_sparse<sparsity::low>>("metall sparse_map low");
}

TEST_CASE("quick_overall offset_ptr sparse_map high") {
    quick_overall<offset_ptr_sparse<sparsity::high>>("offset_ptr sparse_map high");
}

TEST_CASE("quick_overall offset_ptr sparse_map medium") {
    quick_overall<offset_ptr_sparse<sparsity::medium>>("offset_ptr sparse_map medium");
}

TEST_CASE("quick_overall offset_ptr sparse_map low") {
    quick_overall<offset_ptr_sparse<sparsity::low>>("offset_ptr sparse_map low");
}

TEST_CASE("find_all metall sparse_map high") {
    find_all_metall<metall_sparse<sparsity::high>>("metall sparse_map high");
}

TEST_CASE("find_all metall sparse_map medium") {
    find_all_metall<metall_sparse<sparsity::medium>>("metall sparse_map medium");
}

TEST_CASE("find_all metall sparse_map low") {
    find_all_metall<metall_sparse<sparsity::low>>("metall sparse_map low");
}

TEST_CASE("find_all offset_ptr sparse_map high") {
    find_all_hits_or_misses<offset_ptr_sparse<sparsity::high>>("offset_ptr sparse_map high");
}

TEST_CASE("find_all offset_ptr sparse_map medium") {
    find_all_hits_or_misses<offset_ptr_sparse<sparsity::medium>>("offset_ptr sparse_map medium");
}

TEST_CASE("find_all offset_ptr sparse_map low") {
    find_all_hits_or_misses<offset_ptr_sparse<sparsity::low>>("offset_ptr sparse_map low");
}

TEST_CASE("iterate metall sparse_map medium") {
    with_datastore([](metall_source const &source) {
        iterate_large<metall_sparse<sparsity::medium>>("metall sparse_map medium", source);
    });
}

TEST_CASE("throwing_move metall sparse_map medium") {
    with_datastore([](metall_source const &source) {
        throwing_move<metall_sparse<sparsity::medium>>("metall sparse_map medium", source);
    });
}
