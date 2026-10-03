#include "fixtures/allocators.hpp"

#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>

// gcc 14 reports -Wmaybe-uninitialized inside metall (`segment_storage::priv_deallocate_segment_header`)
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
#include <metall/metall.hpp>
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif

#include <sys/mman.h>

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <memory>
#include <memory_resource>
#include <random>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>

/**
 * `allocator_constructs_in_place`: an allocator with its own `construct` and `destroy`, which only construct and
 * destroy in place, opts in. A group then copies and moves trivially copyable values as bytes, without calls of
 * `construct` and `destroy`. `construct_counting_allocator` counts these calls. A metall map with the opt-in runs
 * random operations before and after its datastore is closed and opened at another address.
 */

template<typename T>
struct dice::unordered_sparse::allocator_constructs_in_place<dice::unordered_sparse::tests::construct_counting_allocator<T>> : std::true_type {};

template<typename T, typename Kernel>
struct dice::unordered_sparse::allocator_constructs_in_place<metall::stl_allocator<T, Kernel>> : std::true_type {};

namespace {
    using namespace dice::unordered_sparse;
    using namespace dice::unordered_sparse::tests;

    using slot_u64 = detail::map_slot<std::uint64_t, std::uint64_t>;

    template<typename Allocator, typename Slot = slot_u64>
    using group_of = detail::sparse_array<Slot, Allocator, sparsity::medium>;

    using group_t = group_of<construct_counting_allocator<slot_u64>>;

    // an allocator with `construct` and `destroy` that opts in
    static_assert(group_t::copies_bytes && group_t::shifts_bytes);
    static_assert(group_of<metall::manager::allocator_type<slot_u64>>::copies_bytes);
    static_assert(group_of<metall::manager::allocator_type<slot_u64>>::shifts_bytes);
    // the opt-in does not make a value trivially copyable
    using slot_string = detail::map_slot<std::uint64_t, std::string>;
    static_assert(!group_of<construct_counting_allocator<slot_string>, slot_string>::copies_bytes);
    // an allocator with `construct` and `destroy` that does not opt in
    static_assert(!group_of<std::pmr::polymorphic_allocator<slot_u64>>::copies_bytes);
    // an allocator without `construct` and `destroy` copies bytes and shifts by assignment
    static_assert(group_of<std::allocator<slot_u64>>::copies_bytes && !group_of<std::allocator<slot_u64>>::shifts_bytes);

    using pair_u64 = std::pair<std::uint64_t, std::uint64_t>;

    /// runs `operation` and returns the calls of `construct` and `destroy` it made
    template<typename F>
    construct_counts counted(F &&operation) {
        construct_counts const before = construct_calls;
        std::forward<F>(operation)();
        return construct_counts{.constructs = construct_calls.constructs - before.constructs,
                                .destroys = construct_calls.destroys - before.destroys};
    }

    /// true if the group holds the value `n` mapped to `n` at every index `n` of `indices`, and nothing else
    bool holds_exactly(group_t const &group, std::initializer_list<std::uint64_t> indices) {
        bool right = group.size() == indices.size();
        for (std::uint64_t const n : indices) {
            auto const index = static_cast<group_t::size_type>(n);
            right = right && group.has_value(index) && group.value(index)->key == n && group.value(index)->value == n;
        }
        return right;
    }

    template<typename Map, typename Reference>
    [[nodiscard]] bool same_contents(Map const &map, Reference const &reference) {
        if (map.size() != reference.size()) {
            return false;
        }
        for (auto const &[key, value] : reference) {
            auto const it = map.find(key);
            if (it == map.end() || it->second != value) {
                return false;
            }
        }
        return static_cast<std::size_t>(std::distance(map.begin(), map.end())) == reference.size();
    }

    /**
     * Random insertions and erasures, by key and by iterator, and copies, in a map of up to a few hundred elements.
     * The keys come from a small range, so most groups hold many values, and their storage grows often.
     */
    template<typename Map>
    void run_random_operations(Map &map, std::unordered_map<std::uint64_t, std::uint64_t> &reference, std::uint64_t seed) {
        std::mt19937_64 rng(seed);
        for (int step = 0; step < 20000; ++step) {
            auto const key = rng() % 600;
            switch (rng() % 5) {
                case 0:
                case 1: {
                    auto const value = rng();
                    REQUIRE(map.try_emplace(key, value).second == reference.try_emplace(key, value).second);
                    break;
                }
                case 2: {
                    REQUIRE(map.erase(key) == reference.erase(key));
                    break;
                }
                case 3: {
                    auto const it = map.find(key);
                    if (it != map.end()) {
                        map.erase(it);
                        reference.erase(key);
                    }
                    break;
                }
                default: {
                    if (step % 97 == 0) {
                        auto const copy = map;
                        REQUIRE(same_contents(copy, reference));
                    }
                    break;
                }
            }
        }
        REQUIRE(same_contents(map, reference));
    }

    /**
     * Path of a datastore in the temporary directory that no other test uses. The datastore is removed when the
     * object is destroyed.
     */
    struct datastore_path {
        std::filesystem::path path;

        datastore_path()
            : path(std::filesystem::temp_directory_path() / ("dice_unordered_sparse_tests_constructs_in_place_" + std::to_string(std::random_device{}()))) {
            static_cast<void>(metall::manager::remove(path.c_str()));
        }

        datastore_path(datastore_path const &) = delete;
        datastore_path(datastore_path &&) = delete;
        datastore_path &operator=(datastore_path const &) = delete;
        datastore_path &operator=(datastore_path &&) = delete;

        ~datastore_path() {
            static_cast<void>(metall::manager::remove(path.c_str()));
        }
    };

    /**
     * Keeps an address range mapped without access rights while it is in scope, so that a datastore that was mapped
     * there is mapped at another address when it is opened again.
     */
    struct address_blocker {
        address_blocker(void const *address, std::size_t size) noexcept
            : size_(size) {
            void *const requested = const_cast<void *>(address);
            void *const mapped = ::mmap(requested, size_, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE | MAP_FIXED_NOREPLACE, -1, 0);
            if (mapped == requested) {
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

    private:
        void *block_ = nullptr;
        std::size_t size_;
    };

}  // namespace

TEST_CASE("an allocator that constructs in place: a group copies and moves values as bytes") {
    construct_counting_allocator<slot_u64> alloc;
    group_t group;
    for (std::uint64_t const n : {10, 20, 30, 40}) {
        group.set(alloc, static_cast<group_t::size_type>(n), pair_u64{n, n});
    }
    REQUIRE(holds_exactly(group, {10, 20, 30, 40}));

    // the storage of 4 values is full: the insertion allocates more and copies the 4 values as bytes
    auto const growth = counted([&] {
        group.set(alloc, 5, pair_u64{5, 5});
    });
    CHECK(growth.constructs == 1);
    CHECK(growth.destroys == 0);
    CHECK(holds_exactly(group, {5, 10, 20, 30, 40}));

    // in front of all values, with room for it: the holder of the new value and the new value at its place
    auto const insertion = counted([&] {
        group.set(alloc, 1, pair_u64{1, 1});
    });
    CHECK(insertion.constructs == 2);
    CHECK(insertion.destroys == 1);
    CHECK(holds_exactly(group, {1, 5, 10, 20, 30, 40}));

    // the values after the erased one move as bytes, nothing is destroyed
    auto const erase = counted([&] {
        group.erase(alloc, group.value(1), 1);
    });
    CHECK(erase.constructs == 0);
    CHECK(erase.destroys == 0);
    CHECK(holds_exactly(group, {5, 10, 20, 30, 40}));

    // the copy of the group copies its values as bytes
    construct_counts const before_copy = construct_calls;
    group_t copy(group, alloc);
    CHECK(construct_calls.constructs == before_copy.constructs);
    CHECK(construct_calls.destroys == before_copy.destroys);
    CHECK(holds_exactly(copy, {5, 10, 20, 30, 40}));

    copy.clear(alloc);
    group.clear(alloc);
}

TEST_CASE("an allocator that constructs in place: random insertions and erasures") {
    using map_t = sparse_map<std::uint64_t, std::uint64_t, std::hash<std::uint64_t>, std::equal_to<std::uint64_t>, construct_counting_allocator<pair_u64>>;
    for (std::uint64_t seed = 0; seed < 4; ++seed) {
        auto map = map_t{};
        std::unordered_map<std::uint64_t, std::uint64_t> reference;
        run_random_operations(map, reference, seed);
    }
}

TEST_CASE("a metall map whose allocator constructs in place: random operations, closing, and opening at another address") {
    using map_t = sparse_map<std::uint64_t, std::uint64_t, std::hash<std::uint64_t>, std::equal_to<std::uint64_t>, metall::manager::allocator_type<pair_u64>>;
    datastore_path const store;
    char const *object_name = "map";
    std::unordered_map<std::uint64_t, std::uint64_t> reference;
    void const *last_address = nullptr;
    std::size_t last_size = 0;

    {
        metall::manager manager{metall::create_only, store.path.c_str()};
        REQUIRE(manager.check_sanity());
        last_address = manager.get_address();
        last_size = manager.get_size();
        auto *map = manager.construct<map_t>(object_name)(manager.get_allocator());
        REQUIRE(map != nullptr);
        run_random_operations(*map, reference, 1);
    }

    {
        auto const blocker = address_blocker{last_address, last_size};
        CHECK(blocker.blocks());
        metall::manager manager{metall::open_only, store.path.c_str()};
        REQUIRE(manager.check_sanity());
        REQUIRE(manager.get_address() != last_address);
        auto *map = std::get<0>(manager.find<map_t>(object_name));
        REQUIRE(map != nullptr);
        CHECK(same_contents(*map, reference));
        run_random_operations(*map, reference, 2);

        CHECK(manager.destroy<map_t>(object_name));
        CHECK(manager.all_memory_deallocated());
    }
}
