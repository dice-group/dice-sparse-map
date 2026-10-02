#include "fixtures/checksum.hpp"

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

#include <cctype>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <functional>
#include <iterator>
#include <random>
#include <scoped_allocator>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * `sparse_map` and `sparse_set` in a metall datastore. A container is filled, the datastore is closed
 * and opened again, and the container is checked, changed, checked again and destroyed. Keys and
 * values are integers, because a type that keeps a raw pointer (like `std::string`) cannot live in
 * persistent memory. Before the datastore is opened again, the address range of its last mapping is
 * blocked, so that it is mapped at another address and a raw pointer into the old mapping fails.
 */
namespace {
    using namespace dice::unordered_sparse;
    namespace checksum = dice::unordered_sparse::tests::checksum;

    template<typename T>
    using metall_allocator = metall::manager::allocator_type<T>;

    template<sparsity Sparsity>
    using persistent_map = sparse_map<std::uint64_t,
                                      std::uint64_t,
                                      std::hash<std::uint64_t>,
                                      std::equal_to<std::uint64_t>,
                                      metall_allocator<std::pair<std::uint64_t, std::uint64_t>>,
                                      exception_safety::basic,
                                      Sparsity>;

    template<sparsity Sparsity>
    using persistent_set = sparse_set<std::uint64_t,
                                      std::hash<std::uint64_t>,
                                      std::equal_to<std::uint64_t>,
                                      metall_allocator<std::uint64_t>,
                                      exception_safety::basic,
                                      Sparsity>;

    /// number of elements a container holds after it was filled, and again after it was changed
    constexpr std::uint64_t num_elements = 100000;

    /// the mapped value that belongs to `key`
    constexpr std::uint64_t value_of(std::uint64_t key) noexcept {
        return key * 0x9e3779b97f4a7c15ULL + 1;
    }

    template<typename Container>
    constexpr bool is_map_v = requires { typename Container::mapped_type; };

    template<typename Container>
    void insert_one(Container &container, std::uint64_t key) {
        if constexpr (is_map_v<Container>) {
            container.try_emplace(key, value_of(key));
        } else {
            container.insert(key);
        }
    }

    /// true if `container` holds `key`, and for a map also the right value
    template<typename Container>
    bool holds(Container const &container, std::uint64_t key) {
        auto const it = container.find(key);
        if (it == container.end()) {
            return false;
        }
        if constexpr (is_map_v<Container>) {
            return it->second == value_of(key);
        } else {
            return true;
        }
    }

    /**
     * Order independent checksum of a map or a set. It accepts a container or a vector of the
     * expected elements: pairs for a map, keys for a set.
     */
    template<typename Range>
    std::uint64_t checksum_of(Range const &range) {
        if constexpr (requires { range.begin()->second; }) {
            return checksum::map(range);
        } else {
            std::uint64_t combined = 1;
            std::uint64_t num = 0;
            for (auto const &key : range) {
                combined ^= checksum::mix(key);
                ++num;
            }
            return checksum::combine(combined, num);
        }
    }

    /// checksum of a container after the change: it holds the odd keys below `num_elements` and the keys from `num_elements` up to `num_elements * 3 / 2`
    template<typename Container>
    std::uint64_t expected_checksum_after_change() {
        if constexpr (is_map_v<Container>) {
            std::vector<std::pair<std::uint64_t, std::uint64_t>> expected;
            for (std::uint64_t key = 1; key < num_elements; key += 2) {
                expected.emplace_back(key, value_of(key));
            }
            for (std::uint64_t key = num_elements; key < num_elements + num_elements / 2; ++key) {
                expected.emplace_back(key, value_of(key));
            }
            return checksum_of(expected);
        } else {
            std::vector<std::uint64_t> expected;
            for (std::uint64_t key = 1; key < num_elements; key += 2) {
                expected.push_back(key);
            }
            for (std::uint64_t key = num_elements; key < num_elements + num_elements / 2; ++key) {
                expected.push_back(key);
            }
            return checksum_of(expected);
        }
    }

    /// `text` with every character that is not a letter or a digit replaced by `_`
    std::string file_name_part(std::string text) {
        for (auto &c : text) {
            if (std::isalnum(static_cast<unsigned char>(c)) == 0) {
                c = '_';
            }
        }
        return text;
    }

    /**
     * Path of a datastore in the temporary directory that no other test uses. The datastore is removed
     * when the object is destroyed.
     */
    struct datastore_path {
        std::filesystem::path path;

        explicit datastore_path(std::string const &name)
            : path(std::filesystem::temp_directory_path() / ("dice_sparse_map_tests_metall_" + file_name_part(name) + "_" + std::to_string(std::random_device{}()))) {
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
     * Keeps an address range mapped without access rights while it is in scope. A datastore that was
     * mapped there is mapped at another address when it is opened again, and a container that kept a
     * raw pointer into the old mapping crashes on it instead of reading the old data.
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

    /// where a datastore was mapped the last time it was open
    struct mapping {
        void const *address = nullptr;
        std::size_t size = 0;
    };

    mapping mapping_of(metall::manager const &manager) {
        return mapping{manager.get_address(), manager.get_size()};
    }

    /**
     * Fills a container in a new datastore, closes it, opens it again and checks every element. Then
     * erases half of the elements, inserts new ones, checks again and compares a checksum. Closes and
     * opens the datastore a third time, compares the checksum again and destroys the container.
     */
    template<typename Container>
    void check_round_trip(std::string const &name) {
        datastore_path const store{name};
        char const *object_name = "container";
        auto const expected_checksum = expected_checksum_after_change<Container>();
        mapping last_mapping;

        {
            metall::manager manager{metall::create_only, store.path.c_str()};
            REQUIRE(manager.check_sanity());
            last_mapping = mapping_of(manager);
            auto *container = manager.construct<Container>(object_name)(manager.get_allocator());
            REQUIRE(container != nullptr);
            for (std::uint64_t key = 0; key < num_elements; ++key) {
                insert_one(*container, key);
            }
            REQUIRE(container->size() == num_elements);
        }

        {
            auto const blocker = address_blocker{last_mapping.address, last_mapping.size};
            CHECK(blocker.blocks());
            metall::manager manager{metall::open_only, store.path.c_str()};
            REQUIRE(manager.check_sanity());
            REQUIRE(manager.get_address() != last_mapping.address);
            last_mapping = mapping_of(manager);
            auto *container = std::get<0>(manager.find<Container>(object_name));
            REQUIRE(container != nullptr);
            CHECK(container->size() == num_elements);

            bool all_found = true;
            for (std::uint64_t key = 0; key < num_elements; ++key) {
                all_found = all_found && holds(*container, key);
            }
            CHECK(all_found);
            CHECK_FALSE(container->contains(num_elements));

            std::size_t erased = 0;
            for (std::uint64_t key = 0; key < num_elements; key += 2) {
                erased += container->erase(key);
            }
            CHECK(erased == num_elements / 2);
            for (std::uint64_t key = num_elements; key < num_elements + num_elements / 2; ++key) {
                insert_one(*container, key);
            }
            CHECK(container->size() == num_elements);

            bool odd_found = true;
            bool even_gone = true;
            bool new_found = true;
            for (std::uint64_t key = 0; key < num_elements; ++key) {
                if (key % 2 == 0) {
                    even_gone = even_gone && !container->contains(key);
                } else {
                    odd_found = odd_found && holds(*container, key);
                }
            }
            for (std::uint64_t key = num_elements; key < num_elements + num_elements / 2; ++key) {
                new_found = new_found && holds(*container, key);
            }
            CHECK(odd_found);
            CHECK(even_gone);
            CHECK(new_found);

            CHECK(static_cast<std::uint64_t>(std::distance(container->begin(), container->end())) == num_elements);
            CHECK(checksum_of(*container) == expected_checksum);
        }

        {
            auto const blocker = address_blocker{last_mapping.address, last_mapping.size};
            CHECK(blocker.blocks());
            metall::manager manager{metall::open_only, store.path.c_str()};
            REQUIRE(manager.check_sanity());
            REQUIRE(manager.get_address() != last_mapping.address);
            auto *container = std::get<0>(manager.find<Container>(object_name));
            REQUIRE(container != nullptr);
            CHECK(container->size() == num_elements);
            CHECK(checksum_of(*container) == expected_checksum);

            CHECK(manager.destroy<Container>(object_name));
            CHECK(std::get<0>(manager.find<Container>(object_name)) == nullptr);
            // the container gave back every block it got from the datastore
            CHECK(manager.all_memory_deallocated());
        }
    }

    /// the mapped type of the maps of maps below
    using inner_map = persistent_map<sparsity::medium>;

    /// The outer map passes its allocator to every inner map it constructs (uses-allocator construction).
    using scoped_outer_map = sparse_map<std::uint64_t,
                                        inner_map,
                                        std::hash<std::uint64_t>,
                                        std::equal_to<std::uint64_t>,
                                        std::scoped_allocator_adaptor<metall_allocator<std::pair<std::uint64_t, inner_map>>>>;

    /// The caller passes the allocator to every inner map it inserts.
    using explicit_outer_map = sparse_map<std::uint64_t,
                                          inner_map,
                                          std::hash<std::uint64_t>,
                                          std::equal_to<std::uint64_t>,
                                          metall_allocator<std::pair<std::uint64_t, inner_map>>>;

    /**
     * `std::scoped_allocator_adaptor` moves the inner maps by uses-allocator construction, so the inner
     * map needs the allocator-extended move constructor `sparse_map(sparse_map &&, Allocator const &)`.
     * `sparse_map` does not have it yet. Until it has, the test of `scoped_outer_map` is compiled out
     * and skipped.
     */
    constexpr bool scoped_map_of_maps_compiles = std::is_constructible_v<inner_map,
                                                                         inner_map &&,
                                                                         std::scoped_allocator_adaptor<metall_allocator<std::pair<std::uint64_t, inner_map>>> const &>;

    /// true if the current `sparse_map` can store the map of maps `OuterMap`
    template<typename OuterMap>
    constexpr bool map_of_maps_compiles_v = !std::is_same_v<OuterMap, scoped_outer_map> || scoped_map_of_maps_compiles;

    constexpr std::uint64_t num_outer = 100;
    constexpr std::uint64_t num_inner = 100;

    /// the inner map under `key`, inserted if it is not there
    template<typename OuterMap>
    typename OuterMap::mapped_type &inner_map_at(OuterMap &outer, std::uint64_t key, metall::manager const &manager) {
        if constexpr (std::is_same_v<OuterMap, explicit_outer_map>) {
            outer.try_emplace(key, typename OuterMap::mapped_type::allocator_type{manager.get_allocator()});
            return outer.at(key);
        } else {
            return outer[key];
        }
    }

    /**
     * Fills a map of maps in a new datastore, closes it, opens it again, checks every element and destroys
     * the map. Does nothing for a map of maps that does not compile yet.
     */
    template<typename OuterMap>
    void check_map_of_maps_round_trip(std::string const &name) {
        if constexpr (map_of_maps_compiles_v<OuterMap>) {
            datastore_path const store{name};
            char const *object_name = "map_of_maps";

            std::vector<std::pair<std::uint64_t, std::uint64_t>> expected_inner;
            std::uint64_t expected_checksum = 1;
            for (std::uint64_t outer_key = 0; outer_key < num_outer; ++outer_key) {
                expected_inner.clear();
                for (std::uint64_t inner_key = 0; inner_key < num_inner; ++inner_key) {
                    expected_inner.emplace_back(inner_key, value_of(outer_key * num_inner + inner_key));
                }
                expected_checksum ^= checksum::combine(checksum::mix(outer_key), checksum::map(expected_inner));
            }
            expected_checksum = checksum::combine(expected_checksum, num_outer);

            mapping last_mapping;
            {
                metall::manager manager{metall::create_only, store.path.c_str()};
                REQUIRE(manager.check_sanity());
                last_mapping = mapping_of(manager);
                auto *outer = manager.construct<OuterMap>(object_name)(manager.get_allocator());
                REQUIRE(outer != nullptr);
                for (std::uint64_t outer_key = 0; outer_key < num_outer; ++outer_key) {
                    auto &inner = inner_map_at(*outer, outer_key, manager);
                    for (std::uint64_t inner_key = 0; inner_key < num_inner; ++inner_key) {
                        inner[inner_key] = value_of(outer_key * num_inner + inner_key);
                    }
                }
                REQUIRE(outer->size() == num_outer);
            }

            {
                auto const blocker = address_blocker{last_mapping.address, last_mapping.size};
                CHECK(blocker.blocks());
                metall::manager manager{metall::open_only, store.path.c_str()};
                REQUIRE(manager.check_sanity());
                REQUIRE(manager.get_address() != last_mapping.address);
                auto *outer = std::get<0>(manager.find<OuterMap>(object_name));
                REQUIRE(outer != nullptr);
                CHECK(outer->size() == num_outer);

                auto const datastore_allocator = typename OuterMap::mapped_type::allocator_type{manager.get_allocator()};
                bool all_found = true;
                bool all_in_datastore = true;
                for (std::uint64_t outer_key = 0; outer_key < num_outer; ++outer_key) {
                    auto const it = outer->find(outer_key);
                    if (it == outer->end()) {
                        all_found = false;
                        continue;
                    }
                    auto const &inner = it->second;
                    all_in_datastore = all_in_datastore && inner.get_allocator() == datastore_allocator;
                    all_found = all_found && inner.size() == num_inner;
                    for (std::uint64_t inner_key = 0; inner_key < num_inner; ++inner_key) {
                        auto const inner_it = inner.find(inner_key);
                        all_found = all_found && inner_it != inner.end() && inner_it->second == value_of(outer_key * num_inner + inner_key);
                    }
                }
                CHECK(all_found);
                CHECK(all_in_datastore);
                CHECK(checksum::mapmap(*outer) == expected_checksum);

                // the inner maps still grow after the datastore was opened again
                auto &first = inner_map_at(*outer, 0, manager);
                for (std::uint64_t inner_key = num_inner; inner_key < 10 * num_inner; ++inner_key) {
                    first[inner_key] = inner_key;
                }
                CHECK(outer->at(0).size() == 10 * num_inner);

                CHECK(manager.destroy<OuterMap>(object_name));
                CHECK(manager.all_memory_deallocated());
            }
        }
    }

}  // namespace

TYPE_TO_STRING_AS("sparse_map<metall, high>", persistent_map<sparsity::high>);
TYPE_TO_STRING_AS("sparse_map<metall, medium>", persistent_map<sparsity::medium>);
TYPE_TO_STRING_AS("sparse_map<metall, low>", persistent_map<sparsity::low>);
TYPE_TO_STRING_AS("sparse_set<metall, high>", persistent_set<sparsity::high>);
TYPE_TO_STRING_AS("sparse_set<metall, medium>", persistent_set<sparsity::medium>);
TYPE_TO_STRING_AS("sparse_set<metall, low>", persistent_set<sparsity::low>);

TEST_CASE_TEMPLATE("a container in a metall datastore survives closing and opening",
                   container_t,
                   persistent_map<sparsity::high>,
                   persistent_map<sparsity::medium>,
                   persistent_map<sparsity::low>,
                   persistent_set<sparsity::high>,
                   persistent_set<sparsity::medium>,
                   persistent_set<sparsity::low>) {
    check_round_trip<container_t>(doctest::toString<container_t>().c_str());
}

TEST_CASE("a map of maps that passes the allocator by hand survives closing and opening") {
    check_map_of_maps_round_trip<explicit_outer_map>("explicit_map_of_maps");
}

// skipped until `sparse_map` has `sparse_map(sparse_map &&, Allocator const &)`, see `scoped_map_of_maps_compiles`
TEST_CASE("a map of maps with std::scoped_allocator_adaptor survives closing and opening" * doctest::skip(!scoped_map_of_maps_compiles)) {
    check_map_of_maps_round_trip<scoped_outer_map>("scoped_map_of_maps");
}
