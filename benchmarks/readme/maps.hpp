#ifndef DICE_SPARSE_MAP_BENCHMARKS_README_MAPS_HPP
#define DICE_SPARSE_MAP_BENCHMARKS_README_MAPS_HPP

/**
 * @file
 * The maps of the README benchmark. Every map is in the configuration you get by typing its type
 * name, its own default hash included. With `DSM_README_METALL` defined, every map gets an
 * allocator of a metall manager instead of `std::allocator` and keeps its hash and key equality.
 *
 * Each map is a struct with a `name` (the row in the CSV) and a member alias `type<Key>`.
 */

#include <dice/sparse-map/sparse_map.hpp>

#include <ankerl/unordered_dense.h>
#include <boost/unordered/unordered_flat_map.hpp>

#include <cstddef>
#include <functional>
#include <memory>
#include <type_traits>
#include <unordered_map>
#include <utility>

#if defined(DSM_README_ABSL)
#include <absl/container/flat_hash_map.h>
#include <absl/container/node_hash_map.h>
#endif

#if defined(DSM_README_METALL)
// metall calls std::destroy_at on its pointer to the segment header and reads the pointer after that
// (kernel/segment_storage.hpp), and GCC warns about the read where it inlines the code.
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
#include <metall/container/vector.hpp>
#include <metall/metall.hpp>
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif
#endif

namespace dice::sparse_map::bench::readme {

    /// the mapped type of every map
    using mapped_t = std::size_t;

#if defined(DSM_README_METALL)
    template<typename T>
    using allocator = metall::manager::allocator_type<T>;
#else
    template<typename T>
    using allocator = std::allocator<T>;
#endif

    /**
     * `Plain<Key>` with `allocator` for its value type. Hash and key equality are the ones of
     * `Plain<Key>`, so with `std::allocator` this is `Plain<Key>` itself.
     */
    template<template<typename, typename, typename, typename, typename> typename Map, typename Plain>
    using with_allocator = Map<
        typename Plain::key_type,
        typename Plain::mapped_type,
        typename Plain::hasher,
        typename Plain::key_equal,
        allocator<typename std::allocator_traits<typename Plain::allocator_type>::value_type>
    >;

    template<sh::sparsity sparsity>
    struct sparse_map_of {
        /// the default hash of the map
        template<typename Key>
        using default_hash = typename ::dice::sparse_map::sparse_map<Key, mapped_t>::hasher;

        template<typename Key>
        using plain = ::dice::sparse_map::sparse_map<
            Key,
            mapped_t,
            default_hash<Key>,
            std::equal_to<Key>,
            std::allocator<std::pair<Key, mapped_t>>,
            sparsity
        >;

        template<typename Key>
        using type = ::dice::sparse_map::sparse_map<
            Key,
            mapped_t,
            typename plain<Key>::hasher,
            typename plain<Key>::key_equal,
            allocator<std::pair<Key, mapped_t>>,
            sparsity
        >;
    };

    /// `dice::sparse_map::sparse_map<Key, std::size_t>`, the reference of the plots
    struct sparse_medium : sparse_map_of<sh::sparsity::medium> {
        static constexpr char const *name = "sparse-medium";
        static_assert(std::is_same_v<plain<int>, ::dice::sparse_map::sparse_map<int, mapped_t>>);
    };

    struct sparse_high : sparse_map_of<sh::sparsity::high> {
        static constexpr char const *name = "sparse-high";
    };

    struct sparse_low : sparse_map_of<sh::sparsity::low> {
        static constexpr char const *name = "sparse-low";
    };

    /**
     * `ankerl::unordered_dense::map<Key, std::size_t>`. Version 5.2.0 does not compile with metall's
     * allocator: `operator[]` calls `operator->` on the iterator of its value vector, a `std::vector`
     * with metall's allocator, and with libstdc++ that iterator cannot return an `offset_ptr` as a raw
     * pointer. Its fifth template parameter also takes a container instead of an allocator, so with
     * metall the values are in `metall::container::vector` (a `boost::container::vector`). The group
     * array stays a `std::vector` with the rebound metall allocator.
     */
    struct unordered_dense {
        static constexpr char const *name = "udm";

        template<typename Key>
        using plain = ankerl::unordered_dense::map<Key, mapped_t>;

#if defined(DSM_README_METALL)
        template<typename Key>
        using type = ankerl::unordered_dense::map<
            Key,
            mapped_t,
            typename plain<Key>::hasher,
            typename plain<Key>::key_equal,
            metall::container::vector<std::pair<Key, mapped_t>>
        >;
#else
        template<typename Key>
        using type = plain<Key>;
#endif
    };

#if !defined(DSM_README_METALL)
    /**
     * `ankerl::unordered_dense::segmented_map<Key, std::size_t>`: the values are in a segmented
     * vector, which grows by adding a segment instead of moving the values to a larger array. Only
     * the allocation timeline uses it, with `std::allocator`.
     */
    struct unordered_dense_segmented {
        static constexpr char const *name = "udm-segmented";

        template<typename Key>
        using plain = ankerl::unordered_dense::segmented_map<Key, mapped_t>;

        template<typename Key>
        using type = plain<Key>;
    };
#endif

    /// `ankerl::unordered_dense::map` with metall's allocator, which does not compile (see above)
    struct unordered_dense_allocator {
        static constexpr char const *name = "udm-allocator";

        template<typename Key>
        using plain = ankerl::unordered_dense::map<Key, mapped_t>;

        template<typename Key>
        using type = ankerl::unordered_dense::map<
            Key,
            mapped_t,
            typename plain<Key>::hasher,
            typename plain<Key>::key_equal,
            allocator<std::pair<Key, mapped_t>>
        >;
    };

    struct std_unordered_map {
        static constexpr char const *name = "std";

        template<typename Key>
        using plain = std::unordered_map<Key, mapped_t>;

        template<typename Key>
        using type = with_allocator<std::unordered_map, plain<Key>>;
    };

    struct boost_flat {
        static constexpr char const *name = "boost";

        template<typename Key>
        using plain = boost::unordered_flat_map<Key, mapped_t>;

        template<typename Key>
        using type = with_allocator<boost::unordered_flat_map, plain<Key>>;
    };

#if defined(DSM_README_ABSL)
    struct absl_flat {
        static constexpr char const *name = "absl";

        template<typename Key>
        using plain = absl::flat_hash_map<Key, mapped_t>;

        template<typename Key>
        using type = with_allocator<absl::flat_hash_map, plain<Key>>;
    };

    struct absl_node {
        static constexpr char const *name = "absl-node";

        template<typename Key>
        using plain = absl::node_hash_map<Key, mapped_t>;

        template<typename Key>
        using type = with_allocator<absl::node_hash_map, plain<Key>>;
    };
#endif

#if !defined(DSM_README_METALL)
    // With `std::allocator`, every map is the type you get by typing its name.
    static_assert(std::is_same_v<sparse_medium::type<int>, ::dice::sparse_map::sparse_map<int, mapped_t>>);
    static_assert(std::is_same_v<std_unordered_map::type<int>, std::unordered_map<int, mapped_t>>);
    static_assert(std::is_same_v<boost_flat::type<int>, boost::unordered_flat_map<int, mapped_t>>);
    static_assert(std::is_same_v<unordered_dense::type<int>, ankerl::unordered_dense::map<int, mapped_t>>);
    static_assert(
        std::is_same_v<unordered_dense_segmented::type<int>, ankerl::unordered_dense::segmented_map<int, mapped_t>>
    );
#if defined(DSM_README_ABSL)
    static_assert(std::is_same_v<absl_flat::type<int>, absl::flat_hash_map<int, mapped_t>>);
    static_assert(std::is_same_v<absl_node::type<int>, absl::node_hash_map<int, mapped_t>>);
#endif
#endif

} // namespace dice::sparse_map::bench::readme

#endif // DICE_SPARSE_MAP_BENCHMARKS_README_MAPS_HPP
