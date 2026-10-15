#include <dice/sparse-map/sparse_set.hpp>

#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <functional>
#include <memory>
#include <scoped_allocator>

namespace details {
    template<typename T, typename Alloc>
    using sparse_set = dice::sparse_map::detail_sparse_hash::sparse_hash<
        dice::sparse_map::detail_sparse_hash::set_access<T>,
        dice::sparse_map::tests::test_hash<T>,
        std::equal_to<T>,
        Alloc,
        dice::sparse_map::sh::sparsity::medium,
        dice::sparse_map::sh::allocation_failure::terminating>;
}  // namespace details

template<typename T>
void construction() {
    using value_type = typename T::value_type;
    typename T::set_type(T::set_type::default_init_bucket_count, dice::sparse_map::tests::test_hash<value_type>(), std::equal_to<value_type>(), typename T::allocator_type(), T::set_type::default_max_load_factor);
}


template<typename T>
struct normal_alloc {
    using value_type = T;
    using allocator_type = std::allocator<T>;
    using set_type = details::sparse_set<T, allocator_type>;
};

template<typename T>
struct scoped_alloc {
    using value_type = T;
    using allocator_type = std::scoped_allocator_adaptor<std::allocator<T>>;
    using set_type = details::sparse_set<T, allocator_type>;
};

TEST_SUITE("scoped_allocators/sparse_hash_set_tests") {

    TEST_CASE("normal_construction") {
        construction<normal_alloc<int>>();
    }

    TEST_CASE("scoped_construction") {
        construction<scoped_alloc<int>>();
    }
}
