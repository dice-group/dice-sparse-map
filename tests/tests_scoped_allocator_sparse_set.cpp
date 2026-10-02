#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>

#include <functional>
#include <memory>
#include <scoped_allocator>

namespace details {
    template<typename Key>
    struct key_select {
        using key_type = Key;
        key_type const &operator()(Key const &key) const noexcept {
            return key;
        }
        key_type &operator()(Key &key) noexcept {
            return key;
        }
    };

    template<typename T, typename Alloc>
    using sparse_set = dice::unordered_sparse::detail::sparse_hash<
        T,
        details::key_select<T>,
        void,
        std::hash<T>,
        std::equal_to<T>,
        Alloc,
        dice::unordered_sparse::power_of_two_growth_policy<2>,
        dice::unordered_sparse::exception_safety::basic,
        dice::unordered_sparse::sparsity::medium,
        dice::unordered_sparse::probing::quadratic>;
}  // namespace details

template<typename T>
void construction() {
    using value_type = typename T::value_type;
    typename T::set_type(T::set_type::DEFAULT_INIT_BUCKET_COUNT, std::hash<value_type>(), std::equal_to<value_type>(), typename T::allocator_type(), T::set_type::DEFAULT_MAX_LOAD_FACTOR);
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
