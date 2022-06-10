#include <boost/test/unit_test.hpp>
#include <Dice/sparse-map/sparse_set.hpp>
#include <scoped_allocator>

namespace details {
template <typename Key> struct KeySelect {
  using key_type = Key;
  const key_type &operator()(Key const &key) const noexcept { return key; }
  key_type &operator()(Key &key) noexcept { return key; }
};

template <typename T, typename Alloc>
using sparse_set = Dice::sparse_map::detail_sparse_hash::sparse_hash<
    T, details::KeySelect<T>, void, std::hash<T>, std::equal_to<T>, Alloc,
    Dice::sparse_map::sh::power_of_two_growth_policy<2>,
    Dice::sparse_map::sh::exception_safety::basic,
    Dice::sparse_map::sh::sparsity::medium,
    Dice::sparse_map::sh::probing::quadratic>;
} // namespace details

template <typename T> void construction() {
  using Type = typename T::value_type;
  typename T::Set(T::Set::DEFAULT_INIT_BUCKET_COUNT, std::hash<Type>(),
                  std::equal_to<Type>(), typename T::Allocator(),
                  T::Set::DEFAULT_MAX_LOAD_FACTOR);
}


template <typename T>
struct NORMAL {
  using value_type = T;
  using Allocator = std::allocator<T>;
  using Set = details::sparse_set<T, Allocator>;
};

template <typename T>
struct SCOPED {
  using value_type = T;
  using Allocator = std::scoped_allocator_adaptor<std::allocator<T>>;
  using Set = details::sparse_set<T, Allocator>;
};

BOOST_AUTO_TEST_SUITE(scoped_allocators)
BOOST_AUTO_TEST_SUITE(sparse_hash_set_tests)

BOOST_AUTO_TEST_CASE(normal_construction){construction<NORMAL<int>>();}

BOOST_AUTO_TEST_CASE(scoped_construction){construction<SCOPED<int>>();}

BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()