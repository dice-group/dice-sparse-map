#include <boost/test/unit_test.hpp>
#include <dice/sparse-map/sparse_set.hpp>
#include <scoped_allocator>

namespace details {
template <typename Key> struct KeySelect {
  using key_type = Key;
  const key_type &operator()(Key const &key) const noexcept { return key; }
  key_type &operator()(Key &key) noexcept { return key; }
};

template<typename T>
struct Hash {
  std::size_t operator()(std::vector<T> const &vec) const noexcept {
	  std::hash<T> h;
	  std::size_t ret;

	  for (auto const &e : vec) {
		  ret ^= h(e);
	  }

	  return ret;
  }
};

template <typename T, typename Alloc>
using sparse_set = dice::sparse_map::detail::sparse_hash<
    T, details::KeySelect<T>, Hash<typename T::value_type>, std::equal_to<T>, Alloc,
    dice::sparse_map::power_of_two_growth_policy<2>,
    dice::sparse_map::exception_safety::basic,
    dice::sparse_map::sparsity::medium,
    dice::sparse_map::probing::quadratic>;

} // namespace details

template <typename T> void construction() {
  using Type = typename T::value_type;
  typename T::Set(T::Set::DEFAULT_INIT_BUCKET_COUNT, details::Hash<typename Type::value_type>(),
                  std::equal_to<Type>(), typename T::Allocator(),
                  T::Set::DEFAULT_MAX_LOAD_FACTOR);
}


template <typename T>
struct NORMAL {
  using value_type = std::vector<T>;
  using Allocator = std::allocator<value_type>;
  using Set = details::sparse_set<value_type, Allocator>;
};

template <typename T>
struct SCOPED {
  using value_type = std::vector<T>;
  using Allocator = std::scoped_allocator_adaptor<std::allocator<value_type>, std::allocator<T>>;
  using Set = details::sparse_set<value_type, Allocator>;
};

BOOST_AUTO_TEST_SUITE(scoped_allocators)
BOOST_AUTO_TEST_SUITE(sparse_hash_set_tests)

BOOST_AUTO_TEST_CASE(normal_construction){construction<NORMAL<int>>();}

BOOST_AUTO_TEST_CASE(scoped_construction){construction<SCOPED<int>>();}

BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()