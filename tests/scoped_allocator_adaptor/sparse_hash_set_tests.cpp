#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <dice/sparse_map/sparse_set.hpp>
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
using sparse_set = dice::sparse_map::internal::sparse_hash<
    T, details::KeySelect<T>, Hash<typename T::value_type>, std::equal_to<T>, Alloc,
    dice::sparse_map::power_of_two_growth_policy<2>,
    dice::sparse_map::exception_safety::basic,
    dice::sparse_map::sparsity::medium,
    dice::sparse_map::probing::quadratic,
	dice::sparse_map::default_max_load_factor>;

} // namespace details

template <typename T> void construction() {
  using Type = typename T::value_type;
  typename T::Set(T::Set::default_init_bucket_count, details::Hash<typename Type::value_type>(),
                  std::equal_to<Type>(), typename T::Allocator());
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

TEST_SUITE("sparse set with scoped allocator") {
  TEST_CASE("normal construction"){construction<NORMAL<int>>();}

  TEST_CASE("scoped construction"){construction<SCOPED<int>>();}
}
