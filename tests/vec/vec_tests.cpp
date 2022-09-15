#define BOOST_TEST_MODULE vec_tests
#include <boost/test/unit_test.hpp>
#include <dice/sparse-map/vec.hpp>
#include <dice/sparse-map/sparse_array.hpp>
#include <memory>
#include <scoped_allocator>

BOOST_AUTO_TEST_SUITE(test_vec)

using pair = std::pair<int,int>;
using arr = dice::sparse_map::detail_sparse_hash::sparse_array<pair, std::allocator<pair>, dice::sparse_map::sh::sparsity::medium>;
using dice::sparse_map::detail_sparse_hash::vec;

template<typename T>
using allocator_type = std::allocator<T>;

template<typename T>
using scoped_allocator_type = std::scoped_allocator_adaptor<std::allocator<T>>;

BOOST_AUTO_TEST_CASE(test_std_allocator) {
  allocator_type<arr> allocator;
  vec<arr, allocator_type<arr>> v(allocator);

  for(std::size_t i = 0; i < 100000; ++i) {
    v.emplace_back(arr());
  }
}

BOOST_AUTO_TEST_CASE(test_scoped_allocator) {
  scoped_allocator_type<arr> allocator;
  vec<arr, scoped_allocator_type<arr>> v(allocator);

  for(std::size_t i = 0; i < 100000; ++i) {
    v.emplace_back(arr());
  }
}

BOOST_AUTO_TEST_CASE(test_move_constructor) {
  using v = vec<arr, allocator_type<arr>>;
  allocator_type<arr> allocator;
  v a(allocator);

  for(std::size_t i = 0; i < 100000; ++i) {
    a.emplace_back(arr());
  }

  v b(std::move(a));
}

BOOST_AUTO_TEST_SUITE_END()
