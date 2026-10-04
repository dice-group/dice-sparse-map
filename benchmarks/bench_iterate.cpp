#include "common.hpp"
#include "map_benchmarks.hpp"

#include <doctest/doctest.h>

using namespace dice::unordered_sparse::bench;

/*
 * A pass over all elements of a large map, with the iterators and, for a map with a member `for_each`, with
 * `for_each` (see `iterate_large`). `unordered_dense` and `std::unordered_map` run the loop over the iterators. The
 * metall rows are in `bench_metall.cpp`.
 */

TEST_CASE("iterate sparse_map medium") {
    iterate_large<sparse_medium>("sparse_map medium");
}

TEST_CASE("iterate unordered_dense") {
    iterate_large<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("iterate std::unordered_map") {
    iterate_large<std_family<>>("std::unordered_map");
}
