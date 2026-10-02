#include "common.hpp"
#include "map_benchmarks.hpp"
#include "workloads.hpp"

#include <doctest/doctest.h>
#include <nanobench.h>

#include <cstddef>
#include <string>

using namespace dice::unordered_sparse::bench;

/*
 * A relatively quick benchmark that gives one number for how good a map is: the geometric mean
 * of the scored workloads (see `quick_overall`).
 */

TEST_CASE("quick_overall sparse_map high") {
    quick_overall<sparse_high>("sparse_map high");
}

TEST_CASE("quick_overall sparse_map medium") {
    quick_overall<sparse_medium>("sparse_map medium");
}

TEST_CASE("quick_overall sparse_map low") {
    quick_overall<sparse_low>("sparse_map low");
}

TEST_CASE("quick_overall unordered_dense") {
    quick_overall<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("quick_overall std::unordered_map") {
    quick_overall<std_family<>>("std::unordered_map");
}

TEST_CASE("find_all sparse_map high") {
    find_all_hits_or_misses<sparse_high>("sparse_map high");
}

TEST_CASE("find_all sparse_map medium") {
    find_all_hits_or_misses<sparse_medium>("sparse_map medium");
}

TEST_CASE("find_all sparse_map low") {
    find_all_hits_or_misses<sparse_low>("sparse_map low");
}

TEST_CASE("find_all unordered_dense") {
    find_all_hits_or_misses<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("find_all std::unordered_map") {
    find_all_hits_or_misses<std_family<>>("std::unordered_map");
}

/*
 * The scored workloads with a mapped type whose move constructor can throw (see `throwing_move_value`), not part of
 * the score.
 */
TEST_CASE("throwing_move sparse_map medium") {
    throwing_move<sparse_medium>("sparse_map medium");
}

TEST_CASE("throwing_move unordered_dense") {
    throwing_move<unordered_dense_family<>>("unordered_dense");
}

TEST_CASE("throwing_move std::unordered_map") {
    throwing_move<std_family<>>("std::unordered_map");
}

/*
 * The string hash on its own, not part of the score. `sparse_map`, `sparse_set` and the
 * unordered_dense containers use `ankerl::unordered_dense::hash` here, `std::unordered_map` uses
 * `std::hash`.
 */
TEST_CASE("hash_strings") {
    ankerl::nanobench::Bench bench;
    bench.title("hash_strings").unit("hash").batch(hash_keys().size());
    configure(bench);
    bench.run("ankerl::unordered_dense::hash<std::string>", [] {
        ankerl::nanobench::doNotOptimizeAway(hash_strings<sparse_high::map<std::string, std::size_t>>());
    });
    bench.run("std::hash<std::string>", [] {
        ankerl::nanobench::doNotOptimizeAway(hash_strings<std_family<>::map<std::string, std::size_t>>());
    });
}
