find_package(doctest REQUIRED)
find_package(Boost REQUIRED)
find_package(Metall REQUIRED)
find_package(nanobench REQUIRED)
find_package(unordered_dense REQUIRED)

# One binary with every benchmark. Each bench_<name>.cpp holds doctest test cases, so a run picks
# benchmarks with doctest's filters, e.g. -tc="*sparsity*".
file(GLOB benchmark_sources CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/bench_*.cpp")
add_executable(dice_sparse_map_benchmarks benchmark_main.cpp ${benchmark_sources})
# the tests directory for the allocator fixtures of the tests, e.g. fixtures/offset_ptr_allocator.hpp
target_include_directories(dice_sparse_map_benchmarks PRIVATE
        "${CMAKE_CURRENT_SOURCE_DIR}"
        "${PROJECT_SOURCE_DIR}/tests"
)
target_link_libraries(dice_sparse_map_benchmarks PRIVATE
        dice-sparse-map::dice-sparse-map
        doctest::doctest
        nanobench::nanobench
        Boost::headers
        Metall::Metall
        unordered_dense::unordered_dense
)
