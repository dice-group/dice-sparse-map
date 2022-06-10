cmake_minimum_required(VERSION 3.18)

project(tsl_sparse_map_tests)

add_executable(tsl_sparse_map_tests "main.cpp"
        "custom_allocator_tests.cpp"
        "policy_tests.cpp"
        "popcount_tests.cpp"
        "sparse_map_tests.cpp"
        "sparse_set_tests.cpp"
        "fancy_pointer/sparse_array_tests.cpp"
        "fancy_pointer/sparse_hash_map_tests.cpp"
        "fancy_pointer/sparse_hash_set_tests.cpp"
        "scoped_allocator_adaptor/sparse_array_tests.cpp"
        "scoped_allocator_adaptor/sparse_hash_set_tests.cpp"
        )

target_compile_features(tsl_sparse_map_tests PRIVATE cxx_std_20)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    target_compile_options(tsl_sparse_map_tests PRIVATE -Werror -Wall -Wextra -Wold-style-cast -DTSL_DEBUG -UNDEBUG)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    target_compile_options(tsl_sparse_map_tests PRIVATE /bigobj /WX /W3 /DTSL_DEBUG /UNDEBUG)
endif()

# Boost::unit_test_framework
set(Boost_USE_STATIC_LIBS ON)
find_package(Boost 1.54.0 REQUIRED COMPONENTS unit_test_framework)
target_link_libraries(tsl_sparse_map_tests PRIVATE Boost::unit_test_framework)

# Dice::sparse_map
add_subdirectory(../ ${CMAKE_CURRENT_BINARY_DIR}/Dice/sparse-map)
target_link_libraries(tsl_sparse_map_tests PRIVATE Dice::sparse-map)
