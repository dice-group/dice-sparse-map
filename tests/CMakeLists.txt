find_package(doctest REQUIRED)
find_package(Boost REQUIRED)
find_package(Metall REQUIRED)
find_package(nanobench REQUIRED)

# doctest main, the nanobench implementation and the counter fixture, compiled once and linked into every test
add_library(dice_sparse_map_test_main OBJECT test_main.cpp fixtures/counter.cpp)
target_include_directories(dice_sparse_map_test_main PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_libraries(dice_sparse_map_test_main PUBLIC
        doctest::doctest
        nanobench::nanobench
)

add_custom_target(build_tests)

# Every tests_<name>.cpp is one test executable tests_<name>.
file(GLOB test_sources CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/tests_*.cpp")
foreach (test_source IN LISTS test_sources)
    get_filename_component(test_name "${test_source}" NAME_WE)
    add_executable(${test_name} "${test_source}")
    target_include_directories(${test_name} PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
    target_link_libraries(${test_name} PRIVATE
            dice_sparse_map_test_main
            dice-sparse-map::dice-sparse-map
            doctest::doctest
            nanobench::nanobench
            Boost::headers
            Metall::Metall
    )
    # keep the internal assertions active in every build type
    target_compile_definitions(${test_name} PRIVATE TSL_DEBUG)
    if (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU")
        target_compile_options(${test_name} PRIVATE -UNDEBUG -Wall -Wextra -Wold-style-cast)
    elseif (CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
        target_compile_options(${test_name} PRIVATE /bigobj /W3 /UNDEBUG)
    endif ()
    add_test(NAME ${test_name} COMMAND ${test_name})
    add_dependencies(build_tests ${test_name})
endforeach ()

add_subdirectory(compile)
