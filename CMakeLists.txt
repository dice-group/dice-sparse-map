cmake_minimum_required(VERSION 3.24)
project(dice-sparse-map
        VERSION 0.4.0
        DESCRIPTION "C++ implementation of a memory efficient hash map and hash set based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). We added support for fancy pointers.")
set(POBR_VERSION 3)  # Persisted Object Binary Representation Version

include(cmake/boilerplate_init.cmake)
boilerplate_init()
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/version.hpp.in ${CMAKE_CURRENT_SOURCE_DIR}/include/dice/sparse-map/version.hpp)

option(BUILD_TESTING "Build tests" OFF)
option(BUILD_BENCHMARKS "Build benchmarks" OFF)
option(BUILD_README_BENCHMARKS "Build the benchmarks of the README plots (needs abseil)" OFF)

if (PROJECT_IS_TOP_LEVEL)
    if (BUILD_TESTING OR BUILD_BENCHMARKS OR BUILD_README_BENCHMARKS)
        set(CONAN_INSTALL_ARGS "${CONAN_INSTALL_ARGS};-o=&:with_test_deps=True")
    endif ()
    if (BUILD_README_BENCHMARKS)
        set(CONAN_INSTALL_ARGS "${CONAN_INSTALL_ARGS};-o=&:with_readme_benchmark_deps=True")
    endif ()
    # only boost headers are needed, never the compiled libraries
    set(CONAN_INSTALL_ARGS "${CONAN_INSTALL_ARGS};-o=boost/*:header_only=True")
endif ()

set(style_files
        .clang-format
        .clang-tidy
)
foreach(style_file ${style_files})
    file(DOWNLOAD "https://raw.githubusercontent.com/dice-group/tentris-cpp-coding-guidelines/main/${style_file}"
            "${CMAKE_SOURCE_DIR}/${style_file}"
            TLS_VERIFY ON)
endforeach()

find_package(dice-hash REQUIRED)

add_library(${PROJECT_NAME} INTERFACE)
# Use dice::sparse_map as target, more consistent with other libraries conventions (Boost, Qt, ...)
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

target_include_directories(${PROJECT_NAME}
        INTERFACE
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
        )

target_link_libraries(${PROJECT_NAME}
        INTERFACE
        dice-hash::dice-hash
        )

if (PROJECT_IS_TOP_LEVEL)
    if (BUILD_TESTING)
        message("Tests are configured to be build.")
        include(CTest)
        enable_testing()
        add_subdirectory(tests)
    endif ()

    if (BUILD_BENCHMARKS)
        message("Benchmarks are configured to be build.")
        add_subdirectory(benchmarks)
    endif ()

    if (BUILD_README_BENCHMARKS)
        message("README benchmarks are configured to be build.")
        add_subdirectory(benchmarks/readme)
    endif ()

    include(cmake/install_interface_library.cmake)
    install_interface_library("${PROJECT_NAME}" "${PROJECT_NAME}" "${PROJECT_NAME}" "include")
endif ()
