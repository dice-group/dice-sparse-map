cmake_minimum_required(VERSION 3.24)
project(dice-sparse-map
        VERSION 0.2.6
        DESCRIPTION "C++ implementation of a memory efficient hash map and hash set based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). We added support for fancy pointers.")

include(cmake/boilerplate_init.cmake)
boilerplate_init()

option(BUILD_TESTING "Build tests" OFF)

if (PROJECT_IS_TOP_LEVEL)
    if (BUILD_TESTING)
        set(CONAN_INSTALL_ARGS "${CONAN_INSTALL_ARGS};-o=&:with_test_deps=True")
    endif ()
endif ()

add_library(${PROJECT_NAME} INTERFACE)
# Use dice::sparse_map as target, more consistent with other libraries conventions (Boost, Qt, ...)
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

target_include_directories(${PROJECT_NAME}
        INTERFACE
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
        )

if(MSVC)
    target_sources(${PROJECT_NAME}
            INTERFACE
            "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/tsl-sparse-map.natvis>"
            "$<INSTALL_INTERFACE:${CMAKE_INSTALL_DATAROOTDIR}/tsl-sparse-map.natvis>")
endif()

if (PROJECT_IS_TOP_LEVEL AND BUILD_TESTING)
    message("Tests are configured to be build.")
    include(CTest)
    enable_testing()
    add_subdirectory(tests)
endif ()

if (PROJECT_IS_TOP_LEVEL)
    include(cmake/install_interface_library.cmake)
    install_interface_library("${PROJECT_NAME}" "${PROJECT_NAME}" "${PROJECT_NAME}" "include")
endif ()
