cmake_minimum_required(VERSION 3.18)
project(dice-sparse-map
        VERSION 0.1.0
        DESCRIPTION "C++ implementation of a memory efficient hash map and hash set based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). We added support for fancy pointers.")

include(cmake/boilerplate_init.cmake)
boilerplate_init()

find_package(Boost REQUIRED)

add_library(dice-sparse-map INTERFACE)
# Use Dice::sparse_map as target, more consistent with other libraries conventions (Boost, Qt, ...)
add_library(dice-sparse-map::dice-sparse-map ALIAS dice-sparse-map)
add_library(Dice::sparse-map ALIAS dice-sparse-map)

target_include_directories(
        dice-sparse-map INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>")

target_link_libraries(dice-sparse-map INTERFACE
        Boost::headers
        )

set_target_properties(dice-sparse-map PROPERTIES
        CXX_STANDARD 20
        CXX_EXTENSIONS OFF
        CXX_STANDARD_REQUIRED ON)

if(MSVC)
    target_sources(sparse_map INTERFACE
                   "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/tsl-sparse-map.natvis>"
                   "$<INSTALL_INTERFACE:${CMAKE_INSTALL_DATAROOTDIR}/tsl-sparse-map.natvis>")
endif()

if (IS_TOP_LEVEL)
    include(cmake/install_interface_library.cmake)
    install_interface_library(dice-sparse-map include)
endif ()


