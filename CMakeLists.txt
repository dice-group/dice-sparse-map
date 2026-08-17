cmake_minimum_required(VERSION 3.24)
project(dice-sparse-map
        VERSION 0.3.0
        DESCRIPTION "C++ implementation of a memory efficient hash map and hash set based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). We added support for fancy pointers.")
set(POBR_VERSION 2)  # Persisted Object Binary Representation Version

include(cmake/boilerplate_init.cmake)
boilerplate_init()
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/version.hpp.in ${CMAKE_CURRENT_SOURCE_DIR}/include/dice/sparse-map/version.hpp)

option(BUILD_TESTING "Build tests" OFF)

if (PROJECT_IS_TOP_LEVEL)
    if (BUILD_TESTING)
        set(CONAN_INSTALL_ARGS "${CONAN_INSTALL_ARGS};-o=&:with_test_deps=True")
    endif ()
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

if (PROJECT_IS_TOP_LEVEL)
    if (BUILD_TESTING)
        message("Tests are configured to be build.")
        include(CTest)
        enable_testing()
        add_subdirectory(tests)
    else ()
        # in top-level builds without tests we only need boost headers but not libraries
        # so avoid building the unecessesarily
        set(CONAN_INSTALL_ARGS "${CONAN_INSTALL_ARGS};-o=boost/*:header_only=True")
    endif ()

    include(cmake/install_interface_library.cmake)
    install_interface_library("${PROJECT_NAME}" "${PROJECT_NAME}" "${PROJECT_NAME}" "include")
endif ()
