cmake_minimum_required(VERSION 3.18)
project(dice-sparse-map
        VERSION 0.2.4
        DESCRIPTION "C++ implementation of a memory efficient hash map and hash set based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). We added support for fancy pointers.")

include(cmake/boilerplate_init.cmake)
boilerplate_init()

option(USE_CONAN "Use conan to fetch dependencies" ON)
option(BUILD_TESTING "Build tests" ON)

if (IS_TOP_LEVEL AND USE_CONAN)
    include(cmake/conan_cmake.cmake)

    if (NOT BUILD_TESTING)
        set(CONAN_OPTIONS "-o boost/*:header_only=True")
    endif ()

    install_packages_via_conan("${CMAKE_SOURCE_DIR}/conanfile.py" "${CONAN_OPTIONS}")
endif ()


find_package(Boost REQUIRED COMPONENTS)

add_library(${PROJECT_NAME} INTERFACE)
# Use dice::sparse_map as target, more consistent with other libraries conventions (Boost, Qt, ...)
add_library("${PROJECT_NAME}::${PROJECT_NAME}" ALIAS "${PROJECT_NAME}")

target_include_directories(${PROJECT_NAME}
        INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>")

target_link_libraries(${PROJECT_NAME} INTERFACE
        Boost::headers
        )

if(MSVC)
    target_sources(${PROJECT_NAME} INTERFACE
                   "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/tsl-sparse-map.natvis>"
                   "$<INSTALL_INTERFACE:${CMAKE_INSTALL_DATAROOTDIR}/tsl-sparse-map.natvis>")
endif()

if (IS_TOP_LEVEL AND BUILD_TESTING)
    message("Tests are configured to be build.")
    include(CTest)
    enable_testing()
    add_subdirectory(tests)
endif ()

if (IS_TOP_LEVEL)
    include(cmake/install_interface_library.cmake)
    install_interface_library("${PROJECT_NAME}" "${PROJECT_NAME}" "${PROJECT_NAME}" "include")
endif ()
