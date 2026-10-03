cmake_minimum_required(VERSION 3.15)
project(PackageTest CXX)

find_package(dice-unordered-sparse REQUIRED)

add_executable(example example.cpp)

target_link_libraries(example PUBLIC
        dice-unordered-sparse::dice-unordered-sparse)

set_target_properties(
        example PROPERTIES
        CXX_STANDARD 23
        CXX_STANDARD_REQUIRED ON
        CXX_EXTENSIONS OFF
)
