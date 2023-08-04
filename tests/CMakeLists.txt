include(FetchContent)
FetchContent_Declare(
        DocTest
        GIT_REPOSITORY "https://github.com/doctest/doctest.git"
        GIT_TAG "v2.4.11"
        GIT_SHALLOW TRUE)
FetchContent_MakeAvailable(DocTest)

macro(make_test DIR NAME)
    if ("${DIR}" STREQUAL ".")
        set(TARGET ${NAME})
    else ()
        set(TARGET ${DIR}-${NAME})
    endif ()

    add_executable(${TARGET} ${DIR}/${NAME}.cpp)
    target_link_libraries(${TARGET}
            doctest::doctest
            dice-sparse-map::dice-sparse-map
            Boost::headers
    )
    set_property(TARGET ${TARGET} PROPERTY CXX_STANDARD 20)
    add_test(NAME ${TARGET} COMMAND ${TARGET})

    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU")
        target_compile_options(${TARGET} PRIVATE -Werror -Wall -Wextra -Wold-style-cast -DTSL_DEBUG -UNDEBUG)
    elseif(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
        target_compile_options(${TARGET} PRIVATE /bigobj /WX /W3 /DTSL_DEBUG /UNDEBUG)
    endif()
endmacro ()

find_package(Boost REQUIRED COMPONENTS)

make_test(. custom_allocator_tests)
make_test(. policy_tests)
make_test(. sparse_map_tests)
make_test(. sparse_set_tests)
make_test(fancy_pointer sparse_array_tests)
make_test(fancy_pointer sparse_hash_map_tests)
make_test(fancy_pointer sparse_hash_set_tests)
make_test(scoped_allocator_adaptor sparse_hash_set_tests)
