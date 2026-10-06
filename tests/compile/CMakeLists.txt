# Compile tests: each file is compiled with `-fsyntax-only`. The files in `fail/` must not compile, and the output must
# hold the text of the `static_assert` that rejects them. The files in `pass/` must compile.
#
# The compiler is called directly, not through the build, so that a test does not start a second build in the build
# directory. The include directories come from `dice_sparse_map_compile_flags`, an `OBJECT` library that links the
# library: its `INCLUDE_DIRECTORIES` hold the directories of the library and of its dependencies (dice-hash).
add_library(dice_sparse_map_compile_flags OBJECT EXCLUDE_FROM_ALL empty.cpp)
target_link_libraries(dice_sparse_map_compile_flags PRIVATE dice-sparse-map::dice-sparse-map)

set(compile_flags_file "${CMAKE_CURRENT_BINARY_DIR}/compile_flags.rsp")
file(GENERATE OUTPUT "${compile_flags_file}"
        CONTENT "\"-I$<JOIN:$<TARGET_PROPERTY:dice_sparse_map_compile_flags,INCLUDE_DIRECTORIES>,\"\n\"-I>\"\n")

set(hash_message "Hash must be avalanching")

function(add_compile_test file)
    get_filename_component(name "${file}" NAME_WE)
    get_filename_component(kind "${file}" DIRECTORY)
    set(test_name "compile_${kind}_${name}")
    add_test(NAME "${test_name}"
            COMMAND "${CMAKE_CXX_COMPILER}" -std=c++23 -fsyntax-only "@${compile_flags_file}"
                    "${CMAKE_CURRENT_SOURCE_DIR}/${file}")
    if (kind STREQUAL "fail")
        set_tests_properties("${test_name}" PROPERTIES PASS_REGULAR_EXPRESSION "${ARGN}")
    endif ()
endfunction()

add_compile_test(pass/header_sparse_map.cpp)
add_compile_test(pass/header_sparse_set.cpp)
add_compile_test(pass/header_version.cpp)
add_compile_test(pass/default_hash.cpp)
add_compile_test(pass/overload_with_specialization.cpp)

add_compile_test(fail/map_std_hash.cpp "${hash_message}")
add_compile_test(fail/set_std_hash.cpp "${hash_message}")
