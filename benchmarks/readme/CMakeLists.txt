# The README benchmark: one binary per allocator, key type and map, and a second binary per map
# that counts the bytes requested from the heap. The checks of the map interface and of the
# relocation are in bin/check. See README.md in this directory.
find_package(Boost REQUIRED)
find_package(Metall REQUIRED)
find_package(unordered_dense REQUIRED)
find_package(absl REQUIRED)

# The maps of maps.hpp, per allocator. The maps in `metall_failing_maps` do not compile with
# metall's allocator: absl rejects an allocator whose pointer is not a raw pointer, and
# `unordered_dense_allocator` compiles the benchmark but not `operator[]` and `operator->` of its
# iterator. Their check targets are not part of `all`, so that for example
# `cmake --build <dir> --target dsm_readme_api_metall_u64_absl_flat` shows the error.
set(std_maps sparse_medium sparse_high sparse_low unordered_dense std_unordered_map boost_flat absl_flat absl_node)
set(metall_maps sparse_medium sparse_high sparse_low unordered_dense std_unordered_map boost_flat)
set(metall_failing_maps unordered_dense_allocator absl_flat absl_node)

set(readme_bin_dir "${CMAKE_CURRENT_BINARY_DIR}/bin")

# kind: `time` (the timed panels and rss), `memory` (counts the bytes requested) or `api` (api_check.cpp)
function(dsm_readme_target variant key map kind)
    if (kind STREQUAL "api")
        set(target "dsm_readme_api_${variant}_${key}_${map}")
        add_executable(${target} api_check.cpp)
        set_target_properties(${target} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${readme_bin_dir}/check")
    else ()
        set(target "dsm_readme_${variant}_${key}_${map}")
        if (kind STREQUAL "memory")
            string(APPEND target "_memory")
        endif ()
        add_executable(${target} bench_readme.cpp)
        set_target_properties(${target} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${readme_bin_dir}")
    endif ()
    target_compile_definitions(${target} PRIVATE DSM_README_MAP=${map})
    target_link_libraries(${target} PRIVATE
            dice-unordered-sparse::dice-unordered-sparse
            Boost::headers
            unordered_dense::unordered_dense
    )
    if (key STREQUAL "str")
        target_compile_definitions(${target} PRIVATE DSM_README_STRING_KEYS)
    endif ()
    if (variant STREQUAL "metall")
        target_compile_definitions(${target} PRIVATE DSM_README_METALL)
        target_link_libraries(${target} PRIVATE Metall::Metall)
    endif ()
    if (map MATCHES "^absl_")
        target_compile_definitions(${target} PRIVATE DSM_README_ABSL)
        target_link_libraries(${target} PRIVATE absl::flat_hash_map absl::node_hash_map)
    endif ()
    if (kind STREQUAL "memory")
        target_compile_definitions(${target} PRIVATE DSM_README_COUNT_ALLOC)
        target_link_libraries(${target} PRIVATE ${CMAKE_DL_LIBS})
    endif ()
endfunction()

foreach (key u64 str)
    foreach (map IN LISTS std_maps)
        dsm_readme_target(std ${key} ${map} time)
        dsm_readme_target(std ${key} ${map} memory)
        dsm_readme_target(std ${key} ${map} api)
    endforeach ()
    foreach (map IN LISTS metall_maps)
        dsm_readme_target(metall ${key} ${map} time)
        dsm_readme_target(metall ${key} ${map} api)
    endforeach ()
    foreach (map IN LISTS metall_failing_maps)
        dsm_readme_target(metall ${key} ${map} api)
        set_target_properties(dsm_readme_api_metall_${key}_${map} PROPERTIES EXCLUDE_FROM_ALL TRUE)
    endforeach ()
endforeach ()

# Which maps keep working when their metall datastore is opened at another address.
add_executable(dsm_readme_relocate relocate.cpp)
set_target_properties(dsm_readme_relocate PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${readme_bin_dir}/check")
target_compile_definitions(dsm_readme_relocate PRIVATE DSM_README_METALL)
target_link_libraries(dsm_readme_relocate PRIVATE
        dice-unordered-sparse::dice-unordered-sparse
        Boost::headers
        Metall::Metall
        unordered_dense::unordered_dense
)
