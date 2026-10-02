#ifndef DICE_UNORDERED_SPARSE_HPP
#define DICE_UNORDERED_SPARSE_HPP

/** @file
 * @brief Memory efficient hash map and hash set: `dice::sparse_map` and `dice::sparse_set`.
 *
 * This is the only header to include. The other names of the library are in `dice::unordered_sparse`. They
 * are defined in the headers under `dice/unordered_sparse/detail`, which are not meant to be included directly.
 */

#include <dice/unordered_sparse/detail/sparse_map.hpp>
#include <dice/unordered_sparse/detail/sparse_set.hpp>

namespace dice {

    using unordered_sparse::sparse_map;
    using unordered_sparse::sparse_set;

}  // namespace dice

#endif  // DICE_UNORDERED_SPARSE_HPP
