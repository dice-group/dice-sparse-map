#ifndef DICE_UNORDERED_SPARSE_TESTS_FIXTURES_INLINE_CONTAINERS_HPP
#define DICE_UNORDERED_SPARSE_TESTS_FIXTURES_INLINE_CONTAINERS_HPP

#include <dice/unordered_sparse.hpp>

#include "fixtures/offset_ptr_allocator.hpp"

#include <cstddef>
#include <functional>
#include <memory>
#include <utility>

/**
 * Containers with an inline capacity, which keep up to that many elements in the container object, in the inline group
 * (group 0 of a table of 64 buckets), and the helpers that tell where the elements are.
 */
namespace dice::unordered_sparse::tests {

    /**
     * Puts the key `k` into bucket `k` of a table with more than `k` buckets, and into bucket `k % 64` of the inline
     * group, so that a test knows the group, the bucket and the probe sequence of a key.
     */
    struct identity_hash {
        using is_avalanching = void;

        [[nodiscard]] constexpr std::size_t operator()(std::size_t key) const noexcept {
            return key;
        }
    };

    template<typename Allocator, sparsity Sparsity, std::size_t capacity>
    using identity_inline_map = sparse_map<std::size_t, std::size_t, identity_hash, std::equal_to<std::size_t>, Allocator, Sparsity, capacity>;

    template<typename Allocator, sparsity Sparsity, std::size_t capacity>
    using identity_inline_set = sparse_set<std::size_t, identity_hash, std::equal_to<std::size_t>, Allocator, Sparsity, capacity>;

    using inline_map_4 = identity_inline_map<std::allocator<std::pair<std::size_t, std::size_t>>, sparsity::medium, 4>;
    using inline_map_4_offset_ptr = identity_inline_map<offset_ptr_allocator<std::pair<std::size_t, std::size_t>>, sparsity::high, 4>;
    using inline_map_32 = identity_inline_map<std::allocator<std::pair<std::size_t, std::size_t>>, sparsity::low, 32>;
    using inline_set_4 = identity_inline_set<std::allocator<std::size_t>, sparsity::medium, 4>;
    using inline_set_4_offset_ptr = identity_inline_set<offset_ptr_allocator<std::size_t>, sparsity::high, 4>;
    using inline_set_32 = identity_inline_set<std::allocator<std::size_t>, sparsity::low, 32>;

    /// the address of the key of an element as `*it` gives it
    template<typename Element>
    [[nodiscard]] void const *key_address(Element const &e) {
        if constexpr (requires { e.second; }) {
            return std::addressof(e.first);
        } else {
            return std::addressof(e);
        }
    }

    /// true if `container` has elements and every element lies in the container object, so in the inline group
    template<typename Container>
    [[nodiscard]] bool elements_in_object(Container const &container) {
        auto const *const first = static_cast<void const *>(std::addressof(container));
        auto const *const last = static_cast<void const *>(std::addressof(container) + 1);
        bool all_inside = !container.empty();
        for (auto const &e : container) {
            void const *const key = key_address(e);
            all_inside = all_inside && std::less_equal<>{}(first, key) && std::less<>{}(key, last);
        }
        return all_inside;
    }

}  // namespace dice::unordered_sparse::tests

#endif  // DICE_UNORDERED_SPARSE_TESTS_FIXTURES_INLINE_CONTAINERS_HPP
