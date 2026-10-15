#ifndef DICE_SPARSE_MAP_TESTS_FIXTURES_THROWING_HASH_HPP
#define DICE_SPARSE_MAP_TESTS_FIXTURES_THROWING_HASH_HPP

#include "fixtures/test_types.hpp"

#include <cstddef>
#include <stdexcept>

namespace dice::sparse_map::tests {

    /**
     * Inner hash of `throwing_hash` that returns the number of the key, so that key `k` is in bucket `k` of a table
     * with more than `k` buckets, and key `g * sparse_array::nb_buckets` is in group `g`. It is not avalanching.
     */
    struct number_of_key {
        [[nodiscard]] std::size_t operator()(std::size_t key) const noexcept {
            return key;
        }
    };

    /**
     * Hash function for the exception safety tests. It hashes with `Inner`, which must accept every key type of
     * the table, and it is marked as avalanching, whatever `Inner` returns.
     *
     * Its move assignment throws while `moves_throw` is true, which `fragile_moves` sets for a scope. `std::swap` of
     * two of them move assigns, so a swap throws too.
     *
     * With `calls_can_throw`, a call throws on the n-th call after `calls_until_throw` was set to n, which
     * `bomb_after` does for a scope. Without it, a call cannot throw.
     *
     * The two counters are static per instantiation, so instantiations that differ in `Inner` or in
     * `calls_can_throw` do not disturb each other.
     */
    template<typename Inner, bool calls_can_throw = true>
    struct throwing_hash {
        using is_avalanching = void;

        /// calls left before a call throws, -1 never throws
        inline static int calls_until_throw = -1; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

        /// the move assignment throws while this is true
        inline static bool moves_throw = false; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

        throwing_hash() = default;
        throwing_hash(throwing_hash const &) = default;
        throwing_hash(throwing_hash &&) = default;
        throwing_hash &operator=(throwing_hash const &) = default;
        ~throwing_hash() = default;

        throwing_hash &operator=(throwing_hash && /*other*/) noexcept(false) {
            if (moves_throw) {
                throw std::runtime_error("throwing_hash move assignment");
            }
            return *this;
        }

        template<typename Key>
        [[nodiscard]] std::size_t operator()(Key const &key) const noexcept(!calls_can_throw) {
            if constexpr (calls_can_throw) {
                if (calls_until_throw >= 0 && 0 == calls_until_throw--) {
                    throw std::runtime_error("throwing_hash call");
                }
            }
            return Inner{}(key);
        }

        /**
         * Arms the countdown of the calls while it is in scope.
         */
        struct bomb_after {
            explicit bomb_after(int n) noexcept {
                calls_until_throw = n;
            }
            bomb_after(bomb_after const &) = delete;
            bomb_after(bomb_after &&) = delete;
            bomb_after &operator=(bomb_after const &) = delete;
            bomb_after &operator=(bomb_after &&) = delete;
            ~bomb_after() {
                calls_until_throw = -1;
            }
        };

        /**
         * Makes the move assignment throw while it is in scope.
         */
        struct fragile_moves {
            fragile_moves() noexcept {
                moves_throw = true;
            }
            fragile_moves(fragile_moves const &) = delete;
            fragile_moves(fragile_moves &&) = delete;
            fragile_moves &operator=(fragile_moves const &) = delete;
            fragile_moves &operator=(fragile_moves &&) = delete;
            ~fragile_moves() {
                moves_throw = false;
            }
        };
    };

} // namespace dice::sparse_map::tests

#endif // DICE_SPARSE_MAP_TESTS_FIXTURES_THROWING_HASH_HPP
