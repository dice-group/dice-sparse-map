#include <dice/unordered_sparse.hpp>

#include <doctest/doctest.h>

#include <bit>
#include <cstdint>
#include <random>

/*
 * `select_bit` finds the bucket of the value at an offset of a group. `erase(iterator)` uses it, and a group with holes
 * uses it to find the bucket of the slot of an iterator.
 */

namespace {

    /**
     * The index of the set bit of `bitmap` with `rank` set bits below it, by clearing the lowest set bit `rank` times.
     */
    unsigned naive_select_bit(std::uint64_t bitmap, unsigned rank) {
        for (unsigned i = 0; i < rank; ++i) {
            bitmap &= bitmap - 1U;
        }
        return static_cast<unsigned>(std::countr_zero(bitmap));
    }

    void check_all_ranks(std::uint64_t bitmap) {
        auto const nb_bits = static_cast<unsigned>(std::popcount(bitmap));
        for (unsigned rank = 0; rank < nb_bits; ++rank) {
            REQUIRE(dice::unordered_sparse::detail::select_bit(bitmap, rank) == naive_select_bit(bitmap, rank));
        }
    }

    constexpr bool select_bit_in_a_constant_expression() {
        return dice::unordered_sparse::detail::select_bit(UINT64_C(0x8000000000000001), 0) == 0
               && dice::unordered_sparse::detail::select_bit(UINT64_C(0x8000000000000001), 1) == 63
               && dice::unordered_sparse::detail::select_bit(UINT64_C(0xFFFFFFFFFFFFFFFF), 37) == 37
               && dice::unordered_sparse::detail::select_bit(UINT64_C(0x0000F00000000F00), 4) == 44
               && dice::unordered_sparse::detail::select_bit(UINT64_C(0x0000F00000000F00), 0) == 8;
    }

}  // namespace

TEST_CASE("select_bit finds the set bit of a rank") {
    check_all_ranks(UINT64_C(0xFFFFFFFFFFFFFFFF));
    check_all_ranks(UINT64_C(0x8000000000000000));
    check_all_ranks(UINT64_C(0x0000000000000001));
    check_all_ranks(UINT64_C(0xFF00000000000000));
    check_all_ranks(UINT64_C(0x00000000000000FF));
    check_all_ranks(UINT64_C(0x8080808080808080));
    check_all_ranks(UINT64_C(0x0101010101010101));
    check_all_ranks(UINT64_C(0xAAAAAAAAAAAAAAAA));
    for (unsigned bit = 0; bit < 64; ++bit) {
        check_all_ranks(std::uint64_t{1} << bit);
        check_all_ranks(~(std::uint64_t{1} << bit));
    }

    std::mt19937_64 rng(42);
    for (int i = 0; i < 100000; ++i) {
        // sparse and dense bitmaps
        auto const bitmap = (i % 3 == 0) ? (rng() & rng() & rng()) : (i % 3 == 1) ? rng()
                                                                                  : (rng() | rng());
        check_all_ranks(bitmap);
    }
}

TEST_CASE("select_bit in a constant expression") {
    static_assert(select_bit_in_a_constant_expression());
    CHECK(select_bit_in_a_constant_expression());
}
