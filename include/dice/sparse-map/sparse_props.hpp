#ifndef DICE_SPARSE_MAP_SPARSE_PROPS_HPP
#define DICE_SPARSE_MAP_SPARSE_PROPS_HPP

#include <bit>
#include <cassert>
#include <concepts>

namespace dice::sparse_map {
	namespace detail {
		template<std::unsigned_integral U>
		constexpr bool is_power_of_2(U x) noexcept {
			return std::popcount(x) == 1;
		}

		template<std::unsigned_integral U>
		constexpr U round_up_to_power_of_2(U value) noexcept {
			assert(value > 0);
			auto const highest_bit_pos = sizeof(U) * 8 - std::countl_zero(value - 1);
			return U{1} << highest_bit_pos;
		}
	} // namespace detail

	enum struct probing : bool {
		linear,
		quadratic
	};

	enum struct exception_safety : bool {
		basic,
		strong
	};

	enum struct sparsity : uint8_t {
		high,
		medium,
		low
	};
} // namespace dice::sparse_map

#endif//DICE_SPARSE_MAP_SPARSE_PROPS_HPP
