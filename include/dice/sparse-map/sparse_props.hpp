#ifndef DICE_SPARSE_MAP_SPARSE_PROPS_HPP
#define DICE_SPARSE_MAP_SPARSE_PROPS_HPP

#include <bit>
#include <concepts>

namespace dice::sparse_map {
	namespace detail {
		template<std::unsigned_integral U>
		constexpr bool is_power_of_2(U x) {
			return std::popcount(x) == 1;
		}
	} // namespace detail

	enum class probing {
		linear,
		quadratic
	};

	enum class exception_safety {
		basic,
		strong
	};

	enum class sparsity {
		high,
		medium,
		low
	};
} // namespace dice::sparse_map

#endif//DICE_SPARSE_MAP_SPARSE_PROPS_HPP
