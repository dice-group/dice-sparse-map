#ifndef DICE_SPARSE_MAP_SPARSE_PROPS_HPP
#define DICE_SPARSE_MAP_SPARSE_PROPS_HPP

#include <concepts>
#include <ratio>

namespace dice::sparse_map {
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

	template<typename T>
	concept ratio = requires {
		{ T::num } -> std::convertible_to<std::intmax_t>;
		{ T::den } -> std::convertible_to<std::intmax_t>;
	};

	using default_max_load_factor = std::ratio<1, 2>;
} // namespace dice::sparse_map

#endif//DICE_SPARSE_MAP_SPARSE_PROPS_HPP
