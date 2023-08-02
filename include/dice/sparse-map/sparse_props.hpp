#ifndef DICE_SPARSE_MAP_SPARSE_PROPS_HPP
#define DICE_SPARSE_MAP_SPARSE_PROPS_HPP

#include <bit>
#include <cassert>
#include <concepts>

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
} // namespace dice::sparse_map

#endif//DICE_SPARSE_MAP_SPARSE_PROPS_HPP
