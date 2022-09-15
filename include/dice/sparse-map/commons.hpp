#ifndef COMMONS_HPP
#define COMMONS_HPP
#include <cassert>
#include <memory>
#include <type_traits>
#include <concepts>
#include <limits>
#include <stdexcept>
#include "dice/sparse-map/sparse_growth_policy.hpp"

#ifdef TSL_DEBUG
#define tsl_sh_assert(expr) assert(expr)
#else
#define tsl_sh_assert(expr) (static_cast<void>(0))
#endif

namespace dice::sparse_map {
  namespace sh {
  enum class probing { linear, quadratic };

  enum class exception_safety { basic, strong };

  enum class sparsity { high, medium, low };
  }  // namespace sh

  /* Replacement for const_cast in sparse_array.
 * Can be overloaded for specific fancy pointers
 * (see: include/dice/boost_offset_pointer.h).
 * This is just a workaround.
 * The clean way would be to change the implementation to stop using const_cast.
 */
  template <typename T>
  struct Remove_Const {
      template <typename V>
      static T remove(V iter) {
          return const_cast<T>(iter);
      }
  };

  template<typename U> 
  concept has_is_transparent = requires 
  {
    typename U::is_transparent;
  };

  namespace detail_sparse_hash {
    template <typename U>
    struct is_power_of_two_policy : std::false_type {};

    template <std::size_t GrowthFactor>
    struct is_power_of_two_policy<dice::sparse_map::sh::power_of_two_growth_policy<GrowthFactor>>
        : std::true_type {};

    template <typename T>
    concept power_of_two_policy = is_power_of_two_policy<T>::value;

    template <typename U>
    concept has_mapped_type = !std::same_as<U, void>; 

    template<typename T>
    concept is_nothrow_move_constructible = std::is_nothrow_move_constructible<T>::value;

    inline constexpr bool is_power_of_two(std::size_t value) {
      return value != 0 && (value & (value - 1)) == 0;
    }

    inline std::size_t round_up_to_power_of_two(std::size_t value) {
      if (is_power_of_two(value)) {
        return value;
      }

      if (value == 0) {
        return 1;
      }

      --value;
      for (std::size_t i = 1; i < sizeof(std::size_t) * CHAR_BIT; i *= 2) {
        value |= value >> i;
      }

      return value + 1;
    }

    template <typename T, typename U>
    static T numeric_cast(U value,
                          const char *error_message = "numeric_cast() failed.") {
      T ret = static_cast<T>(value);
      if (static_cast<U>(ret) != value) {
        throw std::runtime_error(error_message);
      }

      const bool is_same_signedness =
          (std::is_unsigned<T>::value && std::is_unsigned<U>::value) ||
          (std::is_signed<T>::value && std::is_signed<U>::value);
      if (!is_same_signedness && (ret < T{}) != (value < U{})) {
        throw std::runtime_error(error_message);
      }

      return ret;
    }
  } // namespace detail_sparse_hash
} // namespace dice::sparse_map
#endif