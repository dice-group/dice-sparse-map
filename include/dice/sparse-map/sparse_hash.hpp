/**
 * MIT License
 *
 * Copyright (c) 2017 Thibaut Goetghebuer-Planchon <tessil@gmx.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#ifndef DICE_SPARSE_MAP_SPARSE_HASH_HPP
#define DICE_SPARSE_MAP_SPARSE_HASH_HPP

#include <algorithm>
#include <bit>
#include <cassert>
#include <climits>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <limits>
#include <memory>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include "boost/container/vector.hpp"
#include "dice/sparse-map/sparse_growth_policy.hpp"

#ifdef TSL_DEBUG
#define tsl_sh_assert(expr) assert(expr)
#else
#define tsl_sh_assert(expr) (static_cast<void>(0))
#endif

namespace dice::sparse_map {

	namespace sh {
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
	}// namespace sh

	namespace detail_sparse_hash {
		template<typename T>
		struct make_void {
			using type = void;
		};

		template<typename U>
		struct is_power_of_two_policy : std::false_type {};

		template<std::size_t GrowthFactor>
		struct is_power_of_two_policy<dice::sparse_map::sh::power_of_two_growth_policy<GrowthFactor>>
			: std::true_type {};

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

		template<typename T, typename U>
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

		/**
		 * Fixed size type used to represent size_type values on serialization. Need to
		 * be big enough to represent a std::size_t on 32 and 64 bits platforms, and
		 * must be the same size on both platforms.
		 */
		using slz_size_type = std::uint64_t;
		static_assert(std::numeric_limits<slz_size_type>::max() >=
							  std::numeric_limits<std::size_t>::max(),
					  "slz_size_type must be >= std::size_t");

		template<class T, class Deserializer>
		static T deserialize_value(Deserializer &deserializer) {
			// MSVC < 2017 is not conformant, circumvent the problem by removing the
			// template keyword
#if defined(_MSC_VER) && _MSC_VER < 1910
			return deserializer.Deserializer::operator()<T>();
#else
			return deserializer.Deserializer::template operator()<T>();
#endif
		}

		/**
		 * WARNING: the sparse_array class doesn't free the ressources allocated through
		 * the allocator passed in parameter in each method. You have to manually call
		 * `clear(Allocator&)` when you don't need a sparse_array object anymore.
		 *
		 * The reason is that the sparse_array doesn't store the allocator to avoid
		 * wasting space in each sparse_array when the allocator has a size > 0. It only
		 * allocates/deallocates objects with the allocator that is passed in parameter.
		 *
		 *
		 *
		 * Index denotes a value between [0, BITMAP_NB_BITS), it is an index similar to
		 * std::vector. Offset denotes the real position in `m_values` corresponding to
		 * an index.
		 *
		 * We are using raw pointers instead of std::vector to avoid loosing
		 * 2*sizeof(size_t) bytes to store the capacity and size of the vector in each
		 * sparse_array. We know we can only store up to BITMAP_NB_BITS elements in the
		 * array, we don't need such big types.
		 *
		 *
		 * T must be nothrow move constructible and/or copy constructible.
		 * Behaviour is undefined if the destructor of T throws an exception.
		 *
		 * See https://smerity.com/articles/2015/google_sparsehash.html for details on
		 * the idea behinds the implementation.
		 *
		 * TODO Check to use std::realloc and std::memmove when possible
		 */
		template<typename T, typename Allocator, dice::sparse_map::sh::sparsity Sparsity>
		struct sparse_array {
			using value_type = T;
			using size_type = std::uint_least8_t;
			using allocator_type = Allocator;
			using allocator_traits = std::allocator_traits<allocator_type>;
			using pointer = typename allocator_traits::pointer;
			using const_pointer = typename allocator_traits::const_pointer;
			using iterator = pointer;
			using const_iterator = const_pointer;

		private:
			using alloc_traits = std::allocator_traits<allocator_type>;

			static constexpr size_type CAPACITY_GROWTH_STEP = []() {
				switch (Sparsity) {
					case dice::sparse_map::sh::sparsity::high:   return 2;
					case dice::sparse_map::sh::sparsity::medium: return 4;
					case dice::sparse_map::sh::sparsity::low:    return 8;
				}
			}();

			using bitmap_type = std::uint_least64_t;
			static constexpr std::size_t BITMAP_NB_BITS = 64;
			static constexpr std::size_t BUCKET_SHIFT = 6;

			static constexpr std::size_t BUCKET_MASK = BITMAP_NB_BITS - 1;

			static_assert(is_power_of_two(BITMAP_NB_BITS),
						  "BITMAP_NB_BITS must be a power of two.");
			static_assert(std::numeric_limits<bitmap_type>::digits >= BITMAP_NB_BITS,
						  "bitmap_type must be able to hold at least BITMAP_NB_BITS.");
			static_assert((std::size_t(1) << BUCKET_SHIFT) == BITMAP_NB_BITS,
						  "(1 << BUCKET_SHIFT) must be equal to BITMAP_NB_BITS.");
			static_assert(std::numeric_limits<size_type>::max() >= BITMAP_NB_BITS,
						  "size_type must be big enough to hold BITMAP_NB_BITS.");
			static_assert(std::is_unsigned<bitmap_type>::value,
						  "bitmap_type must be unsigned.");
			static_assert((std::numeric_limits<bitmap_type>::max() & BUCKET_MASK) ==
								  BITMAP_NB_BITS - 1);

		private:
			pointer m_values = nullptr;

			bitmap_type m_bitmap_vals = 0;
			bitmap_type m_bitmap_deleted_vals = 0;

			size_type m_nb_elements = 0;
			size_type m_capacity = 0;
			bool m_last_array = false;

		public:
			/**
		     * Map an ibucket [0, bucket_count) in the hash table to a sparse_ibucket
		     * (a sparse_array holds multiple buckets, so there is less sparse_array than
		     * bucket_count).
		     *
		     * The bucket ibucket is in
		     * m_sparse_buckets[sparse_ibucket(ibucket)][index_in_sparse_bucket(ibucket)]
		     * instead of something like m_buckets[ibucket] in a classical hash table.
		     */
			static constexpr std::size_t sparse_ibucket(std::size_t ibucket) noexcept {
				return ibucket >> BUCKET_SHIFT;
			}

			/**
		     * Map an ibucket [0, bucket_count) in the hash table to an index in the
		     * sparse_array which corresponds to the bucket.
		     *
		     * The bucket ibucket is in
		     * m_sparse_buckets[sparse_ibucket(ibucket)][index_in_sparse_bucket(ibucket)]
		     * instead of something like m_buckets[ibucket] in a classical hash table.
		     */
			static constexpr size_type index_in_sparse_bucket(std::size_t ibucket) noexcept {
				return static_cast<size_type>(ibucket & BUCKET_MASK);
			}

			static constexpr std::size_t nb_sparse_buckets(std::size_t bucket_count) noexcept {
				if (bucket_count == 0) {
					return 0;
				}

				return std::max<std::size_t>(1, sparse_ibucket(round_up_to_power_of_two(bucket_count)));
			}

		public:
			constexpr sparse_array() noexcept = default;

			//needed for "is_constructible" with no parameters
			constexpr sparse_array(std::allocator_arg_t, [[maybe_unused]] allocator_type const &alloc) noexcept {
			}

			/*explicit sparse_array(bool last_bucket) noexcept
				: m_values(nullptr),
				  m_bitmap_vals(0),
				  m_bitmap_deleted_vals(0),
				  m_nb_elements(0),
				  m_capacity(0),
				  m_last_array(last_bucket) {}*/

			sparse_array(size_type capacity, allocator_type const &calloc) : m_capacity{capacity} {
				if (m_capacity == 0) {
					return;
				}

				auto alloc = calloc;
				m_values = alloc_traits::allocate(alloc, m_capacity);
				tsl_sh_assert(m_values != nullptr);// allocate should throw if there is a failure
			}

			sparse_array(sparse_array const &other) = delete;

			constexpr sparse_array(sparse_array &&other) noexcept : m_values{std::exchange(other.m_values, nullptr)},
																	m_bitmap_vals{std::exchange(other.m_bitmap_vals, 0)},
																	m_bitmap_deleted_vals{std::exchange(other.m_bitmap_deleted_vals, 0)},
																	m_nb_elements{std::exchange(other.m_nb_elements, 0)},
																	m_capacity{std::exchange(other.m_capacity, 0)},
																	m_last_array{other.m_last_array} {
			}

			sparse_array(sparse_array const &other, allocator_type const &calloc) : m_values{nullptr},
																					m_bitmap_vals{other.m_bitmap_vals},
																					m_bitmap_deleted_vals{other.m_bitmap_deleted_vals},
																					m_nb_elements{0},
																					m_capacity(other.m_capacity),
																					m_last_array(other.m_last_array) {
				auto alloc = calloc;

				tsl_sh_assert(other.m_capacity >= other.m_nb_elements);
				if (m_capacity == 0) {
					return;
				}

				m_values = alloc_traits::allocate(alloc, m_capacity);
				tsl_sh_assert(m_values != nullptr);// allocate should throw if there is a failure

				try {
					for (; m_nb_elements < other.m_nb_elements; ++m_nb_elements) {
						new (&m_values[m_nb_elements]) value_type{other.m_values[m_nb_elements]};
					}
				} catch (...) {
					clear(alloc);
					throw;
				}
			}

			sparse_array(sparse_array &&other, Allocator const &calloc) : m_values{nullptr},
																		  m_bitmap_vals{other.m_bitmap_vals},
																		  m_bitmap_deleted_vals{other.m_bitmap_deleted_vals},
																		  m_nb_elements{0},
																		  m_capacity{other.m_capacity},
																		  m_last_array{other.m_last_array} {
				auto alloc = calloc; // the only reason the allocator above is not mutable is because of scoped allocators

				tsl_sh_assert(other.m_capacity >= other.m_nb_elements);
				if (m_capacity == 0) {
					return;
				}

				m_values = alloc_traits::allocate(alloc, m_capacity);
				tsl_sh_assert(m_values != nullptr);// allocate should throw if there is a failure

				try {
					for (size_type i = 0; i < other.m_nb_elements; i++) {
						new (&m_values[i]) value_type{std::move(other.m_values[i])};
						m_nb_elements++;
					}
				} catch (...) {
					clear(alloc);
					throw;
				}
			}

			sparse_array &operator=(sparse_array const &) = delete;

			constexpr sparse_array &operator=(sparse_array &&other) noexcept {
				tsl_sh_assert(this != &other);

				this->m_values = std::exchange(other.m_values, nullptr);
				this->m_bitmap_vals = std::exchange(other.m_bitmap_vals, 0);
				this->m_bitmap_deleted_vals = std::exchange(other.m_bitmap_deleted_vals, 0);
				this->m_nb_elements = std::exchange(other.m_nb_elements, 0);
				this->m_capacity = std::exchange(other.m_capacity, 0);

				return *this;
			}


			// The code that manages the sparse_array must have called clear before
			// destruction. See documentation of sparse_array for more details.
			~sparse_array() noexcept = default;

			/**
			 * @safety This function is only safe to call if the underlying object is non-const
			 */
			static iterator unsafe_mutable_iterator(const_iterator pos) noexcept {
				if constexpr (std::is_pointer_v<const_iterator>) {
					return const_cast<iterator>(pos);
				} else {
					return iterator{const_cast<value_type *>(std::to_address(pos))};
				}
			}

			[[nodiscard]] constexpr iterator begin() noexcept { return m_values; }
			[[nodiscard]] constexpr iterator end() noexcept { return m_values + m_nb_elements; }
			[[nodiscard]] constexpr const_iterator begin() const noexcept { return cbegin(); }
			[[nodiscard]] constexpr const_iterator end() const noexcept { return cend(); }
			[[nodiscard]] constexpr const_iterator cbegin() const noexcept { return m_values; }
			[[nodiscard]] constexpr const_iterator cend() const noexcept { return m_values + m_nb_elements; }

			[[nodiscard]] constexpr bool empty() const noexcept { return m_nb_elements == 0; }

			[[nodiscard]] constexpr size_type size() const noexcept { return m_nb_elements; }

			void clear(allocator_type &alloc) noexcept {
				destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

				m_values = nullptr;
				m_bitmap_vals = 0;
				m_bitmap_deleted_vals = 0;
				m_nb_elements = 0;
				m_capacity = 0;
			}

			[[nodiscard]] constexpr bool last() const noexcept { return m_last_array; }

			constexpr void set_as_last() noexcept { m_last_array = true; }

			[[nodiscard]] constexpr bool has_value(size_type index) const noexcept {
				tsl_sh_assert(index < BITMAP_NB_BITS);
				return (m_bitmap_vals & (bitmap_type(1) << index)) != 0;
			}

			[[nodiscard]] constexpr bool has_deleted_value(size_type index) const noexcept {
				tsl_sh_assert(index < BITMAP_NB_BITS);
				return (m_bitmap_deleted_vals & (bitmap_type(1) << index)) != 0;
			}

			iterator value(size_type index) noexcept {
				tsl_sh_assert(has_value(index));
				return m_values + index_to_offset(index);
			}

			const_iterator value(size_type index) const noexcept {
				tsl_sh_assert(has_value(index));
				return m_values + index_to_offset(index);
			}

			/**
			 * Return iterator to set value.
			 */
			template<typename... Args>
			iterator set(allocator_type &alloc, size_type index, Args &&...value_args) {
				tsl_sh_assert(!has_value(index));

				const size_type offset = index_to_offset(index);
				insert_at_offset(alloc, offset, std::forward<Args>(value_args)...);

				m_bitmap_vals = (m_bitmap_vals | (bitmap_type(1) << index));
				m_bitmap_deleted_vals = (m_bitmap_deleted_vals & ~(bitmap_type(1) << index));

				m_nb_elements++;

				tsl_sh_assert(has_value(index));
				tsl_sh_assert(!has_deleted_value(index));

				return m_values + offset;
			}

			iterator erase(allocator_type &alloc, iterator position) {
				auto const offset = static_cast<size_type>(std::distance(begin(), position));
				return erase(alloc, position, offset_to_index(offset));
			}

			// Return the next value or end if no next value
			iterator erase(allocator_type &alloc, iterator position, size_type index) {
				tsl_sh_assert(has_value(index));
				tsl_sh_assert(!has_deleted_value(index));

				auto const offset = static_cast<size_type>(std::distance(begin(), position));
				erase_at_offset(alloc, offset);

				m_bitmap_vals = (m_bitmap_vals & ~(bitmap_type(1) << index));
				m_bitmap_deleted_vals = (m_bitmap_deleted_vals | (bitmap_type(1) << index));

				m_nb_elements--;

				tsl_sh_assert(!has_value(index));
				tsl_sh_assert(has_deleted_value(index));

				return m_values + offset;
			}

			void swap(sparse_array &other) {
				using std::swap;

				swap(m_values, other.m_values);
				swap(m_bitmap_vals, other.m_bitmap_vals);
				swap(m_bitmap_deleted_vals, other.m_bitmap_deleted_vals);
				swap(m_nb_elements, other.m_nb_elements);
				swap(m_capacity, other.m_capacity);
				swap(m_last_array, other.m_last_array);
			}

		private:
			static void destroy_and_deallocate_values(allocator_type &alloc,
													  pointer values,
													  size_type nb_values,
													  size_type capacity_values) noexcept {
				for (size_type i = 0; i < nb_values; i++) {
					values[i].~value_type();
				}

				alloc_traits::deallocate(alloc, values, capacity_values);
			}

			[[nodiscard]] static constexpr size_type popcount(bitmap_type val) noexcept {
				return std::popcount(val);
			}

			[[nodiscard]] constexpr size_type index_to_offset(size_type index) const noexcept {
				tsl_sh_assert(index < BITMAP_NB_BITS);
				return popcount(m_bitmap_vals & ((bitmap_type(1) << index) - bitmap_type(1)));
			}

			// TODO optimize
			[[nodiscard]] constexpr size_type offset_to_index(size_type offset) const noexcept {
				tsl_sh_assert(offset < m_nb_elements);

				bitmap_type bitmap_vals = m_bitmap_vals;
				size_type index = 0;
				size_type nb_ones = 0;

				while (bitmap_vals != 0) {
					if ((bitmap_vals & 0x1) == 1) {
						if (nb_ones == offset) {
							break;
						}

						nb_ones++;
					}

					index++;
					bitmap_vals = bitmap_vals >> 1;
				}

				return index;
			}

			[[nodiscard]] constexpr size_type next_capacity() const noexcept {
				return static_cast<size_type>(m_capacity + CAPACITY_GROWTH_STEP);
			}

			/**
		     * Insertion
		     *
		     * Two situations:
		     * - Either we are in a situation where
		     * std::is_nothrow_move_constructible<value_type>::value is true. In this
		     * case, on insertion we just reallocate m_values when we reach its capacity
		     * (i.e. m_nb_elements == m_capacity), otherwise we just put the new value at
		     * its appropriate place. We can easily keep the strong exception guarantee as
		     * moving the values around is safe.
		     * - Otherwise we are in a situation where
		     * std::is_nothrow_move_constructible<value_type>::value is false. In this
		     * case on EACH insertion we allocate a new area of m_nb_elements + 1 where we
		     * copy the values of m_values into it and put the new value there. On
		     * success, we set m_values to this new area. Even if slower, it's the only
		     * way to preserve to strong exception guarantee.
		     */
			template<typename ...Args> requires (std::is_nothrow_move_constructible_v<value_type>)
			void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
				if (m_nb_elements < m_capacity) {
					insert_at_offset_no_realloc(offset, std::forward<Args>(value_args)...);
				} else {
					insert_at_offset_realloc(alloc, offset, next_capacity(), std::forward<Args>(value_args)...);
				}
			}

			template<typename ...Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
			void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
				insert_at_offset_realloc(alloc, offset, m_nb_elements + 1, std::forward<Args>(value_args)...);
			}

			template<typename ...Args> requires (std::is_nothrow_move_constructible_v<value_type>)
			void insert_at_offset_no_realloc(size_type offset, Args &&...value_args) {
				tsl_sh_assert(offset <= m_nb_elements);
				tsl_sh_assert(m_nb_elements < m_capacity);

				for (size_type i = m_nb_elements; i > offset; i--) {
					new (&m_values[i]) value_type{std::move(m_values[i - 1])};
					m_values[i - 1].~value_type();
				}

				try {
					new (&m_values[offset]) value_type{std::forward<Args>(value_args)...};
				} catch (...) {
					// revert
					for (size_type i = offset; i < m_nb_elements; i++) {
						new (&m_values[i]) value_type{std::move(m_values[i + 1])};
						m_values[i + 1].~value_type();
					}
					throw;
				}
			}

			template<typename ...Args> requires (std::is_nothrow_move_constructible_v<value_type>)
			void insert_at_offset_realloc(allocator_type &alloc, size_type offset,
										  size_type new_capacity, Args &&...value_args) {
				tsl_sh_assert(new_capacity > m_nb_elements);

				pointer new_values = alloc_traits::allocate(alloc, new_capacity);
				tsl_sh_assert(new_values != nullptr); // Allocate should throw if there is a failure

				try {
					new (&new_values[offset]) value_type{std::forward<Args>(value_args)...};
				} catch (...) {
					alloc_traits::deallocate(alloc, new_values, new_capacity);
					throw;
				}

				// Should not throw from here
				for (size_type i = 0; i < offset; i++) {
					new (&new_values[i]) value_type{std::move(m_values[i])};
				}

				for (size_type i = offset; i < m_nb_elements; i++) {
					new (&new_values[i + 1]) value_type{std::move(m_values[i])};
				}

				destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

				m_values = new_values;
				m_capacity = new_capacity;
			}

			template<typename ...Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
			void insert_at_offset_realloc(allocator_type &alloc, size_type offset, size_type new_capacity, Args &&...value_args) {
				tsl_sh_assert(new_capacity > m_nb_elements);

				pointer new_values = alloc_traits::allocate(alloc, new_capacity);
				tsl_sh_assert(new_values != nullptr); // Allocate should throw if there is a failure

				size_type nb_new_values = 0;
				try {
					for (size_type i = 0; i < offset; i++) {
						new (&new_values[i]) value_type{m_values[i]};
						nb_new_values++;
					}

					new (&new_values[offset]) value_type{std::forward<Args>(value_args)...};
					nb_new_values++;

					for (size_type i = offset; i < m_nb_elements; i++) {
						new (&new_values[i + 1]) value_type{m_values[i]};
						nb_new_values++;
					}
				} catch (...) {
					destroy_and_deallocate_values(alloc, new_values, nb_new_values, new_capacity);
					throw;
				}

				tsl_sh_assert(nb_new_values == m_nb_elements + 1);

				destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

				m_values = new_values;
				m_capacity = new_capacity;
			}

			/**
		     * Erasure
		     *
		     * Two situations:
		     * - Either we are in a situation where
		     * 		std::is_nothrow_move_constructible<value_type>::value is true. Simply
		     * 		destroy the value and left-shift move the value on the right of offset.
		     * - Otherwise we are in a situation where
		     * 		std::is_nothrow_move_constructible<value_type>::value is false. Copy all
		     * 		the values except the one at offset into a new heap area. On success, we
		     * 		set m_values to this new area. Even if slower, it's the only way to
		     * 		preserve to strong exception guarantee.
		     */
			template<typename... Args> requires (std::is_nothrow_move_constructible_v<value_type>)
			void erase_at_offset([[maybe_unused]] allocator_type &alloc, size_type offset) noexcept {
				tsl_sh_assert(offset < m_nb_elements);

				m_values[offset].~value_type();

				for (size_type i = offset + 1; i < m_nb_elements; ++i) {
					new (&m_values[i - 1]) value_type{std::move(m_values[i])};
					m_values[i].~value_type();
				}
			}

			template<typename... Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
			void erase_at_offset(allocator_type &alloc, size_type offset) {
				tsl_sh_assert(offset < m_nb_elements);

				if (offset + 1 == m_nb_elements) {
					// Erasing the last element, don't need to reallocate. We keep the capacity.
					m_values[offset].~value_type();
					return;
				}

				tsl_sh_assert(m_nb_elements > 1);
				auto const new_capacity = m_nb_elements - 1;

				pointer new_values = alloc_traits::allocate(alloc, new_capacity);
				tsl_sh_assert(new_values != nullptr); // Allocate should throw if there is a failure

				size_type nb_new_values = 0;
				try {
					for (size_type i = 0; i < offset; ++i) {
						new (&new_values[i]) value_type{m_values[i]};
						nb_new_values++;
					}

					for (size_type i = offset + 1; i < m_nb_elements; ++i) {
						new (&new_values[i - 1]) value_type{m_values[i]};
						nb_new_values++;
					}
				} catch (...) {
					destroy_and_deallocate_values(alloc, new_values, nb_new_values, new_capacity);
					throw;
				}

				tsl_sh_assert(nb_new_values == m_nb_elements - 1);

				destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

				m_values = new_values;
				m_capacity = new_capacity;
			}
		};

		/**
		 * Internal common class used by `sparse_map` and `sparse_set`.
		 *
		 * `ValueType` is what will be stored by `sparse_hash` (usually `std::pair<Key,
		 * T>` for map and `Key` for set).
		 *
		 * `KeySelect` should be a `FunctionObject` which takes a `ValueType` in
		 * parameter and returns a reference to the key.
		 *
		 * `ValueSelect` should be a `FunctionObject` which takes a `ValueType` in
		 * parameter and returns a reference to the value. `ValueSelect` should be void
		 * if there is no value (in a set for example).
		 *
		 * The strong exception guarantee only holds if `ExceptionSafety` is set to
		 * `dice::sh::exception_safety::strong`.
		 *
		 * `ValueType` must be nothrow move constructible and/or copy constructible.
		 * Behaviour is undefined if the destructor of `ValueType` throws.
		 *
		 *
		 * The class holds its buckets in a 2-dimensional fashion. Instead of having a
		 * linear `std::vector<bucket>` for [0, bucket_count) where each bucket stores
		 * one value, we have a `std::vector<sparse_array>` (m_sparse_buckets_data)
		 * where each `sparse_array` stores multiple values (up to
		 * `sparse_array::BITMAP_NB_BITS`). To convert a one dimensional `ibucket`
		 * position to a position in `std::vector<sparse_array>` and a position in
		 * `sparse_array`, use respectively the methods
		 * `sparse_array::sparse_ibucket(ibucket)` and
		 * `sparse_array::index_in_sparse_bucket(ibucket)`.
		 */
		template<class ValueType, class KeyValueSelect, class Hash,
				 class KeyEqual, class Allocator, dice::sparse_map::sh::growth_policy GrowthPolicy,
				 dice::sparse_map::sh::exception_safety ExceptionSafety, dice::sparse_map::sh::sparsity Sparsity,
				 dice::sparse_map::sh::probing Probing>
		class sparse_hash {
		private:
			template<typename VSel>
			struct GetMappedType {
				using type = void;
				using const_reference = void;
				using reference = void;
			};

			template<typename VSel> requires requires { typename VSel::value_type; }
			struct GetMappedType<VSel> {
				using type = typename VSel::value_type;
				using const_reference = const type &;
				using reference = type &;
			};

		public:
			template<bool IsConst>
			class sparse_iterator;

			using key_type = typename KeyValueSelect::key_type;
			using mapped_type = typename GetMappedType<KeyValueSelect>::type;
			using mapped_const_reference = typename GetMappedType<KeyValueSelect>::const_reference;
			using mapped_reference = typename GetMappedType<KeyValueSelect>::reference;
			using value_type = ValueType;
			using hasher = Hash;
			using key_equal = KeyEqual;
			using allocator_type = Allocator;
			using growth_policy = GrowthPolicy;
			using reference = value_type &;
			using const_reference = const value_type &;
			using size_type = typename std::allocator_traits<allocator_type>::size_type;
			using pointer = typename std::allocator_traits<allocator_type>::pointer;
			using const_pointer = typename std::allocator_traits<allocator_type>::const_pointer;
			using difference_type = typename std::allocator_traits<allocator_type>::difference_type;
			using iterator = sparse_iterator<false>;
			using const_iterator = sparse_iterator<true>;

		private:
			static constexpr bool has_mapped_type = !std::is_same_v<mapped_type, void>;

			using sparse_array =
					dice::sparse_map::detail_sparse_hash::sparse_array<ValueType, Allocator, Sparsity>;

			using sparse_buckets_allocator = typename std::allocator_traits<
					allocator_type>::template rebind_alloc<sparse_array>;
			using sparse_buckets_container =
					boost::container::vector<sparse_array, sparse_buckets_allocator>;

		public:
			/**
		     * The `operator*()` and `operator->()` methods return a const reference and
		     * const pointer respectively to the stored value type (`Key` for a set,
		     * `std::pair<Key, T>` for a map).
		     *
		     * In case of a map, to get a mutable reference to the value `T` associated to
		     * a key (the `.second` in the stored pair), you have to call `value()`.
		     */
			template<bool IsConst>
			class sparse_iterator {
				friend class sparse_hash;

			private:
				using sparse_bucket_iterator = std::conditional_t<IsConst,
																  typename sparse_buckets_container::const_iterator,
																  typename sparse_buckets_container::iterator>;

				using sparse_array_iterator = std::conditional_t<IsConst,
																 typename sparse_array::const_iterator,
																 typename sparse_array::iterator>;

				/**
				 * sparse_array_it should be nullptr if sparse_bucket_it ==
				 * m_sparse_buckets_data.end(). (TODO better way?)
				 */
				sparse_iterator(sparse_bucket_iterator sparse_bucket_it,
								sparse_array_iterator sparse_array_it)
					: m_sparse_buckets_it(sparse_bucket_it),
					  m_sparse_array_it(sparse_array_it) {}

			public:
				using iterator_category = std::forward_iterator_tag;
				using value_type = const typename sparse_hash::value_type;
				using difference_type = std::ptrdiff_t;
				using reference = std::conditional_t<IsConst,
													 typename KeyValueSelect::both_type const &,
													 typename KeyValueSelect::both_type &>;

				using pointer = std::conditional_t<IsConst,
												   typename std::allocator_traits<Allocator>::template rebind_traits<typename KeyValueSelect::both_type>::const_pointer,
												   typename std::allocator_traits<Allocator>::template rebind_traits<typename KeyValueSelect::both_type>::pointer>;

				sparse_iterator() noexcept {}

				// Copy constructor from iterator to const_iterator.
				sparse_iterator(const sparse_iterator<!IsConst> &other) noexcept requires (IsConst)
					: m_sparse_buckets_it(other.m_sparse_buckets_it),
					  m_sparse_array_it(other.m_sparse_array_it) {}

				sparse_iterator(const sparse_iterator &other) = default;
				sparse_iterator(sparse_iterator &&other) = default;
				sparse_iterator &operator=(const sparse_iterator &other) = default;
				sparse_iterator &operator=(sparse_iterator &&other) = default;

				reference operator*() const { return KeyValueSelect::both(*m_sparse_array_it); }

				//with fancy pointers addressof might be problematic.
				pointer operator->() const { return &KeyValueSelect::both(*m_sparse_array_it); }

				sparse_iterator &operator++() {
					tsl_sh_assert(m_sparse_array_it != nullptr);
					++m_sparse_array_it;

					//vector iterator with fancy pointers have a problem with ->
					if (m_sparse_array_it == (*m_sparse_buckets_it).end()) {
						do {
							if ((*m_sparse_buckets_it).last()) {
								++m_sparse_buckets_it;
								m_sparse_array_it = nullptr;
								return *this;
							}

							++m_sparse_buckets_it;
						} while ((*m_sparse_buckets_it).empty());

						m_sparse_array_it = (*m_sparse_buckets_it).begin();
					}

					return *this;
				}

				sparse_iterator operator++(int) {
					sparse_iterator tmp(*this);
					++*this;

					return tmp;
				}

				template<bool OIsConst>
				bool operator==(const sparse_iterator<OIsConst> &other) const noexcept {
					return m_sparse_buckets_it == other.m_sparse_buckets_it && m_sparse_array_it == other.m_sparse_array_it;
				}

				template<bool OIsConst>
				bool operator!=(const sparse_iterator &other) const noexcept {
					return m_sparse_buckets_it != other.m_sparse_buckets_it || m_sparse_array_it != other.m_sparse_array_it;
				}

			private:
				sparse_bucket_iterator m_sparse_buckets_it;
				sparse_array_iterator m_sparse_array_it;
			};

			iterator mutable_iterator(const_iterator pos) noexcept {
				// SAFETY: this is non-const therefore the underlying buckets are also non-const
				// as evidenced by the fact that we can call begin on them
				auto it_sparse_buckets = m_sparse_buckets_data.begin() + std::distance(m_sparse_buckets_data.cbegin(), pos.m_sparse_buckets_it);

				// SAFETY: this is non-const therefore the underlying sparse array is also non-const
				auto it_array = sparse_array::unsafe_mutable_iterator(pos.m_sparse_array_it);

				return iterator(it_sparse_buckets, it_array);
			}

		public:
			sparse_hash(size_type bucket_count, const Hash &hash, const KeyEqual &equal,
						const Allocator &alloc, float max_load_factor)
				: m_sparse_buckets_data(alloc),
				  //         m_sparse_buckets_data(std::allocator_traits<Allocator>::rebind_alloc<sparse_buckets_container::Allocator>(m_alloc)),
				  m_sparse_buckets(static_empty_sparse_bucket_ptr()),
				  m_bucket_count(bucket_count),
				  m_nb_elements(0),
				  m_nb_deleted_buckets(0),
				  m_alloc{alloc},
				  m_h{hash},
				  m_keq{equal},
				  m_gpol{bucket_count} {

				if (m_bucket_count > max_bucket_count()) {
					throw std::length_error("The map exceeds its maximum size.");
				}

				if (m_bucket_count > 0) {
					/*
				     * We can't use the `vector(size_type count, const Allocator& m_alloc)`
				     * constructor as it's only available in C++14 and we need to support
				     * C++11. We thus must resize after using the `vector(const Allocator&
				     * m_alloc)` constructor.
				     *
				     * We can't use `vector(size_type count, const T& value, const Allocator&
				     * m_alloc)` as it requires the value T to be copyable.
				     */
					m_sparse_buckets_data.resize(
							sparse_array::nb_sparse_buckets(bucket_count));
					m_sparse_buckets = m_sparse_buckets_data.data();

					tsl_sh_assert(!m_sparse_buckets_data.empty());
					m_sparse_buckets_data.back().set_as_last();
				}

				this->max_load_factor(max_load_factor);

				// Check in the constructor instead of outside of a function to avoid
				// compilation issues when value_type is not complete.
				static_assert(std::is_nothrow_move_constructible<value_type>::value ||
									  std::is_copy_constructible<value_type>::value,
							  "Key, and T if present, must be nothrow move constructible "
							  "and/or copy constructible.");
			}

			~sparse_hash() { clear(); }

			sparse_hash(const sparse_hash &other)
				: m_sparse_buckets_data(std::allocator_traits<Allocator>::select_on_container_copy_construction(other.m_alloc)),
				  m_bucket_count(other.m_bucket_count),
				  m_nb_elements(other.m_nb_elements),
				  m_nb_deleted_buckets(other.m_nb_deleted_buckets),
				  m_load_threshold_rehash(other.m_load_threshold_rehash),
				  m_load_threshold_clear_deleted(other.m_load_threshold_clear_deleted),
				  m_max_load_factor(other.m_max_load_factor),
				  m_alloc{std::allocator_traits<Allocator>::select_on_container_copy_construction(other.m_alloc)},
				  m_h{other.m_h},
				  m_keq{other.m_keq},
				  m_gpol{other.m_gpol} {
				copy_buckets_from(other),
						m_sparse_buckets = m_sparse_buckets_data.empty()
												   ? static_empty_sparse_bucket_ptr()
												   : m_sparse_buckets_data.data();
			}

			sparse_hash(sparse_hash &&other) noexcept(std::is_nothrow_move_constructible<Allocator>::value
													  && std::is_nothrow_move_constructible<Hash>::value
													  && std::is_nothrow_move_constructible<KeyEqual>::value
													  && std::is_nothrow_move_constructible<GrowthPolicy>::value
													  && std::is_nothrow_move_constructible<sparse_buckets_container>::value)
				: m_sparse_buckets_data(std::move(other.m_sparse_buckets_data)),
				  m_sparse_buckets(m_sparse_buckets_data.empty()
										   ? static_empty_sparse_bucket_ptr()
										   : m_sparse_buckets_data.data()),
				  m_bucket_count(other.m_bucket_count),
				  m_nb_elements(other.m_nb_elements),
				  m_nb_deleted_buckets(other.m_nb_deleted_buckets),
				  m_load_threshold_rehash(other.m_load_threshold_rehash),
				  m_load_threshold_clear_deleted(other.m_load_threshold_clear_deleted),
				  m_max_load_factor(other.m_max_load_factor),
				  m_alloc{std::move(other.m_alloc)},
				  m_h{std::move(other.m_h)},
				  m_keq{std::move(other.m_keq)},
				  m_gpol{std::move(other.m_gpol)} {
				other.m_gpol.clear();
				other.m_sparse_buckets_data.clear();
				other.m_sparse_buckets = static_empty_sparse_bucket_ptr();
				other.m_bucket_count = 0;
				other.m_nb_elements = 0;
				other.m_nb_deleted_buckets = 0;
				other.m_load_threshold_rehash = 0;
				other.m_load_threshold_clear_deleted = 0;
			}

			sparse_hash &operator=(const sparse_hash &other) {
				if (this != &other) {
					clear();

					if (std::allocator_traits<Allocator>::propagate_on_container_copy_assignment::value) {
						m_alloc = other.m_alloc;
					}

					m_h = other.m_h;
					m_keq = other.m_keq;
					m_gpol = other.m_gpol;

					if (std::allocator_traits<Allocator>::propagate_on_container_copy_assignment::value) {
						m_sparse_buckets_data = sparse_buckets_container(other.m_alloc);
					} else {
						if (m_sparse_buckets_data.size() !=
							other.m_sparse_buckets_data.size()) {
							m_sparse_buckets_data = sparse_buckets_container(m_alloc);
						} else {
							m_sparse_buckets_data.clear();
						}
					}

					copy_buckets_from(other);
					m_sparse_buckets = m_sparse_buckets_data.empty()
											   ? static_empty_sparse_bucket_ptr()
											   : m_sparse_buckets_data.data();

					m_bucket_count = other.m_bucket_count;
					m_nb_elements = other.m_nb_elements;
					m_nb_deleted_buckets = other.m_nb_deleted_buckets;
					m_load_threshold_rehash = other.m_load_threshold_rehash;
					m_load_threshold_clear_deleted = other.m_load_threshold_clear_deleted;
					m_max_load_factor = other.m_max_load_factor;
				}

				return *this;
			}

			sparse_hash &operator=(sparse_hash &&other) noexcept {
				clear();

				if (!std::allocator_traits<Allocator>::propagate_on_container_move_assignment::value && m_alloc != other.m_alloc) {
					move_buckets_from(std::move(other));
				} else {
					m_alloc = std::move(other.m_alloc);
					m_sparse_buckets_data = std::move(other.m_sparse_buckets_data);
				}

				m_sparse_buckets = m_sparse_buckets_data.empty()
										   ? static_empty_sparse_bucket_ptr()
										   : m_sparse_buckets_data.data();

				m_h = std::move(other.m_h);
				m_keq = std::move(other.m_keq);
				m_gpol = std::move(other.m_gpol);
				m_bucket_count = other.m_bucket_count;
				m_nb_elements = other.m_nb_elements;
				m_nb_deleted_buckets = other.m_nb_deleted_buckets;
				m_load_threshold_rehash = other.m_load_threshold_rehash;
				m_load_threshold_clear_deleted = other.m_load_threshold_clear_deleted;
				m_max_load_factor = other.m_max_load_factor;

				other.m_gpol.clear();
				other.m_sparse_buckets_data.clear();
				other.m_sparse_buckets = static_empty_sparse_bucket_ptr();
				other.m_bucket_count = 0;
				other.m_nb_elements = 0;
				other.m_nb_deleted_buckets = 0;
				other.m_load_threshold_rehash = 0;
				other.m_load_threshold_clear_deleted = 0;

				return *this;
			}

			allocator_type get_allocator() const {
				return static_cast<const Allocator &>(*this);
			}

			iterator begin() noexcept {
				auto begin = m_sparse_buckets_data.begin();
				//vector iterator with fancy pointers have a problem with ->
				while (begin != m_sparse_buckets_data.end() && (*begin).empty()) {
					++begin;
				}

				//vector iterator with fancy pointers have a problem with ->
				return iterator(begin, (begin != m_sparse_buckets_data.end())
											   ? (*begin).begin()
											   : nullptr);
			}

			const_iterator begin() const noexcept { return cbegin(); }

			const_iterator cbegin() const noexcept {
				auto begin = m_sparse_buckets_data.cbegin();
				//vector iterator with fancy pointers have a problem with ->
				while (begin != m_sparse_buckets_data.cend() && (*begin).empty()) {
					++begin;
				}

				return const_iterator(begin, (begin != m_sparse_buckets_data.cend())
													 ? (*begin).cbegin()
													 : nullptr);
			}

			iterator end() noexcept {
				return iterator(m_sparse_buckets_data.end(), nullptr);
			}

			const_iterator end() const noexcept { return cend(); }

			const_iterator cend() const noexcept {
				return const_iterator(m_sparse_buckets_data.cend(), nullptr);
			}

			bool empty() const noexcept { return m_nb_elements == 0; }

			size_type size() const noexcept { return m_nb_elements; }

			size_type max_size() const noexcept {
				return std::min(std::allocator_traits<Allocator>::max_size(),
								m_sparse_buckets_data.max_size());
			}

			void clear() noexcept {
				for (auto &bucket : m_sparse_buckets_data) {
					bucket.clear(m_alloc);
				}

				m_nb_elements = 0;
				m_nb_deleted_buckets = 0;
			}

			template<typename P>
			std::pair<iterator, bool> insert(P &&value) {
				return insert_impl(KeyValueSelect::key(value), std::forward<P>(value));
			}

			template<typename P>
			iterator insert_hint(const_iterator hint, P &&value) {
				if (hint != cend() &&
					m_keq(KeyValueSelect::key(*hint), KeyValueSelect::key(value))) {
					return mutable_iterator(hint);
				}

				return insert(std::forward<P>(value)).first;
			}

			template<class InputIt>
			void insert(InputIt first, InputIt last) {
				if (std::is_base_of<
							std::forward_iterator_tag,
							typename std::iterator_traits<InputIt>::iterator_category>::value) {
					const auto nb_elements_insert = std::distance(first, last);
					const size_type nb_free_buckets = m_load_threshold_rehash - size();
					tsl_sh_assert(m_load_threshold_rehash >= size());

					if (nb_elements_insert > 0 &&
						nb_free_buckets < size_type(nb_elements_insert)) {
						reserve(size() + size_type(nb_elements_insert));
					}
				}

				for (; first != last; ++first) {
					insert(*first);
				}
			}

			template<class K, class M>
			std::pair<iterator, bool> insert_or_assign(K &&key, M &&obj) {
				auto it = try_emplace(std::forward<K>(key), std::forward<M>(obj));
				if (!it.second) {
					it.first->second = std::forward<M>(obj);
				}

				return it;
			}

			template<class K, class M>
			iterator insert_or_assign(const_iterator hint, K &&key, M &&obj) {
				if (hint != cend() && m_keq(KeyValueSelect::key(*hint), key)) {
					auto it = mutable_iterator(hint);
					it->second = std::forward<M>(obj);

					return it;
				}

				return insert_or_assign(std::forward<K>(key), std::forward<M>(obj)).first;
			}

			template<class... Args>
			std::pair<iterator, bool> emplace(Args &&...args) {
				return insert(value_type(std::forward<Args>(args)...));
			}

			template<class... Args>
			iterator emplace_hint(const_iterator hint, Args &&...args) {
				return insert_hint(hint, value_type(std::forward<Args>(args)...));
			}

			template<class K, class... Args>
			std::pair<iterator, bool> try_emplace(K &&key, Args &&...args) {
				return insert_impl(key, std::piecewise_construct,
								   std::forward_as_tuple(std::forward<K>(key)),
								   std::forward_as_tuple(std::forward<Args>(args)...));
			}

			template<class K, class... Args>
			iterator try_emplace_hint(const_iterator hint, K &&key, Args &&...args) {
				if (hint != cend() && m_keq(KeyValueSelect::key(*hint), key)) {
					return mutable_iterator(hint);
				}

				return try_emplace(std::forward<K>(key), std::forward<Args>(args)...).first;
			}

			/**
			 * Here to avoid `template<class K> size_type erase(const K& key)` being used
			 * when we use an iterator instead of a const_iterator.
			 */
			iterator erase(iterator pos) {
				tsl_sh_assert(pos != end() && m_nb_elements > 0);
				//vector iterator with fancy pointers have a problem with ->
				auto it_sparse_array_next =
						(*pos.m_sparse_buckets_it).erase(m_alloc, pos.m_sparse_array_it);
				m_nb_elements--;
				m_nb_deleted_buckets++;

				if (it_sparse_array_next == (*pos.m_sparse_buckets_it).end()) {
					auto it_sparse_buckets_next = pos.m_sparse_buckets_it;
					do {
						++it_sparse_buckets_next;
					} while (it_sparse_buckets_next != m_sparse_buckets_data.end() &&
							 (*it_sparse_buckets_next).empty());

					if (it_sparse_buckets_next == m_sparse_buckets_data.end()) {
						return end();
					} else {
						return iterator(it_sparse_buckets_next,
										(*it_sparse_buckets_next).begin());
					}
				} else {
					return iterator(pos.m_sparse_buckets_it, it_sparse_array_next);
				}
			}

			iterator erase(const_iterator pos) { return erase(mutable_iterator(pos)); }

			iterator erase(const_iterator first, const_iterator last) {
				if (first == last) {
					return mutable_iterator(first);
				}

				// TODO Optimize, could avoid the call to std::distance.
				const size_type nb_elements_to_erase =
						static_cast<size_type>(std::distance(first, last));
				auto to_delete = mutable_iterator(first);
				for (size_type i = 0; i < nb_elements_to_erase; i++) {
					to_delete = erase(to_delete);
				}

				return to_delete;
			}

			template<class K>
			size_type erase(const K &key) {
				return erase(key, m_h(key));
			}

			template<class K>
			size_type erase(const K &key, std::size_t hash) {
				return erase_impl(key, hash);
			}

			void swap(sparse_hash &other) {
				using std::swap;

				if (std::allocator_traits<Allocator>::propagate_on_container_swap::value) {
					swap(m_alloc, other.m_alloc);
				} else {
					tsl_sh_assert(m_alloc == other.m_alloc);
				}

				swap(m_h, other.m_h);
				swap(m_keq, other.m_keq);
				swap(m_gpol, other.m_gpol);
				swap(m_sparse_buckets_data, other.m_sparse_buckets_data);
				swap(m_sparse_buckets, other.m_sparse_buckets);
				swap(m_bucket_count, other.m_bucket_count);
				swap(m_nb_elements, other.m_nb_elements);
				swap(m_nb_deleted_buckets, other.m_nb_deleted_buckets);
				swap(m_load_threshold_rehash, other.m_load_threshold_rehash);
				swap(m_load_threshold_clear_deleted, other.m_load_threshold_clear_deleted);
				swap(m_max_load_factor, other.m_max_load_factor);
			}

			template<class K> requires (has_mapped_type)
			mapped_reference at(const K &key) {
				return at_impl(*this, key, m_h(key));
			}

			template<class K> requires (has_mapped_type)
			mapped_reference at(const K &key, std::size_t hash) {
				return at_impl(*this, key, hash);
			}

			template<class K> requires (has_mapped_type)
			mapped_const_reference at(const K &key) const {
				return at_impl(*this, key, m_h(key));
			}

			template<class K> requires (has_mapped_type)
			mapped_const_reference at(const K &key, std::size_t hash) const {
				return at_impl(*this, key, hash);
			}

			template<class K> requires (has_mapped_type)
			mapped_reference operator[](K &&key) {
				return try_emplace(std::forward<K>(key)).first->second;
			}

			template<class K>
			bool contains(const K &key) const {
				return contains(key, m_h(key));
			}

			template<class K>
			bool contains(const K &key, std::size_t hash) const {
				return count(key, hash) != 0;
			}

			template<class K>
			size_type count(const K &key) const {
				return count(key, m_h(key));
			}

			template<class K>
			size_type count(const K &key, std::size_t hash) const {
				if (find(key, hash) != cend()) {
					return 1;
				} else {
					return 0;
				}
			}

			template<class K>
			iterator find(const K &key) {
				return find_impl(*this, key, m_h(key));
			}

			template<class K>
			iterator find(const K &key, std::size_t hash) {
				return find_impl(*this, key, hash);
			}

			template<class K>
			const_iterator find(const K &key) const {
				return find_impl(*this, key, m_h(key));
			}

			template<class K>
			const_iterator find(const K &key, std::size_t hash) const {
				return find_impl(*this, key, hash);
			}

			template<class K>
			std::pair<iterator, iterator> equal_range(const K &key) {
				return equal_range(key, m_h(key));
			}

			template<class K>
			std::pair<iterator, iterator> equal_range(const K &key, std::size_t hash) {
				iterator it = find(key, hash);
				return std::make_pair(it, (it == end()) ? it : std::next(it));
			}

			template<class K>
			std::pair<const_iterator, const_iterator> equal_range(const K &key) const {
				return equal_range(key, m_h(key));
			}

			template<class K>
			std::pair<const_iterator, const_iterator> equal_range(
					const K &key, std::size_t hash) const {
				const_iterator it = find(key, hash);
				return std::make_pair(it, (it == cend()) ? it : std::next(it));
			}

			size_type bucket_count() const { return m_bucket_count; }

			size_type max_bucket_count() const {
				return m_sparse_buckets_data.max_size();
			}

			float load_factor() const {
				if (bucket_count() == 0) {
					return 0;
				}

				return float(m_nb_elements) / float(bucket_count());
			}

			float max_load_factor() const { return m_max_load_factor; }

			void max_load_factor(float ml) {
				m_max_load_factor = std::max(0.1f, std::min(ml, 0.8f));
				m_load_threshold_rehash =
						size_type(float(bucket_count()) * m_max_load_factor);

				const float max_load_factor_with_deleted_buckets =
						m_max_load_factor + 0.5f * (1.0f - m_max_load_factor);
				tsl_sh_assert(max_load_factor_with_deleted_buckets > 0.0f &&
							  max_load_factor_with_deleted_buckets <= 1.0f);
				m_load_threshold_clear_deleted =
						size_type(float(bucket_count()) * max_load_factor_with_deleted_buckets);
			}

			void rehash(size_type count) {
				count = std::max(count,
								 size_type(std::ceil(float(size()) / max_load_factor())));
				rehash_impl(count);
			}

			void reserve(size_type count) {
				rehash(size_type(std::ceil(float(count) / max_load_factor())));
			}

			[[nodiscard]] hasher hash_function() const { return m_h; }
			[[nodiscard]] key_equal key_eq() const { return m_keq; }

		private:
			size_type bucket_for_hash(std::size_t hash) const {
				auto const bucket = m_gpol.bucket_for_hash(hash);
				tsl_sh_assert(sparse_array::sparse_ibucket(bucket) < m_sparse_buckets_data.size()
							  || (bucket == 0 && m_sparse_buckets_data.empty()));

				return bucket;
			}

			size_type next_bucket(size_type ibucket, size_type iprobe) const requires (is_power_of_two_policy<growth_policy>::value) {
				(void) iprobe;
				if (Probing == dice::sparse_map::sh::probing::linear) {
					return (ibucket + 1) & m_gpol.mask();
				} else {
					tsl_sh_assert(Probing == dice::sparse_map::sh::probing::quadratic);
					return (ibucket + iprobe) & m_gpol.mask();
				}
			}

			size_type next_bucket(size_type ibucket, size_type iprobe) const requires (!is_power_of_two_policy<growth_policy>::value) {
				(void) iprobe;
				if (Probing == dice::sparse_map::sh::probing::linear) {
					ibucket++;
					return (ibucket != bucket_count()) ? ibucket : 0;
				} else {
					tsl_sh_assert(Probing == dice::sparse_map::sh::probing::quadratic);
					ibucket += iprobe;
					return (ibucket < bucket_count()) ? ibucket : ibucket % bucket_count();
				}
			}

			// TODO encapsulate m_sparse_buckets_data to avoid the managing the allocator
			void copy_buckets_from(const sparse_hash &other) {
				m_sparse_buckets_data.reserve(other.m_sparse_buckets_data.size());

				try {
					for (const auto &bucket : other.m_sparse_buckets_data) {
						m_sparse_buckets_data.emplace_back(bucket, m_alloc);
					}
				} catch (...) {
					clear();
					throw;
				}

				tsl_sh_assert(m_sparse_buckets_data.empty() ||
							  m_sparse_buckets_data.back().last());
			}

			void move_buckets_from(sparse_hash &&other) {
				m_sparse_buckets_data.reserve(other.m_sparse_buckets_data.size());

				try {
					for (auto &&bucket : other.m_sparse_buckets_data) {
						m_sparse_buckets_data.emplace_back(std::move(bucket), m_alloc);
					}
				} catch (...) {
					clear();
					throw;
				}

				tsl_sh_assert(m_sparse_buckets_data.empty() ||
							  m_sparse_buckets_data.back().last());
			}

			template<class K, class... Args>
			std::pair<iterator, bool> insert_impl(const K &key,
												  Args &&...value_type_args) {
				if (size() >= m_load_threshold_rehash) {
					rehash_impl(m_gpol.next_bucket_count());
				} else if (size() + m_nb_deleted_buckets >=
						   m_load_threshold_clear_deleted) {
					clear_deleted_buckets();
				}
				tsl_sh_assert(!m_sparse_buckets_data.empty());

				/**
				 * We must insert the value in the first empty or deleted bucket we find. If
				 * we first find a deleted bucket, we still have to continue the search
				 * until we find an empty bucket or until we have searched all the buckets
				 * to be sure that the value is not in the hash table. We thus remember the
				 * position, if any, of the first deleted bucket we have encountered so we
				 * can insert it there if needed.
				 */
				bool found_first_deleted_bucket = false;
				std::size_t sparse_ibucket_first_deleted = 0;
				typename sparse_array::size_type index_in_sparse_bucket_first_deleted = 0;

				const std::size_t hash = m_h(key);
				std::size_t ibucket = bucket_for_hash(hash);

				std::size_t probe = 0;
				while (true) {
					std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
					auto index_in_sparse_bucket =
							sparse_array::index_in_sparse_bucket(ibucket);

					if (m_sparse_buckets != static_empty_sparse_bucket_ptr()) {
						if (m_sparse_buckets[sparse_ibucket].has_value(index_in_sparse_bucket)) {
							auto value_it =
									m_sparse_buckets[sparse_ibucket].value(index_in_sparse_bucket);
							if (m_keq(key, KeyValueSelect::key(*value_it))) {
								return std::make_pair(
										iterator(m_sparse_buckets_data.begin() + sparse_ibucket,
												 value_it),
										false);
							}
						} else if (m_sparse_buckets[sparse_ibucket].has_deleted_value(
										   index_in_sparse_bucket) &&
								   probe < m_bucket_count) {
							if (!found_first_deleted_bucket) {
								found_first_deleted_bucket = true;
								sparse_ibucket_first_deleted = sparse_ibucket;
								index_in_sparse_bucket_first_deleted = index_in_sparse_bucket;
							}
						} else if (found_first_deleted_bucket) {
							auto it = insert_in_bucket(sparse_ibucket_first_deleted,
													   index_in_sparse_bucket_first_deleted,
													   std::forward<Args>(value_type_args)...);
							m_nb_deleted_buckets--;

							return it;
						} else {
							return insert_in_bucket(sparse_ibucket, index_in_sparse_bucket,
													std::forward<Args>(value_type_args)...);
						}
					} else {
						return insert_in_bucket(sparse_ibucket, index_in_sparse_bucket,
												std::forward<Args>(value_type_args)...);
					}

					probe++;
					ibucket = next_bucket(ibucket, probe);
				}
			}

			template<class... Args>
			std::pair<iterator, bool> insert_in_bucket(
					std::size_t sparse_ibucket,
					typename sparse_array::size_type index_in_sparse_bucket,
					Args &&...value_type_args) {
				// is not called when empty
				auto value_it = m_sparse_buckets[sparse_ibucket].set(
						m_alloc, index_in_sparse_bucket, std::forward<Args>(value_type_args)...);
				m_nb_elements++;

				return std::make_pair(
						iterator(m_sparse_buckets_data.begin() + sparse_ibucket, value_it),
						true);
			}

			template<class K>
			size_type erase_impl(const K &key, std::size_t hash) {
				std::size_t ibucket = bucket_for_hash(hash);

				std::size_t probe = 0;

				if (m_sparse_buckets == static_empty_sparse_bucket_ptr())
					return 0;
				while (true) {
					const std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
					const auto index_in_sparse_bucket =
							sparse_array::index_in_sparse_bucket(ibucket);

					if (m_sparse_buckets[sparse_ibucket].has_value(index_in_sparse_bucket)) {
						auto value_it =
								m_sparse_buckets[sparse_ibucket].value(index_in_sparse_bucket);
						if (m_keq(key, KeyValueSelect::key(*value_it))) {
							m_sparse_buckets[sparse_ibucket].erase(m_alloc, value_it,
																   index_in_sparse_bucket);
							m_nb_elements--;
							m_nb_deleted_buckets++;

							return 1;
						}
					} else if (!m_sparse_buckets[sparse_ibucket].has_deleted_value(
									   index_in_sparse_bucket) ||
							   probe >= m_bucket_count) {
						return 0;
					}

					probe++;
					ibucket = next_bucket(ibucket, probe);
				}
			}

			template<typename Self, class K>
			static auto find_impl(Self &&self, const K &key, std::size_t hash) {
				static constexpr bool is_const = std::is_const_v<std::remove_reference_t<Self>>;
				std::size_t ibucket = self.bucket_for_hash(hash);

				std::size_t probe = 0;
				while (true) {
					const std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
					const auto index_in_sparse_bucket =
							sparse_array::index_in_sparse_bucket(ibucket);

					if (self.m_sparse_buckets == static_empty_sparse_bucket_ptr()) {
						return self.end();
					}
					if (self.m_sparse_buckets[sparse_ibucket].has_value(index_in_sparse_bucket)) {
						auto value_it = self.m_sparse_buckets[sparse_ibucket].value(index_in_sparse_bucket);
						if (self.m_keq(key, KeyValueSelect::key(*value_it))) {
							return sparse_iterator<is_const>{self.m_sparse_buckets_data.begin() + sparse_ibucket, value_it};
						}
					} else if (!self.m_sparse_buckets[sparse_ibucket].has_deleted_value(
									   index_in_sparse_bucket) ||
							   probe >= self.m_bucket_count) {
						return self.end();
					}

					probe++;
					ibucket = self.next_bucket(ibucket, probe);
				}
			}

			template<typename Self, typename K>
			static decltype(auto) at_impl(Self &&self, K const &key, std::size_t hash) {
				if (auto it = find_impl(self, key, hash); it != self.end()) {
					return KeyValueSelect::value(*it);
				}

				throw std::out_of_range{"Couldn't find key."};
			}

			void clear_deleted_buckets() {
				// TODO could be optimized, we could do it in-place instead of allocating a
				// new bucket array.
				rehash_impl(m_bucket_count);
				tsl_sh_assert(m_nb_deleted_buckets == 0);
			}

			template<dice::sparse_map::sh::exception_safety U = ExceptionSafety,
					 typename std::enable_if<U == dice::sparse_map::sh::exception_safety::basic>::type
							 * = nullptr>
			void rehash_impl(size_type count) {
				sparse_hash new_table(count, m_h, m_keq, m_alloc, m_max_load_factor);

				for (auto &bucket : m_sparse_buckets_data) {
					for (auto &val : bucket) {
						new_table.insert_on_rehash(std::move(val));
					}

					// TODO try to reuse some of the memory
					bucket.clear(m_alloc);
				}

				new_table.swap(*this);
			}

			/**
		     * TODO: For now we copy each element into the new map. We could move
		     * them if they are nothrow_move_constructible without triggering
		     * any exception if we reserve enough space in the sparse arrays beforehand.
		     */
			template<dice::sparse_map::sh::exception_safety U = ExceptionSafety,
					 typename std::enable_if<
							 U == dice::sparse_map::sh::exception_safety::strong>::type * = nullptr>
			void rehash_impl(size_type count) {
				sparse_hash new_table(count, m_h, m_keq, m_alloc, m_max_load_factor);

				for (const auto &bucket : m_sparse_buckets_data) {
					for (const auto &val : bucket) {
						new_table.insert_on_rehash(val);
					}
				}

				new_table.swap(*this);
			}

			template<typename K>
			void insert_on_rehash(K &&key_value) {
				const key_type &key = KeyValueSelect::key(key_value);

				const std::size_t hash = m_h(key);
				std::size_t ibucket = bucket_for_hash(hash);

				std::size_t probe = 0;
				while (true) {
					std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
					auto index_in_sparse_bucket =
							sparse_array::index_in_sparse_bucket(ibucket);

					if (!m_sparse_buckets[sparse_ibucket].has_value(index_in_sparse_bucket)) {
						m_sparse_buckets[sparse_ibucket].set(m_alloc, index_in_sparse_bucket,
															 std::forward<K>(key_value));
						m_nb_elements++;

						return;
					} else {
						tsl_sh_assert(!m_keq(
								key, KeyValueSelect::key(*m_sparse_buckets[sparse_ibucket].value(
											 index_in_sparse_bucket))));
					}

					probe++;
					ibucket = next_bucket(ibucket, probe);
				}
			}

		public:
			static constexpr size_type DEFAULT_INIT_BUCKET_COUNT = 0;
			static constexpr float DEFAULT_MAX_LOAD_FACTOR = 0.5f;

			using sparse_array_ptr = typename std::allocator_traits<allocator_type>::template rebind_traits<sparse_array>::pointer;

			/**
		     * Return an nullptr to indicate an empty bucket
		     */
			static sparse_array_ptr static_empty_sparse_bucket_ptr() {
				return {};
			}

		private:
			sparse_buckets_container m_sparse_buckets_data;


			/**
		     * Points to m_sparse_buckets_data.data() if !m_sparse_buckets_data.empty()
		     * otherwise points to static_empty_sparse_bucket_ptr. This variable is useful
		     * to avoid the cost of checking if m_sparse_buckets_data is empty when trying
		     * to find an element.
		     *
		     * TODO Remove m_sparse_buckets_data and only use a pointer instead of a
		     * pointer+vector to save some space in the sparse_hash object.
		     */
			sparse_array_ptr m_sparse_buckets;

			size_type m_bucket_count;
			size_type m_nb_elements;
			size_type m_nb_deleted_buckets;

			/**
		     * Maximum that m_nb_elements can reach before a rehash occurs automatically
		     * to grow the hash table.
		     */
			size_type m_load_threshold_rehash;

			/**
		     * Maximum that m_nb_elements + m_nb_deleted_buckets can reach before cleaning
		     * up the buckets marked as deleted.
		     */
			size_type m_load_threshold_clear_deleted;
			float m_max_load_factor;

			[[no_unique_address]] allocator_type m_alloc;
			[[no_unique_address]] hasher m_h;
			[[no_unique_address]] key_equal m_keq;
			[[no_unique_address]] growth_policy m_gpol;
		};

	}// namespace detail_sparse_hash
}// namespace dice::sparse_map

#endif
