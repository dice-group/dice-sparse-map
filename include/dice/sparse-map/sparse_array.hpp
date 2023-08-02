#ifndef DICE_SPARSE_MAP_SPARSE_ARRAY_HPP
#define DICE_SPARSE_MAP_SPARSE_ARRAY_HPP

#include "dice/sparse-map/sparse_props.hpp"

namespace dice::sparse_map::detail {

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
	template<typename T, typename Allocator, sparsity Sparsity>
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
				case sparsity::high:   return 2;
				case sparsity::medium: return 4;
				case sparsity::low:    return 8;
			}
		}();

		using bitmap_type = std::uint_least64_t;
		static constexpr std::size_t BITMAP_NB_BITS = 64;
		static constexpr std::size_t BUCKET_SHIFT = 6;

		static constexpr std::size_t BUCKET_MASK = BITMAP_NB_BITS - 1;

		static_assert(std::popcount(BITMAP_NB_BITS) == 1,
					  "BITMAP_NB_BITS must be a power of two.");
		static_assert(std::numeric_limits<bitmap_type>::digits >= BITMAP_NB_BITS,
					  "bitmap_type must be able to hold at least BITMAP_NB_BITS.");
		static_assert((std::size_t(1) << BUCKET_SHIFT) == BITMAP_NB_BITS,
					  "(1 << BUCKET_SHIFT) must be equal to BITMAP_NB_BITS.");
		static_assert(std::numeric_limits<size_type>::max() >= BITMAP_NB_BITS,
					  "size_type must be big enough to hold BITMAP_NB_BITS.");
		static_assert(std::is_unsigned<bitmap_type>::value,
					  "bitmap_type must be unsigned.");
		static_assert((std::numeric_limits<bitmap_type>::max() & BUCKET_MASK) == BITMAP_NB_BITS - 1);

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

			return std::max<std::size_t>(1, sparse_ibucket(round_up_to_power_of_2(bucket_count)));
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
			assert(m_values != nullptr);// allocate should throw if there is a failure
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

			assert(other.m_capacity >= other.m_nb_elements);
			if (m_capacity == 0) {
				return;
			}

			m_values = alloc_traits::allocate(alloc, m_capacity);
			assert(m_values != nullptr);// allocate should throw if there is a failure

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

			assert(other.m_capacity >= other.m_nb_elements);
			if (m_capacity == 0) {
				return;
			}

			m_values = alloc_traits::allocate(alloc, m_capacity);
			assert(m_values != nullptr);// allocate should throw if there is a failure

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
			assert(this != &other);

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
			assert(index < BITMAP_NB_BITS);
			return (m_bitmap_vals & (bitmap_type(1) << index)) != 0;
		}

		[[nodiscard]] constexpr bool has_deleted_value(size_type index) const noexcept {
			assert(index < BITMAP_NB_BITS);
			return (m_bitmap_deleted_vals & (bitmap_type(1) << index)) != 0;
		}

		iterator value(size_type index) noexcept {
			assert(has_value(index));
			return m_values + index_to_offset(index);
		}

		const_iterator value(size_type index) const noexcept {
			assert(has_value(index));
			return m_values + index_to_offset(index);
		}

		/**
			 * Return iterator to set value.
			 */
		template<typename... Args>
		iterator set(allocator_type &alloc, size_type index, Args &&...value_args) {
			assert(!has_value(index));

			const size_type offset = index_to_offset(index);
			insert_at_offset(alloc, offset, std::forward<Args>(value_args)...);

			m_bitmap_vals = (m_bitmap_vals | (bitmap_type(1) << index));
			m_bitmap_deleted_vals = (m_bitmap_deleted_vals & ~(bitmap_type(1) << index));

			m_nb_elements++;

			assert(has_value(index));
			assert(!has_deleted_value(index));

			return m_values + offset;
		}

		iterator erase(allocator_type &alloc, iterator position) {
			auto const offset = static_cast<size_type>(std::distance(begin(), position));
			return erase(alloc, position, offset_to_index(offset));
		}

		// Return the next value or end if no next value
		iterator erase(allocator_type &alloc, iterator position, size_type index) {
			assert(has_value(index));
			assert(!has_deleted_value(index));

			auto const offset = static_cast<size_type>(std::distance(begin(), position));
			erase_at_offset(alloc, offset);

			m_bitmap_vals = (m_bitmap_vals & ~(bitmap_type(1) << index));
			m_bitmap_deleted_vals = (m_bitmap_deleted_vals | (bitmap_type(1) << index));

			m_nb_elements--;

			assert(!has_value(index));
			assert(has_deleted_value(index));

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
			assert(index < BITMAP_NB_BITS);
			return popcount(m_bitmap_vals & ((bitmap_type(1) << index) - bitmap_type(1)));
		}

		// TODO optimize
		[[nodiscard]] constexpr size_type offset_to_index(size_type offset) const noexcept {
			assert(offset < m_nb_elements);

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
			assert(offset <= m_nb_elements);
			assert(m_nb_elements < m_capacity);

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
			assert(new_capacity > m_nb_elements);

			pointer new_values = alloc_traits::allocate(alloc, new_capacity);
			assert(new_values != nullptr); // Allocate should throw if there is a failure

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
			assert(new_capacity > m_nb_elements);

			pointer new_values = alloc_traits::allocate(alloc, new_capacity);
			assert(new_values != nullptr); // Allocate should throw if there is a failure

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

			assert(nb_new_values == m_nb_elements + 1);

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
			assert(offset < m_nb_elements);

			m_values[offset].~value_type();

			for (size_type i = offset + 1; i < m_nb_elements; ++i) {
				new (&m_values[i - 1]) value_type{std::move(m_values[i])};
				m_values[i].~value_type();
			}
		}

		template<typename... Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
		void erase_at_offset(allocator_type &alloc, size_type offset) {
			assert(offset < m_nb_elements);

			if (offset + 1 == m_nb_elements) {
				// Erasing the last element, don't need to reallocate. We keep the capacity.
				m_values[offset].~value_type();
				return;
			}

			assert(m_nb_elements > 1);
			auto const new_capacity = m_nb_elements - 1;

			pointer new_values = alloc_traits::allocate(alloc, new_capacity);
			assert(new_values != nullptr); // Allocate should throw if there is a failure

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

			assert(nb_new_values == m_nb_elements - 1);

			destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

			m_values = new_values;
			m_capacity = new_capacity;
		}
	};

} // namespace dice::sparse_map::detail

#endif//DICE_SPARSE_MAP_SPARSE_ARRAY_HPP
