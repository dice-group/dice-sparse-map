#ifndef DICE_SPARSE_MAP_SPARSE_ARRAY_HPP
#define DICE_SPARSE_MAP_SPARSE_ARRAY_HPP

#include "dice/sparse-map/sparse_props.hpp"

namespace dice::sparse_map::detail {

	/**
     * WARNING: the sparse_array_type class doesn't free the resources allocated through
     * the allocator passed in parameter in each method. You have to manually call
     * `clear(Allocator&)` when you don't need a sparse_array_type object anymore.
     *
     * The reason is that the sparse_array_type doesn't store the allocator to avoid
     * wasting space in each sparse_array_type when the allocator has a size > 0. It only
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
     * sparse_array_type. We know we can only store up to BITMAP_NB_BITS elements in the
     * array, we don't need such big types.
     *
     *
     * T must be nothrow move constructible and/or copy constructible.
     * Behaviour is undefined if the destructor of T throws an exception.
     *
     * See https://smerity.com/articles/2015/google_sparsehash.html for details on
     * the idea behinds the implementation.
     */
	template<typename T, typename Allocator, sparsity Sparsity>
	struct sparse_array {
	private:
		using alloc_traits = std::allocator_traits<Allocator>;

	public:
		using value_type = T;
		using size_type = std::uint_least8_t;
		using allocator_type = Allocator;
		using pointer = typename alloc_traits::pointer;
		using const_pointer = typename alloc_traits::const_pointer;
		using iterator = pointer;
		using const_iterator = const_pointer;

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

		static_assert(std::has_single_bit(BITMAP_NB_BITS) == 1,
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

	public:
		/**
		 * Map an ibucket [0, bucket_count) in the hash table to a sparse_ibucket
		 * (a sparse_array_type holds multiple buckets, so there is less sparse_array_type than
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
		 * sparse_array_type which corresponds to the bucket.
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

			return std::max<std::size_t>(1, sparse_ibucket(std::bit_ceil(bucket_count)));
		}

		template<typename ...Args>
		static void construct_at(allocator_type &alloc, pointer p, Args &&...args) noexcept(std::is_nothrow_constructible_v<value_type, Args &&...>) {
			alloc_traits::construct(alloc, std::to_address(p), std::forward<Args>(args)...);
		}

		static void destroy_at(allocator_type &alloc, pointer p) noexcept(std::is_nothrow_destructible_v<value_type>) {
			alloc_traits::destroy(alloc, std::to_address(p));
		}

	public:
		constexpr sparse_array() noexcept = default;

		sparse_array(size_type capacity, allocator_type &alloc) : m_capacity{capacity} {
			if (m_capacity == 0) {
				return;
			}

			m_values = alloc_traits::allocate(alloc, m_capacity);
			assert(m_values != nullptr);// allocate should throw if there is a failure
		}

		sparse_array(sparse_array const &other) = delete;

		sparse_array(sparse_array const &other, allocator_type &alloc) : m_values{nullptr},
																		 m_bitmap_vals{other.m_bitmap_vals},
																		 m_bitmap_deleted_vals{other.m_bitmap_deleted_vals},
																		 m_nb_elements{0},
																		 m_capacity{other.m_capacity} {

			assert(other.m_capacity >= other.m_nb_elements);
			if (m_capacity == 0) {
				return;
			}

			m_values = alloc_traits::allocate(alloc, m_capacity);
			assert(m_values != nullptr);// allocate should throw if there is a failure

			try {
				for (; m_nb_elements < other.m_nb_elements; ++m_nb_elements) {
					construct_at(alloc, m_values + m_nb_elements, other.m_values[m_nb_elements]);
				}
			} catch (...) {
				clear(alloc);
				throw;
			}
		}

		constexpr sparse_array(sparse_array &&other) noexcept : m_values{std::exchange(other.m_values, nullptr)},
																m_bitmap_vals{std::exchange(other.m_bitmap_vals, 0)},
																m_bitmap_deleted_vals{std::exchange(other.m_bitmap_deleted_vals, 0)},
																m_nb_elements{std::exchange(other.m_nb_elements, 0)},
																m_capacity{std::exchange(other.m_capacity, 0)} {
		}

		sparse_array(sparse_array &&other, [[maybe_unused]] allocator_type &alloc) noexcept requires (alloc_traits::is_always_equal::value)
			: sparse_array{std::move(other)} {
		}

		sparse_array(sparse_array &&other, allocator_type &alloc) requires (!alloc_traits::is_always_equal::value)
			: m_bitmap_vals{other.m_bitmap_vals},
			  m_bitmap_deleted_vals{other.m_bitmap_deleted_vals},
			  m_nb_elements{0},
			  m_capacity{other.m_capacity} {

			assert(other.m_capacity >= other.m_nb_elements);
			if (m_capacity == 0) {
				return;
			}

			m_values = alloc_traits::allocate(alloc, m_capacity);
			assert(m_values != nullptr); // allocate should throw if there is a failure

			if constexpr (std::is_trivially_copyable_v<value_type>) {
				std::memcpy(&m_values[0], &other.m_values[0], other.m_nb_elements * sizeof(value_type));
				m_nb_elements = other.m_nb_elements;
			} else if constexpr (std::is_nothrow_move_constructible_v<value_type>) {
				for (size_type i = 0; i < other.m_nb_elements; i++) {
					construct_at(alloc, &m_values[i], std::move(other.m_values[i]));
				}

				m_nb_elements = other.m_nb_elements;
			} else {
				try {
					for (; m_nb_elements < other.m_nb_elements; ++m_nb_elements) {
						construct_at(alloc, &m_values[m_nb_elements], std::move(other.m_values[m_nb_elements]));
					}
				} catch (...) {
					clear(alloc);
					throw;
				}
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


		// The code that manages the sparse_array_type must have called clear before
		// destruction. See documentation of sparse_array_type for more details.
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

		[[nodiscard]] constexpr bool has_value(size_type index) const noexcept {
			assert(index < BITMAP_NB_BITS);
			return (m_bitmap_vals & (bitmap_type{1} << index)) != 0;
		}

		[[nodiscard]] constexpr bool has_deleted_value(size_type index) const noexcept {
			assert(index < BITMAP_NB_BITS);
			return (m_bitmap_deleted_vals & (bitmap_type{1} << index)) != 0;
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
		template<typename ...Args>
		iterator set(allocator_type &alloc, size_type index, Args &&...value_args) {
			assert(!has_value(index));

			const size_type offset = index_to_offset(index);
			insert_at_offset(alloc, offset, std::forward<Args>(value_args)...);

			m_bitmap_vals |= bitmap_type{1} << index;
			m_bitmap_deleted_vals &= ~(bitmap_type{1} << index);

			m_nb_elements += 1;

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

			m_bitmap_vals &= ~(bitmap_type{1} << index);
			m_bitmap_deleted_vals |= bitmap_type{1} << index;

			m_nb_elements -= 1;

			assert(!has_value(index));
			assert(has_deleted_value(index));

			return m_values + offset;
		}

		void swap(sparse_array &other) noexcept {
			using std::swap;

			swap(m_values, other.m_values);
			swap(m_bitmap_vals, other.m_bitmap_vals);
			swap(m_bitmap_deleted_vals, other.m_bitmap_deleted_vals);
			swap(m_nb_elements, other.m_nb_elements);
			swap(m_capacity, other.m_capacity);
		}

	private:
		static void destroy_and_deallocate_values(allocator_type &alloc,
												  pointer values,
												  size_type nb_values,
												  size_type capacity_values) noexcept {
			if constexpr (!std::is_trivially_destructible_v<value_type>) {
				for (size_type i = 0; i < nb_values; i++) {
					destroy_at(alloc, &values[i]);
				}
			}

			alloc_traits::deallocate(alloc, values, capacity_values);
		}

		[[nodiscard]] constexpr size_type index_to_offset(size_type index) const noexcept {
			assert(index < BITMAP_NB_BITS);
			return std::popcount(m_bitmap_vals & ((bitmap_type{1} << index) - bitmap_type{1}));
		}

		[[nodiscard]] constexpr size_t offset_to_index(size_t offset) const noexcept {
			assert(offset < static_cast<size_t>(std::popcount(m_bitmap_vals)));

			size_type index = 0;
			bitmap_type acc = m_bitmap_vals;

			while (true) {
				size_t const ones = std::countr_one(acc);
				if (ones > offset) {
					break;
				}

				acc >>= ones;
				index += ones;
				offset -= ones;

				size_t const skip = std::countr_zero(acc);
				acc >>= skip;
				index += skip;
			}

			return index + offset;
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
				insert_at_offset_no_realloc(alloc, offset, std::forward<Args>(value_args)...);
			} else {
				insert_at_offset_realloc(alloc, offset, next_capacity(), std::forward<Args>(value_args)...);
			}
		}

		template<typename ...Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
		void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
			insert_at_offset_realloc(alloc, offset, m_nb_elements + 1, std::forward<Args>(value_args)...);
		}

		template<typename ...Args> requires (std::is_nothrow_move_constructible_v<value_type>)
		void insert_at_offset_no_realloc(allocator_type &alloc, size_type offset, Args &&...value_args) {
			assert(offset <= m_nb_elements);
			assert(m_nb_elements < m_capacity);

			if constexpr (std::is_trivially_copyable_v<value_type>) {
				std::memmove(&m_values[offset + 1], &m_values[offset], (m_nb_elements - offset) * sizeof(value_type));
			} else {
				for (size_type i = m_nb_elements; i > offset; i--) {
					construct_at(alloc, &m_values[i], std::move(m_values[i - 1]));
					destroy_at(alloc, &m_values[i - 1]);
				}
			}

			try {
				construct_at(alloc, &m_values[offset], std::forward<Args>(value_args)...);
			} catch (...) {
				// revert
				if constexpr (std::is_trivially_copyable_v<value_type>) {
					std::memmove(&m_values[offset], &m_values[offset + 1], (m_nb_elements - offset) * sizeof(value_type));
				} else {
					for (size_type i = offset; i < m_nb_elements; i++) {
						construct_at(alloc, &m_values[i], std::move(m_values[i + 1]));
						destroy_at(alloc, &m_values[i + 1]);
					}
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
				construct_at(alloc, &new_values[offset], std::forward<Args>(value_args)...);
			} catch (...) {
				alloc_traits::deallocate(alloc, new_values, new_capacity);
				throw;
			}

			if constexpr (std::is_trivially_copyable_v<value_type>) {
				if (m_values != nullptr) {
					std::memcpy(&new_values[0], &m_values[0], offset * sizeof(value_type));
					std::memcpy(&new_values[offset + 1], &m_values[offset], (m_nb_elements - offset) * sizeof(value_type));
				}
			} else {
				// Cannot throw here as per requires clause
				for (size_type i = 0; i < offset; i++) {
					construct_at(alloc, &new_values[i], std::move(m_values[i]));
				}

				for (size_type i = offset; i < m_nb_elements; i++) {
					construct_at(alloc, &new_values[i + 1], std::move(m_values[i]));
				}
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
					construct_at(alloc, &new_values[i], m_values[i]);
					nb_new_values++;
				}

				construct_at(alloc, &new_values[offset], std::forward<Args>(value_args)...);
				nb_new_values++;

				for (size_type i = offset; i < m_nb_elements; i++) {
					construct_at(alloc, &new_values[i + 1], m_values[i]);
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

			destroy_at(alloc, &m_values[offset]);

			if constexpr (std::is_trivially_copyable_v<value_type>) {
				std::memmove(&m_values[offset], &m_values[offset + 1], (m_nb_elements - offset - 1) * sizeof(value_type));
			} else {
				for (size_type i = offset + 1; i < m_nb_elements; ++i) {
					construct_at(alloc, &m_values[i - 1], std::move(m_values[i]));
					destroy_at(alloc, &m_values[i]);
				}
			}
		}

		template<typename ...Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
		void erase_at_offset(allocator_type &alloc, size_type offset) {
			assert(offset < m_nb_elements);

			if (offset + 1 == m_nb_elements) {
				// Erasing the last element, don't need to reallocate. We keep the capacity.
				destroy_at(alloc, &m_values[offset]);
				return;
			}

			assert(m_nb_elements > 1);
			auto const new_capacity = m_nb_elements - 1;

			pointer new_values = alloc_traits::allocate(alloc, new_capacity);
			assert(new_values != nullptr); // Allocate should throw if there is a failure

			size_type nb_new_values = 0;
			try {
				for (size_type i = 0; i < offset; ++i) {
					construct_at(alloc, &new_values[i], m_values[i]);
					nb_new_values++;
				}

				for (size_type i = offset + 1; i < m_nb_elements; ++i) {
					construct_at(alloc, &new_values[i - 1], m_values[i]);
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
