#ifndef DICE_SPARSE_MAP_SPARSE_BUCKET_HPP
#define DICE_SPARSE_MAP_SPARSE_BUCKET_HPP

#include <algorithm>
#include <bit>
#include <cassert>
#include <cstring>
#include <limits>
#include <memory>

#include "../sparse_props.hpp"

namespace dice::sparse_map::internal {

	/**
     * WARNING: the sparse_bucket class doesn't free the resources allocated through
     * the allocator passed in parameter in each method. You have to manually call
     * `clear(Allocator&)` when you don't need a sparse_array_type object anymore.
     *
     * The reason is that the sparse_array_type doesn't store the allocator to avoid
     * wasting space in each sparse_array_type when the allocator has a size > 0. It only
     * allocates/deallocates objects with the allocator that is passed in parameter.
     *
     *
     *
     * Index denotes a value between [0, discriminant_bits), it is an index similar to
     * std::vector. Offset denotes the real position in `values_` corresponding to
     * an index.
     *
     * We are using raw pointers instead of std::vector to avoid loosing
     * 2*sizeof(size_t) bytes to store the capacity and size of the vector in each
     * sparse_array_type. We know we can only store up to discriminant_bits elements in the
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
	struct sparse_bucket {
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

		static constexpr size_type capacity_growth_step = []() {
			switch (Sparsity) {
				case sparsity::high:   return 2;
				case sparsity::medium: return 4;
				case sparsity::low:    return 8;
			}
		}();

		using discriminant_type = std::uint_least64_t;
		static constexpr std::size_t discriminant_bits = 64;

		static constexpr std::size_t bucket_shift = 6;
		static constexpr std::size_t bucket_mask = discriminant_bits - 1;

		static_assert(std::has_single_bit(discriminant_bits),
					  "discriminant_bits must be a power of two.");
		static_assert(std::numeric_limits<discriminant_type>::digits >= discriminant_bits,
					  "discriminant_type must be able to hold at least discriminant_bits.");
		static_assert((std::size_t(1) << bucket_shift) == discriminant_bits,
					  "(1 << bucket_shift) must be equal to discriminant_bits.");
		static_assert(std::numeric_limits<size_type>::max() >= discriminant_bits,
					  "size_type must be big enough to hold discriminant_bits.");
		static_assert(std::is_unsigned<discriminant_type>::value,
					  "discriminant_type must be unsigned.");
		static_assert((std::numeric_limits<discriminant_type>::max() & bucket_mask) == discriminant_bits - 1);

	private:
		pointer values_ = nullptr;

		discriminant_type value_discriminant_ = 0;
		discriminant_type deleted_discriminant_ = 0;

		size_type size_ = 0;
		size_type capacity_ = 0;

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
		[[nodiscard]] static constexpr std::size_t sparse_ibucket(std::size_t ibucket) noexcept {
			return ibucket >> bucket_shift;
		}

		/**
		 * Map an ibucket [0, bucket_count) in the hash table to an index in the
		 * sparse_array_type which corresponds to the bucket.
		 *
		 * The bucket ibucket is in
		 * m_sparse_buckets[sparse_ibucket(ibucket)][index_in_sparse_bucket(ibucket)]
		 * instead of something like m_buckets[ibucket] in a classical hash table.
		 */
		[[nodiscard]] static constexpr size_type index_in_sparse_bucket(std::size_t ibucket) noexcept {
			return static_cast<size_type>(ibucket & bucket_mask);
		}

		[[nodiscard]] static constexpr std::size_t nb_sparse_buckets(std::size_t bucket_count) noexcept {
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
		constexpr sparse_bucket() noexcept = default;

		sparse_bucket(sparse_bucket const &other) = delete;
		sparse_bucket(sparse_bucket &&other) = delete;
		sparse_bucket &operator=(sparse_bucket const &) = delete;
		sparse_bucket &operator=(sparse_bucket &&) = delete;

		// The code that manages the bucket must have called clear before
		// destruction. See documentation of sparse_array_type for more details.
		~sparse_bucket() noexcept = default;

		sparse_bucket(size_type capacity, allocator_type &alloc) : capacity_{capacity} {
			if (capacity_ == 0) {
				return;
			}

			values_ = alloc_traits::allocate(alloc, capacity_);
			assert(values_ != nullptr);// allocate should throw if there is a failure
		}

		sparse_bucket(sparse_bucket const &other, allocator_type &alloc) : values_{nullptr},
																		   value_discriminant_{other.value_discriminant_},
																		   deleted_discriminant_{other.deleted_discriminant_},
																		   size_{0},
																		   capacity_{other.capacity_} {

			assert(other.capacity_ >= other.size_);
			if (capacity_ == 0) {
				return;
			}

			values_ = alloc_traits::allocate(alloc, capacity_);
			assert(values_ != nullptr);// allocate should throw if there is a failure

			try {
				for (; size_ < other.size_; ++size_) {
					construct_at(alloc, values_ + size_, other.values_[size_]);
				}
			} catch (...) {
				clear(alloc);
				throw;
			}
		}

		sparse_bucket(sparse_bucket &&other, allocator_type &alloc) : value_discriminant_{other.value_discriminant_},
																	  deleted_discriminant_{other.deleted_discriminant_},
																	  size_{0},
																	  capacity_{other.capacity_} {
			// this ctor must only be called when the allocator is actually different
			// cannot check if the allocators were actually different, but the static_assert helps
			static_assert(!alloc_traits::is_always_equal::value);

			assert(other.capacity_ >= other.size_);
			if (capacity_ == 0) {
				return;
			}

			values_ = alloc_traits::allocate(alloc, capacity_);
			assert(values_ != nullptr); // allocate should throw if there is a failure

			if constexpr (std::is_trivially_copyable_v<value_type>) {
				std::memcpy(&values_[0], &other.values_[0], other.size_ * sizeof(value_type));
				size_ = other.size_;
			} else if constexpr (std::is_nothrow_move_constructible_v<value_type>) {
				for (size_type i = 0; i < other.size_; i++) {
					construct_at(alloc, &values_[i], std::move(other.values_[i]));
				}

				size_ = other.size_;
			} else {
				try {
					for (; size_ < other.size_; ++size_) {
						construct_at(alloc, &values_[size_], std::move(other.values_[size_]));
					}
				} catch (...) {
					clear(alloc);
					throw;
				}
			}

			other.clear(alloc);
		}

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

		[[nodiscard]] constexpr iterator begin() noexcept { return values_; }
		[[nodiscard]] constexpr iterator end() noexcept { return values_ + size_; }
		[[nodiscard]] constexpr const_iterator begin() const noexcept { return cbegin(); }
		[[nodiscard]] constexpr const_iterator end() const noexcept { return cend(); }
		[[nodiscard]] constexpr const_iterator cbegin() const noexcept { return values_; }
		[[nodiscard]] constexpr const_iterator cend() const noexcept { return values_ + size_; }

		[[nodiscard]] constexpr bool empty() const noexcept { return size_ == 0; }

		[[nodiscard]] constexpr size_type size() const noexcept { return size_; }

		void destroy_deallocate(allocator_type &alloc) noexcept(std::is_nothrow_destructible_v<value_type>) {
			destroy_and_deallocate_values(alloc, values_, size_, capacity_);
		}

		void clear(allocator_type &alloc) noexcept(std::is_nothrow_destructible_v<value_type>) {
			destroy_deallocate(alloc);

			values_ = nullptr;
			value_discriminant_ = 0;
			deleted_discriminant_ = 0;
			size_ = 0;
			capacity_ = 0;
		}

		[[nodiscard]] constexpr bool has_value(size_type index) const noexcept {
			assert(index < discriminant_bits);
			return (value_discriminant_ & (discriminant_type{1} << index)) != 0;
		}

		[[nodiscard]] constexpr bool has_deleted_value(size_type index) const noexcept {
			assert(index < discriminant_bits);
			return (deleted_discriminant_ & (discriminant_type{1} << index)) != 0;
		}

		[[nodiscard]] iterator value(size_type index) noexcept {
			assert(has_value(index));
			return values_ + index_to_offset(index);
		}

		[[nodiscard]] const_iterator value(size_type index) const noexcept {
			assert(has_value(index));
			return values_ + index_to_offset(index);
		}

		/**
		 * Return iterator to set value.
		 */
		template<typename ...Args>
		iterator set(allocator_type &alloc, size_type index, Args &&...value_args) {
			assert(!has_value(index));

			const size_type offset = index_to_offset(index);
			insert_at_offset(alloc, offset, std::forward<Args>(value_args)...);

			value_discriminant_ |= discriminant_type{1} << index;
			deleted_discriminant_ &= ~(discriminant_type{1} << index);

			size_ += 1;

			assert(has_value(index));
			assert(!has_deleted_value(index));

			return values_ + offset;
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

			value_discriminant_ &= ~(discriminant_type{1} << index);
			deleted_discriminant_ |= discriminant_type{1} << index;

			size_ -= 1;

			assert(!has_value(index));
			assert(has_deleted_value(index));

			return values_ + offset;
		}

		void swap(sparse_bucket &other) noexcept {
			using std::swap;

			swap(values_, other.values_);
			swap(value_discriminant_, other.value_discriminant_);
			swap(deleted_discriminant_, other.deleted_discriminant_);
			swap(size_, other.size_);
			swap(capacity_, other.capacity_);
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
			assert(index < discriminant_bits);
			return std::popcount(value_discriminant_ & ((discriminant_type{1} << index) - discriminant_type{1}));
		}

		[[nodiscard]] constexpr size_t offset_to_index(size_t offset) const noexcept {
			assert(offset < static_cast<size_t>(std::popcount(value_discriminant_)));

			size_type index = 0;
			discriminant_type acc = value_discriminant_;

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
			return static_cast<size_type>(capacity_ + capacity_growth_step);
		}

		/**
         * Insertion
         *
         * Two situations:
         * - Either we are in a situation where
         * std::is_nothrow_move_constructible<value_type>::value is true. In this
         * case, on insertion we just reallocate values_ when we reach its capacity
         * (i.e. size_ == capacity_), otherwise we just put the new value at
         * its appropriate place. We can easily keep the strong exception guarantee as
         * moving the values around is safe.
         * - Otherwise we are in a situation where
         * std::is_nothrow_move_constructible<value_type>::value is false. In this
         * case on EACH insertion we allocate a new area of size_ + 1 where we
         * copy the values of values_ into it and put the new value there. On
         * success, we set values_ to this new area. Even if slower, it's the only
         * way to preserve to strong exception guarantee.
         */
		template<typename ...Args> requires (std::is_nothrow_move_constructible_v<value_type>)
		void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
			if (size_ < capacity_) {
				insert_at_offset_no_realloc(alloc, offset, std::forward<Args>(value_args)...);
			} else {
				insert_at_offset_realloc(alloc, offset, next_capacity(), std::forward<Args>(value_args)...);
			}
		}

		template<typename ...Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
		void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
			insert_at_offset_realloc(alloc, offset, size_ + 1, std::forward<Args>(value_args)...);
		}

		template<typename ...Args> requires (std::is_nothrow_move_constructible_v<value_type>)
		void insert_at_offset_no_realloc(allocator_type &alloc, size_type offset, Args &&...value_args) {
			assert(offset <= size_);
			assert(size_ < capacity_);

			if constexpr (std::is_trivially_copyable_v<value_type>) {
				std::memmove(&values_[offset + 1], &values_[offset], (size_ - offset) * sizeof(value_type));
			} else {
				for (size_type i = size_; i > offset; i--) {
					construct_at(alloc, &values_[i], std::move(values_[i - 1]));
					destroy_at(alloc, &values_[i - 1]);
				}
			}

			try {
				construct_at(alloc, &values_[offset], std::forward<Args>(value_args)...);
			} catch (...) {
				// revert
				if constexpr (std::is_trivially_copyable_v<value_type>) {
					std::memmove(&values_[offset], &values_[offset + 1], (size_ - offset) * sizeof(value_type));
				} else {
					for (size_type i = offset; i < size_; i++) {
						construct_at(alloc, &values_[i], std::move(values_[i + 1]));
						destroy_at(alloc, &values_[i + 1]);
					}
				}

				throw;
			}
		}

		template<typename ...Args> requires (std::is_nothrow_move_constructible_v<value_type>)
		void insert_at_offset_realloc(allocator_type &alloc, size_type offset,
									  size_type new_capacity, Args &&...value_args) {
			assert(new_capacity > size_);

			pointer new_values = alloc_traits::allocate(alloc, new_capacity);
			assert(new_values != nullptr); // Allocate should throw if there is a failure

			try {
				construct_at(alloc, &new_values[offset], std::forward<Args>(value_args)...);
			} catch (...) {
				alloc_traits::deallocate(alloc, new_values, new_capacity);
				throw;
			}

			if constexpr (std::is_trivially_copyable_v<value_type>) {
				if (values_ != nullptr) {
					std::memcpy(&new_values[0], &values_[0], offset * sizeof(value_type));
					std::memcpy(&new_values[offset + 1], &values_[offset], (size_ - offset) * sizeof(value_type));
				}
			} else {
				// Cannot throw here as per requires clause
				for (size_type i = 0; i < offset; i++) {
					construct_at(alloc, &new_values[i], std::move(values_[i]));
				}

				for (size_type i = offset; i < size_; i++) {
					construct_at(alloc, &new_values[i + 1], std::move(values_[i]));
				}
			}

			destroy_and_deallocate_values(alloc, values_, size_, capacity_);

			values_ = new_values;
			capacity_ = new_capacity;
		}

		template<typename ...Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
		void insert_at_offset_realloc(allocator_type &alloc, size_type offset, size_type new_capacity, Args &&...value_args) {
			assert(new_capacity > size_);

			pointer new_values = alloc_traits::allocate(alloc, new_capacity);
			assert(new_values != nullptr); // Allocate should throw if there is a failure

			size_type nb_new_values = 0;
			try {
				for (size_type i = 0; i < offset; i++) {
					construct_at(alloc, &new_values[i], values_[i]);
					nb_new_values++;
				}

				construct_at(alloc, &new_values[offset], std::forward<Args>(value_args)...);
				nb_new_values++;

				for (size_type i = offset; i < size_; i++) {
					construct_at(alloc, &new_values[i + 1], values_[i]);
					nb_new_values++;
				}
			} catch (...) {
				destroy_and_deallocate_values(alloc, new_values, nb_new_values, new_capacity);
				throw;
			}

			assert(nb_new_values == size_ + 1);

			destroy_and_deallocate_values(alloc, values_, size_, capacity_);

			values_ = new_values;
			capacity_ = new_capacity;
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
         * 		set values_ to this new area. Even if slower, it's the only way to
         * 		preserve to strong exception guarantee.
         */
		template<typename... Args> requires (std::is_nothrow_move_constructible_v<value_type>)
		void erase_at_offset([[maybe_unused]] allocator_type &alloc, size_type offset) noexcept {
			assert(offset < size_);

			destroy_at(alloc, &values_[offset]);

			if constexpr (std::is_trivially_copyable_v<value_type>) {
				std::memmove(&values_[offset], &values_[offset + 1], (size_ - offset - 1) * sizeof(value_type));
			} else {
				for (size_type i = offset + 1; i < size_; ++i) {
					construct_at(alloc, &values_[i - 1], std::move(values_[i]));
					destroy_at(alloc, &values_[i]);
				}
			}
		}

		template<typename ...Args> requires (!std::is_nothrow_move_constructible_v<value_type>)
		void erase_at_offset(allocator_type &alloc, size_type offset) {
			assert(offset < size_);

			if (offset + 1 == size_) {
				// Erasing the last element, don't need to reallocate. We keep the capacity.
				destroy_at(alloc, &values_[offset]);
				return;
			}

			assert(size_ > 1);
			auto const new_capacity = size_ - 1;

			pointer new_values = alloc_traits::allocate(alloc, new_capacity);
			assert(new_values != nullptr); // Allocate should throw if there is a failure

			size_type nb_new_values = 0;
			try {
				for (size_type i = 0; i < offset; ++i) {
					construct_at(alloc, &new_values[i], values_[i]);
					nb_new_values++;
				}

				for (size_type i = offset + 1; i < size_; ++i) {
					construct_at(alloc, &new_values[i - 1], values_[i]);
					nb_new_values++;
				}
			} catch (...) {
				destroy_and_deallocate_values(alloc, new_values, nb_new_values, new_capacity);
				throw;
			}

			assert(nb_new_values == size_ - 1);

			destroy_and_deallocate_values(alloc, values_, size_, capacity_);

			values_ = new_values;
			capacity_ = new_capacity;
		}
	};

} // namespace dice::sparse_map::internal

#endif//DICE_SPARSE_MAP_SPARSE_BUCKET_HPP
