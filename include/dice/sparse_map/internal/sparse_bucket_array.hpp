#ifndef DICE_SPARSE_MAP_SPARSE_BUCKET_ARRAY_HPP
#define DICE_SPARSE_MAP_SPARSE_BUCKET_ARRAY_HPP

#include "../sparse_props.hpp"
#include "sparse_bucket.hpp"

namespace dice::sparse_map::detail {

	template<typename T, typename Allocator, sparsity Sparsity>
	struct sparse_bucket_array {
	private:
		using element_alloc_traits = std::allocator_traits<Allocator>;
		using bucket_alloc_traits = std::allocator_traits<Allocator>::template rebind_traits<sparse_bucket<T, Allocator, Sparsity>>;

		using bucket_allocator_type = typename bucket_alloc_traits::allocator_type;
		using element_allocator_type = typename element_alloc_traits::allocator_type;

	public:
		using bucket_type = typename bucket_alloc_traits::value_type;
		using value_type = bucket_type;
		using pointer = typename bucket_alloc_traits::pointer;
        using const_pointer = typename bucket_alloc_traits::const_pointer;
		using iterator = pointer;
		using const_iterator = const_pointer;
		using size_type = typename bucket_alloc_traits::size_type;
		using difference_type = typename bucket_alloc_traits::difference_type;
		using reference = bucket_type &;
		using const_reference = bucket_type const &;

	private:
		pointer buckets_ = nullptr;
		size_type size_ = 0;
		[[no_unique_address]] bucket_allocator_type bucket_alloc_;
		[[no_unique_address]] element_allocator_type elem_alloc_; // this allocator lives here so that the allocator management code doesn't need to be written twice

		pointer make_new_buckets(size_type new_size) {
			pointer new_buckets = bucket_alloc_traits::allocate(bucket_alloc_, new_size);
			assert(new_buckets != nullptr);

			static_assert(std::is_nothrow_default_constructible_v<bucket_type>);
			for (size_type ix = 0; ix < new_size; ++ix) {
				new (&new_buckets[ix]) bucket_type{};
			}

			return new_buckets;
		}

		void resize_drop_old(size_type new_size) {
			if (new_size <= size_) {
				return;
			}

			pointer new_buckets = make_new_buckets(new_size);
			clear_deallocate();
			buckets_ = new_buckets;
			size_ = new_size;
		}

		void move_buckets_from(sparse_bucket_array &&other) {
			resize_drop_old(other.size_);

			try {
				for (size_type ix = 0; ix < other.size_; ++ix) {
					new (&buckets_[ix]) bucket_type{std::move(other.buckets_[ix]), elem_alloc_};
				}
			} catch (...) {
				clear_deallocate();
				throw;
			}
		}

		void copy_buckets_from(sparse_bucket_array const &other) {
			resize_drop_old(other.size_);

			try {
				for (size_type ix = 0; ix < other.size_; ++ix) {
					new (&buckets_[ix]) bucket_type{other.buckets_[ix], elem_alloc_};
				}
			} catch (...) {
				clear_deallocate();
				throw;
			}
		}

		void clear_deallocate() noexcept {
			clear_buckets();
			forget_deallocate();
			buckets_ = nullptr;
			size_ = 0;
		}

	public:
		explicit constexpr sparse_bucket_array(size_type size, element_allocator_type const &alloc) : buckets_{nullptr},
																									  size_{0},
																									  bucket_alloc_{alloc},
																									  elem_alloc_{alloc} {
			if (size == 0) {
				return;
			}

			size = bucket_type::nb_sparse_buckets(size);
			buckets_ = make_new_buckets(size);
			size_ = size;
		}

		sparse_bucket_array(sparse_bucket_array const &other) : bucket_alloc_{bucket_alloc_traits::select_on_container_copy_construction(other.bucket_alloc_)},
																elem_alloc_{element_alloc_traits::select_on_container_copy_construction(other.elem_alloc_)} {
			copy_buckets_from(other);
		}

		constexpr sparse_bucket_array(sparse_bucket_array &&other) noexcept : buckets_{std::exchange(other.buckets_, nullptr)},
																			  size_{std::exchange(other.size_, 0)},
																			  bucket_alloc_{std::move(other.bucket_alloc_)},
																			  elem_alloc_{std::move(other.elem_alloc_)} {
		}

		sparse_bucket_array &operator=(sparse_bucket_array const &other) {
			if (this == &other) {
				return *this;
			}

			// can potentially reuse our existing buffer:
			// propagate  + eq  => reuse buffer
			// propagate  + neq => cannot reuse buffer
			// npropagate + eq  => reuse buffer
			// npropagate + neq => cannot reuse buffer

			if (bucket_alloc_traits::is_always_equal::value || bucket_alloc_ == other.bucket_alloc_) {
				// allocator before and after are equal
				clear_buckets();
				copy_buckets_from(other);
				return *this;
			}

			clear_deallocate();
			if constexpr (bucket_alloc_traits::propagate_on_container_copy_assignment::value) {
				bucket_alloc_ = other.bucket_alloc_;
				elem_alloc_ = other.elem_alloc_;
			}

			copy_buckets_from(other);
			return *this;
		}

		sparse_bucket_array &operator=(sparse_bucket_array &&other) noexcept {
			assert(this != &other);

			// we can always steal the other array's buffer except when we are not supposed to
			// propagate the allocator and they are not equal

			if constexpr (!bucket_alloc_traits::propagate_on_container_move_assignment::value && !bucket_alloc_traits::is_always_equal::value) {
				if (bucket_alloc_ != other.bucket_alloc_) {
					move_buckets_from(std::move(other));
					return *this;
				}
			}

			clear_deallocate();
			buckets_ = std::exchange(other.buckets_, nullptr);
			size_ = std::exchange(other.size_, 0);
			bucket_alloc_ = std::move(other.bucket_alloc_);
			elem_alloc_ = std::move(other.elem_alloc_);

			return *this;
		}

		~sparse_bucket_array() noexcept {
			for (size_type ix = 0; ix < size_; ++ix) {
				buckets_[ix].destroy_deallocate(elem_alloc_);
			}
			bucket_alloc_traits::deallocate(bucket_alloc_, buckets_, size_);
		}

		void swap(sparse_bucket_array &other) noexcept {
			using std::swap;

			static_assert(bucket_alloc_traits::propagate_on_container_swap::value,
						  "Not swapping allocators is not implemented");

			swap(buckets_, other.buckets_);
			swap(size_, other.size_);
			swap(bucket_alloc_, other.bucket_alloc_);
			swap(elem_alloc_, other.elem_alloc_);
		}

		void forget_deallocate() {
			bucket_alloc_traits::deallocate(bucket_alloc_, buckets_, size_);
			buckets_ = nullptr;
			size_ = 0;
		}

		void clear_buckets() noexcept {
			for (size_type ix = 0; ix < size_; ++ix) {
				buckets_[ix].clear(elem_alloc_);
			}
		}

		[[nodiscard]] constexpr iterator begin() noexcept { return buckets_; }
		[[nodiscard]] constexpr iterator end() noexcept { return buckets_ + size_; }
		[[nodiscard]] constexpr const_iterator begin() const noexcept { return buckets_; }
		[[nodiscard]] constexpr const_iterator end() const noexcept { return buckets_ + size_; }
		[[nodiscard]] constexpr const_iterator cbegin() const noexcept { return buckets_; }
		[[nodiscard]] constexpr const_iterator cend() const noexcept { return buckets_ + size_; }

		[[nodiscard]] constexpr bool empty() const noexcept { return size_ == 0; }
		[[nodiscard]] constexpr size_type size() const noexcept { return size_; }
		[[nodiscard]] constexpr size_type max_size() const noexcept { return bucket_alloc_traits::max_size(bucket_alloc_); };

		reference operator[](size_type const ix) noexcept {
			assert(ix < size_);
			return buckets_[ix];
		}

		const_reference operator[](size_type const ix) const noexcept {
			assert(ix < size_);
			return buckets_[ix];
		}

		element_allocator_type &element_allocator() noexcept {
			return elem_alloc_;
		}

		element_allocator_type const &element_allocator() const noexcept {
			return elem_alloc_;
		}
	};

} // namespace dice::sparse_map::detail

#endif//DICE_SPARSE_MAP_SPARSE_BUCKET_ARRAY_HPP
