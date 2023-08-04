#ifndef DICE_SPARSE_MAP_SPARSE_BUCKET_ARRAY_HPP
#define DICE_SPARSE_MAP_SPARSE_BUCKET_ARRAY_HPP

#include "dice/sparse-map/sparse_props.hpp"
#include "dice/sparse-map/sparse_bucket.hpp"

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
		size_type cap_ = 0;
		[[no_unique_address]] bucket_allocator_type bucket_alloc_;
		[[no_unique_address]] element_allocator_type elem_alloc_; // this allocator lives here so that the allocator management code doesn't need to be written twice

		[[nodiscard]] static constexpr size_type next_cap(size_type cap) noexcept {
			return static_cast<size_type>(static_cast<double>(cap) * 1.5);
		}

		void move_buckets_from(sparse_bucket_array &&other) {
			assert(size_ == 0);
			reserve(other.size_);

			try {
				for (auto &&bucket : other) {
					emplace_back(std::move(bucket));
				}
			} catch (...) {
				clear();
				throw;
			}
		}

		void copy_buckets_from(sparse_bucket_array const &other) {
			assert(size_ == 0);
			reserve(other.size_);

			try {
				for (auto const &bucket : other) {
					emplace_back(bucket);
				}
			} catch (...) {
				clear();
				throw;
			}
		}

		void clear_deallocate() noexcept {
			clear();
			bucket_alloc_traits::deallocate(bucket_alloc_, buckets_, cap_);
			buckets_ = nullptr;
			cap_ = 0;
		}

	public:
		explicit constexpr sparse_bucket_array(element_allocator_type const &alloc) : buckets_{nullptr},
																					  size_{0},
																					  cap_{0},
																					  bucket_alloc_{alloc},
																					  elem_alloc_{alloc} {
		}

		sparse_bucket_array(sparse_bucket_array const &other) : bucket_alloc_{bucket_alloc_traits::select_on_container_copy_construction(other.bucket_alloc_)},
																elem_alloc_{element_alloc_traits::select_on_container_copy_construction(other.elem_alloc_)} {
            reserve(other.size_);
			copy_buckets_from(other);
		}

		constexpr sparse_bucket_array(sparse_bucket_array &&other) noexcept : buckets_{std::exchange(other.buckets_, nullptr)},
																			  size_{std::exchange(other.size_, 0)},
																			  cap_{other.cap_},
																			  bucket_alloc_{std::move(other.bucket_alloc_)},
																			  elem_alloc_{std::move(other.elem_alloc_)} {
		}

		sparse_bucket_array &operator=(sparse_bucket_array const &other) {
			if (this == &other) {
				return *this;
			}

			// no need to fully realloc, either:
			// 1. alloc is not propagated
			// 2. alloc is propagated but is same as this
			// => need to fully realloc if propagate and not same as this

			if constexpr (bucket_alloc_traits::propagate_on_container_copy_assignment::value) {
				if (bucket_alloc_ != other.bucket_alloc_) {
					clear_deallocate();
					bucket_alloc_ = other.bucket_alloc_;
					elem_alloc_ = other.elem_alloc_;

					copy_buckets_from(other);
					return *this;
				}
			}

			clear();
			copy_buckets_from(other);
			return *this;
		}

		sparse_bucket_array &operator=(sparse_bucket_array &&other) noexcept {
			assert(this != &other);

			clear_deallocate();

			if constexpr (!bucket_alloc_traits::propagate_on_container_move_assignment::value) {
				if (bucket_alloc_ != other.bucket_alloc_) {
					bucket_alloc_ = std::move(other.bucket_alloc_);
					elem_alloc_ = std::move(other.elem_alloc_);

					move_buckets_from(std::move(other));
					return *this;
				}
			}

			buckets_ = std::exchange(other.buckets_, nullptr);
			size_ = std::exchange(other.size_, 0);
			cap_ = std::exchange(other.cap_, 0);

			return *this;
		}

		~sparse_bucket_array() noexcept {
			clear_deallocate();
		}

		void reserve(size_type capacity) {
			if (capacity <= cap_) {
				return;
			}

			if (capacity > max_size()) [[unlikely]] {
				throw std::length_error{"maximum possible capacity exceeded"};
			}

			pointer new_data = bucket_alloc_traits::allocate(bucket_alloc_, capacity);
			assert(new_data != nullptr);

			for (size_type ix = 0; ix < size_; ++ix) {
				new (&new_data[ix]) bucket_type{std::move(buckets_[ix])};
			}

			static_assert(std::is_trivially_destructible_v<bucket_type>);
			bucket_alloc_traits::deallocate(bucket_alloc_, buckets_, cap_);
			buckets_ = new_data;
			cap_ = capacity;
		}

		void fill(size_type size) {
			assert(size <= cap_);

			for (size_type ix = 0; ix < size; ++ix) {
				new (&buckets_[ix]) bucket_type{};
			}

			size_ = size;
		}

		void resize(size_type size) {
			reserve(size);
			fill(size);
		}

		void swap(sparse_bucket_array &other) noexcept {
			using std::swap;

			swap(buckets_, other.buckets_);
			swap(size_, other.size_);
			swap(cap_, other.cap_);

			if constexpr (bucket_alloc_traits::propagate_on_container_swap::value) {
				swap(bucket_alloc_, other.bucket_alloc_);
				swap(elem_alloc_, other.elem_alloc_);
			}
		}

		void clear_buckets() noexcept {
			for (size_type ix = 0; ix < size_; ++ix) {
				buckets_[ix].clear(elem_alloc_);
			}
		}

		void clear() noexcept {
			clear_buckets();
			size_ = 0;
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

		template<typename ...Args>
		iterator emplace_back(Args &&...args) {
			if (size_ < cap_) {
				new (std::to_address(buckets_ + size_)) bucket_type{std::forward<Args>(args)..., elem_alloc_};
				return buckets_ + size_++;
			}

			auto const new_cap = next_cap(cap_);
			pointer new_data = bucket_alloc_traits::allocate(bucket_alloc_, new_cap);

			try {
				new (&new_data[size_]) bucket_type{std::forward<Args>(args)..., elem_alloc_};
			} catch (...) {
				bucket_alloc_traits::deallocate(bucket_alloc_, new_data, new_cap);
				throw;
			}

			for (size_type ix = 0; ix < size_; ++ix) {
				new (&new_data[ix]) bucket_type{std::move(buckets_[ix])};
			}

			static_assert(std::is_trivially_destructible_v<bucket_type>);
			bucket_alloc_traits::deallocate(bucket_alloc_, buckets_, cap_);

			buckets_ = new_data;
			cap_ = new_cap;
			return buckets_ + size_++;
		}

		element_allocator_type &element_allocator() noexcept {
			return elem_alloc_;
		}
	};

} // namespace dice::sparse_map::detail

#endif//DICE_SPARSE_MAP_SPARSE_BUCKET_ARRAY_HPP
