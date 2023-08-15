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

#include "sparse_bucket.hpp"
#include "sparse_bucket_array.hpp"
#include "../sparse_growth_policy.hpp"

namespace dice::sparse_map::internal {
	template<typename U>
	struct is_power_of_two_policy : std::false_type {
	};

	template<std::size_t GrowthFactor>
	struct is_power_of_two_policy<power_of_two_growth_policy<GrowthFactor>> : std::true_type {
	};

	/**
	 * Internal common class used by `sparse_map` and `sparse_set`.
	 *
	 * `ValueType` is what will be stored by `sparse_hash` (usually `std::pair<Key,
	 * T>` for map and `Key` for set).
	 *
	 * `k_select` should be a `FunctionObject` which takes a `ValueType` in
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
	 * one value, we have a `std::vector<sparse_array_type>` (buckets_)
	 * where each `sparse_array_type` stores multiple values (up to
	 * `sparse_array_type::discriminant_bits`). To convert a one dimensional `ibucket`
	 * position to a position in `std::vector<sparse_array_type>` and a position in
	 * `sparse_array_type`, use respectively the methods
	 * `sparse_array_type::sparse_ibucket(ibucket)` and
	 * `sparse_array_type::index_in_sparse_bucket(ibucket)`.
	 */
	template<typename ValueType,
			 typename KeyValueSelect,
			 typename Hash,
			 typename KeyEqual,
			 typename Allocator,
			 growth_policy GrowthPolicy,
			 exception_safety ExceptionSafety,
			 sparsity Sparsity,
			 probing Probing,
			 ratio MaxLoadFactor>
	struct sparse_hash {
	private:
		template<typename VSel>
		struct get_mapped_type {
			using type = void;
			using const_reference = void;
			using reference = void;
		};

		template<typename VSel> requires requires { typename VSel::value_type; }
		struct get_mapped_type<VSel> {
			using type = typename VSel::value_type;
			using const_reference = type const &;
			using reference = type &;
		};

	public:
		template<bool IsConst>
		struct sparse_iterator;

		using key_type = typename KeyValueSelect::key_type;
		using mapped_type = typename get_mapped_type<KeyValueSelect>::type;
		using mapped_const_reference = typename get_mapped_type<KeyValueSelect>::const_reference;
		using mapped_reference = typename get_mapped_type<KeyValueSelect>::reference;
		using value_type = ValueType;
		using hasher = Hash;
		using key_equal = KeyEqual;
		using allocator_type = Allocator;
		using growth_policy = GrowthPolicy;
		using reference = value_type &;
		using const_reference = value_type const &;
		using size_type = typename std::allocator_traits<allocator_type>::size_type;
		using pointer = typename std::allocator_traits<allocator_type>::pointer;
		using const_pointer = typename std::allocator_traits<allocator_type>::const_pointer;
		using difference_type = typename std::allocator_traits<allocator_type>::difference_type;
		using iterator = sparse_iterator<false>;
		using const_iterator = sparse_iterator<true>;

		static constexpr size_type default_init_bucket_count = 0;

		static constexpr float max_load_factor = static_cast<float>(MaxLoadFactor::num) / static_cast<float>(MaxLoadFactor::den);
		static_assert(max_load_factor >= 0.1f && max_load_factor <= 0.8f,
					  "Specified invalid MaxLoadFactor, must be in range [0.1, 0.8]");

	private:
		[[nodiscard]] static constexpr size_type calc_load_threshold_rehash(size_type bucket_count) noexcept {
			return size_type(float(bucket_count) * max_load_factor);
		}

		[[nodiscard]] static constexpr size_type calc_load_threshold_clear_deleted(size_type bucket_count) noexcept {
			float const max_load_factor_with_deleted_buckets = max_load_factor + 0.5f * (1.0f - max_load_factor);
			assert(max_load_factor_with_deleted_buckets > 0.0f && max_load_factor_with_deleted_buckets <= 1.0f);

			return size_type(float(bucket_count) * max_load_factor_with_deleted_buckets);
		}


		static constexpr bool has_mapped_type = !std::is_same_v<mapped_type, void>;

		using sparse_bucket_array_type = sparse_bucket_array<value_type, allocator_type, Sparsity>;
		using sparse_bucket_type = typename sparse_bucket_array_type::bucket_type;

	private:
		growth_policy gpol_;

		sparse_bucket_array_type buckets_;
		size_type bucket_count_;

		size_type n_elements_;
		size_type n_deleted_elements_;

		/**
		 * Maximum that size_ can reach before a rehash occurs automatically
		 * to grow the hash table.
		 */
		size_type load_threshold_rehash_;

		/**
		 * Maximum that size_ + n_deleted_elements_ can reach before cleaning
		 * up the buckets marked as deleted.
		 */
		size_type load_threshold_clear_deleted_;

		[[no_unique_address]] hasher h_;
		[[no_unique_address]] key_equal keq_;

	public:
		template<bool IsConst>
		struct sparse_iterator {
		private:
			friend sparse_hash;

			using sparse_bucket_array_iterator = std::conditional_t<IsConst,
																	typename sparse_bucket_array_type::const_iterator,
																	typename sparse_bucket_array_type::iterator>;

			using sparse_bucket_iterator = std::conditional_t<IsConst,
															  typename sparse_bucket_type::const_iterator,
															  typename sparse_bucket_type::iterator>;

		private:
			sparse_bucket_array_iterator cur_bucket_;
			sparse_bucket_array_iterator end_bucket_;
			sparse_bucket_iterator bucket_it_;

		private:
			/**
			 * sparse_array_it should be nullptr if sparse_bucket_it ==
			 * buckets_.end(). (TODO better way?)
			 */
			sparse_iterator(sparse_bucket_array_iterator bucket,
							sparse_bucket_array_iterator end_bucket,
							sparse_bucket_iterator bucket_it) noexcept : cur_bucket_{bucket},
																		 end_bucket_{end_bucket},
																		 bucket_it_{bucket_it} {

				assert((cur_bucket_ == end_bucket_ && bucket_it_ == nullptr)
					   || (cur_bucket_ != end_bucket_ && bucket_it_ != nullptr));
			}

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

			// Copy constructor from iterator to const_iterator.
			sparse_iterator(sparse_iterator<!IsConst> const &other) noexcept requires (IsConst) : cur_bucket_{other.cur_bucket_},
				  end_bucket_{other.end_bucket_},
				  bucket_it_{other.bucket_it_} {
			}

			sparse_iterator(sparse_iterator const &other) noexcept = default;
			sparse_iterator(sparse_iterator &&other) noexcept = default;
			sparse_iterator &operator=(sparse_iterator const &other) noexcept = default;
			sparse_iterator &operator=(sparse_iterator &&other) noexcept = default;

			reference operator*() const noexcept { return KeyValueSelect::both(*bucket_it_); }
			pointer operator->() const noexcept { return &KeyValueSelect::both(*bucket_it_); }

			sparse_iterator &operator++() noexcept {
				assert(bucket_it_ != nullptr);
				++bucket_it_;

				if (bucket_it_ != (*cur_bucket_).end()) {
					return *this;
				}

				do {
					if (++cur_bucket_ == end_bucket_) {
						bucket_it_ = nullptr;
						return *this;
					}
				} while ((*cur_bucket_).empty());

				bucket_it_ = (*cur_bucket_).begin();
				return *this;
			}

			sparse_iterator operator++(int) noexcept {
				auto tmp = *this;
				++*this;
				return tmp;
			}

			template<bool OIsConst>
			bool operator==(sparse_iterator<OIsConst> const &other) const noexcept {
				return cur_bucket_ == other.cur_bucket_ && bucket_it_ == other.bucket_it_;
			}

			template<bool OIsConst>
			bool operator!=(sparse_iterator<OIsConst> const &other) const noexcept {
				return cur_bucket_ != other.cur_bucket_ || bucket_it_ != other.bucket_it_;
			}

			template<bool OIsConst>
			bool operator<(sparse_iterator<OIsConst> const &other) const noexcept {
				return cur_bucket_ < other.cur_bucket_ || (cur_bucket_ == other.cur_bucket_ && bucket_it_ < other.bucket_it_);
			}
		};

		iterator mutable_iterator(const_iterator pos) noexcept {
			// SAFETY: this is non-const therefore the underlying buckets are also non-const
			// as evidenced by the fact that we can call begin on them
			auto it_sparse_buckets = buckets_.begin() + std::distance(buckets_.cbegin(), pos.cur_bucket_);

			// SAFETY: this is non-const therefore the underlying sparse array is also non-const
			auto it_array = sparse_bucket_type::unsafe_mutable_iterator(pos.bucket_it_);

			return iterator{it_sparse_buckets, buckets_.end(), it_array};
		}

	public:
		sparse_hash(size_type bucket_count,
					hasher const &hash,
					key_equal const &equal,
					allocator_type const &alloc) : gpol_{bucket_count},
												   buckets_{bucket_count, alloc},
												   bucket_count_{bucket_count},
												   n_elements_{0},
												   n_deleted_elements_{0},
												   load_threshold_rehash_{calc_load_threshold_rehash(bucket_count)},
												   load_threshold_clear_deleted_{calc_load_threshold_clear_deleted(bucket_count)},
												   h_{hash},
												   keq_{equal} {

			// Check in the constructor instead of outside of a function to avoid
			// compilation issues when value_type is not complete.
			static_assert(std::is_nothrow_move_constructible<value_type>::value ||
								  std::is_copy_constructible<value_type>::value,
						  "Key, and T if present, must be nothrow move constructible "
						  "and/or copy constructible.");
		}

		sparse_hash(sparse_hash const &other) = default;
		sparse_hash &operator=(sparse_hash const &other) = default;

		sparse_hash(sparse_hash &&other) noexcept : gpol_{std::move(other.gpol_)},
													buckets_{std::move(other.buckets_)},
													bucket_count_{std::exchange(other.bucket_count_, 0)},
													n_elements_{std::exchange(other.n_elements_, 0)},
													n_deleted_elements_{std::exchange(other.n_deleted_elements_, 0)},
													load_threshold_rehash_{std::exchange(other.load_threshold_rehash_, 0)},
													load_threshold_clear_deleted_{std::exchange(other.load_threshold_clear_deleted_, 0)},
													h_{std::move(other.h_)},
													keq_{std::move(other.keq_)} {
			other.gpol_.clear();
		}

		sparse_hash &operator=(sparse_hash &&other) noexcept {
			assert(this != &other);

			gpol_ = std::move(other.gpol_);
			other.gpol_.clear();

			buckets_ = std::move(other.buckets_);
			bucket_count_ = std::exchange(other.bucket_count_, 0);
			n_elements_ = std::exchange(other.n_elements_, 0);
			n_deleted_elements_ = std::exchange(other.n_deleted_elements_, 0);
			load_threshold_rehash_ = std::exchange(other.load_threshold_rehash_, 0);
			load_threshold_clear_deleted_ = std::exchange(other.load_threshold_clear_deleted_, 0);

			h_ = std::move(other.h_);
			keq_ = std::move(other.keq_);

			return *this;
		}

		~sparse_hash() = default;

		allocator_type get_allocator() const {
			return buckets_.element_allocator();
		}

		[[nodiscard]] iterator begin() noexcept {
			auto begin = buckets_.begin();
			while (begin != buckets_.end() && begin->empty()) {
				++begin;
			}

			return iterator{begin,
							buckets_.end(),
							begin != buckets_.end() ? begin->begin() : nullptr};
		}

		[[nodiscard]] const_iterator begin() const noexcept {
			return cbegin();
		}

		[[nodiscard]] const_iterator cbegin() const noexcept {
			auto begin = buckets_.begin();
			while (begin != buckets_.end() && begin->empty()) {
				++begin;
			}

			return const_iterator{begin,
								  buckets_.end(),
								  begin != buckets_.end() ? begin->begin() : nullptr};
		}

		[[nodiscard]] iterator end() noexcept {
			return iterator{buckets_.end(),
							buckets_.end(),
							nullptr};
		}

		[[nodiscard]] const_iterator end() const noexcept {
			return cend();
		}

		[[nodiscard]] const_iterator cend() const noexcept {
			return const_iterator{buckets_.end(),
								  buckets_.end(),
								  nullptr};
		}

		[[nodiscard]] bool empty() const noexcept { return n_elements_ == 0; }

		[[nodiscard]] size_type size() const noexcept { return n_elements_; }

		[[nodiscard]] size_type max_size() const noexcept {
			return std::min(std::allocator_traits<Allocator>::max_size(),
							buckets_.max_size());
		}

		void clear() noexcept {
			buckets_.clear_buckets();
			n_elements_ = 0;
			n_deleted_elements_ = 0;
		}

		template<typename P>
		std::pair<iterator, bool> insert(P &&value) {
			return insert_impl(KeyValueSelect::key(value), std::forward<P>(value));
		}

		template<typename P>
		iterator insert_hint(const_iterator hint, P &&value) {
			if (hint != cend() && keq_(KeyValueSelect::key(*hint), KeyValueSelect::key(value))) {
				return mutable_iterator(hint);
			}

			return insert(std::forward<P>(value)).first;
		}

		template<typename InputIt>
		void insert(InputIt first, InputIt last) {
			if constexpr (std::is_base_of_v<std::forward_iterator_tag, typename std::iterator_traits<InputIt>::iterator_category>) {
				const auto nb_elements_insert = std::distance(first, last);
				const size_type nb_free_buckets = load_threshold_rehash_ - size();
				assert(load_threshold_rehash_ >= size());

				if (nb_elements_insert > 0 &&
					nb_free_buckets < size_type(nb_elements_insert)) {
					reserve(size() + size_type(nb_elements_insert));
				}
			}

			for (; first != last; ++first) {
				insert(*first);
			}
		}

		template<typename K, typename M>
		std::pair<iterator, bool> insert_or_assign(K &&key, M &&obj) {
			auto it = try_emplace(std::forward<K>(key), std::forward<M>(obj));
			if (!it.second) {
				it.first->second = std::forward<M>(obj);
			}

			return it;
		}

		template<typename K, typename M>
		iterator insert_or_assign(const_iterator hint, K &&key, M &&obj) {
			if (hint != cend() && keq_(KeyValueSelect::key(*hint), key)) {
				auto it = mutable_iterator(hint);
				it->second = std::forward<M>(obj);

				return it;
			}

			return insert_or_assign(std::forward<K>(key), std::forward<M>(obj)).first;
		}

		template<typename ...Args>
		std::pair<iterator, bool> emplace(Args &&...args) {
			return insert(value_type(std::forward<Args>(args)...));
		}

		template<typename ...Args>
		iterator emplace_hint(const_iterator hint, Args &&...args) {
			return insert_hint(hint, value_type(std::forward<Args>(args)...));
		}

		template<typename K, typename ...Args>
		std::pair<iterator, bool> try_emplace(K &&key, Args &&...args) {
			return insert_impl(key, std::piecewise_construct,
							   std::forward_as_tuple(std::forward<K>(key)),
							   std::forward_as_tuple(std::forward<Args>(args)...));
		}

		template<typename K, typename ...Args>
		iterator try_emplace_hint(const_iterator hint, K &&key, Args &&...args) {
			if (hint != cend() && keq_(KeyValueSelect::key(*hint), key)) {
				return mutable_iterator(hint);
			}

			return try_emplace(std::forward<K>(key), std::forward<Args>(args)...).first;
		}

		/**
		 * Here to avoid `template<class K> size_type erase(const K& key)` being used
		 * when we use an iterator instead of a const_iterator.
		 */
		iterator erase(iterator pos) {
			assert(pos != end() && n_elements_ > 0);
			//vector iterator with fancy pointers have a problem with ->
			auto next_bucket_it = pos.cur_bucket_->erase(buckets_.element_allocator(), pos.bucket_it_);
			n_elements_--;
			n_deleted_elements_++;

			if (next_bucket_it != pos.cur_bucket_->end()) {
				return iterator{pos.cur_bucket_,
								buckets_.end(),
								next_bucket_it};
			}

			auto it_sparse_buckets_next = pos.cur_bucket_;
			do {
				++it_sparse_buckets_next;
			} while (it_sparse_buckets_next != buckets_.end()
					 && (*it_sparse_buckets_next).empty());

			if (it_sparse_buckets_next == buckets_.end()) {
				return end();
			} else {
				return iterator{it_sparse_buckets_next,
								buckets_.end(),
								it_sparse_buckets_next->begin()};
			}
		}

		iterator erase(const_iterator pos) {
			return erase(mutable_iterator(pos));
		}

		[[deprecated("This function is originally from tsl::sparse_hash but it doesn't really make much sense to use, because\n"
					 "the items between two iterators are effectively random (they are not sorted or anything) so you don't know what you were deleting "
					 "(except if you manually checked all of them, in which case you could have just deleted them then)\n"
					 "So this function is effectively just: 'please erase distance(first, last) number of random elements'")]]
		iterator erase(const_iterator first, const_iterator last) {
			auto const nb_elements_to_erase = static_cast<size_type>(std::distance(first, last));
			auto to_delete = mutable_iterator(first);
			for (size_type i = 0; i < nb_elements_to_erase; ++i) {
				to_delete = erase(to_delete);
			}

			return to_delete;
		}

		template<typename K>
		size_type erase(K const &key) {
			return erase(key, h_(key));
		}

		template<typename K>
		size_type erase(K const &key, std::size_t hash) {
			return erase_impl(key, hash);
		}

		void swap(sparse_hash &other) noexcept {
			using std::swap;

			swap(buckets_, other.buckets_);
			swap(bucket_count_, other.bucket_count_);
			swap(n_elements_, other.n_elements_);
			swap(n_deleted_elements_, other.n_deleted_elements_);
			swap(load_threshold_rehash_, other.load_threshold_rehash_);
			swap(load_threshold_clear_deleted_, other.load_threshold_clear_deleted_);
			swap(h_, other.h_);
			swap(keq_, other.keq_);
			swap(gpol_, other.gpol_);
		}

		template<typename K> requires (has_mapped_type)
		mapped_reference at(K const &key) {
			return at_impl(*this, key, h_(key));
		}

		template<typename K> requires (has_mapped_type)
		mapped_reference at(K const &key, std::size_t hash) {
			return at_impl(*this, key, hash);
		}

		template<typename K> requires (has_mapped_type)
		mapped_const_reference at(K const &key) const {
			return at_impl(*this, key, h_(key));
		}

		template<typename K> requires (has_mapped_type)
		mapped_const_reference at(K const &key, std::size_t hash) const {
			return at_impl(*this, key, hash);
		}

		template<typename K> requires (has_mapped_type)
		mapped_reference operator[](K &&key) {
			return try_emplace(std::forward<K>(key)).first->second;
		}

		template<typename K>
		[[nodiscard]] bool contains(K const &key) const noexcept {
			return find(key, h_(key)) != cend();
		}

		template<typename K>
		[[nodiscard]] bool contains(K const &key, std::size_t hash) const noexcept {
			return find(key, hash) != cend();
		}

		template<typename K>
		[[nodiscard]] size_type count(K const &key) const noexcept {
			return count(key, h_(key));
		}

		template<typename K>
		[[nodiscard]] size_type count(K const &key, std::size_t hash) const noexcept {
			return static_cast<size_type>(find(key, hash) != cend());
		}

		template<typename K>
		[[nodiscard]] iterator find(K const &key) noexcept {
			return find_impl(*this, key, h_(key));
		}

		template<typename K>
		[[nodiscard]] iterator find(K const &key, std::size_t hash) noexcept {
			return find_impl(*this, key, hash);
		}

		template<typename K>
		[[nodiscard]] const_iterator find(K const &key) const noexcept {
			return find_impl(*this, key, h_(key));
		}

		template<typename K>
		[[nodiscard]] const_iterator find(K const &key, std::size_t hash) const noexcept {
			return find_impl(*this, key, hash);
		}

		template<typename K>
		std::pair<iterator, iterator> equal_range(K const &key) noexcept {
			return equal_range(key, h_(key));
		}

		template<typename K>
		std::pair<iterator, iterator> equal_range(K const &key, std::size_t hash) noexcept {
			iterator it = find(key, hash);
			return std::make_pair(it, it == end() ? it : std::next(it));
		}

		template<typename K>
		std::pair<const_iterator, const_iterator> equal_range(K const &key) const noexcept {
			return equal_range(key, h_(key));
		}

		template<typename K>
		std::pair<const_iterator, const_iterator> equal_range(K const &key, std::size_t hash) const noexcept {
			const_iterator it = find(key, hash);
			return std::make_pair(it, (it == cend()) ? it : std::next(it));
		}

		[[nodiscard]] size_type bucket_count() const noexcept {
			return bucket_count_;
		}

		[[nodiscard]] size_type max_bucket_count() const noexcept {
			return buckets_.max_size();
		}

		[[nodiscard]] float load_factor() const noexcept {
			if (bucket_count_ == 0) {
				return 0;
			}

			return static_cast<float>(n_elements_) / static_cast<float>(bucket_count_);
		}

		void rehash(size_type count) {
			count = std::max(count, size_type(std::ceil(float(size()) / max_load_factor)));
			rehash_impl(count);
		}

		void reserve(size_type count) {
			rehash(size_type(std::ceil(float(count) / max_load_factor)));
		}

		[[nodiscard]] hasher hash_function() const { return h_; }
		[[nodiscard]] key_equal key_eq() const { return keq_; }

	private:
		[[nodiscard]] size_type bucket_for_hash(std::size_t hash) const noexcept {
			auto const bucket = gpol_.bucket_for_hash(hash);
			assert(sparse_bucket_type::sparse_ibucket(bucket) < buckets_.size()
				   || (bucket == 0 && buckets_.empty()));

			return bucket;
		}

		[[nodiscard]] size_type next_bucket(size_type ibucket, [[maybe_unused]] size_type iprobe) const noexcept requires (is_power_of_two_policy<growth_policy>::value) {
			if constexpr (Probing == probing::linear) {
				return (ibucket + 1) & gpol_.mask();
			} else {
				assert(Probing == probing::quadratic);
				return (ibucket + iprobe) & gpol_.mask();
			}
		}

		[[nodiscard]] size_type next_bucket(size_type ibucket, [[maybe_unused]] size_type iprobe) const noexcept requires (!is_power_of_two_policy<growth_policy>::value) {
			if constexpr (Probing == probing::linear) {
				ibucket++;
				return ibucket != bucket_count_ ? ibucket : 0;
			} else {
				assert(Probing == probing::quadratic);
				ibucket += iprobe;
				return ibucket < bucket_count_ ? ibucket : ibucket % bucket_count_;
			}
		}

		template<typename K, typename ...Args>
		std::pair<iterator, bool> insert_impl(K const &key, Args &&...value_type_args) {
			if (buckets_.empty()) [[unlikely]] {
				rehash_impl(gpol_.next_bucket_count());
			}

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
			typename sparse_bucket_type::size_type index_in_sparse_bucket_first_deleted = 0;

			auto const hash = h_(key);
			auto ibucket = bucket_for_hash(hash);

			std::size_t probe = 0;
			while (true) {
				auto const sparse_ibucket = sparse_bucket_type::sparse_ibucket(ibucket);
				auto const index_in_sparse_bucket = sparse_bucket_type::index_in_sparse_bucket(ibucket);

				if (buckets_[sparse_ibucket].has_value(index_in_sparse_bucket)) {
					auto value_it = buckets_[sparse_ibucket].value(index_in_sparse_bucket);
					if (keq_(key, KeyValueSelect::key(*value_it))) {
						return std::make_pair(iterator{std::next(buckets_.begin(), sparse_ibucket),
													   buckets_.end(),
													   value_it},
											  false);
					}
				} else if (buckets_[sparse_ibucket].has_deleted_value(index_in_sparse_bucket) && probe < bucket_count_) {
					if (!found_first_deleted_bucket) {
						found_first_deleted_bucket = true;
						sparse_ibucket_first_deleted = sparse_ibucket;
						index_in_sparse_bucket_first_deleted = index_in_sparse_bucket;
					}
				} else {
					/**
					 * At this point we are sure that the value does not exist
					 * in the hash table.
					 * First check if we satisfy load and delete thresholds, and if not,
					 * rehash the hash table (and therefore start over). Otherwise, just
					 * insert the value into the appropriate bucket.
					 */
					if (size() >= load_threshold_rehash_) {
						rehash_impl(gpol_.next_bucket_count());
						return insert_impl(key, std::forward<Args>(value_type_args)...);
					}

					if (size() + n_deleted_elements_ >= load_threshold_clear_deleted_) {
						clear_deleted_buckets();
						return insert_impl(key, std::forward<Args>(value_type_args)...);
					}

					if (found_first_deleted_bucket) {
						auto it = insert_in_bucket(sparse_ibucket_first_deleted,
												   index_in_sparse_bucket_first_deleted,
												   std::forward<Args>(value_type_args)...);
						n_deleted_elements_ -= 1;
						return it;
					}

					return insert_in_bucket(sparse_ibucket, index_in_sparse_bucket,
											std::forward<Args>(value_type_args)...);
				}

				probe++;
				ibucket = next_bucket(ibucket, probe);
			}
		}

		template<typename ...Args>
		std::pair<iterator, bool> insert_in_bucket(std::size_t sparse_ibucket,
												   typename sparse_bucket_type::size_type index_in_sparse_bucket,
												   Args &&...value_type_args) {
			// is not called when empty
			auto value_it = buckets_[sparse_ibucket].set(buckets_.element_allocator(),
														 index_in_sparse_bucket,
														 std::forward<Args>(value_type_args)...);
			n_elements_++;

			return std::make_pair(iterator{std::next(buckets_.begin(), sparse_ibucket),
										   buckets_.end(),
										   value_it},
					true);
		}

		template<typename K>
		size_type erase_impl(K const &key, std::size_t hash) {
			if (buckets_.empty()) {
				return 0;
			}

			std::size_t ibucket = bucket_for_hash(hash);
			std::size_t probe = 0;

			while (true) {
				auto const sparse_ibucket = sparse_bucket_type::sparse_ibucket(ibucket);
				auto const index_in_sparse_bucket = sparse_bucket_type::index_in_sparse_bucket(ibucket);

				auto &bucket = buckets_[sparse_ibucket];

				if (bucket.has_value(index_in_sparse_bucket)) {
					auto value_it = bucket.value(index_in_sparse_bucket);

					if (keq_(key, KeyValueSelect::key(*value_it))) {
						bucket.erase(buckets_.element_allocator(), value_it, index_in_sparse_bucket);
						n_elements_ -= 1;
						n_deleted_elements_ += 1;

						return 1;
					}
				} else if (!bucket.has_deleted_value(index_in_sparse_bucket) || probe >= bucket_count_) {
					return 0;
				}

				probe++;
				ibucket = next_bucket(ibucket, probe);
			}
		}

		template<typename Self, typename K>
		[[nodiscard]] static auto find_impl(Self &&self, K const &key, std::size_t hash) noexcept {
			if (self.buckets_.empty()) {
				return self.end();
			}

			std::size_t ibucket = self.bucket_for_hash(hash);
			std::size_t probe = 0;

			while (true) {
				auto const sparse_ibucket = sparse_bucket_type::sparse_ibucket(ibucket);
				auto const index_in_sparse_bucket = sparse_bucket_type::index_in_sparse_bucket(ibucket);

				auto &bucket = self.buckets_[sparse_ibucket];

				if (bucket.has_value(index_in_sparse_bucket)) {
					auto value_it = bucket.value(index_in_sparse_bucket);
					if (self.keq_(key, KeyValueSelect::key(*value_it))) {
						static constexpr bool is_const = std::is_const_v<std::remove_reference_t<Self>>;

						return sparse_iterator<is_const>{std::next(self.buckets_.begin(), sparse_ibucket),
														 self.buckets_.end(),
														 value_it};
					}
				} else if (!bucket.has_deleted_value(index_in_sparse_bucket) || probe >= self.bucket_count_) {
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
			rehash_impl(bucket_count_);
			assert(n_deleted_elements_ == 0);
		}

		void rehash_impl(size_type count) requires (ExceptionSafety == exception_safety::basic) {
			sparse_hash new_table(count, h_, keq_, buckets_.element_allocator());

			for (auto &bucket : buckets_) {
				for (auto &val : bucket) {
					new_table.insert_on_rehash(std::move(val));
				}

				bucket.destroy_deallocate(buckets_.element_allocator());
			}

			buckets_.forget_deallocate();
			new_table.swap(*this);
		}

		/**
		 * TODO: For now we copy each element into the new map. We could move
		 * them if they are nothrow_move_constructible without triggering
		 * any exception if we reserve enough space in the sparse arrays beforehand.
		 */
		void rehash_impl(size_type count) requires (ExceptionSafety == exception_safety::strong) {
			sparse_hash new_table(count, h_, keq_, buckets_.element_allocator());

			for (auto const &bucket : buckets_) {
				for (auto const &val : bucket) {
					new_table.insert_on_rehash(val);
				}
			}

			new_table.swap(*this);
		}

		template<typename K>
		void insert_on_rehash(K &&key_value) {
			key_type const &key = KeyValueSelect::key(key_value);

			std::size_t const hash = h_(key);
			std::size_t ibucket = bucket_for_hash(hash);
			std::size_t probe = 0;

			while (true) {
				auto const sparse_ibucket = sparse_bucket_type::sparse_ibucket(ibucket);
				auto const index_in_sparse_bucket = sparse_bucket_type::index_in_sparse_bucket(ibucket);

				auto &bucket = buckets_[sparse_ibucket];

				if (!bucket.has_value(index_in_sparse_bucket)) {
					bucket.set(buckets_.element_allocator(), index_in_sparse_bucket, std::forward<K>(key_value));
					n_elements_++;

					return;
				} else {
					assert(!keq_(key, KeyValueSelect::key(*bucket.value(index_in_sparse_bucket))));
				}

				probe++;
				ibucket = next_bucket(ibucket, probe);
			}
		}
	};

} // namespace dice::sparse_map::internal

#endif//DICE_SPARSE_MAP_SPARSE_HASH_HPP
