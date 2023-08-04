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

#include <boost/interprocess/containers/vector.hpp>

#include "dice/sparse-map/sparse_bucket.hpp"
#include "dice/sparse-map/sparse_growth_policy.hpp"
#include "dice/sparse-map/sparse_bucket_array.hpp"

namespace dice::sparse_map::detail {
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
	 * one value, we have a `std::vector<sparse_array_type>` (m_sparse_buckets_data)
	 * where each `sparse_array_type` stores multiple values (up to
	 * `sparse_array_type::BITMAP_NB_BITS`). To convert a one dimensional `ibucket`
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
			 probing Probing>
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

		static constexpr size_type DEFAULT_INIT_BUCKET_COUNT = 0;
		static constexpr float DEFAULT_MAX_LOAD_FACTOR = 0.5f;

	private:
		static constexpr bool has_mapped_type = !std::is_same_v<mapped_type, void>;

		using sparse_bucket_array_type = sparse_bucket_array<value_type, allocator_type, Sparsity>;
		using sparse_bucket_type = typename sparse_bucket_array_type::bucket_type;

	private:
		sparse_bucket_array_type m_sparse_buckets_data;

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

		[[no_unique_address]] hasher m_h;
		[[no_unique_address]] key_equal m_keq;
		[[no_unique_address]] growth_policy m_gpol;

	public:
		template<bool IsConst>
		class sparse_iterator {
		private:
			friend class sparse_hash;

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
			 * m_sparse_buckets_data.end(). (TODO better way?)
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
		};

		iterator mutable_iterator(const_iterator pos) noexcept {
			// SAFETY: this is non-const therefore the underlying buckets are also non-const
			// as evidenced by the fact that we can call begin on them
			auto it_sparse_buckets = m_sparse_buckets_data.begin() + std::distance(m_sparse_buckets_data.cbegin(), pos.cur_bucket_);

			// SAFETY: this is non-const therefore the underlying sparse array is also non-const
			auto it_array = sparse_bucket_type::unsafe_mutable_iterator(pos.bucket_it_);

			return iterator{it_sparse_buckets, m_sparse_buckets_data.end(), it_array};
		}

	public:
		sparse_hash(size_type bucket_count, Hash const &hash, KeyEqual const &equal,
					allocator_type const &alloc, float max_load_factor) : m_sparse_buckets_data{alloc},
																		  m_bucket_count{bucket_count},
																		  m_nb_elements{0},
																		  m_nb_deleted_buckets{0},
																		  m_h{hash},
																		  m_keq{equal},
																		  m_gpol{bucket_count} {
			if (m_bucket_count > max_bucket_count()) {
				throw std::length_error("The map exceeds its maximum size.");
			}

			if (m_bucket_count > 0) {
				m_sparse_buckets_data.resize(bucket_count);
				assert(!m_sparse_buckets_data.empty());
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

		sparse_hash(const sparse_hash &other) = default;

		sparse_hash(sparse_hash &&other) noexcept(std::is_nothrow_move_constructible_v<sparse_bucket_array_type>
												  && std::is_nothrow_move_constructible_v<hasher>
												  && std::is_nothrow_move_constructible_v<key_equal>
												  && std::is_nothrow_move_constructible_v<GrowthPolicy>)
			: m_sparse_buckets_data(std::move(other.m_sparse_buckets_data)),
			  m_bucket_count(other.m_bucket_count),
			  m_nb_elements(other.m_nb_elements),
			  m_nb_deleted_buckets(other.m_nb_deleted_buckets),
			  m_load_threshold_rehash(other.m_load_threshold_rehash),
			  m_load_threshold_clear_deleted(other.m_load_threshold_clear_deleted),
			  m_max_load_factor(other.m_max_load_factor),
			  m_h{std::move(other.m_h)},
			  m_keq{std::move(other.m_keq)},
			  m_gpol{std::move(other.m_gpol)} {
			other.m_gpol.clear();
			other.m_bucket_count = 0;
			other.m_nb_elements = 0;
			other.m_nb_deleted_buckets = 0;
			other.m_load_threshold_rehash = 0;
			other.m_load_threshold_clear_deleted = 0;
		}

		sparse_hash &operator=(const sparse_hash &other) {
			if (this == &other) {
				return *this;
			}

			clear();

			m_sparse_buckets_data = other.m_sparse_buckets_data;
			m_bucket_count = other.m_bucket_count;
			m_nb_elements = other.m_nb_elements;
			m_nb_deleted_buckets = other.m_nb_deleted_buckets;
			m_load_threshold_rehash = other.m_load_threshold_rehash;
			m_load_threshold_clear_deleted = other.m_load_threshold_clear_deleted;
			m_max_load_factor = other.m_max_load_factor;
			m_h = other.m_h;
			m_keq = other.m_keq;
			m_gpol = other.m_gpol;

			return *this;
		}

		sparse_hash &operator=(sparse_hash &&other) noexcept {
			assert(this != &other);

			m_sparse_buckets_data = std::move(other.m_sparse_buckets_data);
			m_bucket_count = std::exchange(other.m_bucket_count, 0);
			m_nb_elements = std::exchange(other.m_nb_elements, 0);
			m_nb_deleted_buckets = std::exchange(other.m_nb_deleted_buckets, 0);
			m_load_threshold_rehash = std::exchange(other.m_load_threshold_rehash, 0);
			m_load_threshold_clear_deleted = std::exchange(other.m_load_threshold_clear_deleted, 0);
			m_max_load_factor = other.m_max_load_factor;

			m_h = std::move(other.m_h);
			m_keq = std::move(other.m_keq);
			m_gpol = std::move(other.m_gpol);
			other.m_gpol.clear();

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
			return iterator{begin,
							m_sparse_buckets_data.end(),
							begin != m_sparse_buckets_data.end() ? (*begin).begin() : nullptr};
		}

		const_iterator begin() const noexcept {
			return cbegin();
		}

		const_iterator cbegin() const noexcept {
			auto begin = m_sparse_buckets_data.begin();
			//vector iterator with fancy pointers have a problem with ->
			while (begin != m_sparse_buckets_data.end() && (*begin).empty()) {
				++begin;
			}

			return const_iterator{begin,
								  m_sparse_buckets_data.end(),
								  begin != m_sparse_buckets_data.end() ? (*begin).begin() : nullptr};
		}

		iterator end() noexcept {
			return iterator{m_sparse_buckets_data.end(),
							m_sparse_buckets_data.end(),
							nullptr};
		}

		const_iterator end() const noexcept {
			return cend();
		}

		const_iterator cend() const noexcept {
			return const_iterator{m_sparse_buckets_data.end(),
								  m_sparse_buckets_data.end(),
								  nullptr};
		}

		bool empty() const noexcept { return m_nb_elements == 0; }

		size_type size() const noexcept { return m_nb_elements; }

		size_type max_size() const noexcept {
			return std::min(std::allocator_traits<Allocator>::max_size(),
							m_sparse_buckets_data.max_size());
		}

		void clear() noexcept {
			m_sparse_buckets_data.clear_buckets();
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
				assert(m_load_threshold_rehash >= size());

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
			assert(pos != end() && m_nb_elements > 0);
			//vector iterator with fancy pointers have a problem with ->
			auto next_bucket_it = pos.cur_bucket_->erase(m_sparse_buckets_data.element_allocator(), pos.bucket_it_);
			m_nb_elements--;
			m_nb_deleted_buckets++;

			if (next_bucket_it != pos.cur_bucket_->end()) {
				return iterator{pos.cur_bucket_,
								m_sparse_buckets_data.end(),
								next_bucket_it};
			}

			auto it_sparse_buckets_next = pos.cur_bucket_;
			do {
				++it_sparse_buckets_next;
			} while (it_sparse_buckets_next != m_sparse_buckets_data.end()
					 && (*it_sparse_buckets_next).empty());

			if (it_sparse_buckets_next == m_sparse_buckets_data.end()) {
				return end();
			} else {
				return iterator{it_sparse_buckets_next,
								m_sparse_buckets_data.end(),
								it_sparse_buckets_next->begin()};
			}
		}

		iterator erase(const_iterator pos) {
			return erase(mutable_iterator(pos));
		}

		iterator erase(const_iterator first, const_iterator last) {
			//TODO why doesn't this work
			/*auto it = mutable_iterator(first);
			while (it != last) {
				it = erase(it);
			}

			return it;*/

			// TODO Optimize, could avoid the call to std::distance.
			auto const nb_elements_to_erase = static_cast<size_type>(std::distance(first, last));
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

			swap(m_sparse_buckets_data, other.m_sparse_buckets_data);
			swap(m_bucket_count, other.m_bucket_count);
			swap(m_nb_elements, other.m_nb_elements);
			swap(m_nb_deleted_buckets, other.m_nb_deleted_buckets);
			swap(m_load_threshold_rehash, other.m_load_threshold_rehash);
			swap(m_load_threshold_clear_deleted, other.m_load_threshold_clear_deleted);
			swap(m_max_load_factor, other.m_max_load_factor);
			swap(m_h, other.m_h);
			swap(m_keq, other.m_keq);
			swap(m_gpol, other.m_gpol);
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
			assert(max_load_factor_with_deleted_buckets > 0.0f &&
						  max_load_factor_with_deleted_buckets <= 1.0f);
			m_load_threshold_clear_deleted =
					size_type(float(bucket_count()) * max_load_factor_with_deleted_buckets);
		}

		void rehash(size_type count) {
			count = std::max(count, size_type(std::ceil(float(size()) / max_load_factor())));
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
			assert(sparse_bucket_type::sparse_ibucket(bucket) < m_sparse_buckets_data.size()
				   || (bucket == 0 && m_sparse_buckets_data.empty()));

			return bucket;
		}

		size_type next_bucket(size_type ibucket, [[maybe_unused]] size_type iprobe) const requires (is_power_of_two_policy<growth_policy>::value) {
			if constexpr (Probing == probing::linear) {
				return (ibucket + 1) & m_gpol.mask();
			} else {
				assert(Probing == probing::quadratic);
				return (ibucket + iprobe) & m_gpol.mask();
			}
		}

		size_type next_bucket(size_type ibucket, [[maybe_unused]] size_type iprobe) const requires (!is_power_of_two_policy<growth_policy>::value) {
			if constexpr (Probing == probing::linear) {
				ibucket++;
				return (ibucket != bucket_count()) ? ibucket : 0;
			} else {
				assert(Probing == probing::quadratic);
				ibucket += iprobe;
				return (ibucket < bucket_count()) ? ibucket : ibucket % bucket_count();
			}
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
			//assert(!m_sparse_buckets_data.empty());

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

			const std::size_t hash = m_h(key);
			std::size_t ibucket = bucket_for_hash(hash);

			std::size_t probe = 0;
			while (true) {
				std::size_t sparse_ibucket = sparse_bucket_type::sparse_ibucket(ibucket);
				auto index_in_sparse_bucket = sparse_bucket_type::index_in_sparse_bucket(ibucket);

				if (!m_sparse_buckets_data.empty()) {
					if (m_sparse_buckets_data[sparse_ibucket].has_value(index_in_sparse_bucket)) {
						auto value_it = m_sparse_buckets_data[sparse_ibucket].value(index_in_sparse_bucket);
						if (m_keq(key, KeyValueSelect::key(*value_it))) {
							return std::make_pair(iterator{std::next(m_sparse_buckets_data.begin(), sparse_ibucket),
														   m_sparse_buckets_data.end(),
														   value_it},
												  false);
						}
					} else if (m_sparse_buckets_data[sparse_ibucket].has_deleted_value(index_in_sparse_bucket) && probe < m_bucket_count) {
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
		std::pair<iterator, bool> insert_in_bucket(std::size_t sparse_ibucket,
												   typename sparse_bucket_type::size_type index_in_sparse_bucket,
												   Args &&...value_type_args) {
			// is not called when empty
			auto value_it = m_sparse_buckets_data[sparse_ibucket].set(m_sparse_buckets_data.element_allocator(),
																	  index_in_sparse_bucket,
																	  std::forward<Args>(value_type_args)...);
			m_nb_elements++;

			return std::make_pair(iterator{std::next(m_sparse_buckets_data.begin(), sparse_ibucket),
										   m_sparse_buckets_data.end(),
										   value_it},
					true);
		}

		template<class K>
		size_type erase_impl(K const &key, std::size_t hash) {
			if (m_sparse_buckets_data.empty()) {
				return 0;
			}

			std::size_t ibucket = bucket_for_hash(hash);
			std::size_t probe = 0;

			while (true) {
				auto const sparse_ibucket = sparse_bucket_type::sparse_ibucket(ibucket);
				auto const index_in_sparse_bucket = sparse_bucket_type::index_in_sparse_bucket(ibucket);

				auto &bucket = m_sparse_buckets_data[sparse_ibucket];

				if (bucket.has_value(index_in_sparse_bucket)) {
					auto value_it = bucket.value(index_in_sparse_bucket);

					if (m_keq(key, KeyValueSelect::key(*value_it))) {
						bucket.erase(m_sparse_buckets_data.element_allocator(), value_it, index_in_sparse_bucket);
						m_nb_elements--;
						m_nb_deleted_buckets++;

						return 1;
					}
				} else if (!bucket.has_deleted_value(index_in_sparse_bucket) || probe >= m_bucket_count) {
					return 0;
				}

				probe++;
				ibucket = next_bucket(ibucket, probe);
			}
		}

		template<typename Self, class K>
		static auto find_impl(Self &&self, K const &key, std::size_t hash) {
			if (self.m_sparse_buckets_data.empty()) {
				return self.end();
			}

			std::size_t ibucket = self.bucket_for_hash(hash);
			std::size_t probe = 0;

			while (true) {
				auto const sparse_ibucket = sparse_bucket_type::sparse_ibucket(ibucket);
				auto const index_in_sparse_bucket = sparse_bucket_type::index_in_sparse_bucket(ibucket);

				auto &bucket = self.m_sparse_buckets_data[sparse_ibucket];

				if (bucket.has_value(index_in_sparse_bucket)) {
					auto value_it = bucket.value(index_in_sparse_bucket);
					if (self.m_keq(key, KeyValueSelect::key(*value_it))) {
						static constexpr bool is_const = std::is_const_v<std::remove_reference_t<Self>>;

						return sparse_iterator<is_const>{std::next(self.m_sparse_buckets_data.begin(), sparse_ibucket),
														 self.m_sparse_buckets_data.end(),
														 value_it};
					}
				} else if (!bucket.has_deleted_value(index_in_sparse_bucket) || probe >= self.m_bucket_count) {
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
			assert(m_nb_deleted_buckets == 0);
		}

		void rehash_impl(size_type count) requires (ExceptionSafety == exception_safety::basic) {
			sparse_hash new_table(count, m_h, m_keq, m_sparse_buckets_data.element_allocator(), m_max_load_factor);

			for (auto &bucket : m_sparse_buckets_data) {
				for (auto &val : bucket) {
					new_table.insert_on_rehash(std::move(val));
				}

				// TODO try to reuse some of the memory
				bucket.clear(m_sparse_buckets_data.element_allocator());
			}

			new_table.swap(*this);
		}

		/**
		 * TODO: For now we copy each element into the new map. We could move
		 * them if they are nothrow_move_constructible without triggering
		 * any exception if we reserve enough space in the sparse arrays beforehand.
		 */
		void rehash_impl(size_type count) requires (ExceptionSafety == exception_safety::strong) {
			sparse_hash new_table(count, m_h, m_keq, m_sparse_buckets_data.element_allocator(), m_max_load_factor);

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

			std::size_t const hash = m_h(key);
			std::size_t ibucket = bucket_for_hash(hash);
			std::size_t probe = 0;

			while (true) {
				auto const sparse_ibucket = sparse_bucket_type::sparse_ibucket(ibucket);
				auto const index_in_sparse_bucket = sparse_bucket_type::index_in_sparse_bucket(ibucket);

				auto &bucket = m_sparse_buckets_data[sparse_ibucket];

				if (!bucket.has_value(index_in_sparse_bucket)) {
					bucket.set(m_sparse_buckets_data.element_allocator(), index_in_sparse_bucket, std::forward<K>(key_value));
					m_nb_elements++;

					return;
				} else {
					assert(!m_keq(key, KeyValueSelect::key(*bucket.value(index_in_sparse_bucket))));
				}

				probe++;
				ibucket = next_bucket(ibucket, probe);
			}
		}
	};
}// namespace dice::sparse_map

#endif
