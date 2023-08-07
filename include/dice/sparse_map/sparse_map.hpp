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
#ifndef DICE_SPARSE_MAP_SPARSE_MAP_HPP
#define DICE_SPARSE_MAP_SPARSE_MAP_HPP

#include <cstddef>
#include <functional>
#include <initializer_list>
#include <memory>
#include <type_traits>
#include <utility>

#include "internal/sparse_hash.hpp"

namespace dice::sparse_map {

	/**
	 * Implementation of a sparse hash map using open-addressing with quadratic
	 * probing. The goal on the hash map is to be the most memory efficient
	 * possible, even at low load factor, while keeping reasonable performances.
	 *
	 * `GrowthPolicy` defines how the map grows and consequently how a hash value is
	 * mapped to a bucket. By default the map uses
	 * `dice::sh::power_of_two_growth_policy`. This policy keeps the number of
	 * buckets to a power of two and uses a mask to map the hash to a bucket instead
	 * of the slow modulo. Other growth policies are available and you may define
	 * your own growth policy, check `dice::sh::power_of_two_growth_policy` for the
	 * interface.
	 *
	 * `ExceptionSafety` defines the exception guarantee provided by the class. By
	 * default only the basic exception safety is guaranteed which mean that all
	 * resources used by the hash map will be freed (no memory leaks) but the hash
	 * map may end-up in an undefined state if an exception is thrown (undefined
	 * here means that some elements may be missing). This can ONLY happen on rehash
	 * (either on insert or if `rehash` is called explicitly) and will occur if the
	 * Allocator can't allocate memory (`std::bad_alloc`) or if the copy constructor
	 * (when a nothrow move constructor is not available) throws an exception. This
	 * can be avoided by calling `reserve` beforehand. This basic guarantee is
	 * similar to the one of `google::sparse_hash_map` and `spp::sparse_hash_map`.
	 * It is possible to ask for the strong exception guarantee with
	 * `dice::sh::exception_safety::strong`, the drawback is that the map will be
	 * slower on rehashes and will also need more memory on rehashes.
	 *
	 * `Sparsity` defines how much the hash set will compromise between insertion
	 * speed and memory usage. A high sparsity means less memory usage but longer
	 * insertion times, and vice-versa for low sparsity. The default
	 * `dice::sh::sparsity::medium` sparsity offers a good compromise. It doesn't
	 * change the lookup speed.
	 *
	 * `Key` and `T` must be nothrow move constructible and/or copy constructible.
	 *
	 * If the destructor of `Key` or `T` throws an exception, the behaviour of the
	 * class is undefined.
	 *
	 * Iterators invalidation:
	 *  - clear, operator=, reserve, rehash: always invalidate the iterators.
	 *  - insert, emplace, emplace_hint, operator[]: if there is an effective
	 * insert, invalidate the iterators.
	 *  - erase: always invalidate the iterators.
	 */
	template<typename Key, typename T,
			 typename Hash = std::hash<Key>,
			 typename KeyEqual = std::equal_to<Key>,
			 typename Allocator = std::allocator<std::pair<Key, T>>,
			 growth_policy GrowthPolicy = power_of_two_growth_policy<2>,
			 exception_safety ExceptionSafety = exception_safety::basic,
			 sparsity Sparsity = sparsity::medium,
			 ratio MaxLoadFactor = std::ratio<1, 2>>
	struct sparse_map {
	private:
		static constexpr bool key_equal_is_transparent = requires {
			typename KeyEqual::is_transparent;
		};

		struct kv_select {
			using key_type = Key;
			using value_type = T;
			using both_type = std::pair<Key const, T>;

			template<typename K>
			static key_type const &key(std::pair<K, T> const &key_value) noexcept {
				return key_value.first;
			}

			template<typename K>
			static value_type const &value(std::pair<K, T> const &key_value) noexcept {
				return key_value.second;
			}

			template<typename K>
			static value_type &value(std::pair<K, T> &key_value) noexcept {
				return key_value.second;
			}

			template<typename K>
			static both_type const &both(std::pair<K, T> const &key_value) noexcept {
				return reinterpret_cast<both_type const &>(key_value);
			}

			template<typename K>
			static both_type &both(std::pair<K, T> &key_value) noexcept {
				return reinterpret_cast<both_type &>(key_value);
			}
		};

		using ht = internal::sparse_hash<std::pair<Key, T>, kv_select, Hash, KeyEqual, Allocator,
										 GrowthPolicy, ExceptionSafety, Sparsity, probing::quadratic, MaxLoadFactor>;

		ht ht_;

	public:
		using key_type = typename ht::key_type;
		using mapped_type = T;
		using value_type = typename ht::value_type;
		using size_type = typename ht::size_type;
		using difference_type = typename ht::difference_type;
		using hasher = typename ht::hasher;
		using key_equal = typename ht::key_equal;
		using allocator_type = typename ht::allocator_type;
		using reference = typename ht::reference;
		using const_reference = typename ht::const_reference;
		using pointer = typename ht::pointer;
		using const_pointer = typename ht::const_pointer;
		using iterator = typename ht::iterator;
		using const_iterator = typename ht::const_iterator;
		static constexpr float max_load_factor = ht::max_load_factor;

	public:
		sparse_map() noexcept(ht::default_init_bucket_count == 0) : ht_{ht::default_init_bucket_count, {}, {}, {}} {}

		explicit sparse_map(size_type bucket_count,
							hasher const &hash = {},
							key_equal const &equal = {},
							allocator_type const &alloc = {}) : ht_{bucket_count, hash, equal, alloc} {
		}

		sparse_map(size_type bucket_count, allocator_type const &alloc) : ht_{bucket_count, {}, {}, alloc} {
		}

		sparse_map(size_type bucket_count, hasher const &hash, allocator_type const &alloc) : ht_{bucket_count, hash, {}, alloc} {
		}

		explicit sparse_map(allocator_type const &alloc) noexcept(ht::default_init_bucket_count == 0) : ht_{ht::default_init_bucket_count, {}, {}, alloc} {
		}

		template<typename InputIt>
		sparse_map(InputIt first,
				   InputIt last,
				   size_type bucket_count = ht::default_init_bucket_count,
				   hasher const &hash = {},
				   key_equal const &equal = {},
				   allocator_type const &alloc = {}) : ht_{bucket_count, hash, equal, alloc} {
			insert(first, last);
		}

		template<typename InputIt>
		sparse_map(InputIt first,
				   InputIt last,
				   size_type bucket_count,
				   allocator_type const &alloc) : sparse_map{first, last, bucket_count, {}, {}, alloc} {
		}

		template<typename InputIt>
		sparse_map(InputIt first,
				   InputIt last,
				   size_type bucket_count,
				   hasher const &hash, allocator_type const &alloc) : sparse_map{first, last, bucket_count, hash, {}, alloc} {
		}

		sparse_map(std::initializer_list<value_type> init,
				   size_type bucket_count = ht::default_init_bucket_count,
				   hasher const &hash = {},
				   key_equal const &equal = {},
				   allocator_type const &alloc = {}) : sparse_map{init.begin(), init.end(), bucket_count, hash, equal, alloc} {
		}

		sparse_map(std::initializer_list<value_type> init,
				   size_type bucket_count,
				   allocator_type const &alloc) : sparse_map(init.begin(), init.end(), bucket_count, {}, {}, alloc) {
		}

		sparse_map(std::initializer_list<value_type> init,
				   size_type bucket_count,
				   hasher const &hash,
				   allocator_type const &alloc) : sparse_map{init.begin(), init.end(), bucket_count, hash, {}, alloc} {
		}

		sparse_map &operator=(std::initializer_list<value_type> ilist) {
			ht_.clear();

			ht_.reserve(ilist.size());
			ht_.insert(ilist.begin(), ilist.end());

			return *this;
		}

		[[nodiscard]] allocator_type get_allocator() const { return ht_.get_allocator(); }

		[[nodiscard]] iterator begin() noexcept { return ht_.begin(); }
		[[nodiscard]] const_iterator begin() const noexcept { return ht_.begin(); }
		[[nodiscard]] const_iterator cbegin() const noexcept { return ht_.cbegin(); }

		[[nodiscard]] iterator end() noexcept { return ht_.end(); }
		[[nodiscard]] const_iterator end() const noexcept { return ht_.end(); }
		[[nodiscard]] const_iterator cend() const noexcept { return ht_.cend(); }

		[[nodiscard]] bool empty() const noexcept { return ht_.empty(); }
		[[nodiscard]] size_type size() const noexcept { return ht_.size(); }
		[[nodiscard]] size_type max_size() const noexcept { return ht_.max_size(); }

		void clear() noexcept {
			ht_.clear();
		}

		std::pair<iterator, bool> insert(value_type const &value) {
			return ht_.insert(value);
		}

		template<typename P> requires (std::is_constructible_v<value_type, P &&>)
		std::pair<iterator, bool> insert(P &&value) {
			return ht_.emplace(std::forward<P>(value));
		}

		std::pair<iterator, bool> insert(value_type &&value) {
			return ht_.insert(std::move(value));
		}

		iterator insert(const_iterator hint, value_type const &value) {
			return ht_.insert_hint(hint, value);
		}

		template<typename P> requires (std::is_constructible_v<value_type, P &&>)
		iterator insert(const_iterator hint, P &&value) {
			return ht_.emplace_hint(hint, std::forward<P>(value));
		}

		iterator insert(const_iterator hint, value_type &&value) {
			return ht_.insert_hint(hint, std::move(value));
		}

		template<typename InputIt>
		void insert(InputIt first, InputIt last) {
			ht_.insert(first, last);
		}

		void insert(std::initializer_list<value_type> ilist) {
			ht_.insert(ilist.begin(), ilist.end());
		}

		template<typename M>
		std::pair<iterator, bool> insert_or_assign(key_type const &k, M &&obj) {
			return ht_.insert_or_assign(k, std::forward<M>(obj));
		}

		template<typename M>
		std::pair<iterator, bool> insert_or_assign(key_type &&k, M &&obj) {
			return ht_.insert_or_assign(std::move(k), std::forward<M>(obj));
		}

		template<typename M>
		iterator insert_or_assign(const_iterator hint, key_type const &k, M &&obj) {
			return ht_.insert_or_assign(hint, k, std::forward<M>(obj));
		}

		template<typename M>
		iterator insert_or_assign(const_iterator hint, key_type &&k, M &&obj) {
			return ht_.insert_or_assign(hint, std::move(k), std::forward<M>(obj));
		}

		/**
		 * Due to the way elements are stored, emplace will need to move or copy the
		 * key-value once. The method is equivalent to
		 * `insert(value_type(std::forward<Args>(args)...));`.
		 *
		 * Mainly here for compatibility with the `std::unordered_map` interface.
		 */
		template<typename ...Args>
		std::pair<iterator, bool> emplace(Args &&...args) {
			return ht_.emplace(std::forward<Args>(args)...);
		}

		/**
	     * Due to the way elements are stored, emplace_hint will need to move or copy
	     * the key-value once. The method is equivalent to `insert(hint,
	     * value_type(std::forward<Args>(args)...));`.
	     *
	     * Mainly here for compatibility with the `std::unordered_map` interface.
	     */
		template<typename ...Args>
		iterator emplace_hint(const_iterator hint, Args &&...args) {
			return ht_.emplace_hint(hint, std::forward<Args>(args)...);
		}

		template<typename ...Args>
		std::pair<iterator, bool> try_emplace(key_type const &k, Args &&...args) {
			return ht_.try_emplace(k, std::forward<Args>(args)...);
		}

		template<typename ...Args>
		std::pair<iterator, bool> try_emplace(key_type &&k, Args &&...args) {
			return ht_.try_emplace(std::move(k), std::forward<Args>(args)...);
		}

		template<typename ...Args>
		iterator try_emplace(const_iterator hint, key_type const &k, Args &&...args) {
			return ht_.try_emplace_hint(hint, k, std::forward<Args>(args)...);
		}

		template<typename ...Args>
		iterator try_emplace(const_iterator hint, key_type &&k, Args &&...args) {
			return ht_.try_emplace_hint(hint, std::move(k),
										 std::forward<Args>(args)...);
		}

		iterator erase(iterator pos) { return ht_.erase(pos); }
		iterator erase(const_iterator pos) { return ht_.erase(pos); }
		iterator erase(const_iterator first, const_iterator last) {
			return ht_.erase(first, last);
		}
		size_type erase(key_type const &key) { return ht_.erase(key); }

		/**
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		size_type erase(key_type const &key, std::size_t precalculated_hash) {
			return ht_.erase(key, precalculated_hash);
		}

		/**
   		 * This overload only participates in the overload resolution if the typedef
   		 * `KeyEqual::is_transparent` exists. If so, `K` must be hashable and
   		 * comparable to `Key`.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		size_type erase(K const &key) {
			return ht_.erase(key);
		}

		/**
   		 * @copydoc erase(const K& key)
   		 *
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		size_type erase(K const &key, std::size_t precalculated_hash) {
			return ht_.erase(key, precalculated_hash);
		}

		void swap(sparse_map &other) noexcept {
			other.ht_.swap(ht_);
		}

		T &at(Key const &key) { return ht_.at(key); }

		/**
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		T &at(Key const &key, std::size_t precalculated_hash) {
			return ht_.at(key, precalculated_hash);
		}

		T const &at(Key const &key) const { return ht_.at(key); }

		/**
   		 * @copydoc at(const Key& key, std::size_t precalculated_hash)
   		 */
		T const &at(Key const &key, std::size_t precalculated_hash) const {
			return ht_.at(key, precalculated_hash);
		}

		/**
   		 * This overload only participates in the overload resolution if the typedef
   		 * `KeyEqual::is_transparent` exists. If so, `K` must be hashable and
   		 * comparable to `Key`.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		T &at(K const &key) {
			return ht_.at(key);
		}

		/**
   		 * @copydoc at(const K& key)
   		 *
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		T &at(K const &key, std::size_t precalculated_hash) {
			return ht_.at(key, precalculated_hash);
		}

		/**
   		 * @copydoc at(const K& key)
   		 */
		template<typename K> requires (key_equal_is_transparent)
		T const &at(K const &key) const {
			return ht_.at(key);
		}

		/**
   		 * @copydoc at(const K& key, std::size_t precalculated_hash)
   		 */
		template<typename K> requires (key_equal_is_transparent)
		T const &at(K const &key, std::size_t precalculated_hash) const {
			return ht_.at(key, precalculated_hash);
		}

		T &operator[](Key const &key) { return ht_[key]; }
		T &operator[](Key &&key) { return ht_[std::move(key)]; }

		[[nodiscard]] size_type count(Key const &key) const noexcept {
			return ht_.count(key);
		}

		/**
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		[[nodiscard]] size_type count(Key const &key, std::size_t precalculated_hash) const noexcept {
			return ht_.count(key, precalculated_hash);
		}

		/**
   		 * This overload only participates in the overload resolution if the typedef
   		 * `KeyEqual::is_transparent` exists. If so, `K` must be hashable and
   		 * comparable to `Key`.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] size_type count(K const &key) const noexcept {
			return ht_.count(key);
		}

		/**
   		 * @copydoc count(const K& key) const
   		 *
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] size_type count(K const &key, std::size_t precalculated_hash) const noexcept {
			return ht_.count(key, precalculated_hash);
		}

		[[nodiscard]] iterator find(Key const &key) noexcept {
			return ht_.find(key);
		}

		/**
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		[[nodiscard]] iterator find(Key const &key, std::size_t precalculated_hash) noexcept {
			return ht_.find(key, precalculated_hash);
		}

		[[nodiscard]] const_iterator find(Key const &key) const noexcept {
			return ht_.find(key);
		}

		/**
   		 * @copydoc find(const Key& key, std::size_t precalculated_hash)
   		 */
		[[nodiscard]] const_iterator find(Key const &key, std::size_t precalculated_hash) const noexcept {
			return ht_.find(key, precalculated_hash);
		}

		/**
   		 * This overload only participates in the overload resolution if the typedef
   		 * `KeyEqual::is_transparent` exists. If so, `K` must be hashable and
   		 * comparable to `Key`.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] iterator find(K const &key) noexcept {
			return ht_.find(key);
		}

		/**
   		 * @copydoc find(const K& key)
   		 *
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] iterator find(K const &key, std::size_t precalculated_hash) noexcept {
			return ht_.find(key, precalculated_hash);
		}

		/**
   	 	 * @copydoc find(const K& key)
   	 	 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] const_iterator find(K const &key) const noexcept {
			return ht_.find(key);
		}

		/**
   		 * @copydoc find(const K& key)
   		 *
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] const_iterator find(K const &key, std::size_t precalculated_hash) const noexcept {
			return ht_.find(key, precalculated_hash);
		}

		[[nodiscard]] bool contains(Key const &key) const noexcept {
			return ht_.contains(key);
		}

		/**
   		 * Use the hash value 'precalculated_hash' instead of hashing the key. The
   		 * hash value should be the same as hash_function()(key). Useful to speed-up
   		 * the lookup if you already have the hash.
   		 */
		[[nodiscard]] bool contains(Key const &key, std::size_t precalculated_hash) const noexcept {
			return ht_.contains(key, precalculated_hash);
		}

		/**
   		 * This overload only participates in the overload resolution if the typedef
   		 * KeyEqual::is_transparent exists. If so, K must be hashable and comparable
   		 * to Key.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] bool contains(K const &key) const noexcept {
			return ht_.contains(key);
		}

		/**
   		 * @copydoc contains(const K& key) const
   		 *
   		 * Use the hash value 'precalculated_hash' instead of hashing the key. The
   		 * hash value should be the same as hash_function()(key). Useful to speed-up
   		 * the lookup if you already have the hash.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] bool contains(K const &key, std::size_t precalculated_hash) const noexcept {
			return ht_.contains(key, precalculated_hash);
		}

		[[nodiscard]] std::pair<iterator, iterator> equal_range(Key const &key) {
			return ht_.equal_range(key);
		}

		/**
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		[[nodiscard]] std::pair<iterator, iterator> equal_range(Key const &key,
																std::size_t precalculated_hash) {
			return ht_.equal_range(key, precalculated_hash);
		}

		[[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(Key const &key) const {
			return ht_.equal_range(key);
		}

		/**
   		 * @copydoc equal_range(const Key& key, std::size_t precalculated_hash)
   		 */
		[[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(Key const &key,
																			std::size_t precalculated_hash) const {
			return ht_.equal_range(key, precalculated_hash);
		}

		/**
   		 * This overload only participates in the overload resolution if the typedef
   		 * `KeyEqual::is_transparent` exists. If so, `K` must be hashable and
   		 * comparable to `Key`.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] std::pair<iterator, iterator> equal_range(K const &key) {
			return ht_.equal_range(key);
		}

		/**
   		 * @copydoc equal_range(const K& key)
   		 *
   		 * Use the hash value `precalculated_hash` instead of hashing the key. The
   		 * hash value should be the same as `hash_function()(key)`, otherwise the
   		 * behaviour is undefined. Useful to speed-up the lookup if you already have
   		 * the hash.
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] std::pair<iterator, iterator> equal_range(K const &key,
																std::size_t precalculated_hash) {
			return ht_.equal_range(key, precalculated_hash);
		}

		/**
   		 * @copydoc equal_range(const K& key)
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(K const &key) const {
			return ht_.equal_range(key);
		}

		/**
   		 * @copydoc equal_range(const K& key, std::size_t precalculated_hash)
   		 */
		template<typename K> requires (key_equal_is_transparent)
		[[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(K const &key,
																			std::size_t precalculated_hash) const {
			return ht_.equal_range(key, precalculated_hash);
		}

		[[nodiscard]] size_type bucket_count() const noexcept { return ht_.bucket_count(); }
		[[nodiscard]] size_type max_bucket_count() const noexcept { return ht_.max_bucket_count(); }

		[[nodiscard]] float load_factor() const noexcept { return ht_.load_factor(); }

		void rehash(size_type count) { ht_.rehash(count); }
		void reserve(size_type count) { ht_.reserve(count); }

		[[nodiscard]] hasher hash_function() const { return ht_.hash_function(); }
		[[nodiscard]] key_equal key_eq() const { return ht_.key_eq(); }

		friend bool operator==(sparse_map const &lhs, sparse_map const &rhs) {
			if (lhs.size() != rhs.size()) {
				return false;
			}

			for (auto const &element_lhs : lhs) {
				const auto it_element_rhs = rhs.find(element_lhs.first);
				if (it_element_rhs == rhs.cend() ||
					element_lhs.second != it_element_rhs->second) {
					return false;
				}
			}

			return true;
		}

		friend bool operator!=(sparse_map const &lhs, sparse_map const &rhs) {
			return !operator==(lhs, rhs);
		}

		friend void swap(sparse_map &lhs, sparse_map &rhs) { lhs.swap(rhs); }
	};

	/**
	 * Same as `dice::sparse_map<Key, T, Hash, KeyEqual, Allocator,
	 * dice::sh::prime_growth_policy>`.
	 */
	template<typename Key, typename T,
			 typename Hash = std::hash<Key>,
			 typename KeyEqual = std::equal_to<Key>,
			 typename Allocator = std::allocator<std::pair<Key, T>>>
	using sparse_pg_map = sparse_map<Key, T, Hash, KeyEqual, Allocator, prime_growth_policy>;

}// namespace dice::sparse_map

#endif // DICE_SPARSE_MAP_SPARSE_MAP_HPP