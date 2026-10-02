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
#ifndef DICE_SPARSE_MAP_SPARSE_SET_HPP
#define DICE_SPARSE_MAP_SPARSE_SET_HPP

#include <cstddef>
#include <functional>
#include <initializer_list>
#include <memory>
#include <ranges>
#include <type_traits>
#include <utility>

#include "dice/sparse-map/sparse_hash.hpp"

namespace dice::sparse_map {

    /**
     * A hash set with open addressing and quadratic probing that uses as little memory as possible, also at a
     * low load factor, and keeps good performance.
     *
     * The buckets are grouped by 64. A group stores only its occupied buckets, densely, and a bitmap tells which
     * buckets are occupied.
     *
     * `Hash` must be avalanching, see `dice::sparse_map::sh::hash_is_avalanching`: a
     * hash picks its bucket with its low bits as it is. The default
     * `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>` declares
     * `is_avalanching` for every key type. A `dice::hash::dice_hash_overload` for
     * your own key type must keep the avalanche of the policy.
     *
     * The number of buckets is 0 or a power of two and doubles when the table
     * grows. A hash picks its bucket with a mask.
     *
     * The interface follows `std::unordered_set`, with these differences:
     *  - The iterators are forward iterators.
     *  - There is no bucket interface beyond `bucket_count`, and no node handles.
     *  - Heterogeneous lookup and erasure are enabled by `KeyEqual::is_transparent` alone.
     *  - Lookups can take a precalculated hash, see the overloads with a `precalculated_hash` parameter.
     *
     * The exception guarantee follows from the type of the elements. If the insertion of one element throws, the set
     * holds the same elements as before, except in one case: a rehash can leave the set empty. An insertion, `rehash`
     * or `reserve` can rehash. `reserve` avoids rehashes.
     * How a rehash transfers the elements depends on their type:
     * - Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old
     *   buckets is freed while they are moved. If the hash function throws while the elements are moved, or the
     *   allocator with `allocation_failure::throwing`, the set is empty. Any other exception leaves the set unchanged.
     * - Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and
     *   the new buckets are in memory at the same time. An exception leaves the set unchanged.
     *
     * `Sparsity` trades insertion speed for memory. A group grows its storage by 2 (`sh::sparsity::high`),
     * 4 (`sh::sparsity::medium`, default) or 8 (`sh::sparsity::low`) elements at a time. High sparsity means
     * less memory and slower insertions. The lookup speed does not depend on it.
     *
     * `AllocationFailure` says what happens when an allocation of the table fails, that is when the allocator
     * throws. With the default `dice::sparse_map::sh::allocation_failure::terminating` the process ends with
     * `std::abort()`. With `dice::sparse_map::sh::allocation_failure::throwing` the exception of the allocator
     * propagates, and the guarantees above hold for it. A size limit of the table throws `std::length_error` in both
     * modes.
     *
     * `Key` must be nothrow move constructible and/or copy constructible. The behaviour is undefined if the
     * destructor of `Key` throws.
     *
     * Iterator invalidation:
     *  - `clear`, `operator=`, `reserve`, `rehash`: always invalidate the iterators.
     *  - `insert`, `emplace`, `emplace_hint`: invalidate the iterators if an element is inserted.
     *  - `erase`: always invalidates the iterators. Use the returned iterator.
     */
    template<typename Key,
             typename Hash = detail_sparse_hash::default_hash<Key>,
             typename KeyEqual = std::equal_to<Key>,
             typename Allocator = std::allocator<Key>,
             sh::sparsity Sparsity = sh::sparsity::medium,
             sh::allocation_failure AllocationFailure = sh::allocation_failure::terminating>
    struct sparse_set {
        static_assert(sh::hash_is_avalanching_v<Hash>,
                      "Hash must be avalanching: sparse_set picks the bucket with the low bits of the hash as it is. "
                      "Mark an avalanching hash function with `using is_avalanching = void;` or with a specialization "
                      "of dice::sparse_map::sh::hash_is_avalanching. std::hash is not avalanching. "
                      "dice::hash::DiceHash is avalanching only for some policies.");

    private:
        using ht = detail_sparse_hash::sparse_hash<detail_sparse_hash::set_policy<Key>, Hash, KeyEqual, Allocator, Sparsity, AllocationFailure>;

        template<typename, typename, typename, typename, sh::sparsity, sh::allocation_failure>
        friend struct sparse_set;

        /// heterogeneous lookup and erasure are enabled by `KeyEqual::is_transparent`
        static constexpr bool is_transparent = detail_sparse_hash::IsTransparent<KeyEqual>;

    public:
        using key_type = Key;
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

    private:
        /// a key type for the heterogeneous overloads: transparent, and not an iterator
        template<typename K>
        static constexpr bool heterogeneous_key = is_transparent && detail_sparse_hash::NotIterator<K, iterator, const_iterator>;

    public:
        /*
         * Constructors
         */
        sparse_set()
            : sparse_set(ht::default_init_bucket_count) {
        }

        explicit sparse_set(size_type bucket_count,
                            Hash const &hash = Hash(),
                            KeyEqual const &equal = KeyEqual(),
                            Allocator const &alloc = Allocator())
            : ht_(bucket_count, hash, equal, alloc, ht::default_max_load_factor) {
        }

        sparse_set(size_type bucket_count, Allocator const &alloc)
            : sparse_set(bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_set(size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(bucket_count, hash, KeyEqual(), alloc) {
        }

        explicit sparse_set(Allocator const &alloc)
            : sparse_set(ht::default_init_bucket_count, alloc) {
        }

        template<detail_sparse_hash::LegacyInputIterator InputIt>
        sparse_set(InputIt first,
                   InputIt last,
                   size_type bucket_count = ht::default_init_bucket_count,
                   Hash const &hash = Hash(),
                   KeyEqual const &equal = KeyEqual(),
                   Allocator const &alloc = Allocator())
            : sparse_set(bucket_count, hash, equal, alloc) {
            insert(first, last);
        }

        template<detail_sparse_hash::LegacyInputIterator InputIt>
        sparse_set(InputIt first, InputIt last, size_type bucket_count, Allocator const &alloc)
            : sparse_set(first, last, bucket_count, Hash(), KeyEqual(), alloc) {
        }

        template<detail_sparse_hash::LegacyInputIterator InputIt>
        sparse_set(InputIt first, InputIt last, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(first, last, bucket_count, hash, KeyEqual(), alloc) {
        }

        template<detail_sparse_hash::LegacyInputIterator InputIt>
        sparse_set(InputIt first, InputIt last, Allocator const &alloc)
            : sparse_set(first, last, ht::default_init_bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_set(std::initializer_list<value_type> init,
                   size_type bucket_count = ht::default_init_bucket_count,
                   Hash const &hash = Hash(),
                   KeyEqual const &equal = KeyEqual(),
                   Allocator const &alloc = Allocator())
            : sparse_set(init.begin(), init.end(), bucket_count, hash, equal, alloc) {
        }

        sparse_set(std::initializer_list<value_type> init, size_type bucket_count, Allocator const &alloc)
            : sparse_set(init.begin(), init.end(), bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_set(std::initializer_list<value_type> init, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(init.begin(), init.end(), bucket_count, hash, KeyEqual(), alloc) {
        }

        sparse_set(std::initializer_list<value_type> init, Allocator const &alloc)
            : sparse_set(init.begin(), init.end(), ht::default_init_bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_set(sparse_set const &other) = default;
        sparse_set(sparse_set &&other) = default;

        ~sparse_set() = default;

        sparse_set &operator=(sparse_set const &other) = default;
        sparse_set &operator=(sparse_set &&other) = default;

        sparse_set &operator=(std::initializer_list<value_type> ilist) {
            ht_.clear();
            ht_.reserve(ilist.size());
            ht_.insert(ilist.begin(), ilist.end());

            return *this;
        }

        [[nodiscard]] allocator_type get_allocator() const noexcept {
            return ht_.get_allocator();
        }

        /*
         * Iterators
         */
        [[nodiscard]] iterator begin() noexcept {
            return ht_.begin();
        }

        [[nodiscard]] const_iterator begin() const noexcept {
            return ht_.begin();
        }

        [[nodiscard]] const_iterator cbegin() const noexcept {
            return ht_.cbegin();
        }

        [[nodiscard]] iterator end() noexcept {
            return ht_.end();
        }

        [[nodiscard]] const_iterator end() const noexcept {
            return ht_.end();
        }

        [[nodiscard]] const_iterator cend() const noexcept {
            return ht_.cend();
        }

        /*
         * Capacity
         */
        [[nodiscard]] bool empty() const noexcept {
            return ht_.empty();
        }

        [[nodiscard]] size_type size() const noexcept {
            return ht_.size();
        }

        [[nodiscard]] size_type max_size() const noexcept {
            return ht_.max_size();
        }

        /*
         * Modifiers
         */
        void clear() noexcept {
            ht_.clear();
        }

        std::pair<iterator, bool> insert(value_type const &value) {
            return ht_.insert(value);
        }

        std::pair<iterator, bool> insert(value_type &&value) {
            return ht_.insert(std::move(value));
        }

        iterator insert(const_iterator hint, value_type const &value) {
            return ht_.insert_hint(hint, value);
        }

        iterator insert(const_iterator hint, value_type &&value) {
            return ht_.insert_hint(hint, std::move(value));
        }

        template<detail_sparse_hash::LegacyInputIterator InputIt>
        void insert(InputIt first, InputIt last) {
            ht_.insert(first, last);
        }

        void insert(std::initializer_list<value_type> ilist) {
            ht_.insert(ilist.begin(), ilist.end());
        }

        /**
         * Constructs a `value_type` from `args`, and inserts it if it is not in the set yet. Like
         * `insert(value_type(std::forward<Args>(args)...))`.
         */
        template<typename... Args>
        std::pair<iterator, bool> emplace(Args &&...args) {
            return ht_.emplace(std::forward<Args>(args)...);
        }

        /**
         * Like `insert(hint, value_type(std::forward<Args>(args)...))`.
         */
        template<typename... Args>
        iterator emplace_hint(const_iterator hint, Args &&...args) {
            return ht_.emplace_hint(hint, std::forward<Args>(args)...);
        }

        iterator erase(iterator pos) {
            return ht_.erase(pos);
        }

        iterator erase(const_iterator pos) {
            return ht_.erase(pos);
        }

        iterator erase(const_iterator first, const_iterator last) {
            return ht_.erase(first, last);
        }

        size_type erase(key_type const &key) {
            return ht_.erase(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        size_type erase(key_type const &key, std::size_t precalculated_hash) {
            return ht_.erase(key, precalculated_hash);
        }

        /**
         * Heterogeneous `erase` (C++23). Only if `KeyEqual::is_transparent` exists. `K` must be hashable and
         * comparable to `Key`.
         */
        template<typename K>
        requires heterogeneous_key<K>
        size_type erase(K &&key) {
            return ht_.erase(key);
        }

        /**
         * @copydoc erase(K &&key)
         *
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`.
         */
        template<typename K>
        requires heterogeneous_key<K>
        size_type erase(K &&key, std::size_t precalculated_hash) {
            return ht_.erase(key, precalculated_hash);
        }

        void swap(sparse_set &other) noexcept(noexcept(std::declval<ht &>().swap(std::declval<ht &>()))) {
            other.ht_.swap(ht_);
        }

        /*
         * Lookup
         */
        [[nodiscard]] size_type count(key_type const &key) const {
            return ht_.count(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        [[nodiscard]] size_type count(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.count(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] size_type count(K const &key) const {
            return ht_.count(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] size_type count(K const &key, std::size_t precalculated_hash) const {
            return ht_.count(key, precalculated_hash);
        }

        [[nodiscard]] iterator find(key_type const &key) {
            return ht_.find(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        [[nodiscard]] iterator find(key_type const &key, std::size_t precalculated_hash) {
            return ht_.find(key, precalculated_hash);
        }

        [[nodiscard]] const_iterator find(key_type const &key) const {
            return ht_.find(key);
        }

        [[nodiscard]] const_iterator find(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.find(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] iterator find(K const &key) {
            return ht_.find(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] iterator find(K const &key, std::size_t precalculated_hash) {
            return ht_.find(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] const_iterator find(K const &key) const {
            return ht_.find(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] const_iterator find(K const &key, std::size_t precalculated_hash) const {
            return ht_.find(key, precalculated_hash);
        }

        [[nodiscard]] bool contains(key_type const &key) const {
            return ht_.contains(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        [[nodiscard]] bool contains(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.contains(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] bool contains(K const &key) const {
            return ht_.contains(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] bool contains(K const &key, std::size_t precalculated_hash) const {
            return ht_.contains(key, precalculated_hash);
        }

        [[nodiscard]] std::pair<iterator, iterator> equal_range(key_type const &key) {
            return ht_.equal_range(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        [[nodiscard]] std::pair<iterator, iterator> equal_range(key_type const &key, std::size_t precalculated_hash) {
            return ht_.equal_range(key, precalculated_hash);
        }

        [[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(key_type const &key) const {
            return ht_.equal_range(key);
        }

        [[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.equal_range(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] std::pair<iterator, iterator> equal_range(K const &key) {
            return ht_.equal_range(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] std::pair<iterator, iterator> equal_range(K const &key, std::size_t precalculated_hash) {
            return ht_.equal_range(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(K const &key) const {
            return ht_.equal_range(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(K const &key, std::size_t precalculated_hash) const {
            return ht_.equal_range(key, precalculated_hash);
        }

        /*
         * Bucket interface
         */
        [[nodiscard]] size_type bucket_count() const noexcept {
            return ht_.bucket_count();
        }

        [[nodiscard]] size_type max_bucket_count() const noexcept {
            return ht_.max_bucket_count();
        }

        /*
         * Hash policy
         */
        [[nodiscard]] float load_factor() const noexcept {
            return ht_.load_factor();
        }

        [[nodiscard]] float max_load_factor() const noexcept {
            return ht_.max_load_factor();
        }

        /**
         * Sets the maximum load factor. It is clamped to [0.1, 0.8].
         */
        void max_load_factor(float ml) noexcept {
            ht_.max_load_factor(ml);
        }

        void rehash(size_type count) {
            ht_.rehash(count);
        }

        void reserve(size_type count) {
            ht_.reserve(count);
        }

        /*
         * Observers
         */
        [[nodiscard]] hasher hash_function() const {
            return ht_.hash_function();
        }

        [[nodiscard]] key_equal key_eq() const {
            return ht_.key_eq();
        }

        /*
         * Other
         */

        /**
         * Converts a `const_iterator` into an `iterator`.
         */
        [[nodiscard]] iterator mutable_iterator(const_iterator pos) noexcept {
            return ht_.mutable_iterator(pos);
        }

        friend bool operator==(sparse_set const &lhs, sparse_set const &rhs) {
            if (lhs.size() != rhs.size()) {
                return false;
            }

            for (auto const &element : lhs) {
                if (!rhs.contains(element)) {
                    return false;
                }
            }

            return true;
        }

        friend void swap(sparse_set &lhs, sparse_set &rhs) noexcept(noexcept(lhs.swap(rhs))) {
            lhs.swap(rhs);
        }

    private:
        ht ht_;
    };

}  // namespace dice::sparse_map

#endif
