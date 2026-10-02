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
#ifndef DICE_UNORDERED_SPARSE_DETAIL_SPARSE_SET_HPP
#define DICE_UNORDERED_SPARSE_DETAIL_SPARSE_SET_HPP

#include <cstddef>
#include <functional>
#include <initializer_list>
#include <memory>
#include <ranges>
#include <type_traits>
#include <utility>

#include <dice/unordered_sparse/detail/sparse_hash.hpp>

namespace dice::unordered_sparse {

    /**
     * A hash set with open addressing and quadratic probing that uses as little memory as possible, also at a
     * low load factor, and keeps good performance.
     *
     * The buckets are grouped by 64. A group stores only its occupied buckets, densely, and a bitmap tells which
     * buckets are occupied. The bucket count is 0 or a power of two and doubles when the table grows. The hash
     * picks the bucket with a mask. A hash function that is not marked as avalanching (see
     * `hash_is_avalanching`) is mixed first.
     *
     * The interface follows `std::unordered_set`, with these differences:
     *  - The iterators are forward iterators.
     *  - There is no bucket interface beyond `bucket_count`, and no node handles.
     *  - Heterogeneous lookup and insertion are enabled by `KeyEqual::is_transparent` alone.
     *  - Lookups can take a precalculated hash, see the overloads with a `precalculated_hash` parameter.
     *
     * If an insertion throws, the set holds the same elements as before, except in one case where it is empty. It
     * comes from a rehash, which an insertion, `merge`, `rehash` or `reserve` can do. `reserve` avoids rehashes. How
     * a rehash transfers the elements depends on their type:
     * - Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old
     *   buckets is freed while they are moved. If the hash function or the allocator throws while the elements are
     *   moved, the set is empty. Any other exception leaves the set unchanged.
     * - Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and
     *   the new buckets are in memory at the same time. An exception leaves the set unchanged.
     *
     * `sparsity` trades insertion speed for memory. A group grows its storage by 2 (`unordered_sparse::sparsity::high`),
     * 4 (`unordered_sparse::sparsity::medium`, default) or 8 (`unordered_sparse::sparsity::low`) elements at a time. High sparsity means
     * less memory and slower insertions. The lookup speed does not depend on it.
     *
     * `Key` must be nothrow move constructible and/or copy constructible. The behaviour is undefined if the
     * destructor of `Key` throws.
     *
     * Iterator invalidation:
     *  - `clear`, `operator=`, `reserve`, `rehash`, `merge`: always invalidate the iterators.
     *  - `insert`, `emplace`, `emplace_hint`: invalidate the iterators if an element is inserted.
     *  - `erase`: always invalidates the iterators. Use the returned iterator.
     */
    template<typename Key,
             typename Hash = std::hash<Key>,
             typename KeyEqual = std::equal_to<Key>,
             typename Allocator = std::allocator<Key>,
             unordered_sparse::sparsity sparsity = unordered_sparse::sparsity::medium>
    struct sparse_set {
    private:
        using ht = detail::sparse_hash<detail::set_policy<Key>, Hash, KeyEqual, Allocator, sparsity>;

        template<typename, typename, typename, typename, unordered_sparse::sparsity>
        friend struct sparse_set;

        /// heterogeneous lookup and insertion are enabled by `KeyEqual::is_transparent`
        static constexpr bool is_transparent = detail::IsTransparent<KeyEqual>;

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
        static constexpr bool heterogeneous_key = is_transparent && detail::NotIterator<K, iterator, const_iterator>;

    public:
        /*
         * Constructors
         */
        constexpr sparse_set()
            : sparse_set(ht::default_init_bucket_count) {
        }

        constexpr explicit sparse_set(size_type bucket_count,
                                      Hash const &hash = Hash(),
                                      KeyEqual const &equal = KeyEqual(),
                                      Allocator const &alloc = Allocator())
            : ht_(bucket_count, hash, equal, alloc, ht::default_max_load_factor) {
        }

        constexpr sparse_set(size_type bucket_count, Allocator const &alloc)
            : sparse_set(bucket_count, Hash(), KeyEqual(), alloc) {
        }

        constexpr sparse_set(size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(bucket_count, hash, KeyEqual(), alloc) {
        }

        constexpr explicit sparse_set(Allocator const &alloc)
            : sparse_set(ht::default_init_bucket_count, alloc) {
        }

        template<detail::LegacyInputIterator InputIt>
        constexpr sparse_set(InputIt first,
                             InputIt last,
                             size_type bucket_count = ht::default_init_bucket_count,
                             Hash const &hash = Hash(),
                             KeyEqual const &equal = KeyEqual(),
                             Allocator const &alloc = Allocator())
            : sparse_set(bucket_count, hash, equal, alloc) {
            insert(first, last);
        }

        template<detail::LegacyInputIterator InputIt>
        constexpr sparse_set(InputIt first, InputIt last, size_type bucket_count, Allocator const &alloc)
            : sparse_set(first, last, bucket_count, Hash(), KeyEqual(), alloc) {
        }

        template<detail::LegacyInputIterator InputIt>
        constexpr sparse_set(InputIt first, InputIt last, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(first, last, bucket_count, hash, KeyEqual(), alloc) {
        }

        template<detail::LegacyInputIterator InputIt>
        constexpr sparse_set(InputIt first, InputIt last, Allocator const &alloc)
            : sparse_set(first, last, ht::default_init_bucket_count, Hash(), KeyEqual(), alloc) {
        }

        /**
         * Constructs the set from the elements of `range` (`std::from_range` constructor of C++23).
         */
        template<detail::ContainerCompatibleRange<value_type> R>
        constexpr sparse_set(std::from_range_t /*tag*/,
                             R &&range,
                             size_type bucket_count = ht::default_init_bucket_count,
                             Hash const &hash = Hash(),
                             KeyEqual const &equal = KeyEqual(),
                             Allocator const &alloc = Allocator())
            : sparse_set(bucket_count, hash, equal, alloc) {
            insert_range(std::forward<R>(range));
        }

        template<detail::ContainerCompatibleRange<value_type> R>
        constexpr sparse_set(std::from_range_t /*tag*/, R &&range, size_type bucket_count, Allocator const &alloc)
            : sparse_set(std::from_range, std::forward<R>(range), bucket_count, Hash(), KeyEqual(), alloc) {
        }

        template<detail::ContainerCompatibleRange<value_type> R>
        constexpr sparse_set(std::from_range_t /*tag*/, R &&range, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(std::from_range, std::forward<R>(range), bucket_count, hash, KeyEqual(), alloc) {
        }

        template<detail::ContainerCompatibleRange<value_type> R>
        constexpr sparse_set(std::from_range_t /*tag*/, R &&range, Allocator const &alloc)
            : sparse_set(std::from_range, std::forward<R>(range), ht::default_init_bucket_count, Hash(), KeyEqual(), alloc) {
        }

        constexpr sparse_set(std::initializer_list<value_type> init,
                             size_type bucket_count = ht::default_init_bucket_count,
                             Hash const &hash = Hash(),
                             KeyEqual const &equal = KeyEqual(),
                             Allocator const &alloc = Allocator())
            : sparse_set(init.begin(), init.end(), bucket_count, hash, equal, alloc) {
        }

        constexpr sparse_set(std::initializer_list<value_type> init, size_type bucket_count, Allocator const &alloc)
            : sparse_set(init.begin(), init.end(), bucket_count, Hash(), KeyEqual(), alloc) {
        }

        constexpr sparse_set(std::initializer_list<value_type> init, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(init.begin(), init.end(), bucket_count, hash, KeyEqual(), alloc) {
        }

        constexpr sparse_set(std::initializer_list<value_type> init, Allocator const &alloc)
            : sparse_set(init.begin(), init.end(), ht::default_init_bucket_count, Hash(), KeyEqual(), alloc) {
        }

        constexpr sparse_set(sparse_set const &other) = default;
        constexpr sparse_set(sparse_set &&other) = default;

        /**
         * Copies `other` into a set with the allocator `alloc`.
         */
        constexpr sparse_set(sparse_set const &other, std::type_identity_t<Allocator> const &alloc)
            : ht_(other.ht_, alloc) {
        }

        /**
         * Moves `other` into a set with the allocator `alloc`. If `alloc` is not equal to the allocator of `other`,
         * the elements are moved one by one.
         */
        constexpr sparse_set(sparse_set &&other, std::type_identity_t<Allocator> const &alloc)
            : ht_(std::move(other.ht_), alloc) {
        }

        constexpr ~sparse_set() = default;

        sparse_set &operator=(sparse_set const &other) = default;
        sparse_set &operator=(sparse_set &&other) = default;

        constexpr sparse_set &operator=(std::initializer_list<value_type> ilist) {
            ht_.clear();
            ht_.reserve(ilist.size());
            ht_.insert(ilist.begin(), ilist.end());

            return *this;
        }

        [[nodiscard]] constexpr allocator_type get_allocator() const noexcept {
            return ht_.get_allocator();
        }

        /*
         * Iterators
         */
        [[nodiscard]] constexpr iterator begin() noexcept {
            return ht_.begin();
        }

        [[nodiscard]] constexpr const_iterator begin() const noexcept {
            return ht_.begin();
        }

        [[nodiscard]] constexpr const_iterator cbegin() const noexcept {
            return ht_.cbegin();
        }

        [[nodiscard]] constexpr iterator end() noexcept {
            return ht_.end();
        }

        [[nodiscard]] constexpr const_iterator end() const noexcept {
            return ht_.end();
        }

        [[nodiscard]] constexpr const_iterator cend() const noexcept {
            return ht_.cend();
        }

        /*
         * Capacity
         */
        [[nodiscard]] constexpr bool empty() const noexcept {
            return ht_.empty();
        }

        [[nodiscard]] constexpr size_type size() const noexcept {
            return ht_.size();
        }

        [[nodiscard]] constexpr size_type max_size() const noexcept {
            return ht_.max_size();
        }

        /*
         * Modifiers
         */
        constexpr void clear() noexcept {
            ht_.clear();
        }

        constexpr std::pair<iterator, bool> insert(value_type const &value) {
            return ht_.insert(value);
        }

        constexpr std::pair<iterator, bool> insert(value_type &&value) {
            return ht_.insert(std::move(value));
        }

        /**
         * Heterogeneous `insert` (C++26). Only if `KeyEqual::is_transparent` exists. The element is constructed
         * from `key` only if it is inserted.
         */
        template<typename K>
        requires (heterogeneous_key<K> && std::is_constructible_v<value_type, K &&>)
        constexpr std::pair<iterator, bool> insert(K &&key) {
            return ht_.insert_key(std::forward<K>(key));
        }

        constexpr iterator insert(const_iterator hint, value_type const &value) {
            return ht_.insert_hint(hint, value);
        }

        constexpr iterator insert(const_iterator hint, value_type &&value) {
            return ht_.insert_hint(hint, std::move(value));
        }

        template<typename K>
        requires (heterogeneous_key<K> && std::is_constructible_v<value_type, K &&>)
        constexpr iterator insert(const_iterator hint, K &&key) {
            if (hint != cend() && key_eq()(*hint, key)) {
                return mutable_iterator(hint);
            }
            return ht_.insert_key(std::forward<K>(key)).first;
        }

        template<detail::LegacyInputIterator InputIt>
        constexpr void insert(InputIt first, InputIt last) {
            ht_.insert(first, last);
        }

        constexpr void insert(std::initializer_list<value_type> ilist) {
            ht_.insert(ilist.begin(), ilist.end());
        }

        /**
         * Inserts every element of `range` that is not in the set yet (C++23).
         */
        template<detail::ContainerCompatibleRange<value_type> R>
        constexpr void insert_range(R &&range) {
            ht_.insert_range(std::forward<R>(range));
        }

        /**
         * Constructs a `value_type` from `args` with the allocator of the set, and inserts it if it is not in the set
         * yet. A scoped or a polymorphic allocator passes itself on to the element.
         */
        template<typename... Args>
        constexpr std::pair<iterator, bool> emplace(Args &&...args) {
            return ht_.emplace(std::forward<Args>(args)...);
        }

        /**
         * Constructs a `value_type` from `args` with the allocator of the set, and inserts it like
         * `insert(hint, value)`.
         */
        template<typename... Args>
        constexpr iterator emplace_hint(const_iterator hint, Args &&...args) {
            return ht_.emplace_hint(hint, std::forward<Args>(args)...);
        }

        constexpr iterator erase(iterator pos) {
            return ht_.erase(pos);
        }

        constexpr iterator erase(const_iterator pos) {
            return ht_.erase(pos);
        }

        constexpr iterator erase(const_iterator first, const_iterator last) {
            return ht_.erase(first, last);
        }

        constexpr size_type erase(key_type const &key) {
            return ht_.erase(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        constexpr size_type erase(key_type const &key, std::size_t precalculated_hash) {
            return ht_.erase(key, precalculated_hash);
        }

        /**
         * Heterogeneous `erase` (C++23). Only if `KeyEqual::is_transparent` exists. `K` must be hashable and
         * comparable to `Key`.
         */
        template<typename K>
        requires heterogeneous_key<K>
        constexpr size_type erase(K &&key) {
            return ht_.erase(key);
        }

        /**
         * @copydoc erase(K &&key)
         *
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`.
         */
        template<typename K>
        requires heterogeneous_key<K>
        constexpr size_type erase(K &&key, std::size_t precalculated_hash) {
            return ht_.erase(key, precalculated_hash);
        }

        constexpr void swap(sparse_set &other) noexcept(noexcept(std::declval<ht &>().swap(std::declval<ht &>()))) {
            other.ht_.swap(ht_);
        }

        /**
         * Moves the elements of `source` that are not in this set into this set (C++17). The elements are moved, or
         * copied if their move constructor can throw, and erased from `source`. Elements that are in this set stay in
         * `source`. If it throws, the element that was being merged and the ones not reached yet stay in `source`.
         * The elements merged before it are in this set, unless a rehash of this set empties it, see the class
         * documentation.
         */
        template<typename H2, typename P2, unordered_sparse::sparsity S2>
        constexpr void merge(sparse_set<Key, H2, P2, Allocator, S2> &source) {
            ht_.merge(source.ht_);
        }

        template<typename H2, typename P2, unordered_sparse::sparsity S2>
        constexpr void merge(sparse_set<Key, H2, P2, Allocator, S2> &&source) {
            ht_.merge(source.ht_);
        }

        /*
         * Lookup
         */
        [[nodiscard]] constexpr size_type count(key_type const &key) const {
            return ht_.count(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        [[nodiscard]] constexpr size_type count(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.count(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr size_type count(K const &key) const {
            return ht_.count(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr size_type count(K const &key, std::size_t precalculated_hash) const {
            return ht_.count(key, precalculated_hash);
        }

        [[nodiscard]] constexpr iterator find(key_type const &key) {
            return ht_.find(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        [[nodiscard]] constexpr iterator find(key_type const &key, std::size_t precalculated_hash) {
            return ht_.find(key, precalculated_hash);
        }

        [[nodiscard]] constexpr const_iterator find(key_type const &key) const {
            return ht_.find(key);
        }

        [[nodiscard]] constexpr const_iterator find(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.find(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr iterator find(K const &key) {
            return ht_.find(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr iterator find(K const &key, std::size_t precalculated_hash) {
            return ht_.find(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr const_iterator find(K const &key) const {
            return ht_.find(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr const_iterator find(K const &key, std::size_t precalculated_hash) const {
            return ht_.find(key, precalculated_hash);
        }

        [[nodiscard]] constexpr bool contains(key_type const &key) const {
            return ht_.contains(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        [[nodiscard]] constexpr bool contains(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.contains(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr bool contains(K const &key) const {
            return ht_.contains(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr bool contains(K const &key, std::size_t precalculated_hash) const {
            return ht_.contains(key, precalculated_hash);
        }

        [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(key_type const &key) {
            return ht_.equal_range(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(key_type const &key, std::size_t precalculated_hash) {
            return ht_.equal_range(key, precalculated_hash);
        }

        [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(key_type const &key) const {
            return ht_.equal_range(key);
        }

        [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.equal_range(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(K const &key) {
            return ht_.equal_range(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(K const &key, std::size_t precalculated_hash) {
            return ht_.equal_range(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(K const &key) const {
            return ht_.equal_range(key);
        }

        template<typename K>
        requires is_transparent
        [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(K const &key, std::size_t precalculated_hash) const {
            return ht_.equal_range(key, precalculated_hash);
        }

        /*
         * Bucket interface
         */
        [[nodiscard]] constexpr size_type bucket_count() const noexcept {
            return ht_.bucket_count();
        }

        [[nodiscard]] constexpr size_type max_bucket_count() const noexcept {
            return ht_.max_bucket_count();
        }

        /*
         * Hash policy
         */
        [[nodiscard]] constexpr float load_factor() const noexcept {
            return ht_.load_factor();
        }

        [[nodiscard]] constexpr float max_load_factor() const noexcept {
            return ht_.max_load_factor();
        }

        /**
         * Sets the maximum load factor. It is clamped to [0.1, 0.8].
         */
        constexpr void max_load_factor(float ml) noexcept {
            ht_.max_load_factor(ml);
        }

        constexpr void rehash(size_type count) {
            ht_.rehash(count);
        }

        constexpr void reserve(size_type count) {
            ht_.reserve(count);
        }

        /*
         * Observers
         */
        [[nodiscard]] constexpr hasher hash_function() const {
            return ht_.hash_function();
        }

        [[nodiscard]] constexpr key_equal key_eq() const {
            return ht_.key_eq();
        }

        /*
         * Other
         */

        /**
         * Converts a `const_iterator` into an `iterator`.
         */
        [[nodiscard]] constexpr iterator mutable_iterator(const_iterator pos) noexcept {
            return ht_.mutable_iterator(pos);
        }

        friend constexpr bool operator==(sparse_set const &lhs, sparse_set const &rhs) {
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

        friend constexpr void swap(sparse_set &lhs, sparse_set &rhs) noexcept(noexcept(lhs.swap(rhs))) {
            lhs.swap(rhs);
        }

    private:
        ht ht_;
    };

    /**
     * Erases every element `e` of `set` for which `pred(e)` is true (C++20).
     * @return the number of erased elements
     */
    template<typename Key, typename Hash, typename KeyEqual, typename Allocator, unordered_sparse::sparsity sparsity, typename Predicate>
    constexpr typename sparse_set<Key, Hash, KeyEqual, Allocator, sparsity>::size_type
    erase_if(sparse_set<Key, Hash, KeyEqual, Allocator, sparsity> &set, Predicate pred) {
        auto const old_size = set.size();
        for (auto it = set.cbegin(); it != set.cend();) {
            if (pred(*it)) {
                it = set.erase(it);
            } else {
                ++it;
            }
        }
        return old_size - set.size();
    }

    /*
     * Deduction guides, as for `std::unordered_set`
     */
    template<detail::LegacyInputIterator InputIt,
             typename Hash = std::hash<detail::iter_value_t<InputIt>>,
             typename KeyEqual = std::equal_to<detail::iter_value_t<InputIt>>,
             typename Allocator = std::allocator<detail::iter_value_t<InputIt>>>
    requires (!std::is_integral_v<Hash> && !detail::AllocatorLike<Hash> && !detail::AllocatorLike<KeyEqual> && detail::AllocatorLike<Allocator>)
    sparse_set(InputIt, InputIt, std::size_t = 0, Hash = Hash(), KeyEqual = KeyEqual(), Allocator = Allocator())
        -> sparse_set<detail::iter_value_t<InputIt>, Hash, KeyEqual, Allocator>;

    template<std::ranges::input_range R,
             typename Hash = std::hash<std::ranges::range_value_t<R>>,
             typename KeyEqual = std::equal_to<std::ranges::range_value_t<R>>,
             typename Allocator = std::allocator<std::ranges::range_value_t<R>>>
    requires (!std::is_integral_v<Hash> && !detail::AllocatorLike<Hash> && !detail::AllocatorLike<KeyEqual> && detail::AllocatorLike<Allocator>)
    sparse_set(std::from_range_t, R &&, std::size_t = 0, Hash = Hash(), KeyEqual = KeyEqual(), Allocator = Allocator())
        -> sparse_set<std::ranges::range_value_t<R>, Hash, KeyEqual, Allocator>;

    template<typename Key,
             typename Hash = std::hash<Key>,
             typename KeyEqual = std::equal_to<Key>,
             typename Allocator = std::allocator<Key>>
    requires (!std::is_integral_v<Hash> && !detail::AllocatorLike<Hash> && !detail::AllocatorLike<KeyEqual> && detail::AllocatorLike<Allocator>)
    sparse_set(std::initializer_list<Key>, std::size_t = 0, Hash = Hash(), KeyEqual = KeyEqual(), Allocator = Allocator())
        -> sparse_set<Key, Hash, KeyEqual, Allocator>;

    template<detail::LegacyInputIterator InputIt, detail::AllocatorLike Allocator>
    sparse_set(InputIt, InputIt, std::size_t, Allocator)
        -> sparse_set<detail::iter_value_t<InputIt>, std::hash<detail::iter_value_t<InputIt>>, std::equal_to<detail::iter_value_t<InputIt>>, Allocator>;

    template<detail::LegacyInputIterator InputIt, detail::AllocatorLike Allocator>
    sparse_set(InputIt, InputIt, Allocator)
        -> sparse_set<detail::iter_value_t<InputIt>, std::hash<detail::iter_value_t<InputIt>>, std::equal_to<detail::iter_value_t<InputIt>>, Allocator>;

    template<detail::LegacyInputIterator InputIt, typename Hash, detail::AllocatorLike Allocator>
    requires (!std::is_integral_v<Hash> && !detail::AllocatorLike<Hash>)
    sparse_set(InputIt, InputIt, std::size_t, Hash, Allocator)
        -> sparse_set<detail::iter_value_t<InputIt>, Hash, std::equal_to<detail::iter_value_t<InputIt>>, Allocator>;

    template<std::ranges::input_range R, detail::AllocatorLike Allocator>
    sparse_set(std::from_range_t, R &&, std::size_t, Allocator)
        -> sparse_set<std::ranges::range_value_t<R>, std::hash<std::ranges::range_value_t<R>>, std::equal_to<std::ranges::range_value_t<R>>, Allocator>;

    template<std::ranges::input_range R, detail::AllocatorLike Allocator>
    sparse_set(std::from_range_t, R &&, Allocator)
        -> sparse_set<std::ranges::range_value_t<R>, std::hash<std::ranges::range_value_t<R>>, std::equal_to<std::ranges::range_value_t<R>>, Allocator>;

    template<std::ranges::input_range R, typename Hash, detail::AllocatorLike Allocator>
    requires (!std::is_integral_v<Hash> && !detail::AllocatorLike<Hash>)
    sparse_set(std::from_range_t, R &&, std::size_t, Hash, Allocator)
        -> sparse_set<std::ranges::range_value_t<R>, Hash, std::equal_to<std::ranges::range_value_t<R>>, Allocator>;

    template<typename Key, detail::AllocatorLike Allocator>
    sparse_set(std::initializer_list<Key>, std::size_t, Allocator)
        -> sparse_set<Key, std::hash<Key>, std::equal_to<Key>, Allocator>;

    template<typename Key, detail::AllocatorLike Allocator>
    sparse_set(std::initializer_list<Key>, Allocator)
        -> sparse_set<Key, std::hash<Key>, std::equal_to<Key>, Allocator>;

    template<typename Key, typename Hash, detail::AllocatorLike Allocator>
    requires (!std::is_integral_v<Hash> && !detail::AllocatorLike<Hash>)
    sparse_set(std::initializer_list<Key>, std::size_t, Hash, Allocator)
        -> sparse_set<Key, Hash, std::equal_to<Key>, Allocator>;

}  // namespace dice::unordered_sparse

#endif
