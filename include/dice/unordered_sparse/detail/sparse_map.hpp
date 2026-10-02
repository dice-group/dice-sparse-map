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
#ifndef DICE_UNORDERED_SPARSE_DETAIL_SPARSE_MAP_HPP
#define DICE_UNORDERED_SPARSE_DETAIL_SPARSE_MAP_HPP

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
     * A hash map with open addressing and quadratic probing that uses as little memory as possible, also at a
     * low load factor, and keeps good performance.
     *
     * The buckets are grouped by 64. A group stores only its occupied buckets, densely, and a bitmap tells which
     * buckets are occupied. The bucket count is 0 or a power of two and doubles when the table grows. The hash
     * picks the bucket with a mask. A hash function that is not marked as avalanching (see
     * `hash_is_avalanching`) is mixed first.
     *
     * The interface follows `std::unordered_map`, with these differences:
     *  - `value_type` is `std::pair<Key, T>`. Elements are stored as `Key` and `T` in a standard layout type,
     *    not as `std::pair`. The iterators return a proxy reference: `*it` is `std::pair<Key const &, T &>`,
     *    and `it->second` is a mutable reference to the mapped value. Bind it with `auto &&` or `auto const &`,
     *    not with `auto &`.
     *  - The iterators are forward iterators. They model `std::forward_iterator`, but a map iterator meets only
     *    the Cpp17InputIterator requirements, because its reference type is not `value_type &`.
     *  - There is no bucket interface beyond `bucket_count`, and no node handles.
     *  - `emplace` constructs the element first and inserts it if its key is not in the map.
     *  - Heterogeneous lookup and erasure are enabled by `KeyEqual::is_transparent` alone.
     *  - Lookups can take a precalculated hash, see the overloads with a `precalculated_hash` parameter.
     *
     * If an insertion throws, the map holds the same elements as before, except in one case where it is empty. It
     * comes from a rehash, which an insertion, `rehash` or `reserve` can do. `reserve` avoids rehashes. How a rehash
     * transfers the elements depends on their type:
     * - Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old
     *   buckets is freed while they are moved. If the hash function or the allocator throws while the elements are
     *   moved, the map is empty. Any other exception leaves the map unchanged.
     * - Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and
     *   the new buckets are in memory at the same time. An exception leaves the map unchanged.
     *
     * `sparsity` trades insertion speed for memory. A group grows its storage by 2 (`unordered_sparse::sparsity::high`),
     * 4 (`unordered_sparse::sparsity::medium`, default) or 8 (`unordered_sparse::sparsity::low`) elements at a time. High sparsity means
     * less memory and slower insertions. The lookup speed does not depend on it.
     *
     * `Key` and `T` must be nothrow move constructible and/or copy constructible. The behaviour is undefined if
     * the destructor of `Key` or `T` throws.
     *
     * Iterator invalidation:
     *  - `clear`, `operator=`, `reserve`, `rehash`: always invalidate the iterators.
     *  - `insert`, `emplace`, `emplace_hint`, `try_emplace`, `insert_or_assign`, `operator[]`: invalidate the
     *    iterators if an element is inserted.
     *  - `erase`: always invalidates the iterators. Use the returned iterator.
     */
    template<typename Key,
             typename T,
             typename Hash = std::hash<Key>,
             typename KeyEqual = std::equal_to<Key>,
             typename Allocator = std::allocator<std::pair<Key, T>>,
             unordered_sparse::sparsity sparsity = unordered_sparse::sparsity::medium>
    struct sparse_map {
    private:
        using ht = detail::sparse_hash<detail::map_policy<Key, T>, Hash, KeyEqual, Allocator, sparsity>;

        template<typename, typename, typename, typename, typename, unordered_sparse::sparsity>
        friend struct sparse_map;

        /// heterogeneous lookup and erasure are enabled by `KeyEqual::is_transparent`
        static constexpr bool is_transparent = detail::IsTransparent<KeyEqual>;

    public:
        using key_type = Key;
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

    private:
        /// a key type for the heterogeneous overloads: transparent, and not an iterator
        template<typename K>
        static constexpr bool heterogeneous_key = is_transparent && detail::NotIterator<K, iterator, const_iterator>;

    public:
        /*
         * Constructors
         */
        sparse_map()
            : sparse_map(ht::default_init_bucket_count) {
        }

        explicit sparse_map(size_type bucket_count,
                            Hash const &hash = Hash(),
                            KeyEqual const &equal = KeyEqual(),
                            Allocator const &alloc = Allocator())
            : ht_(bucket_count, hash, equal, alloc, ht::default_max_load_factor) {
        }

        sparse_map(size_type bucket_count, Allocator const &alloc)
            : sparse_map(bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_map(size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_map(bucket_count, hash, KeyEqual(), alloc) {
        }

        explicit sparse_map(Allocator const &alloc)
            : sparse_map(ht::default_init_bucket_count, alloc) {
        }

        template<detail::LegacyInputIterator InputIt>
        sparse_map(InputIt first,
                   InputIt last,
                   size_type bucket_count = ht::default_init_bucket_count,
                   Hash const &hash = Hash(),
                   KeyEqual const &equal = KeyEqual(),
                   Allocator const &alloc = Allocator())
            : sparse_map(bucket_count, hash, equal, alloc) {
            insert(first, last);
        }

        template<detail::LegacyInputIterator InputIt>
        sparse_map(InputIt first, InputIt last, size_type bucket_count, Allocator const &alloc)
            : sparse_map(first, last, bucket_count, Hash(), KeyEqual(), alloc) {
        }

        template<detail::LegacyInputIterator InputIt>
        sparse_map(InputIt first, InputIt last, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_map(first, last, bucket_count, hash, KeyEqual(), alloc) {
        }

        template<detail::LegacyInputIterator InputIt>
        sparse_map(InputIt first, InputIt last, Allocator const &alloc)
            : sparse_map(first, last, ht::default_init_bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_map(std::initializer_list<value_type> init,
                   size_type bucket_count = ht::default_init_bucket_count,
                   Hash const &hash = Hash(),
                   KeyEqual const &equal = KeyEqual(),
                   Allocator const &alloc = Allocator())
            : sparse_map(init.begin(), init.end(), bucket_count, hash, equal, alloc) {
        }

        sparse_map(std::initializer_list<value_type> init, size_type bucket_count, Allocator const &alloc)
            : sparse_map(init.begin(), init.end(), bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_map(std::initializer_list<value_type> init, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_map(init.begin(), init.end(), bucket_count, hash, KeyEqual(), alloc) {
        }

        sparse_map(std::initializer_list<value_type> init, Allocator const &alloc)
            : sparse_map(init.begin(), init.end(), ht::default_init_bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_map(sparse_map const &other) = default;
        sparse_map(sparse_map &&other) = default;

        ~sparse_map() = default;

        sparse_map &operator=(sparse_map const &other) = default;
        sparse_map &operator=(sparse_map &&other) = default;

        sparse_map &operator=(std::initializer_list<value_type> ilist) {
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

        template<typename P>
        requires std::is_constructible_v<value_type, P &&>
        std::pair<iterator, bool> insert(P &&value) {
            return ht_.emplace(std::forward<P>(value));
        }

        iterator insert(const_iterator hint, value_type const &value) {
            return ht_.insert_hint(hint, value);
        }

        iterator insert(const_iterator hint, value_type &&value) {
            return ht_.insert_hint(hint, std::move(value));
        }

        template<typename P>
        requires std::is_constructible_v<value_type, P &&>
        iterator insert(const_iterator hint, P &&value) {
            return ht_.emplace_hint(hint, std::forward<P>(value));
        }

        template<detail::LegacyInputIterator InputIt>
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
            return ht_.insert_or_assign_hint(hint, k, std::forward<M>(obj));
        }

        template<typename M>
        iterator insert_or_assign(const_iterator hint, key_type &&k, M &&obj) {
            return ht_.insert_or_assign_hint(hint, std::move(k), std::forward<M>(obj));
        }

        /**
         * Constructs a `value_type` from `args`, and inserts it if its key is not in the map yet. Like
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

        template<typename... Args>
        std::pair<iterator, bool> try_emplace(key_type const &k, Args &&...args) {
            return ht_.try_emplace(k, std::forward<Args>(args)...);
        }

        template<typename... Args>
        std::pair<iterator, bool> try_emplace(key_type &&k, Args &&...args) {
            return ht_.try_emplace(std::move(k), std::forward<Args>(args)...);
        }

        template<typename... Args>
        iterator try_emplace(const_iterator hint, key_type const &k, Args &&...args) {
            return ht_.try_emplace_hint(hint, k, std::forward<Args>(args)...);
        }

        template<typename... Args>
        iterator try_emplace(const_iterator hint, key_type &&k, Args &&...args) {
            return ht_.try_emplace_hint(hint, std::move(k), std::forward<Args>(args)...);
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

        void swap(sparse_map &other) noexcept(noexcept(std::declval<ht &>().swap(std::declval<ht &>()))) {
            other.ht_.swap(ht_);
        }

        /*
         * Lookup
         */

        /**
         * @throws std::out_of_range if there is no element with the key `key`
         */
        T &at(key_type const &key) {
            return ht_.at(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        T &at(key_type const &key, std::size_t precalculated_hash) {
            return ht_.at(key, precalculated_hash);
        }

        T const &at(key_type const &key) const {
            return ht_.at(key);
        }

        /**
         * @copydoc at(key_type const &key, std::size_t precalculated_hash)
         */
        T const &at(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.at(key, precalculated_hash);
        }

        /**
         * Heterogeneous `at`. Only if `KeyEqual::is_transparent` exists. `K` must be hashable and comparable to
         * `Key`.
         */
        template<typename K>
        requires is_transparent
        T &at(K const &key) {
            return ht_.at(key);
        }

        template<typename K>
        requires is_transparent
        T &at(K const &key, std::size_t precalculated_hash) {
            return ht_.at(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        T const &at(K const &key) const {
            return ht_.at(key);
        }

        template<typename K>
        requires is_transparent
        T const &at(K const &key, std::size_t precalculated_hash) const {
            return ht_.at(key, precalculated_hash);
        }

        T &operator[](key_type const &key) {
            return ht_[key];
        }

        T &operator[](key_type &&key) {
            return ht_[std::move(key)];
        }

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

        friend bool operator==(sparse_map const &lhs, sparse_map const &rhs) {
            if (lhs.size() != rhs.size()) {
                return false;
            }

            for (auto const &[key, value] : lhs) {
                auto const it_rhs = rhs.find(key);
                if (it_rhs == rhs.cend() || !(value == it_rhs->second)) {
                    return false;
                }
            }

            return true;
        }

        friend void swap(sparse_map &lhs, sparse_map &rhs) noexcept(noexcept(lhs.swap(rhs))) {
            lhs.swap(rhs);
        }

    private:
        ht ht_;
    };

}  // namespace dice::unordered_sparse

#endif
