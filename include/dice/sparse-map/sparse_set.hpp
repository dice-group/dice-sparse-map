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
     *  - Lookups can take a precalculated hash, see the overloads with a `precalculated_hash` parameter.
     *
     * The heterogeneous overloads take a key of another type than `Key`, for example a `std::string_view` for a
     * `std::string` key. As in the standard library, they exist only if `Hash::is_transparent` and
     * `KeyEqual::is_transparent` both exist, and the hash function and the key equality must accept the other type.
     * Otherwise the argument is converted to `Key` first.
     *
     * The exception guarantee follows from the type of the elements. If the insertion of one element throws, the set
     * holds the same elements as before, except in one case: a rehash can leave the set empty. An insertion, `merge`,
     * `rehash` or `reserve` can rehash. `reserve` avoids rehashes.
     * How a rehash transfers the elements depends on their type:
     * - Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old
     *   buckets is freed while they are moved. If the hash function throws while the elements are moved, or the
     *   allocator with `allocation_failure::throwing`, the set is empty. Any other exception leaves the set unchanged.
     * - Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and
     *   the new buckets are in memory at the same time. An exception leaves the set unchanged.
     *
     * `erase` does not throw unless the hash function or the key equality throws, as for `std::unordered_set`.
     *
     * The set never moves an element whose move constructor can throw. It copies it. An erase of such an element
     * destroys it in place and leaves a hole: the memory of the element stays with its group of 64 buckets. A new
     * element in the bucket of a hole takes that memory without an allocation. The group gives the memory of its
     * holes back when it gets a new element in another bucket (it then copies its elements into new memory of the
     * exact size), when it is rehashed, and when its last element is erased. A copy of the set has no holes and
     * holds its elements in memory of the exact size. The clean-up rehash, which also removes the marks of erased
     * elements, bounds the number of holes.
     *
     * `begin()` is constant time: the set keeps the index of its first group of 64 buckets that holds an element.
     * An erase that empties this group looks for the next group with an element, one group after the other. So the
     * erase costs one step for every empty group in between, and erasing all elements in the order of iteration, as
     * in `while (!set.empty()) { set.erase(set.begin()); }`, costs one step per group in total.
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
     * All member functions are `constexpr`. A set works in a constant expression if its hash function, key equality,
     * allocator and elements do, for example with `std::allocator` and a hash function with a `constexpr` call
     * operator. The default hash function `dice::hash::DiceHash` is not `constexpr`, so a set in a constant
     * expression needs another hash function.
     *
     * `Key` must be nothrow move constructible and/or copy constructible. The behaviour is undefined if the
     * destructor of `Key` throws. If `Key` is nothrow move constructible, the set moves its elements with
     * `std::allocator_traits<Allocator>::construct` and expects that this does not throw either. With a scoped or a
     * polymorphic allocator, that is the allocator-extended move constructor of `Key`, which does not throw when the
     * allocators are equal. `std::vector` makes the same assumption. If `Key` is also nothrow move assignable, an
     * insertion or an erase moves the elements after it in its group of 64 buckets with their move assignment, as
     * `std::vector::insert` and `std::vector::erase` do. Then an insertion in the middle of a group that has room
     * constructs two elements through the allocator (the new element in a temporary, and one at the end of the group)
     * and destroys one (the temporary), and an erase destroys one.
     *
     * Invalidation of iterators, references and pointers to elements: a group stores its elements densely, and an
     * insertion or an erasure can move them in the group or to new memory. So, unlike with `std::unordered_set`,
     * references and pointers to elements are invalidated like the iterators.
     *  - `clear`, `operator=`, `reserve`, `rehash`: may invalidate the iterators, references and pointers.
     *  - `merge`: always invalidates the iterators, references and pointers of both sets.
     *  - `insert_range`, and `insert` of a range or a list: may invalidate the iterators, references and pointers
     *    also if no element is inserted, because they reserve room for the whole range first.
     *  - `insert`, `emplace`, `emplace_hint`: invalidate the iterators, references and pointers if an element is
     *    inserted.
     *  - `erase`: always invalidates the iterators, references and pointers. Use the returned iterator.
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
        using ht = detail_sparse_hash::sparse_hash<detail_sparse_hash::set_access<Key>, Hash, KeyEqual, Allocator, Sparsity, AllocationFailure>;

        template<typename, typename, typename, typename, sh::sparsity, sh::allocation_failure>
        friend struct sparse_set;

        /// the heterogeneous overloads exist if `Hash::is_transparent` and `KeyEqual::is_transparent` exist
        static constexpr bool is_transparent = detail_sparse_hash::IsTransparent<Hash> && detail_sparse_hash::IsTransparent<KeyEqual>;

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

        template<typename InputIt>
        constexpr sparse_set(InputIt first,
                             InputIt last,
                             size_type bucket_count = ht::default_init_bucket_count,
                             Hash const &hash = Hash(),
                             KeyEqual const &equal = KeyEqual(),
                             Allocator const &alloc = Allocator())
            : sparse_set(bucket_count, hash, equal, alloc) {
            insert(first, last);
        }

        template<typename InputIt>
        constexpr sparse_set(InputIt first, InputIt last, size_type bucket_count, Allocator const &alloc)
            : sparse_set(first, last, bucket_count, Hash(), KeyEqual(), alloc) {
        }

        template<typename InputIt>
        constexpr sparse_set(InputIt first, InputIt last, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(first, last, bucket_count, hash, KeyEqual(), alloc) {
        }

        template<typename InputIt>
        constexpr sparse_set(InputIt first, InputIt last, Allocator const &alloc)
            : sparse_set(first, last, ht::default_init_bucket_count, Hash(), KeyEqual(), alloc) {
        }

        /**
         * Constructs the set from the elements of `range` (`std::from_range` constructor of C++23).
         */
        template<detail_sparse_hash::ContainerCompatibleRange<value_type> R>
        constexpr sparse_set(std::from_range_t /*tag*/,
                             R &&range,
                             size_type bucket_count = ht::default_init_bucket_count,
                             Hash const &hash = Hash(),
                             KeyEqual const &equal = KeyEqual(),
                             Allocator const &alloc = Allocator())
            : sparse_set(bucket_count, hash, equal, alloc) {
            insert_range(std::forward<R>(range));
        }

        template<detail_sparse_hash::ContainerCompatibleRange<value_type> R>
        constexpr sparse_set(std::from_range_t /*tag*/, R &&range, size_type bucket_count, Allocator const &alloc)
            : sparse_set(std::from_range, std::forward<R>(range), bucket_count, Hash(), KeyEqual(), alloc) {
        }

        template<detail_sparse_hash::ContainerCompatibleRange<value_type> R>
        constexpr sparse_set(std::from_range_t /*tag*/, R &&range, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(std::from_range, std::forward<R>(range), bucket_count, hash, KeyEqual(), alloc) {
        }

        template<detail_sparse_hash::ContainerCompatibleRange<value_type> R>
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
         * the elements are moved one by one, or copied if their move constructor can throw.
         */
        constexpr sparse_set(sparse_set &&other, std::type_identity_t<Allocator> const &alloc)
            : ht_(std::move(other.ht_), alloc) {
        }

        constexpr ~sparse_set() = default;

        constexpr sparse_set &operator=(sparse_set const &other) = default;
        constexpr sparse_set &operator=(sparse_set &&other) = default;

        constexpr sparse_set &operator=(std::initializer_list<value_type> ilist) {
            ht_.clear();
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
         * Heterogeneous `insert` (C++26). Only if `Hash::is_transparent` and `KeyEqual::is_transparent` exist.
         * The element is constructed from `key` only if it is inserted.
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
            return ht_.insert_key_hint(hint, std::forward<K>(key));
        }

        template<typename InputIt>
        constexpr void insert(InputIt first, InputIt last) {
            ht_.insert(first, last);
        }

        constexpr void insert(std::initializer_list<value_type> ilist) {
            ht_.insert(ilist.begin(), ilist.end());
        }

        /**
         * Inserts every element of `range` that is not in the set yet (C++23).
         */
        template<detail_sparse_hash::ContainerCompatibleRange<value_type> R>
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

        constexpr iterator erase(iterator pos) noexcept {
            return ht_.erase(pos);
        }

        constexpr iterator erase(const_iterator pos) noexcept {
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
         * Heterogeneous `erase` (C++23). Only if `Hash::is_transparent` and `KeyEqual::is_transparent` exist. `K`
         * must be hashable and comparable to `Key`.
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
         * Moves the elements of `source` that are not in this set into this set, and erases them from `source`
         * (C++17). An element is copied if its move constructor can throw. On an exception, the element being merged
         * and the ones after it stay in `source`; the ones merged before are in this set, unless a rehash emptied it
         * (see the class documentation).
         */
        template<typename Hash2, typename KeyEqual2, sh::sparsity Sparsity2, sh::allocation_failure AllocationFailure2>
        constexpr void merge(sparse_set<Key, Hash2, KeyEqual2, Allocator, Sparsity2, AllocationFailure2> &source) {
            ht_.merge(source.ht_);
        }

        template<typename Hash2, typename KeyEqual2, sh::sparsity Sparsity2, sh::allocation_failure AllocationFailure2>
        constexpr void merge(sparse_set<Key, Hash2, KeyEqual2, Allocator, Sparsity2, AllocationFailure2> &&source) {
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
    template<typename Key, typename Hash, typename KeyEqual, typename Allocator, sh::sparsity Sparsity, sh::allocation_failure AllocationFailure, typename Predicate>
    constexpr typename sparse_set<Key, Hash, KeyEqual, Allocator, Sparsity, AllocationFailure>::size_type
    erase_if(sparse_set<Key, Hash, KeyEqual, Allocator, Sparsity, AllocationFailure> &set, Predicate pred) {
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
     * Deduction guides, as for `std::unordered_set`. A guide without a hash function takes the default hash function
     * of `sparse_set`.
     */
    template<detail_sparse_hash::LegacyInputIterator InputIt,
             typename Hash = detail_sparse_hash::default_hash<detail_sparse_hash::iter_value_type_t<InputIt>>,
             typename KeyEqual = std::equal_to<detail_sparse_hash::iter_value_type_t<InputIt>>,
             typename Allocator = std::allocator<detail_sparse_hash::iter_value_type_t<InputIt>>>
    requires (detail_sparse_hash::HashLike<Hash> && !detail_sparse_hash::AllocatorLike<KeyEqual> && detail_sparse_hash::AllocatorLike<Allocator>)
    sparse_set(InputIt, InputIt, std::size_t = 0, Hash = Hash(), KeyEqual = KeyEqual(), Allocator = Allocator())
        -> sparse_set<detail_sparse_hash::iter_value_type_t<InputIt>, Hash, KeyEqual, Allocator>;

    template<std::ranges::input_range R,
             typename Hash = detail_sparse_hash::default_hash<std::ranges::range_value_t<R>>,
             typename KeyEqual = std::equal_to<std::ranges::range_value_t<R>>,
             typename Allocator = std::allocator<std::ranges::range_value_t<R>>>
    requires (detail_sparse_hash::HashLike<Hash> && !detail_sparse_hash::AllocatorLike<KeyEqual> && detail_sparse_hash::AllocatorLike<Allocator>)
    sparse_set(std::from_range_t, R &&, std::size_t = 0, Hash = Hash(), KeyEqual = KeyEqual(), Allocator = Allocator())
        -> sparse_set<std::ranges::range_value_t<R>, Hash, KeyEqual, Allocator>;

    template<typename Key,
             typename Hash = detail_sparse_hash::default_hash<Key>,
             typename KeyEqual = std::equal_to<Key>,
             typename Allocator = std::allocator<Key>>
    requires (detail_sparse_hash::HashLike<Hash> && !detail_sparse_hash::AllocatorLike<KeyEqual> && detail_sparse_hash::AllocatorLike<Allocator>)
    sparse_set(std::initializer_list<Key>, std::size_t = 0, Hash = Hash(), KeyEqual = KeyEqual(), Allocator = Allocator())
        -> sparse_set<Key, Hash, KeyEqual, Allocator>;

    template<detail_sparse_hash::LegacyInputIterator InputIt, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(InputIt, InputIt, std::size_t, Allocator)
        -> sparse_set<detail_sparse_hash::iter_value_type_t<InputIt>, detail_sparse_hash::default_hash<detail_sparse_hash::iter_value_type_t<InputIt>>, std::equal_to<detail_sparse_hash::iter_value_type_t<InputIt>>, Allocator>;

    template<detail_sparse_hash::LegacyInputIterator InputIt, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(InputIt, InputIt, Allocator)
        -> sparse_set<detail_sparse_hash::iter_value_type_t<InputIt>, detail_sparse_hash::default_hash<detail_sparse_hash::iter_value_type_t<InputIt>>, std::equal_to<detail_sparse_hash::iter_value_type_t<InputIt>>, Allocator>;

    template<detail_sparse_hash::LegacyInputIterator InputIt, detail_sparse_hash::HashLike Hash, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(InputIt, InputIt, std::size_t, Hash, Allocator)
        -> sparse_set<detail_sparse_hash::iter_value_type_t<InputIt>, Hash, std::equal_to<detail_sparse_hash::iter_value_type_t<InputIt>>, Allocator>;

    template<std::ranges::input_range R, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(std::from_range_t, R &&, std::size_t, Allocator)
        -> sparse_set<std::ranges::range_value_t<R>, detail_sparse_hash::default_hash<std::ranges::range_value_t<R>>, std::equal_to<std::ranges::range_value_t<R>>, Allocator>;

    template<std::ranges::input_range R, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(std::from_range_t, R &&, Allocator)
        -> sparse_set<std::ranges::range_value_t<R>, detail_sparse_hash::default_hash<std::ranges::range_value_t<R>>, std::equal_to<std::ranges::range_value_t<R>>, Allocator>;

    template<std::ranges::input_range R, detail_sparse_hash::HashLike Hash, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(std::from_range_t, R &&, std::size_t, Hash, Allocator)
        -> sparse_set<std::ranges::range_value_t<R>, Hash, std::equal_to<std::ranges::range_value_t<R>>, Allocator>;

    template<typename Key, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(std::initializer_list<Key>, std::size_t, Allocator)
        -> sparse_set<Key, detail_sparse_hash::default_hash<Key>, std::equal_to<Key>, Allocator>;

    template<typename Key, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(std::initializer_list<Key>, Allocator)
        -> sparse_set<Key, detail_sparse_hash::default_hash<Key>, std::equal_to<Key>, Allocator>;

    template<typename Key, detail_sparse_hash::HashLike Hash, detail_sparse_hash::AllocatorLike Allocator>
    sparse_set(std::initializer_list<Key>, std::size_t, Hash, Allocator)
        -> sparse_set<Key, Hash, std::equal_to<Key>, Allocator>;

}  // namespace dice::sparse_map

#endif
