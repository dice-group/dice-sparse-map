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

#include <concepts>
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
     * buckets are occupied. The bucket count is 0 or a power of two of at least 64 (one group) and doubles when
     * the table grows. The hash picks the bucket with a mask. A hash function that is not marked as avalanching
     * (see `hash_is_avalanching`) is mixed first.
     *
     * The interface follows `std::unordered_set`, with these differences:
     *  - The iterators are forward iterators.
     *  - There is no bucket interface beyond `bucket_count`, and no node handles.
     *  - Lookups can take a precalculated hash, see the overloads with a `precalculated_hash` parameter.
     *  - `for_each(f)` calls `f` with every key.
     *
     * The heterogeneous overloads take a key of another type than `Key`, for example a `std::string_view` for a
     * `std::string` key. As in the standard library, they exist only if `Hash::is_transparent` and
     * `KeyEqual::is_transparent` both exist, and the hash function and the key equality must accept the other type.
     * Otherwise the argument is converted to `Key` first.
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
     * `erase` does not throw unless the hash function or the key equality throws, as for `std::unordered_set`.
     *
     * The set never moves an element whose move constructor can throw. It copies it. An erase of such an element
     * destroys it in place and leaves a hole: the memory of the element stays with its group of 64 buckets. A new
     * element in the bucket of a hole takes that memory without an allocation. The group gives the memory of its
     * holes back when it gets a new element in another bucket (it then copies its elements into new memory of the
     * exact size), when it is rehashed, and when its last element is erased. A copy of the set has no holes. The
     * clean-up rehash, which also removes the marks of erased elements, bounds the number of holes.
     *
     * `begin()` is constant time: the set keeps the index of its first group of 64 buckets that holds an element.
     * An erase that empties this group looks for the next group with an element, one group after the other. So the
     * erase costs one step for every empty group in between, and erasing all elements in the order of iteration, as
     * in `while (!set.empty()) { set.erase(set.begin()); }`, costs one step per group in total.
     *
     * `sparsity` trades insertion speed for memory. A group grows its storage by 2 (`unordered_sparse::sparsity::high`),
     * 4 (`unordered_sparse::sparsity::medium`, default) or 8 (`unordered_sparse::sparsity::low`) elements at a time. High sparsity means
     * less memory and slower insertions. The lookup speed does not depend on it.
     *
     * `Key` must be nothrow move constructible and/or copy constructible. The behaviour is undefined if the
     * destructor of `Key` throws. If `Key` is nothrow move constructible, the set moves its elements with
     * `std::allocator_traits<Allocator>::construct` and expects that this does not throw either. With a scoped or a
     * polymorphic allocator, that is the allocator-extended move constructor of `Key`, which does not throw when the
     * allocators are equal. `std::vector` makes the same assumption. If `Key` is also nothrow move assignable, an
     * insertion or an erase moves the elements after it in its group of 64 buckets with their move assignment, as
     * `std::vector::insert` and `std::vector::erase` do. Then an insertion constructs two elements through the
     * allocator (the new element in a temporary, and one at the end of the group) and destroys one (the temporary),
     * and an erase destroys one. An allocator whose `construct` and `destroy` only construct and destroy in place can
     * opt in with `allocator_constructs_in_place`: then trivially copyable elements are copied and moved as bytes,
     * without its `construct` and `destroy`.
     *
     * `inline_capacity` (default 0) is the number of elements that a set keeps in the set object itself, without
     * an allocation. The set object then holds group 0 of a table of 64 buckets: a bitmap of the buckets that hold an
     * element, a bitmap of the deleted buckets and room for `inline_capacity` elements. A lookup is the one of a table of
     * 64 buckets. When an insertion finds the inline group full, the elements move into an allocated group of 64
     * buckets, in the same buckets and in the same order, without a rehash. A lower maximum load factor lowers the
     * limit to the load threshold of 64 buckets. An erase of an inline element moves the elements after it, and marks
     * its bucket as deleted only while an element can be outside the bucket of its hash: it allocates nothing and keeps
     * the order of the other elements. The inline capacity is at
     * most 32. Inline storage makes the set object larger: with `std::allocator` on 64 bit targets,
     * `sizeof(sparse_set<std::uint64_t>)` is 72 with the inline capacity 0 and also with 4 (the inline group takes the place of the state of the buckets), and 80 with 6. The inline capacity 0 has no inline group at all.
     *
     * An inline capacity other than 0 does not compile for these elements:
     *  - Keys whose move constructor can throw (a `static_assert`). The inline elements move one by one in a move,
     *    a swap and the move into an allocated group, and `noexcept` of the move constructor and of `swap` does not
     *    depend on the element type.
     *  - Keys with an alignment larger than the alignment of the allocator's pointer type and `size_type` (a
     *    `static_assert`). That is 8 bytes with `std::allocator` on 64 bit targets.
     *  - A `Key` that is incomplete where the set type is instantiated, for example a set that is a member of a
     *    type it stores (the compiler reports the incomplete type). The set object holds the elements.
     *
     * Move construction, move assignment and `swap` move the inline elements one by one. So, unlike with
     * `std::unordered_set`, they invalidate the iterators, references and pointers to inline elements, and a byte
     * copy of a set object is not a move. In a constant expression the inline group holds no element: the first
     * insertion allocates the group of 64 buckets. `bucket_count()` of a set with inline elements is 64, of an empty
     * inline set 0. A bucket count from 1 to 64, in the constructor or in `rehash(n)`, gives an empty set an
     * allocated group of 64 buckets instead of the inline group.
     *
     * Invalidation of iterators, references and pointers to elements: a group stores its elements densely, and an
     * insertion or an erasure can move them in the group or to new memory. So, unlike with `std::unordered_set`,
     * references and pointers to elements are invalidated like the iterators.
     *  - `clear`, `operator=`, `reserve`, `rehash`: may invalidate the iterators, references and pointers.
     *  - `merge`: always invalidates the iterators, references and pointers of both sets.
     *  - `insert`, `emplace`, `emplace_hint`: invalidate the iterators, references and pointers if an element is
     *    inserted.
     *  - `erase`: always invalidates the iterators, references and pointers. Use the returned iterator.
     *  - move constructor, move assignment, `swap`: invalidate the iterators, references and pointers to inline
     *    elements.
     */
    template<typename Key,
             typename Hash = std::hash<Key>,
             typename KeyEqual = std::equal_to<Key>,
             typename Allocator = std::allocator<Key>,
             unordered_sparse::sparsity sparsity = unordered_sparse::sparsity::medium,
             std::size_t inline_capacity = 0>
    struct sparse_set {
    private:
        using ht = detail::sparse_hash<detail::set_policy<Key>, Hash, KeyEqual, Allocator, sparsity, inline_capacity>;

        template<typename, typename, typename, typename, unordered_sparse::sparsity, std::size_t>
        friend struct sparse_set;

        /// the heterogeneous overloads exist if `Hash::is_transparent` and `KeyEqual::is_transparent` exist
        static constexpr bool is_transparent = detail::IsTransparent<Hash> && detail::IsTransparent<KeyEqual>;

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

        /**
         * Calls `f` with every key, as `Key const &`, in the order of the iterators. A constraint checks that `f` can
         * be called with the key. The loop runs over the groups of 64 buckets and over the keys of each group, which
         * lie in one array. A set that keeps its keys in the inline group (see `inline_capacity`) has one such array
         * in the set object.
         *
         * `f` may read the set. It must not insert or erase keys or change the set in another way (`clear`, `rehash`,
         * `reserve`, assignment, `swap`, `merge`), otherwise the behaviour is undefined. If `f` throws, the exception
         * leaves `for_each`, and the set is not changed.
         */
        template<typename F>
        requires std::invocable<F &, const_reference>
        constexpr void for_each(F &&f) const {
            ht_.for_each(f);
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
         * Moves the elements of `source` that are not in this set into this set (C++17). The elements are moved, or
         * copied if their move constructor can throw, and erased from `source`. Elements that are in this set stay in
         * `source`. If it throws, the element that was being merged and the ones not reached yet stay in `source`.
         * The elements merged before it are in this set, unless a rehash of this set empties it, see the class
         * documentation.
         */
        template<typename H2, typename P2, unordered_sparse::sparsity S2, std::size_t C2>
        constexpr void merge(sparse_set<Key, H2, P2, Allocator, S2, C2> &source) {
            ht_.merge(source.ht_);
        }

        template<typename H2, typename P2, unordered_sparse::sparsity S2, std::size_t C2>
        constexpr void merge(sparse_set<Key, H2, P2, Allocator, S2, C2> &&source) {
            ht_.merge(source.ht_);
        }

        /*
         * Lookup
         */
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr size_type count(key_type const &key) const {
            return ht_.count(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr size_type count(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.count(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr size_type count(K const &key) const {
            return ht_.count(key);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr size_type count(K const &key, std::size_t precalculated_hash) const {
            return ht_.count(key, precalculated_hash);
        }

        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr iterator find(key_type const &key) {
            return ht_.find(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr iterator find(key_type const &key, std::size_t precalculated_hash) {
            return ht_.find(key, precalculated_hash);
        }

        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr const_iterator find(key_type const &key) const {
            return ht_.find(key);
        }

        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr const_iterator find(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.find(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr iterator find(K const &key) {
            return ht_.find(key);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr iterator find(K const &key, std::size_t precalculated_hash) {
            return ht_.find(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr const_iterator find(K const &key) const {
            return ht_.find(key);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr const_iterator find(K const &key, std::size_t precalculated_hash) const {
            return ht_.find(key, precalculated_hash);
        }

        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr bool contains(key_type const &key) const {
            return ht_.contains(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr bool contains(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.contains(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr bool contains(K const &key) const {
            return ht_.contains(key);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr bool contains(K const &key, std::size_t precalculated_hash) const {
            return ht_.contains(key, precalculated_hash);
        }

        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(key_type const &key) {
            return ht_.equal_range(key);
        }

        /**
         * Uses `precalculated_hash` instead of hashing the key. It must be `hash_function()(key)`, otherwise the
         * behaviour is undefined. Saves the hashing if the caller has the hash already.
         */
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(key_type const &key, std::size_t precalculated_hash) {
            return ht_.equal_range(key, precalculated_hash);
        }

        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(key_type const &key) const {
            return ht_.equal_range(key);
        }

        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(key_type const &key, std::size_t precalculated_hash) const {
            return ht_.equal_range(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(K const &key) {
            return ht_.equal_range(key);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(K const &key, std::size_t precalculated_hash) {
            return ht_.equal_range(key, precalculated_hash);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(K const &key) const {
            return ht_.equal_range(key);
        }

        template<typename K>
        requires is_transparent
        DICE_UNORDERED_SPARSE_ALWAYS_INLINE [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(K const &key, std::size_t precalculated_hash) const {
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
    template<typename Key, typename Hash, typename KeyEqual, typename Allocator, unordered_sparse::sparsity sparsity, std::size_t inline_capacity, typename Predicate>
    constexpr typename sparse_set<Key, Hash, KeyEqual, Allocator, sparsity, inline_capacity>::size_type
    erase_if(sparse_set<Key, Hash, KeyEqual, Allocator, sparsity, inline_capacity> &set, Predicate pred) {
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
