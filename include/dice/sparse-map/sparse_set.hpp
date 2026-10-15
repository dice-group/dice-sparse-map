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
#include <type_traits>
#include <utility>

#include "dice/sparse-map/boost_offset_pointer.hpp"
#include "dice/sparse-map/sparse_hash.hpp"

namespace dice::sparse_map {

    /**
     * Implementation of a sparse hash set using open-addressing with quadratic
     * probing. The goal on the hash set is to be the most memory efficient
     * possible, even at low load factor, while keeping reasonable performances.
     *
     * `GrowthPolicy` defines how the set grows and consequently how a hash value is
     * mapped to a bucket. By default the set uses
     * `dice::sh::power_of_two_growth_policy`. This policy keeps the number of
     * buckets to a power of two and uses a mask to map the hash to a bucket instead
     * of the slow modulo. Other growth policies are available and you may define
     * your own growth policy, check `dice::sh::power_of_two_growth_policy` for the
     * interface.
     *
     * `ExceptionSafety` defines the exception guarantee provided by the class. By
     * default only the basic exception safety is guaranteed which mean that all
     * resources used by the hash set will be freed (no memory leaks) but the hash
     * set may end-up in an undefined state if an exception is thrown (undefined
     * here means that some elements may be missing). This can ONLY happen on rehash
     * (either on insert or if `rehash` is called explicitly) and will occur if the
     * Allocator can't allocate memory (`std::bad_alloc`) or if the copy constructor
     * (when a nothrow move constructor is not available) throws an exception. This
     * can be avoided by calling `reserve` beforehand. This basic guarantee is
     * similar to the one of `google::sparse_hash_map` and `spp::sparse_hash_map`.
     * It is possible to ask for the strong exception guarantee with
     * `dice::sh::exception_safety::strong`, the drawback is that the set will be
     * slower on rehashes and will also need more memory on rehashes.
     *
     * `Sparsity` defines how much the hash set will compromise between insertion
     * speed and memory usage. A high sparsity means less memory usage but longer
     * insertion times, and vice-versa for low sparsity. The default
     * `dice::sh::sparsity::medium` sparsity offers a good compromise. It doesn't
     * change the lookup speed.
     *
     * `Key` must be nothrow move constructible and/or copy constructible.
     *
     * If the destructor of `Key` throws an exception, the behaviour of the class is
     * undefined.
     *
     * Iterators invalidation:
     *  - clear, operator=, reserve, rehash: always invalidate the iterators.
     *  - insert, emplace, emplace_hint: if there is an effective insert, invalidate
     * the iterators.
     *  - erase: always invalidate the iterators.
     */
    template<class Key, class Hash = std::hash<Key>, class KeyEqual = std::equal_to<Key>, class Allocator = std::allocator<Key>, class GrowthPolicy = dice::sparse_map::sh::power_of_two_growth_policy<2>, dice::sparse_map::sh::exception_safety ExceptionSafety = dice::sparse_map::sh::exception_safety::basic, dice::sparse_map::sh::sparsity Sparsity = dice::sparse_map::sh::sparsity::medium>
    class sparse_set {
    private:
        class KeySelect {
        public:
            using key_type = Key;

            key_type const &operator()(Key const &key) const noexcept {
                return key;
            }

            key_type &operator()(Key &key) noexcept {
                return key;
            }
        };

        using ht = detail_sparse_hash::sparse_hash<Key, KeySelect, void, Hash, KeyEqual, Allocator, GrowthPolicy, ExceptionSafety, Sparsity, dice::sparse_map::sh::probing::quadratic>;

    public:
        using key_type = typename ht::key_type;
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

        /*
         * Constructors
         */
        sparse_set()
            : sparse_set(ht::DEFAULT_INIT_BUCKET_COUNT) {
        }

        explicit sparse_set(size_type bucket_count, Hash const &hash = Hash(), KeyEqual const &equal = KeyEqual(), Allocator const &alloc = Allocator())
            : ht_(bucket_count, hash, equal, alloc, ht::DEFAULT_MAX_LOAD_FACTOR) {
        }

        sparse_set(size_type bucket_count, Allocator const &alloc)
            : sparse_set(bucket_count, Hash(), KeyEqual(), alloc) {
        }

        sparse_set(size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(bucket_count, hash, KeyEqual(), alloc) {
        }

        explicit sparse_set(Allocator const &alloc)
            : sparse_set(ht::DEFAULT_INIT_BUCKET_COUNT, alloc) {
        }

        template<class InputIt>
        sparse_set(InputIt first, InputIt last, size_type bucket_count = ht::DEFAULT_INIT_BUCKET_COUNT, Hash const &hash = Hash(), KeyEqual const &equal = KeyEqual(), Allocator const &alloc = Allocator())
            : sparse_set(bucket_count, hash, equal, alloc) {
            insert(first, last);
        }

        template<class InputIt>
        sparse_set(InputIt first, InputIt last, size_type bucket_count, Allocator const &alloc)
            : sparse_set(first, last, bucket_count, Hash(), KeyEqual(), alloc) {
        }

        template<class InputIt>
        sparse_set(InputIt first, InputIt last, size_type bucket_count, Hash const &hash, Allocator const &alloc)
            : sparse_set(first, last, bucket_count, hash, KeyEqual(), alloc) {
        }

        sparse_set(std::initializer_list<value_type> init,
                   size_type bucket_count = ht::DEFAULT_INIT_BUCKET_COUNT,
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

        sparse_set &operator=(std::initializer_list<value_type> ilist) {
            ht_.clear();

            ht_.reserve(ilist.size());
            ht_.insert(ilist.begin(), ilist.end());

            return *this;
        }

        allocator_type get_allocator() const {
            return ht_.get_allocator();
        }

        /*
         * Iterators
         */
        iterator begin() noexcept {
            return ht_.begin();
        }
        const_iterator begin() const noexcept {
            return ht_.begin();
        }
        const_iterator cbegin() const noexcept {
            return ht_.cbegin();
        }

        iterator end() noexcept {
            return ht_.end();
        }
        const_iterator end() const noexcept {
            return ht_.end();
        }
        const_iterator cend() const noexcept {
            return ht_.cend();
        }

        /*
         * Capacity
         */
        bool empty() const noexcept {
            return ht_.empty();
        }
        size_type size() const noexcept {
            return ht_.size();
        }
        size_type max_size() const noexcept {
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

        template<class InputIt>
        void insert(InputIt first, InputIt last) {
            ht_.insert(first, last);
        }

        void insert(std::initializer_list<value_type> ilist) {
            ht_.insert(ilist.begin(), ilist.end());
        }

        /**
         * Due to the way elements are stored, emplace will need to move or copy the
         * key-value once. The method is equivalent to
         * `insert(value_type(std::forward<Args>(args)...));`.
         *
         * Mainly here for compatibility with the `std::unordered_map` interface.
         */
        template<class... Args>
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
        template<class... Args>
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
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
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
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        size_type erase(K const &key, std::size_t precalculated_hash) {
            return ht_.erase(key, precalculated_hash);
        }

        void swap(sparse_set &other) {
            other.ht_.swap(ht_);
        }

        /*
         * Lookup
         */
        size_type count(Key const &key) const {
            return ht_.count(key);
        }

        /**
         * Use the hash value `precalculated_hash` instead of hashing the key. The
         * hash value should be the same as `hash_function()(key)`, otherwise the
         * behaviour is undefined. Useful to speed-up the lookup if you already have
         * the hash.
         */
        size_type count(Key const &key, std::size_t precalculated_hash) const {
            return ht_.count(key, precalculated_hash);
        }

        /**
         * This overload only participates in the overload resolution if the typedef
         * `KeyEqual::is_transparent` exists. If so, `K` must be hashable and
         * comparable to `Key`.
         */
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        size_type count(K const &key) const {
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
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        size_type count(K const &key, std::size_t precalculated_hash) const {
            return ht_.count(key, precalculated_hash);
        }

        iterator find(Key const &key) {
            return ht_.find(key);
        }

        /**
         * Use the hash value `precalculated_hash` instead of hashing the key. The
         * hash value should be the same as `hash_function()(key)`, otherwise the
         * behaviour is undefined. Useful to speed-up the lookup if you already have
         * the hash.
         */
        iterator find(Key const &key, std::size_t precalculated_hash) {
            return ht_.find(key, precalculated_hash);
        }

        const_iterator find(Key const &key) const {
            return ht_.find(key);
        }

        /**
         * @copydoc find(const Key& key, std::size_t precalculated_hash)
         */
        const_iterator find(Key const &key, std::size_t precalculated_hash) const {
            return ht_.find(key, precalculated_hash);
        }

        /**
         * This overload only participates in the overload resolution if the typedef
         * `KeyEqual::is_transparent` exists. If so, `K` must be hashable and
         * comparable to `Key`.
         */
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        iterator find(K const &key) {
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
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        iterator find(K const &key, std::size_t precalculated_hash) {
            return ht_.find(key, precalculated_hash);
        }

        /**
         * @copydoc find(const K& key)
         */
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        const_iterator find(K const &key) const {
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
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        const_iterator find(K const &key, std::size_t precalculated_hash) const {
            return ht_.find(key, precalculated_hash);
        }

        bool contains(Key const &key) const {
            return ht_.contains(key);
        }

        /**
         * Use the hash value 'precalculated_hash' instead of hashing the key. The
         * hash value should be the same as hash_function()(key). Useful to speed-up
         * the lookup if you already have the hash.
         */
        bool contains(Key const &key, std::size_t precalculated_hash) const {
            return ht_.contains(key, precalculated_hash);
        }

        /**
         * This overload only participates in the overload resolution if the typedef
         * KeyEqual::is_transparent exists. If so, K must be hashable and comparable
         * to Key.
         */
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        bool contains(K const &key) const {
            return ht_.contains(key);
        }

        /**
         * @copydoc contains(const K& key) const
         *
         * Use the hash value 'precalculated_hash' instead of hashing the key. The
         * hash value should be the same as hash_function()(key). Useful to speed-up
         * the lookup if you already have the hash.
         */
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        bool contains(K const &key, std::size_t precalculated_hash) const {
            return ht_.contains(key, precalculated_hash);
        }

        std::pair<iterator, iterator> equal_range(Key const &key) {
            return ht_.equal_range(key);
        }

        /**
         * Use the hash value `precalculated_hash` instead of hashing the key. The
         * hash value should be the same as `hash_function()(key)`, otherwise the
         * behaviour is undefined. Useful to speed-up the lookup if you already have
         * the hash.
         */
        std::pair<iterator, iterator> equal_range(Key const &key,
                                                  std::size_t precalculated_hash) {
            return ht_.equal_range(key, precalculated_hash);
        }

        std::pair<const_iterator, const_iterator> equal_range(Key const &key) const {
            return ht_.equal_range(key);
        }

        /**
         * @copydoc equal_range(const Key& key, std::size_t precalculated_hash)
         */
        std::pair<const_iterator, const_iterator> equal_range(
            Key const &key,
            std::size_t precalculated_hash) const {
            return ht_.equal_range(key, precalculated_hash);
        }

        /**
         * This overload only participates in the overload resolution if the typedef
         * `KeyEqual::is_transparent` exists. If so, `K` must be hashable and
         * comparable to `Key`.
         */
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        std::pair<iterator, iterator> equal_range(K const &key) {
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
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        std::pair<iterator, iterator> equal_range(K const &key,
                                                  std::size_t precalculated_hash) {
            return ht_.equal_range(key, precalculated_hash);
        }

        /**
         * @copydoc equal_range(const K& key)
         */
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        std::pair<const_iterator, const_iterator> equal_range(K const &key) const {
            return ht_.equal_range(key);
        }

        /**
         * @copydoc equal_range(const K& key, std::size_t precalculated_hash)
         */
        template<class K>
        requires (detail_sparse_hash::has_is_transparent<KeyEqual>)
        std::pair<const_iterator, const_iterator> equal_range(
            K const &key,
            std::size_t precalculated_hash) const {
            return ht_.equal_range(key, precalculated_hash);
        }

        /*
         * Bucket interface
         */
        size_type bucket_count() const {
            return ht_.bucket_count();
        }
        size_type max_bucket_count() const {
            return ht_.max_bucket_count();
        }

        /*
         *  Hash policy
         */
        float load_factor() const {
            return ht_.load_factor();
        }
        float max_load_factor() const {
            return ht_.max_load_factor();
        }
        void max_load_factor(float ml) {
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
        hasher hash_function() const {
            return ht_.hash_function();
        }
        key_equal key_eq() const {
            return ht_.key_eq();
        }

        /*
         * Other
         */

        /**
         * Convert a `const_iterator` to an `iterator`.
         */
        iterator mutable_iterator(const_iterator pos) {
            return ht_.mutable_iterator(pos);
        }

        /**
         * Serialize the set through the `serializer` parameter.
         *
         * The `serializer` parameter must be a function object that supports the
         * following call:
         *  - `void operator()(const U& value);` where the types `std::uint64_t`,
         * `float` and `Key` must be supported for U.
         *
         * The implementation leaves binary compatibility (endianness, IEEE 754 for
         * floats, ...) of the types it serializes in the hands of the `Serializer`
         * function object if compatibility is required.
         */
        template<class Serializer>
        void serialize(Serializer &serializer) const {
            ht_.serialize(serializer);
        }

        /**
         * Deserialize a previously serialized set through the `deserializer`
         * parameter.
         *
         * The `deserializer` parameter must be a function object that supports the
         * following calls:
         *  - `template<typename U> U operator()();` where the types `std::uint64_t`,
         * `float` and `Key` must be supported for U.
         *
         * If the deserialized hash set type is hash compatible with the serialized
         * set, the deserialization process can be sped up by setting
         * `hash_compatible` to true. To be hash compatible, the Hash, KeyEqual and
         * GrowthPolicy must behave the same way than the ones used on the serialized
         * set. The `std::size_t` must also be of the same size as the one on the
         * platform used to serialize the set. If these criteria are not met, the
         * behaviour is undefined with `hash_compatible` sets to true.
         *
         * The behaviour is undefined if the type `Key` of the `sparse_set` is not the
         * same as the type used during serialization.
         *
         * The implementation leaves binary compatibility (endianness, IEEE 754 for
         * floats, size of int, ...) of the types it deserializes in the hands of the
         * `Deserializer` function object if compatibility is required.
         */
        template<class Deserializer>
        static sparse_set deserialize(Deserializer &deserializer,
                                      bool hash_compatible = false) {
            sparse_set set(0);
            set.ht_.deserialize(deserializer, hash_compatible);

            return set;
        }

        friend bool operator==(sparse_set const &lhs, sparse_set const &rhs) {
            if (lhs.size() != rhs.size()) {
                return false;
            }

            for (auto const &element_lhs : lhs) {
                auto const it_element_rhs = rhs.find(element_lhs);
                if (it_element_rhs == rhs.cend()) {
                    return false;
                }
            }

            return true;
        }

        friend bool operator!=(sparse_set const &lhs, sparse_set const &rhs) {
            return !operator==(lhs, rhs);
        }

        friend void swap(sparse_set &lhs, sparse_set &rhs) {
            lhs.swap(rhs);
        }

    private:
        ht ht_;
    };

    /**
     * Same as `dice::sparse_set<Key, Hash, KeyEqual, Allocator,
     * dice::sh::prime_growth_policy>`.
     */
    template<class Key, class Hash = std::hash<Key>, class KeyEqual = std::equal_to<Key>, class Allocator = std::allocator<Key>>
    using sparse_pg_set = sparse_set<Key, Hash, KeyEqual, Allocator, dice::sparse_map::sh::prime_growth_policy>;

}  // namespace dice::sparse_map

#endif
