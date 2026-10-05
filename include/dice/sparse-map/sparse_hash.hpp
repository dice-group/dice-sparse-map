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
#include <cstdlib>
#include <iterator>
#include <limits>
#include <memory>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include <dice/hash/DiceHash.hpp>

/**
 * Internal consistency checks. Enabled when `DICE_SPARSE_MAP_DEBUG` or `TSL_DEBUG` is defined.
 */
#if defined(DICE_SPARSE_MAP_DEBUG) || defined(TSL_DEBUG)
#define DICE_SPARSE_MAP_ASSERT(expr) assert(expr)
#else
#define DICE_SPARSE_MAP_ASSERT(expr) (static_cast<void>(0))
#endif

namespace dice::sparse_map {

    namespace sh {
        /**
         * The values of the `ExceptionSafety` template parameter of `sparse_map` and `sparse_set`. Both are
         * accepted and have no effect. The exception guarantee follows from the type of the elements, see the
         * class documentation of `sparse_map`.
         */
        enum class exception_safety {
            basic,
            strong
        };

        enum class sparsity {
            high,
            medium,
            low
        };

        /**
         * What `sparse_map` and `sparse_set` do when an allocation of the table fails, that is when the `allocate`
         * of the allocator throws.
         *
         * - `terminating`: the process ends with `std::abort()`.
         * - `throwing`: the exception of the allocator propagates (`std::bad_alloc` for `std::allocator`).
         *
         * In both cases a size limit of the table throws `std::length_error`. An allocation that an element makes
         * itself, for example in the copy constructor of a `std::string`, is an exception of the element and
         * propagates in both cases.
         */
        enum class allocation_failure {
            terminating,
            throwing
        };
    }  // namespace sh

    /* Replacement for const_cast in sparse_array.
     * Can be overloaded for specific fancy pointers
     * (see: include/dice/boost_offset_pointer.h).
     * This is just a workaround.
     * The clean way would be to change the implementation to stop using const_cast.
     */
    template<typename T>
    struct Remove_Const {
        template<typename V>
        static T remove(V iter) {
            return const_cast<T>(iter);
        }
    };

    namespace detail_sparse_hash {
        /**
         * @return true if `Hash` declares the member type `is_avalanching`, false if it does not or if that type has
         * a `value` that is false (like `std::false_type`)
         */
        template<typename Hash>
        constexpr bool declares_is_avalanching() noexcept {
            if constexpr (requires { typename Hash::is_avalanching; }) {
                if constexpr (requires { Hash::is_avalanching::value; }) {
                    return static_cast<bool>(Hash::is_avalanching::value);
                } else {
                    return true;
                }
            } else {
                return false;
            }
        }

        /**
         * The default hash function of `sparse_map` and `sparse_set`. It names the policy, so that the default of
         * `dice::hash::DiceHash` does not decide where the elements are placed.
         */
        template<typename Key>
        using default_hash = dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>;
    }  // namespace detail_sparse_hash

    namespace sh {
        /**
         * A hash function is avalanching if each bit of the input changes each bit of the result with a probability
         * of about one half. `sparse_map` and `sparse_set` pick the bucket with the low bits of the hash as it is, so
         * they accept only hash functions for which this trait is true.
         *
         * The trait is true if `Hash` has the public member type `is_avalanching`, its own or of a public base, and
         * false if `Hash` does not have it, if it is not accessible, or if that type has a `value` that is false (like
         * `std::false_type`). A `value` must be a constant expression. So `using is_avalanching = void;` as in
         * `ankerl::unordered_dense` and `using is_avalanching = std::true_type;` as in `boost::unordered` both mark a
         * hash function. Specialize the trait to mark a hash function that you cannot change. `std::hash` is not
         * avalanching: for integers, libstdc++ and libc++ return the value itself. `dice::hash::DiceHash` declares
         * `is_avalanching` if its policy does.
         */
        template<typename Hash>
        struct hash_is_avalanching : std::bool_constant<detail_sparse_hash::declares_is_avalanching<Hash>()> {};

        template<typename Hash>
        inline constexpr bool hash_is_avalanching_v = hash_is_avalanching<Hash>::value;
    }  // namespace sh

    namespace detail_sparse_hash {
        template<typename T>
        concept has_is_transparent = requires {
            typename T::is_transparent;
        };

        inline constexpr bool is_power_of_two(std::size_t value) {
            return value != 0 && (value & (value - 1)) == 0;
        }

        inline std::size_t round_up_to_power_of_two(std::size_t value) {
            if (is_power_of_two(value)) {
                return value;
            }

            if (value == 0) {
                return 1;
            }

            --value;
            for (std::size_t i = 1; i < sizeof(std::size_t) * CHAR_BIT; i *= 2) {
                value |= value >> i;
            }

            return value + 1;
        }

        /**
         * Allocates `n` objects with `alloc`. With `sh::allocation_failure::terminating` an exception of the
         * allocator ends the process with `std::abort()`. With `sh::allocation_failure::throwing` it propagates.
         */
        template<dice::sparse_map::sh::allocation_failure AllocationFailure, typename Allocator>
        [[nodiscard]] typename std::allocator_traits<Allocator>::pointer allocate(
            Allocator &alloc,
            typename std::allocator_traits<Allocator>::size_type n) noexcept(AllocationFailure == dice::sparse_map::sh::allocation_failure::terminating) {
            if constexpr (AllocationFailure == dice::sparse_map::sh::allocation_failure::terminating) {
                try {
                    return detail_sparse_hash::allocate<dice::sparse_map::sh::allocation_failure::throwing>(alloc, n);
                } catch (...) {
                    std::abort();
                }
            } else {
                return std::allocator_traits<Allocator>::allocate(alloc, n);
            }
        }

        /**
         * Calls `f`, which allocates through a container of the standard library, like `std::vector::resize`. With
         * `sh::allocation_failure::terminating` any exception of `f` other than `std::length_error` ends the process
         * with `std::abort()`. With `sh::allocation_failure::throwing` every exception propagates. Use it only where
         * the elements of the container construct and move without exceptions, so that only the allocator and the
         * size limit of the container can throw.
         */
        template<dice::sparse_map::sh::allocation_failure AllocationFailure, typename F>
        void allocate_with(F &&f) {
            if constexpr (AllocationFailure == dice::sparse_map::sh::allocation_failure::terminating) {
                try {
                    std::forward<F>(f)();
                } catch (std::length_error const &) {
                    throw;
                } catch (...) {
                    std::abort();
                }
            } else {
                std::forward<F>(f)();
            }
        }

        /**
         * WARNING: the sparse_array class doesn't free the ressources allocated through
         * the allocator passed in parameter in each method. You have to manually call
         * `clear(Allocator&)` when you don't need a sparse_array object anymore.
         *
         * The reason is that the sparse_array doesn't store the allocator to avoid
         * wasting space in each sparse_array when the allocator has a size > 0. It only
         * allocates/deallocates objects with the allocator that is passed in parameter.
         *
         *
         *
         * Index denotes a value between [0, BITMAP_NB_BITS), it is an index similar to
         * std::vector. Offset denotes the real position in `values_` corresponding to
         * an index.
         *
         * We are using raw pointers instead of std::vector to avoid loosing
         * 2*sizeof(size_t) bytes to store the capacity and size of the vector in each
         * sparse_array. We know we can only store up to BITMAP_NB_BITS elements in the
         * array, we don't need such big types.
         *
         *
         * T must be nothrow move constructible and/or copy constructible.
         * Behaviour is undefined if the destructor of T throws an exception.
         *
         * See https://smerity.com/articles/2015/google_sparsehash.html for details on
         * the idea behinds the implementation.
         *
         * TODO Check to use std::realloc and std::memmove when possible
         */
        template<typename T, typename Allocator, dice::sparse_map::sh::sparsity Sparsity, dice::sparse_map::sh::allocation_failure AllocationFailure = dice::sparse_map::sh::allocation_failure::terminating>
        class sparse_array {
        public:
            using value_type = T;
            using size_type = std::uint_least8_t;
            using allocator_type = Allocator;
            using allocator_traits = std::allocator_traits<allocator_type>;
            using pointer = typename allocator_traits::pointer;
            using const_pointer = typename allocator_traits::const_pointer;
            using iterator = pointer;
            using const_iterator = const_pointer;

        private:
            static constexpr size_type CAPACITY_GROWTH_STEP = (Sparsity == dice::sparse_map::sh::sparsity::high) ? 2
                                                              : (Sparsity == dice::sparse_map::sh::sparsity::medium)
                                                                  ? 4
                                                                  : 8;  // (Sparsity == dice::sh::sparsity::low)

            using bitmap_type = std::uint_least64_t;
            static constexpr std::size_t BITMAP_NB_BITS = 64;
            static constexpr std::size_t BUCKET_SHIFT = 6;

            static constexpr std::size_t BUCKET_MASK = BITMAP_NB_BITS - 1;

            static_assert(is_power_of_two(BITMAP_NB_BITS),
                          "BITMAP_NB_BITS must be a power of two.");
            static_assert(std::numeric_limits<bitmap_type>::digits >= BITMAP_NB_BITS,
                          "bitmap_type must be able to hold at least BITMAP_NB_BITS.");
            static_assert((std::size_t(1) << BUCKET_SHIFT) == BITMAP_NB_BITS,
                          "(1 << BUCKET_SHIFT) must be equal to BITMAP_NB_BITS.");
            static_assert(std::numeric_limits<size_type>::max() >= BITMAP_NB_BITS,
                          "size_type must be big enough to hold BITMAP_NB_BITS.");
            static_assert(std::is_unsigned<bitmap_type>::value,
                          "bitmap_type must be unsigned.");
            static_assert((std::numeric_limits<bitmap_type>::max() & BUCKET_MASK) == BITMAP_NB_BITS - 1,
                          "");

        public:
            /**
             * Map an ibucket [0, bucket_count) in the hash table to a sparse_ibucket
             * (a sparse_array holds multiple buckets, so there is less sparse_array than
             * bucket_count).
             *
             * The bucket ibucket is in
             * sparse_buckets_[sparse_ibucket(ibucket)][index_in_sparse_bucket(ibucket)]
             * instead of something like buckets_[ibucket] in a classical hash table.
             */
            static std::size_t sparse_ibucket(std::size_t ibucket) {
                return ibucket >> BUCKET_SHIFT;
            }

            /**
             * Map an ibucket [0, bucket_count) in the hash table to an index in the
             * sparse_array which corresponds to the bucket.
             *
             * The bucket ibucket is in
             * sparse_buckets_[sparse_ibucket(ibucket)][index_in_sparse_bucket(ibucket)]
             * instead of something like buckets_[ibucket] in a classical hash table.
             */
            static typename sparse_array::size_type index_in_sparse_bucket(
                std::size_t ibucket) {
                return static_cast<typename sparse_array::size_type>(
                    ibucket & sparse_array::BUCKET_MASK);
            }

            static std::size_t nb_sparse_buckets(std::size_t bucket_count) noexcept {
                if (bucket_count == 0) {
                    return 0;
                }

                return std::max<std::size_t>(
                    1,
                    sparse_ibucket(dice::sparse_map::detail_sparse_hash::round_up_to_power_of_two(
                        bucket_count)));
            }

        public:
            sparse_array() noexcept
                : values_(nullptr),
                  bitmap_vals_(0),
                  bitmap_deleted_vals_(0),
                  nb_elements_(0),
                  capacity_(0),
                  last_array_(false) {
            }

            // needed for "is_constructible" with no parameters
            sparse_array(std::allocator_arg_t, Allocator const &) noexcept
                : sparse_array() {
            }

            explicit sparse_array(bool last_bucket) noexcept
                : values_(nullptr),
                  bitmap_vals_(0),
                  bitmap_deleted_vals_(0),
                  nb_elements_(0),
                  capacity_(0),
                  last_array_(last_bucket) {
            }

            // const Allocator needed for MoveInsertable requirement
            sparse_array(size_type capacity, Allocator const &const_alloc)
                : values_(nullptr),
                  bitmap_vals_(0),
                  bitmap_deleted_vals_(0),
                  nb_elements_(0),
                  capacity_(capacity),
                  last_array_(false) {
                if (capacity_ > 0) {
                    auto alloc = const_cast<Allocator &>(const_alloc);
                    values_ = detail_sparse_hash::allocate<AllocationFailure>(alloc, capacity_);
                    DICE_SPARSE_MAP_ASSERT(values_ != nullptr);  // allocate should throw if there is a failure
                }
            }

            // const Allocator needed for MoveInsertable requirement
            sparse_array(sparse_array const &other, Allocator const &const_alloc)
                : values_(nullptr),
                  bitmap_vals_(other.bitmap_vals_),
                  bitmap_deleted_vals_(other.bitmap_deleted_vals_),
                  nb_elements_(0),
                  capacity_(other.capacity_),
                  last_array_(other.last_array_) {
                DICE_SPARSE_MAP_ASSERT(other.capacity_ >= other.nb_elements_);
                if (capacity_ == 0) {
                    return;
                }

                auto alloc = const_cast<Allocator &>(const_alloc);
                values_ = detail_sparse_hash::allocate<AllocationFailure>(alloc, capacity_);
                DICE_SPARSE_MAP_ASSERT(values_ != nullptr);  // allocate should throw if there is a failure
                try {
                    for (size_type i = 0; i < other.nb_elements_; i++) {
                        construct_value(alloc, values_ + i, other.values_[i]);
                        nb_elements_++;
                    }
                } catch (...) {
                    clear(alloc);
                    throw;
                }
            }

            sparse_array(sparse_array &&other) noexcept
                : values_(other.values_),
                  bitmap_vals_(other.bitmap_vals_),
                  bitmap_deleted_vals_(other.bitmap_deleted_vals_),
                  nb_elements_(other.nb_elements_),
                  capacity_(other.capacity_),
                  last_array_(other.last_array_) {
                other.values_ = nullptr;
                other.bitmap_vals_ = 0;
                other.bitmap_deleted_vals_ = 0;
                other.nb_elements_ = 0;
                other.capacity_ = 0;
            }

            // const Allocator needed for MoveInsertable requirement
            sparse_array(sparse_array &&other, Allocator const &const_alloc)
                : values_(nullptr),
                  bitmap_vals_(other.bitmap_vals_),
                  bitmap_deleted_vals_(other.bitmap_deleted_vals_),
                  nb_elements_(0),
                  capacity_(other.capacity_),
                  last_array_(other.last_array_) {
                DICE_SPARSE_MAP_ASSERT(other.capacity_ >= other.nb_elements_);
                if (capacity_ == 0) {
                    return;
                }

                auto alloc = const_cast<Allocator &>(const_alloc);
                values_ = detail_sparse_hash::allocate<AllocationFailure>(alloc, capacity_);
                DICE_SPARSE_MAP_ASSERT(values_ != nullptr);  // allocate should throw if there is a failure
                try {
                    for (size_type i = 0; i < other.nb_elements_; i++) {
                        construct_value(alloc, values_ + i, std::move(other.values_[i]));
                        nb_elements_++;
                    }
                } catch (...) {
                    clear(alloc);
                    throw;
                }
            }

            sparse_array &operator=(sparse_array const &) = delete;
            sparse_array &operator=(sparse_array &&other) noexcept {
                this->values_ = other.values_;
                this->bitmap_vals_ = other.bitmap_vals_;
                this->bitmap_deleted_vals_ = other.bitmap_deleted_vals_;
                this->nb_elements_ = other.nb_elements_;
                this->capacity_ = other.capacity_;
                other.values_ = nullptr;
                other.bitmap_vals_ = 0;
                other.bitmap_deleted_vals_ = 0;
                other.nb_elements_ = 0;
                other.capacity_ = 0;
                return *this;
            }


            ~sparse_array() noexcept {
                // The code that manages the sparse_array must have called clear before
                // destruction. See documentation of sparse_array for more details.
                DICE_SPARSE_MAP_ASSERT(capacity_ == 0 && nb_elements_ == 0 && values_ == nullptr);
            }

            iterator begin() noexcept {
                return values_;
            }
            iterator end() noexcept {
                return values_ + nb_elements_;
            }
            const_iterator begin() const noexcept {
                return cbegin();
            }
            const_iterator end() const noexcept {
                return cend();
            }
            const_iterator cbegin() const noexcept {
                return values_;
            }
            const_iterator cend() const noexcept {
                return values_ + nb_elements_;
            }

            bool empty() const noexcept {
                return nb_elements_ == 0;
            }

            size_type size() const noexcept {
                return nb_elements_;
            }

            void clear(allocator_type &alloc) noexcept {
                destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);

                values_ = nullptr;
                bitmap_vals_ = 0;
                bitmap_deleted_vals_ = 0;
                nb_elements_ = 0;
                capacity_ = 0;
            }

            bool last() const noexcept {
                return last_array_;
            }

            void set_as_last() noexcept {
                last_array_ = true;
            }

            bool has_value(size_type index) const noexcept {
                DICE_SPARSE_MAP_ASSERT(index < BITMAP_NB_BITS);
                return (bitmap_vals_ & (bitmap_type(1) << index)) != 0;
            }

            bool has_deleted_value(size_type index) const noexcept {
                DICE_SPARSE_MAP_ASSERT(index < BITMAP_NB_BITS);
                return (bitmap_deleted_vals_ & (bitmap_type(1) << index)) != 0;
            }

            iterator value(size_type index) noexcept {
                DICE_SPARSE_MAP_ASSERT(has_value(index));
                return values_ + index_to_offset(index);
            }

            const_iterator value(size_type index) const noexcept {
                DICE_SPARSE_MAP_ASSERT(has_value(index));
                return values_ + index_to_offset(index);
            }

            /**
             * Return iterator to set value.
             */
            template<typename... Args>
            iterator set(allocator_type &alloc, size_type index, Args &&...value_args) {
                DICE_SPARSE_MAP_ASSERT(!has_value(index));

                size_type const offset = index_to_offset(index);
                insert_at_offset(alloc, offset, std::forward<Args>(value_args)...);

                bitmap_vals_ = (bitmap_vals_ | (bitmap_type(1) << index));
                bitmap_deleted_vals_ = (bitmap_deleted_vals_ & ~(bitmap_type(1) << index));

                nb_elements_++;

                DICE_SPARSE_MAP_ASSERT(has_value(index));
                DICE_SPARSE_MAP_ASSERT(!has_deleted_value(index));

                return values_ + offset;
            }

            iterator erase(allocator_type &alloc, iterator position) {
                size_type const offset = static_cast<size_type>(std::distance(begin(), position));
                return erase(alloc, position, offset_to_index(offset));
            }

            // Return the next value or end if no next value
            iterator erase(allocator_type &alloc, iterator position, size_type index) {
                DICE_SPARSE_MAP_ASSERT(has_value(index));
                DICE_SPARSE_MAP_ASSERT(!has_deleted_value(index));

                size_type const offset = static_cast<size_type>(std::distance(begin(), position));
                erase_at_offset(alloc, offset);

                bitmap_vals_ = (bitmap_vals_ & ~(bitmap_type(1) << index));
                bitmap_deleted_vals_ = (bitmap_deleted_vals_ | (bitmap_type(1) << index));

                nb_elements_--;

                DICE_SPARSE_MAP_ASSERT(!has_value(index));
                DICE_SPARSE_MAP_ASSERT(has_deleted_value(index));

                return values_ + offset;
            }

            void swap(sparse_array &other) {
                using std::swap;

                swap(values_, other.values_);
                swap(bitmap_vals_, other.bitmap_vals_);
                swap(bitmap_deleted_vals_, other.bitmap_deleted_vals_);
                swap(nb_elements_, other.nb_elements_);
                swap(capacity_, other.capacity_);
                swap(last_array_, other.last_array_);
            }

            static iterator mutable_iterator(const_iterator pos) {
                return ::dice::sparse_map::Remove_Const<iterator>::template remove<const_iterator>(pos);
            }

        private:
            template<typename... Args>
            static void construct_value(allocator_type &alloc, pointer value, Args &&...value_args) {
                std::allocator_traits<allocator_type>::construct(
                    alloc,
                    std::to_address(value),
                    std::forward<Args>(value_args)...);
            }

            static void destroy_value(allocator_type &alloc, pointer value) noexcept {
                std::allocator_traits<allocator_type>::destroy(alloc, std::to_address(value));
            }

            static void destroy_and_deallocate_values(
                allocator_type &alloc,
                pointer values,
                size_type nb_values,
                size_type capacity_values) noexcept {
                if (capacity_values == 0) {
                    DICE_SPARSE_MAP_ASSERT(nb_values == 0);
                    // nothing to deallocate
                    // alloc.deallocate(nullptr, 0) is invalid
                    return;
                }

                for (size_type i = 0; i < nb_values; i++) {
                    destroy_value(alloc, values + i);
                }
                alloc.deallocate(values, capacity_values);
            }

            static size_type popcount(bitmap_type val) noexcept {
                return static_cast<size_type>(std::popcount(val));
            }

            size_type index_to_offset(size_type index) const noexcept {
                DICE_SPARSE_MAP_ASSERT(index < BITMAP_NB_BITS);
                return popcount(bitmap_vals_ & ((bitmap_type(1) << index) - bitmap_type(1)));
            }

            // TODO optimize
            size_type offset_to_index(size_type offset) const noexcept {
                DICE_SPARSE_MAP_ASSERT(offset < nb_elements_);

                bitmap_type bitmap_vals = bitmap_vals_;
                size_type index = 0;
                size_type nb_ones = 0;

                while (bitmap_vals != 0) {
                    if ((bitmap_vals & 0x1) == 1) {
                        if (nb_ones == offset) {
                            break;
                        }

                        nb_ones++;
                    }

                    index++;
                    bitmap_vals = bitmap_vals >> 1;
                }

                return index;
            }

            size_type next_capacity() const noexcept {
                return static_cast<size_type>(capacity_ + CAPACITY_GROWTH_STEP);
            }

            /**
             * Insertion
             *
             * Two situations:
             * - Either we are in a situation where
             * std::is_nothrow_move_constructible<value_type>::value is true. In this
             * case, on insertion we just reallocate values_ when we reach its capacity
             * (i.e. nb_elements_ == capacity_), otherwise we just put the new value at
             * its appropriate place. We can easily keep the strong exception guarantee as
             * moving the values around is safe.
             * - Otherwise we are in a situation where
             * std::is_nothrow_move_constructible<value_type>::value is false. In this
             * case on EACH insertion we allocate a new area of nb_elements_ + 1 where we
             * copy the values of values_ into it and put the new value there. On
             * success, we set values_ to this new area. Even if slower, it's the only
             * way to preserve to strong exception guarantee.
             */
            template<typename... Args, typename U = value_type, typename std::enable_if<std::is_nothrow_move_constructible<U>::value>::type * = nullptr>
            void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
                if (nb_elements_ < capacity_) {
                    insert_at_offset_no_realloc(alloc, offset, std::forward<Args>(value_args)...);
                } else {
                    insert_at_offset_realloc(alloc, offset, next_capacity(), std::forward<Args>(value_args)...);
                }
            }

            template<typename... Args, typename U = value_type, typename std::enable_if<!std::is_nothrow_move_constructible<U>::value>::type * = nullptr>
            void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
                insert_at_offset_realloc(alloc, offset, nb_elements_ + 1, std::forward<Args>(value_args)...);
            }

            template<typename... Args, typename U = value_type, typename std::enable_if<std::is_nothrow_move_constructible<U>::value>::type * = nullptr>
            void insert_at_offset_no_realloc(allocator_type &alloc, size_type offset, Args &&...value_args) {
                DICE_SPARSE_MAP_ASSERT(offset <= nb_elements_);
                DICE_SPARSE_MAP_ASSERT(nb_elements_ < capacity_);

                for (size_type i = nb_elements_; i > offset; i--) {
                    construct_value(alloc, values_ + i, std::move(values_[i - 1]));
                    destroy_value(alloc, values_ + i - 1);
                }

                try {
                    construct_value(alloc, values_ + offset, std::forward<Args>(value_args)...);
                } catch (...) {
                    for (size_type i = offset; i < nb_elements_; i++) {
                        construct_value(alloc, values_ + i, std::move(values_[i + 1]));
                        destroy_value(alloc, values_ + i + 1);
                    }
                    throw;
                }
            }

            template<typename... Args, typename U = value_type, typename std::enable_if<std::is_nothrow_move_constructible<U>::value>::type * = nullptr>
            void insert_at_offset_realloc(allocator_type &alloc, size_type offset, size_type new_capacity, Args &&...value_args) {
                DICE_SPARSE_MAP_ASSERT(new_capacity > nb_elements_);

                pointer new_values = detail_sparse_hash::allocate<AllocationFailure>(alloc, new_capacity);
                // Allocate should throw if there is a failure
                DICE_SPARSE_MAP_ASSERT(new_values != nullptr);

                try {
                    construct_value(alloc, new_values + offset, std::forward<Args>(value_args)...);
                } catch (...) {
                    alloc.deallocate(new_values, new_capacity);
                    throw;
                }

                // Should not throw from here
                for (size_type i = 0; i < offset; i++) {
                    construct_value(alloc, new_values + i, std::move(values_[i]));
                }

                for (size_type i = offset; i < nb_elements_; i++) {
                    construct_value(alloc, new_values + i + 1, std::move(values_[i]));
                }

                destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);

                values_ = new_values;
                capacity_ = new_capacity;
            }

            template<typename... Args, typename U = value_type, typename std::enable_if<!std::is_nothrow_move_constructible<U>::value>::type * = nullptr>
            void insert_at_offset_realloc(allocator_type &alloc, size_type offset, size_type new_capacity, Args &&...value_args) {
                DICE_SPARSE_MAP_ASSERT(new_capacity > nb_elements_);

                value_type *new_values = detail_sparse_hash::allocate<AllocationFailure>(alloc, new_capacity);
                // Allocate should throw if there is a failure
                DICE_SPARSE_MAP_ASSERT(new_values != nullptr);

                size_type nb_new_values = 0;
                try {
                    for (size_type i = 0; i < offset; i++) {
                        construct_value(alloc, new_values + i, values_[i]);
                        nb_new_values++;
                    }

                    construct_value(alloc, new_values + offset, std::forward<Args>(value_args)...);
                    nb_new_values++;

                    for (size_type i = offset; i < nb_elements_; i++) {
                        construct_value(alloc, new_values + i + 1, values_[i]);
                        nb_new_values++;
                    }
                } catch (...) {
                    destroy_and_deallocate_values(alloc, new_values, nb_new_values, new_capacity);
                    throw;
                }

                DICE_SPARSE_MAP_ASSERT(nb_new_values == nb_elements_ + 1);

                destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);

                values_ = new_values;
                capacity_ = new_capacity;
            }

            /**
             * Erasure
             *
             * Two situations:
             * - Either we are in a situation where
             * std::is_nothrow_move_constructible<value_type>::value is true. Simply
             * destroy the value and left-shift move the value on the right of offset.
             * - Otherwise we are in a situation where
             * std::is_nothrow_move_constructible<value_type>::value is false. Copy all
             * the values except the one at offset into a new heap area. On success, we
             * set values_ to this new area. Even if slower, it's the only way to
             * preserve to strong exception guarantee.
             */
            template<typename... Args, typename U = value_type, typename std::enable_if<std::is_nothrow_move_constructible<U>::value>::type * = nullptr>
            void erase_at_offset(allocator_type &alloc, size_type offset) noexcept {
                DICE_SPARSE_MAP_ASSERT(offset < nb_elements_);

                destroy_value(alloc, values_ + offset);

                for (size_type i = offset + 1; i < nb_elements_; i++) {
                    construct_value(alloc, values_ + i - 1, std::move(values_[i]));
                    destroy_value(alloc, values_ + i);
                }
            }

            template<typename... Args, typename U = value_type, typename std::enable_if<!std::is_nothrow_move_constructible<U>::value>::type * = nullptr>
            void erase_at_offset(allocator_type &alloc, size_type offset) {
                DICE_SPARSE_MAP_ASSERT(offset < nb_elements_);

                // Erasing the last element, don't need to reallocate. We keep the capacity.
                if (offset + 1 == nb_elements_) {
                    destroy_value(alloc, values_ + offset);
                    return;
                }

                DICE_SPARSE_MAP_ASSERT(nb_elements_ > 1);
                size_type const new_capacity = nb_elements_ - 1;

                value_type *new_values = detail_sparse_hash::allocate<AllocationFailure>(alloc, new_capacity);
                // Allocate should throw if there is a failure
                DICE_SPARSE_MAP_ASSERT(new_values != nullptr);

                size_type nb_new_values = 0;
                try {
                    for (size_type i = 0; i < nb_elements_; i++) {
                        if (i != offset) {
                            construct_value(alloc, new_values + nb_new_values, values_[i]);
                            nb_new_values++;
                        }
                    }
                } catch (...) {
                    destroy_and_deallocate_values(alloc, new_values, nb_new_values, new_capacity);
                    throw;
                }

                DICE_SPARSE_MAP_ASSERT(nb_new_values == nb_elements_ - 1);

                destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);

                values_ = new_values;
                capacity_ = new_capacity;
            }

        private:
            pointer values_;

            bitmap_type bitmap_vals_;
            bitmap_type bitmap_deleted_vals_;

            size_type nb_elements_;
            size_type capacity_;
            bool last_array_;
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
         * A rehash that throws leaves the table unchanged or empty, depending on the type of the elements
         * and on what throws. See `rehash_impl`.
         *
         * `ValueType` must be nothrow move constructible and/or copy constructible.
         * Behaviour is undefined if the destructor of `ValueType` throws.
         *
         *
         * The class holds its buckets in a 2-dimensional fashion. Instead of having a
         * linear `std::vector<bucket>` for [0, bucket_count) where each bucket stores
         * one value, we have a `std::vector<sparse_array>` (sparse_buckets_data_)
         * where each `sparse_array` stores multiple values (up to
         * `sparse_array::BITMAP_NB_BITS`). To convert a one dimensional `ibucket`
         * position to a position in `std::vector<sparse_array>` and a position in
         * `sparse_array`, use respectively the methods
         * `sparse_array::sparse_ibucket(ibucket)` and
         * `sparse_array::index_in_sparse_bucket(ibucket)`.
         *
         * The number of buckets is 0 or a power of two. A hash picks its bucket with a mask.
         *
         * The hasher, the key equality and the allocator are held as members, not as base classes. A standard
         * layout class may declare its non-static data members in one class of the hierarchy only. Keeping
         * everything in `sparse_hash` therefore makes `sparse_hash`, and with it `sparse_map` and `sparse_set`, a
         * standard layout type whenever `Hash`, `KeyEqual`, `Allocator` and the bucket container are standard
         * layout themselves. The members are marked potentially overlapping, so empty ones still cost no space.
         */
        template<class ValueType, class KeySelect, class ValueSelect, class Hash, class KeyEqual, class Allocator, dice::sparse_map::sh::sparsity Sparsity, dice::sparse_map::sh::allocation_failure AllocationFailure>
        class sparse_hash {
        private:
            template<typename U>
            using has_mapped_type =
                typename std::integral_constant<bool, !std::is_same<U, void>::value>;

        public:
            template<bool IsConst>
            class sparse_iterator;

            using key_type = typename KeySelect::key_type;
            using value_type = ValueType;
            using hasher = Hash;
            using key_equal = KeyEqual;
            using allocator_type = Allocator;
            using reference = value_type &;
            using const_reference = value_type const &;
            using size_type = typename std::allocator_traits<allocator_type>::size_type;
            using pointer = typename std::allocator_traits<allocator_type>::pointer;
            using const_pointer = typename std::allocator_traits<allocator_type>::const_pointer;
            using difference_type = typename std::allocator_traits<allocator_type>::difference_type;
            using iterator = sparse_iterator<false>;
            using const_iterator = sparse_iterator<true>;

        private:
            using sparse_array = dice::sparse_map::detail_sparse_hash::sparse_array<ValueType, Allocator, Sparsity, AllocationFailure>;

            using sparse_buckets_allocator = typename std::allocator_traits<
                allocator_type>::template rebind_alloc<sparse_array>;
            using sparse_buckets_container = std::vector<sparse_array, sparse_buckets_allocator>;

        public:
            /**
             * The `operator*()` and `operator->()` methods return a const reference and
             * const pointer respectively to the stored value type (`Key` for a set,
             * `std::pair<Key, T>` for a map).
             *
             * In case of a map, to get a mutable reference to the value `T` associated to
             * a key (the `.second` in the stored pair), you have to call `value()`.
             */
            template<bool IsConst>
            class sparse_iterator {
                friend class sparse_hash;

            private:
                using sparse_bucket_iterator = typename std::conditional<
                    IsConst,
                    typename sparse_buckets_container::const_iterator,
                    typename sparse_buckets_container::iterator>::type;

                using sparse_array_iterator =
                    typename std::conditional<IsConst,
                                              typename sparse_array::const_iterator,
                                              typename sparse_array::iterator>::type;

                /**
                 * sparse_array_it should be nullptr if sparse_bucket_it ==
                 * sparse_buckets_data_.end(). (TODO better way?)
                 */
                sparse_iterator(sparse_bucket_iterator sparse_bucket_it,
                                sparse_array_iterator sparse_array_it)
                    : sparse_buckets_it_(sparse_bucket_it),
                      sparse_array_it_(sparse_array_it) {
                }

            public:
                using iterator_category = std::forward_iterator_tag;
                using value_type = typename sparse_hash::value_type const;
                using difference_type = std::ptrdiff_t;
                using reference = value_type &;
                using pointer = typename sparse_hash::const_pointer;

                sparse_iterator() noexcept {
                }

                // Copy constructor from iterator to const_iterator.
                template<bool TIsConst = IsConst,
                         typename std::enable_if<TIsConst>::type * = nullptr>
                sparse_iterator(sparse_iterator<!TIsConst> const &other) noexcept
                    : sparse_buckets_it_(other.sparse_buckets_it_),
                      sparse_array_it_(other.sparse_array_it_) {
                }

                sparse_iterator(sparse_iterator const &other) = default;
                sparse_iterator(sparse_iterator &&other) = default;
                sparse_iterator &operator=(sparse_iterator const &other) = default;
                sparse_iterator &operator=(sparse_iterator &&other) = default;

                typename sparse_hash::key_type const &key() const {
                    return KeySelect()(*sparse_array_it_);
                }

                template<class U = ValueSelect,
                         typename std::enable_if<has_mapped_type<U>::value && IsConst>::type * = nullptr>
                typename U::value_type const &value() const {
                    return U()(*sparse_array_it_);
                }

                template<class U = ValueSelect,
                         typename std::enable_if<has_mapped_type<U>::value && !IsConst>::type * = nullptr>
                typename U::value_type &value() {
                    return U()(*sparse_array_it_);
                }

                reference operator*() const {
                    return *sparse_array_it_;
                }

                // with fancy pointers addressof might be problematic.
                pointer operator->() const {
                    return std::addressof(*sparse_array_it_);
                }

                sparse_iterator &operator++() {
                    DICE_SPARSE_MAP_ASSERT(sparse_array_it_ != nullptr);
                    ++sparse_array_it_;

                    // vector iterator with fancy pointers have a problem with ->
                    if (sparse_array_it_ == (*sparse_buckets_it_).end()) {
                        do {
                            if ((*sparse_buckets_it_).last()) {
                                ++sparse_buckets_it_;
                                sparse_array_it_ = nullptr;
                                return *this;
                            }

                            ++sparse_buckets_it_;
                        } while ((*sparse_buckets_it_).empty());

                        sparse_array_it_ = (*sparse_buckets_it_).begin();
                    }

                    return *this;
                }

                sparse_iterator operator++(int) {
                    sparse_iterator tmp(*this);
                    ++*this;

                    return tmp;
                }

                friend bool operator==(sparse_iterator const &lhs,
                                       sparse_iterator const &rhs) {
                    return lhs.sparse_buckets_it_ == rhs.sparse_buckets_it_ && lhs.sparse_array_it_ == rhs.sparse_array_it_;
                }

                friend bool operator!=(sparse_iterator const &lhs,
                                       sparse_iterator const &rhs) {
                    return !(lhs == rhs);
                }

            private:
                sparse_bucket_iterator sparse_buckets_it_;
                sparse_array_iterator sparse_array_it_;
            };

        public:
            sparse_hash(size_type bucket_count, Hash const &hash, KeyEqual const &equal, Allocator const &alloc, float max_load_factor)
                : alloc_(alloc),
                  hash_(hash),
                  key_equal_(equal),
                  mask_(round_bucket_count(bucket_count)),
                  sparse_buckets_data_(alloc),
                  //         sparse_buckets_data_(std::allocator_traits<Allocator>::rebind_alloc<sparse_buckets_container::Allocator>(alloc)),
                  sparse_buckets_(static_empty_sparse_bucket_ptr()),
                  bucket_count_(bucket_count),
                  nb_elements_(0),
                  nb_deleted_buckets_(0) {
                if (bucket_count_ > max_bucket_count()) {
                    throw std::length_error("The map exceeds its maximum size.");
                }

                if (bucket_count_ > 0) {
                    /*
                     * We can't use the `vector(size_type count, const Allocator& alloc)`
                     * constructor as it's only available in C++14 and we need to support
                     * C++11. We thus must resize after using the `vector(const Allocator&
                     * alloc)` constructor.
                     *
                     * We can't use `vector(size_type count, const T& value, const Allocator&
                     * alloc)` as it requires the value T to be copyable.
                     */
                    detail_sparse_hash::allocate_with<AllocationFailure>([&] {
                        sparse_buckets_data_.resize(sparse_array::nb_sparse_buckets(bucket_count));
                    });
                    sparse_buckets_ = sparse_buckets_data_.data();

                    DICE_SPARSE_MAP_ASSERT(!sparse_buckets_data_.empty());
                    sparse_buckets_data_.back().set_as_last();
                }

                this->max_load_factor(max_load_factor);

                // Check in the constructor instead of outside of a function to avoid
                // compilation issues when value_type is not complete.
                static_assert(std::is_nothrow_move_constructible<value_type>::value || std::is_copy_constructible<value_type>::value,
                              "Key, and T if present, must be nothrow move constructible "
                              "and/or copy constructible.");
            }

            ~sparse_hash() {
                clear();
            }

            sparse_hash(sparse_hash const &other)
                : alloc_(std::allocator_traits<
                         Allocator>::select_on_container_copy_construction(other.alloc_)),
                  hash_(other.hash_),
                  key_equal_(other.key_equal_),
                  mask_(other.mask_),
                  sparse_buckets_data_(
                      std::allocator_traits<
                          Allocator>::select_on_container_copy_construction(other.alloc_)),
                  bucket_count_(other.bucket_count_),
                  nb_elements_(other.nb_elements_),
                  nb_deleted_buckets_(other.nb_deleted_buckets_),
                  load_threshold_rehash_(other.load_threshold_rehash_),
                  load_threshold_clear_deleted_(other.load_threshold_clear_deleted_),
                  max_load_factor_(other.max_load_factor_) {
                copy_buckets_from(other),
                    sparse_buckets_ = sparse_buckets_data_.empty()
                                          ? static_empty_sparse_bucket_ptr()
                                          : sparse_buckets_data_.data();
            }

            sparse_hash(sparse_hash &&other) noexcept(
                std::is_nothrow_move_constructible<Allocator>::value && std::is_nothrow_move_constructible<Hash>::value && std::is_nothrow_move_constructible<KeyEqual>::value && std::is_nothrow_move_constructible<sparse_buckets_container>::value)
                : alloc_(std::move(other.alloc_)),
                  hash_(std::move(other.hash_)),
                  key_equal_(std::move(other.key_equal_)),
                  mask_(other.mask_),
                  sparse_buckets_data_(std::move(other.sparse_buckets_data_)),
                  sparse_buckets_(sparse_buckets_data_.empty()
                                      ? static_empty_sparse_bucket_ptr()
                                      : sparse_buckets_data_.data()),
                  bucket_count_(other.bucket_count_),
                  nb_elements_(other.nb_elements_),
                  nb_deleted_buckets_(other.nb_deleted_buckets_),
                  load_threshold_rehash_(other.load_threshold_rehash_),
                  load_threshold_clear_deleted_(other.load_threshold_clear_deleted_),
                  max_load_factor_(other.max_load_factor_) {
                other.mask_ = 0;
                other.sparse_buckets_data_.clear();
                other.sparse_buckets_ = static_empty_sparse_bucket_ptr();
                other.bucket_count_ = 0;
                other.nb_elements_ = 0;
                other.nb_deleted_buckets_ = 0;
                other.load_threshold_rehash_ = 0;
                other.load_threshold_clear_deleted_ = 0;
            }

            sparse_hash &operator=(sparse_hash const &other) {
                if (this != &other) {
                    clear();

                    if constexpr (std::allocator_traits<
                                      Allocator>::propagate_on_container_copy_assignment::value) {
                        alloc_ = other.alloc_;
                    }

                    hash_ = other.hash_;
                    key_equal_ = other.key_equal_;
                    mask_ = other.mask_;

                    if constexpr (std::allocator_traits<
                                      Allocator>::propagate_on_container_copy_assignment::value) {
                        sparse_buckets_data_ = sparse_buckets_container(other.alloc_);
                    } else {
                        if (sparse_buckets_data_.size() != other.sparse_buckets_data_.size()) {
                            sparse_buckets_data_ = sparse_buckets_container(alloc_);
                        } else {
                            sparse_buckets_data_.clear();
                        }
                    }

                    copy_buckets_from(other);
                    sparse_buckets_ = sparse_buckets_data_.empty()
                                          ? static_empty_sparse_bucket_ptr()
                                          : sparse_buckets_data_.data();

                    bucket_count_ = other.bucket_count_;
                    nb_elements_ = other.nb_elements_;
                    nb_deleted_buckets_ = other.nb_deleted_buckets_;
                    load_threshold_rehash_ = other.load_threshold_rehash_;
                    load_threshold_clear_deleted_ = other.load_threshold_clear_deleted_;
                    max_load_factor_ = other.max_load_factor_;
                }

                return *this;
            }

            sparse_hash &operator=(sparse_hash &&other) noexcept {
                clear();

                if (not std::allocator_traits<
                        Allocator>::propagate_on_container_move_assignment::value
                    and (alloc_ != other.alloc_)) {
                    move_buckets_from(std::move(other));
                } else {
                    alloc_ = std::move(other.alloc_);
                    sparse_buckets_data_ = std::move(other.sparse_buckets_data_);
                }

                sparse_buckets_ = sparse_buckets_data_.empty()
                                      ? static_empty_sparse_bucket_ptr()
                                      : sparse_buckets_data_.data();

                hash_ = std::move(other.hash_);
                key_equal_ = std::move(other.key_equal_);
                mask_ = other.mask_;
                bucket_count_ = other.bucket_count_;
                nb_elements_ = other.nb_elements_;
                nb_deleted_buckets_ = other.nb_deleted_buckets_;
                load_threshold_rehash_ = other.load_threshold_rehash_;
                load_threshold_clear_deleted_ = other.load_threshold_clear_deleted_;
                max_load_factor_ = other.max_load_factor_;

                other.mask_ = 0;
                other.sparse_buckets_data_.clear();
                other.sparse_buckets_ = static_empty_sparse_bucket_ptr();
                other.bucket_count_ = 0;
                other.nb_elements_ = 0;
                other.nb_deleted_buckets_ = 0;
                other.load_threshold_rehash_ = 0;
                other.load_threshold_clear_deleted_ = 0;

                return *this;
            }

            allocator_type get_allocator() const {
                return alloc_;
            }

            /*
             * Iterators
             */
            iterator begin() noexcept {
                auto begin = sparse_buckets_data_.begin();
                // vector iterator with fancy pointers have a problem with ->
                while (begin != sparse_buckets_data_.end() && (*begin).empty()) {
                    ++begin;
                }

                // vector iterator with fancy pointers have a problem with ->
                return iterator(begin, (begin != sparse_buckets_data_.end()) ? (*begin).begin() : nullptr);
            }

            const_iterator begin() const noexcept {
                return cbegin();
            }

            const_iterator cbegin() const noexcept {
                auto begin = sparse_buckets_data_.cbegin();
                // vector iterator with fancy pointers have a problem with ->
                while (begin != sparse_buckets_data_.cend() && (*begin).empty()) {
                    ++begin;
                }

                return const_iterator(begin, (begin != sparse_buckets_data_.cend()) ? (*begin).cbegin() : nullptr);
            }

            iterator end() noexcept {
                return iterator(sparse_buckets_data_.end(), nullptr);
            }

            const_iterator end() const noexcept {
                return cend();
            }

            const_iterator cend() const noexcept {
                return const_iterator(sparse_buckets_data_.cend(), nullptr);
            }

            /*
             * Capacity
             */
            bool empty() const noexcept {
                return nb_elements_ == 0;
            }

            size_type size() const noexcept {
                return nb_elements_;
            }

            size_type max_size() const noexcept {
                return std::min(std::allocator_traits<Allocator>::max_size(),
                                sparse_buckets_data_.max_size());
            }

            /*
             * Modifiers
             */
            void clear() noexcept {
                for (auto &bucket : sparse_buckets_data_) {
                    bucket.clear(alloc_);
                }

                nb_elements_ = 0;
                nb_deleted_buckets_ = 0;
            }

            template<typename P>
            std::pair<iterator, bool> insert(P &&value) {
                return insert_impl(KeySelect()(value), std::forward<P>(value));
            }

            template<typename P>
            iterator insert_hint(const_iterator hint, P &&value) {
                if (hint != cend() && compare_keys(KeySelect()(*hint), KeySelect()(value))) {
                    return mutable_iterator(hint);
                }

                return insert(std::forward<P>(value)).first;
            }

            template<class InputIt>
            void insert(InputIt first, InputIt last) {
                if constexpr (std::is_base_of<
                                  std::forward_iterator_tag,
                                  typename std::iterator_traits<InputIt>::iterator_category>::value) {
                    auto const nb_elements_insert = std::distance(first, last);
                    size_type const nb_free_buckets = load_threshold_rehash_ - size();
                    DICE_SPARSE_MAP_ASSERT(load_threshold_rehash_ >= size());

                    if (nb_elements_insert > 0 && nb_free_buckets < size_type(nb_elements_insert)) {
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
                    it.first.value() = std::forward<M>(obj);
                }

                return it;
            }

            template<class K, class M>
            iterator insert_or_assign(const_iterator hint, K &&key, M &&obj) {
                if (hint != cend() && compare_keys(KeySelect()(*hint), key)) {
                    auto it = mutable_iterator(hint);
                    it.value() = std::forward<M>(obj);

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
                return insert_impl(key, std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<Args>(args)...));
            }

            template<class K, class... Args>
            iterator try_emplace_hint(const_iterator hint, K &&key, Args &&...args) {
                if (hint != cend() && compare_keys(KeySelect()(*hint), key)) {
                    return mutable_iterator(hint);
                }

                return try_emplace(std::forward<K>(key), std::forward<Args>(args)...).first;
            }

            /**
             * Here to avoid `template<class K> size_type erase(const K& key)` being used
             * when we use an iterator instead of a const_iterator.
             */
            iterator erase(iterator pos) {
                DICE_SPARSE_MAP_ASSERT(pos != end() && nb_elements_ > 0);
                // vector iterator with fancy pointers have a problem with ->
                auto it_sparse_array_next = (*pos.sparse_buckets_it_).erase(alloc_, pos.sparse_array_it_);
                nb_elements_--;
                nb_deleted_buckets_++;

                if (it_sparse_array_next == (*pos.sparse_buckets_it_).end()) {
                    auto it_sparse_buckets_next = pos.sparse_buckets_it_;
                    do {
                        ++it_sparse_buckets_next;
                    } while (it_sparse_buckets_next != sparse_buckets_data_.end() && (*it_sparse_buckets_next).empty());

                    if (it_sparse_buckets_next == sparse_buckets_data_.end()) {
                        return end();
                    } else {
                        return iterator(it_sparse_buckets_next,
                                        (*it_sparse_buckets_next).begin());
                    }
                } else {
                    return iterator(pos.sparse_buckets_it_, it_sparse_array_next);
                }
            }

            iterator erase(const_iterator pos) {
                return erase(mutable_iterator(pos));
            }

            iterator erase(const_iterator first, const_iterator last) {
                if (first == last) {
                    return mutable_iterator(first);
                }

                // TODO Optimize, could avoid the call to std::distance.
                size_type const nb_elements_to_erase = static_cast<size_type>(std::distance(first, last));
                auto to_delete = mutable_iterator(first);
                for (size_type i = 0; i < nb_elements_to_erase; i++) {
                    to_delete = erase(to_delete);
                }

                return to_delete;
            }

            template<class K>
            size_type erase(K const &key) {
                return erase(key, hash_key(key));
            }

            template<class K>
            size_type erase(K const &key, std::size_t hash) {
                return erase_impl(key, hash);
            }

            void swap(sparse_hash &other) {
                using std::swap;

                if constexpr (std::allocator_traits<Allocator>::propagate_on_container_swap::value) {
                    swap(alloc_, other.alloc_);
                } else {
                    DICE_SPARSE_MAP_ASSERT(alloc_ == other.alloc_);
                }

                swap(hash_, other.hash_);
                swap(key_equal_, other.key_equal_);
                swap(mask_, other.mask_);
                swap(sparse_buckets_data_, other.sparse_buckets_data_);
                swap(sparse_buckets_, other.sparse_buckets_);
                swap(bucket_count_, other.bucket_count_);
                swap(nb_elements_, other.nb_elements_);
                swap(nb_deleted_buckets_, other.nb_deleted_buckets_);
                swap(load_threshold_rehash_, other.load_threshold_rehash_);
                swap(load_threshold_clear_deleted_, other.load_threshold_clear_deleted_);
                swap(max_load_factor_, other.max_load_factor_);
            }

            /*
             * Lookup
             */
            template<
                class K,
                class U = ValueSelect,
                typename std::enable_if<has_mapped_type<U>::value>::type * = nullptr>
            typename U::value_type &at(K const &key) {
                return at(key, hash_key(key));
            }

            template<
                class K,
                class U = ValueSelect,
                typename std::enable_if<has_mapped_type<U>::value>::type * = nullptr>
            typename U::value_type &at(K const &key, std::size_t hash) {
                return const_cast<typename U::value_type &>(
                    static_cast<sparse_hash const *>(this)->at(key, hash));
            }

            template<
                class K,
                class U = ValueSelect,
                typename std::enable_if<has_mapped_type<U>::value>::type * = nullptr>
            typename U::value_type const &at(K const &key) const {
                return at(key, hash_key(key));
            }

            template<
                class K,
                class U = ValueSelect,
                typename std::enable_if<has_mapped_type<U>::value>::type * = nullptr>
            typename U::value_type const &at(K const &key, std::size_t hash) const {
                auto it = find(key, hash);
                if (it != cend()) {
                    return it.value();
                } else {
                    throw std::out_of_range("Couldn't find key.");
                }
            }

            template<
                class K,
                class U = ValueSelect,
                typename std::enable_if<has_mapped_type<U>::value>::type * = nullptr>
            typename U::value_type &operator[](K &&key) {
                return try_emplace(std::forward<K>(key)).first.value();
            }

            template<class K>
            bool contains(K const &key) const {
                return contains(key, hash_key(key));
            }

            template<class K>
            bool contains(K const &key, std::size_t hash) const {
                return count(key, hash) != 0;
            }

            template<class K>
            size_type count(K const &key) const {
                return count(key, hash_key(key));
            }

            template<class K>
            size_type count(K const &key, std::size_t hash) const {
                if (find(key, hash) != cend()) {
                    return 1;
                } else {
                    return 0;
                }
            }

            template<class K>
            iterator find(K const &key) {
                return find_impl(key, hash_key(key));
            }

            template<class K>
            iterator find(K const &key, std::size_t hash) {
                return find_impl(key, hash);
            }

            template<class K>
            const_iterator find(K const &key) const {
                return find_impl(key, hash_key(key));
            }

            template<class K>
            const_iterator find(K const &key, std::size_t hash) const {
                return find_impl(key, hash);
            }

            template<class K>
            std::pair<iterator, iterator> equal_range(K const &key) {
                return equal_range(key, hash_key(key));
            }

            template<class K>
            std::pair<iterator, iterator> equal_range(K const &key, std::size_t hash) {
                iterator it = find(key, hash);
                return std::make_pair(it, (it == end()) ? it : std::next(it));
            }

            template<class K>
            std::pair<const_iterator, const_iterator> equal_range(K const &key) const {
                return equal_range(key, hash_key(key));
            }

            template<class K>
            std::pair<const_iterator, const_iterator> equal_range(
                K const &key,
                std::size_t hash) const {
                const_iterator it = find(key, hash);
                return std::make_pair(it, (it == cend()) ? it : std::next(it));
            }

            /*
             * Bucket interface
             */
            size_type bucket_count() const {
                return bucket_count_;
            }

            size_type max_bucket_count() const {
                return sparse_buckets_data_.max_size();
            }

            /*
             * Hash policy
             */
            float load_factor() const {
                if (bucket_count() == 0) {
                    return 0;
                }

                return float(nb_elements_) / float(bucket_count());
            }

            float max_load_factor() const {
                return max_load_factor_;
            }

            void max_load_factor(float ml) {
                max_load_factor_ = std::max(0.1f, std::min(ml, 0.8f));
                load_threshold_rehash_ = size_type(float(bucket_count()) * max_load_factor_);

                float const max_load_factor_with_deleted_buckets = max_load_factor_ + 0.5f * (1.0f - max_load_factor_);
                DICE_SPARSE_MAP_ASSERT(max_load_factor_with_deleted_buckets > 0.0f && max_load_factor_with_deleted_buckets <= 1.0f);
                load_threshold_clear_deleted_ = size_type(float(bucket_count()) * max_load_factor_with_deleted_buckets);
            }

            void rehash(size_type count) {
                count = std::max(count,
                                 size_type(std::ceil(float(size()) / max_load_factor())));
                rehash_impl(count);
            }

            void reserve(size_type count) {
                rehash(size_type(std::ceil(float(count) / max_load_factor())));
            }

            /*
             * Observers
             */
            hasher hash_function() const {
                return hash_;
            }

            key_equal key_eq() const {
                return key_equal_;
            }

            /*
             * Other
             */
            iterator mutable_iterator(const_iterator pos) {
                auto it_sparse_buckets = sparse_buckets_data_.begin() + std::distance(sparse_buckets_data_.cbegin(), pos.sparse_buckets_it_);

                return iterator(it_sparse_buckets,
                                sparse_array::mutable_iterator(pos.sparse_array_it_));
            }

        private:
            template<class K>
            std::size_t hash_key(K const &key) const {
                return hash_(key);
            }

            template<class K1, class K2>
            bool compare_keys(const K1 &key1, const K2 &key2) const {
                return key_equal_(key1, key2);
            }

            size_type bucket_for_hash(std::size_t hash) const {
                std::size_t const bucket = hash & mask_;
                DICE_SPARSE_MAP_ASSERT(sparse_array::sparse_ibucket(bucket) < sparse_buckets_data_.size() || (bucket == 0 && sparse_buckets_data_.empty()));

                return bucket;
            }

            /**
             * @return the bucket of the probe `iprobe` after bucket `ibucket` (quadratic probing)
             */
            size_type next_bucket(size_type ibucket, size_type iprobe) const {
                return (ibucket + iprobe) & mask_;
            }

            /**
             * Largest power of two of `std::size_t`, the largest bucket count.
             */
            static constexpr std::size_t max_power_of_two_bucket_count = (std::numeric_limits<std::size_t>::max() / 2) + 1;

            /**
             * Rounds `bucket_count` up to a power of two. 0 stays 0.
             * @return the mask that picks a bucket from a hash
             * @throws std::length_error if `bucket_count` is larger than the largest power of two of `std::size_t`
             */
            static std::size_t round_bucket_count(size_type &bucket_count) {
                if (bucket_count > max_power_of_two_bucket_count) {
                    throw std::length_error("The hash table exceeds its maximum size.");
                }
                if (bucket_count == 0) {
                    return 0;
                }
                bucket_count = round_up_to_power_of_two(bucket_count);
                return bucket_count - 1;
            }

            /**
             * @return the bucket count after the next growth, twice the current one
             * @throws std::length_error if the table cannot grow any more
             */
            std::size_t next_bucket_count() const {
                if ((mask_ + 1) > max_power_of_two_bucket_count / 2) {
                    throw std::length_error("The hash table exceeds its maximum size.");
                }
                return (mask_ + 1) * 2;
            }

            // TODO encapsulate sparse_buckets_data_ to avoid the managing the allocator
            void copy_buckets_from(sparse_hash const &other) {
                detail_sparse_hash::allocate_with<AllocationFailure>([&] {
                    sparse_buckets_data_.reserve(other.sparse_buckets_data_.size());
                });

                try {
                    for (auto const &bucket : other.sparse_buckets_data_) {
                        sparse_buckets_data_.emplace_back(bucket, alloc_);
                    }
                } catch (...) {
                    clear();
                    throw;
                }

                DICE_SPARSE_MAP_ASSERT(sparse_buckets_data_.empty() || sparse_buckets_data_.back().last());
            }

            void move_buckets_from(sparse_hash &&other) {
                detail_sparse_hash::allocate_with<AllocationFailure>([&] {
                    sparse_buckets_data_.reserve(other.sparse_buckets_data_.size());
                });

                try {
                    for (auto &&bucket : other.sparse_buckets_data_) {
                        sparse_buckets_data_.emplace_back(std::move(bucket), alloc_);
                    }
                } catch (...) {
                    clear();
                    throw;
                }

                DICE_SPARSE_MAP_ASSERT(sparse_buckets_data_.empty() || sparse_buckets_data_.back().last());
            }

            template<class K, class... Args>
            std::pair<iterator, bool> insert_impl(K const &key,
                                                  Args &&...value_type_args) {
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
                typename sparse_array::size_type index_in_sparse_bucket_first_deleted = 0;

                std::size_t const hash = hash_key(key);
                std::size_t ibucket = bucket_for_hash(hash);

                auto confirmed_insert = [&](std::size_t sparse_ibucket, typename sparse_array::size_type index_in_sparse_bucket) {
                    if (size() >= load_threshold_rehash_) {
                        rehash_impl(next_bucket_count());
                        return insert_impl(key, std::forward<Args>(value_type_args)...);
                    } else if (size() + nb_deleted_buckets_ >= load_threshold_clear_deleted_) {
                        clear_deleted_buckets();
                        return insert_impl(key, std::forward<Args>(value_type_args)...);
                    }

                    if (found_first_deleted_bucket) {
                        auto it = insert_in_bucket(sparse_ibucket_first_deleted,
                                                   index_in_sparse_bucket_first_deleted,
                                                   std::forward<Args>(value_type_args)...);
                        nb_deleted_buckets_--;

                        return it;
                    }

                    return insert_in_bucket(sparse_ibucket, index_in_sparse_bucket, std::forward<Args>(value_type_args)...);
                };

                std::size_t probe = 0;
                while (true) {
                    std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);

                    if (sparse_buckets_ != static_empty_sparse_bucket_ptr()) {
                        if (sparse_buckets_[sparse_ibucket].has_value(index_in_sparse_bucket)) {
                            auto value_it = sparse_buckets_[sparse_ibucket].value(index_in_sparse_bucket);
                            if (compare_keys(key, KeySelect()(*value_it))) {
                                return std::make_pair(
                                    iterator(sparse_buckets_data_.begin() + sparse_ibucket,
                                             value_it),
                                    false);
                            }
                        } else if (sparse_buckets_[sparse_ibucket].has_deleted_value(
                                       index_in_sparse_bucket)
                                   && probe < bucket_count_) {
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
                            return confirmed_insert(sparse_ibucket, index_in_sparse_bucket);
                        }
                    } else {
                        /**
                         * At this point we are sure that the value does not exist
                         * in the hash table.
                         * First check if we satisfy load and delete thresholds, and if not,
                         * rehash the hash table (and therefore start over). Otherwise, just
                         * insert the value into the appropriate bucket.
                         */
                        return confirmed_insert(sparse_ibucket, index_in_sparse_bucket);
                    }

                    probe++;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

            template<class... Args>
            std::pair<iterator, bool> insert_in_bucket(
                std::size_t sparse_ibucket,
                typename sparse_array::size_type index_in_sparse_bucket,
                Args &&...value_type_args) {
                // is not called when empty
                auto value_it = sparse_buckets_[sparse_ibucket].set(
                    alloc_,
                    index_in_sparse_bucket,
                    std::forward<Args>(value_type_args)...);
                nb_elements_++;

                return std::make_pair(
                    iterator(sparse_buckets_data_.begin() + sparse_ibucket, value_it),
                    true);
            }

            template<class K>
            size_type erase_impl(K const &key, std::size_t hash) {
                std::size_t ibucket = bucket_for_hash(hash);

                std::size_t probe = 0;

                if (sparse_buckets_ == static_empty_sparse_bucket_ptr()) {
                    return 0;
                }
                while (true) {
                    std::size_t const sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto const index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);

                    if (sparse_buckets_[sparse_ibucket].has_value(index_in_sparse_bucket)) {
                        auto value_it = sparse_buckets_[sparse_ibucket].value(index_in_sparse_bucket);
                        if (compare_keys(key, KeySelect()(*value_it))) {
                            sparse_buckets_[sparse_ibucket].erase(alloc_, value_it, index_in_sparse_bucket);
                            nb_elements_--;
                            nb_deleted_buckets_++;

                            return 1;
                        }
                    } else if (!sparse_buckets_[sparse_ibucket].has_deleted_value(
                                   index_in_sparse_bucket)
                               || probe >= bucket_count_) {
                        return 0;
                    }

                    probe++;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

            template<class K>
            iterator find_impl(K const &key, std::size_t hash) {
                return mutable_iterator(
                    static_cast<sparse_hash const *>(this)->find(key, hash));
            }

            template<class K>
            const_iterator find_impl(K const &key, std::size_t hash) const {
                std::size_t ibucket = bucket_for_hash(hash);

                std::size_t probe = 0;
                while (true) {
                    std::size_t const sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto const index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);

                    if (sparse_buckets_ == static_empty_sparse_bucket_ptr()) {
                        return cend();
                    }
                    if (sparse_buckets_[sparse_ibucket].has_value(index_in_sparse_bucket)) {
                        auto value_it = sparse_buckets_[sparse_ibucket].value(index_in_sparse_bucket);
                        if (compare_keys(key, KeySelect()(*value_it))) {
                            return const_iterator(sparse_buckets_data_.cbegin() + sparse_ibucket,
                                                  value_it);
                        }
                    } else if (!sparse_buckets_[sparse_ibucket].has_deleted_value(
                                   index_in_sparse_bucket)
                               || probe >= bucket_count_) {
                        return cend();
                    }

                    probe++;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

            void clear_deleted_buckets() {
                // TODO could be optimized, we could do it in-place instead of allocating a
                // new bucket array.
                rehash_impl(bucket_count_);
                DICE_SPARSE_MAP_ASSERT(nb_deleted_buckets_ == 0);
            }

            /**
             * True if a rehash copies the elements and frees the old groups at the end, see `rehash_impl`.
             */
            static constexpr bool copy_on_rehash = !std::is_nothrow_move_constructible_v<value_type>;

            /**
             * Moves or copies all elements into a new table with at least `count` buckets. How depends on the type of
             * the elements.
             *
             * An element whose move constructor cannot throw is moved, one old group after the other. Each old group
             * is freed right after its elements are moved. If the hash function or the allocator throws while the
             * elements are moved, the table is cleared, so it is empty afterwards. An exception of the allocator
             * before, while the new table allocates its buckets, leaves the table unchanged.
             *
             * An element whose move constructor can throw is copied. The old groups are freed at the end, so the old
             * and the new groups are in memory at the same time. The table stays untouched until all copies are made,
             * so an exception leaves it unchanged.
             *
             * The table takes the new buckets with `swap_storage`, which cannot throw.
             *
             * The allocator throws here only with `sh::allocation_failure::throwing`. With
             * `sh::allocation_failure::terminating` a failed allocation ends the process.
             */
            void rehash_impl(size_type count) {
                sparse_hash new_table(count, hash_, key_equal_, alloc_, max_load_factor_);

                if constexpr (copy_on_rehash) {
                    for (auto const &bucket : sparse_buckets_data_) {
                        for (auto const &value : bucket) {
                            new_table.insert_on_rehash(value);
                        }
                    }
                } else {
                    try {
                        for (auto &bucket : sparse_buckets_data_) {
                            for (auto &value : bucket) {
                                new_table.insert_on_rehash(std::move(value));
                            }
                            bucket.clear(alloc_);
                        }
                    } catch (...) {
                        clear();
                        throw;
                    }
                }

                swap_storage(new_table);
            }

            /**
             * Swaps the buckets and the counters with `other`, which has the same allocator, hash function, key
             * equality and maximum load factor. Unlike `swap`, it cannot throw.
             */
            void swap_storage(sparse_hash &other) noexcept {
                using std::swap;
                swap(mask_, other.mask_);
                swap(sparse_buckets_data_, other.sparse_buckets_data_);
                sparse_buckets_ = sparse_buckets_data_.empty() ? static_empty_sparse_bucket_ptr() : sparse_buckets_data_.data();
                other.sparse_buckets_ = other.sparse_buckets_data_.empty() ? static_empty_sparse_bucket_ptr() : other.sparse_buckets_data_.data();
                swap(bucket_count_, other.bucket_count_);
                swap(nb_elements_, other.nb_elements_);
                swap(nb_deleted_buckets_, other.nb_deleted_buckets_);
                swap(load_threshold_rehash_, other.load_threshold_rehash_);
                swap(load_threshold_clear_deleted_, other.load_threshold_clear_deleted_);
            }

            template<typename K>
            void insert_on_rehash(K &&key_value) {
                std::size_t const hash = hash_key(KeySelect()(key_value));
                std::size_t ibucket = bucket_for_hash(hash);

                std::size_t probe = 0;
                while (true) {
                    std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);

                    if (!sparse_buckets_[sparse_ibucket].has_value(index_in_sparse_bucket)) {
                        sparse_buckets_[sparse_ibucket].set(alloc_, index_in_sparse_bucket, std::forward<K>(key_value));
                        nb_elements_++;

                        return;
                    } else {
                        DICE_SPARSE_MAP_ASSERT(!compare_keys(
                            KeySelect()(key_value),
                            KeySelect()(*sparse_buckets_[sparse_ibucket].value(
                                index_in_sparse_bucket))));
                    }

                    probe++;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

        public:
            static constexpr size_type DEFAULT_INIT_BUCKET_COUNT = 0;
            static constexpr float DEFAULT_MAX_LOAD_FACTOR = 0.5f;

            using sparse_array_ptr = typename std::allocator_traits<allocator_type>::template rebind_traits<sparse_array>::pointer;
            /**
             * Return an nullptr to indicate an empty bucket
             */
            static sparse_array_ptr static_empty_sparse_bucket_ptr() {
                return {};
            }

        private:
            [[no_unique_address]] Allocator alloc_;
            [[no_unique_address]] Hash hash_;
            [[no_unique_address]] KeyEqual key_equal_;

            /**
             * `bucket_count_ - 1`, or 0 without buckets. Declared before `bucket_count_`: the constructor rounds its
             * `bucket_count` parameter in place in the initializer of `mask_`, and `bucket_count_` is initialized from
             * the rounded value.
             */
            std::size_t mask_;

            sparse_buckets_container sparse_buckets_data_;


            /**
             * Points to sparse_buckets_data_.data() if !sparse_buckets_data_.empty()
             * otherwise points to static_empty_sparse_bucket_ptr. This variable is useful
             * to avoid the cost of checking if sparse_buckets_data_ is empty when trying
             * to find an element.
             *
             * TODO Remove sparse_buckets_data_ and only use a pointer instead of a
             * pointer+vector to save some space in the sparse_hash object.
             */

            sparse_array_ptr sparse_buckets_;

            size_type bucket_count_;
            size_type nb_elements_;
            size_type nb_deleted_buckets_;

            /**
             * Maximum that nb_elements_ can reach before a rehash occurs automatically
             * to grow the hash table.
             */
            size_type load_threshold_rehash_;

            /**
             * Maximum that nb_elements_ + nb_deleted_buckets_ can reach before cleaning
             * up the buckets marked as deleted.
             */
            size_type load_threshold_clear_deleted_;
            float max_load_factor_;
        };

    }  // namespace detail_sparse_hash
}  // namespace dice::sparse_map

#endif
