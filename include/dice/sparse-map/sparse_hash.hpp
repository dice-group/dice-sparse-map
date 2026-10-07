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
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <iterator>
#include <limits>
#include <memory>
#include <new>
#include <ranges>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>

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
        concept IsTransparent = requires { typename T::is_transparent; };

        /**
         * `K` is a lookup key that cannot be mistaken for an iterator. Overloads that take a key of any type use
         * it, so that `erase(it)` keeps calling the iterator overload.
         */
        template<typename K, typename Iterator, typename ConstIterator>
        concept NotIterator = !std::is_convertible_v<K, Iterator> && !std::is_convertible_v<K, ConstIterator>;

        /**
         * What the constructors and `insert(first, last)` take as an input iterator: a copyable type that can be
         * dereferenced and incremented. Looser than `std::input_iterator`, so that iterators without
         * `std::iterator_traits` work, too.
         */
        template<typename It>
        concept LegacyInputIterator = std::copyable<It> && requires (It it) {
            *it;
            ++it;
        };

        /**
         * `std::ceil(value)` as `std::size_t`, for a non-negative `value`. Saturates at the maximum of `std::size_t`.
         */
        [[nodiscard]] inline std::size_t ceil_to_size(float value) noexcept {
            if (!(value < static_cast<float>(std::numeric_limits<std::size_t>::max()))) {
                return std::numeric_limits<std::size_t>::max();
            }
            auto const truncated = static_cast<std::size_t>(value);
            return static_cast<float>(truncated) < value ? truncated + 1 : truncated;
        }

        /**
         * Allocates `n` objects with `alloc`. With `sh::allocation_failure::terminating` any exception of the
         * allocator other than `std::length_error` ends the process with `std::abort()`. With
         * `sh::allocation_failure::throwing` every exception propagates. Every allocation of `sparse_array` and
         * `sparse_hash` goes through this function.
         */
        template<sh::allocation_failure AllocationFailure, typename Allocator>
        [[nodiscard]] typename std::allocator_traits<Allocator>::pointer allocate(
            Allocator &alloc,
            typename std::allocator_traits<Allocator>::size_type n) {
            if constexpr (AllocationFailure == dice::sparse_map::sh::allocation_failure::terminating) {
                try {
                    return std::allocator_traits<Allocator>::allocate(alloc, n);
                } catch (std::length_error const &) {
                    throw;
                } catch (...) {
                    std::abort();
                }
            } else {
                return std::allocator_traits<Allocator>::allocate(alloc, n);
            }
        }

        /**
         * One `T`, constructed with `allocator_traits<Allocator>::construct` in storage on the stack, and destroyed
         * with `allocator_traits<Allocator>::destroy`. It holds a new element while the table moves its elements,
         * because the arguments of the new element may refer to an element that is moved.
         */
        template<typename T, typename Allocator>
        struct value_holder {
            template<typename... Args>
            explicit value_holder(Allocator &alloc, Args &&...args)
                : alloc_(alloc) {
                std::allocator_traits<Allocator>::construct(alloc_, std::addressof(storage_.value), std::forward<Args>(args)...);
            }

            value_holder(value_holder const &) = delete;
            value_holder(value_holder &&) = delete;
            value_holder &operator=(value_holder const &) = delete;
            value_holder &operator=(value_holder &&) = delete;

            ~value_holder() {
                std::allocator_traits<Allocator>::destroy(alloc_, std::addressof(storage_.value));
            }

            [[nodiscard]] T &get() noexcept {
                return storage_.value;
            }

        private:
            /**
             * Storage for a `T` that is constructed and destroyed by hand.
             */
            union storage_type {
                storage_type() noexcept {
                }

                ~storage_type() {
                }

                T value;
            };

            Allocator &alloc_;
            storage_type storage_;
        };

        /**
         * Element access of `sparse_hash` for a map. The elements are stored as `std::pair<Key, T>`. The iterators
         * return `std::pair<Key, T> const &`. The mapped value is mutable through `iterator::value()`.
         */
        template<typename Key, typename T>
        struct map_access {
            static constexpr bool is_map = true;

            using key_type = Key;
            using mapped_type = T;
            using value_type = std::pair<Key, T>;
            using slot_type = value_type;
            using reference = value_type const &;
            using const_reference = value_type const &;

            [[nodiscard]] static Key const &key(slot_type const &slot) noexcept {
                return slot.first;
            }

            [[nodiscard]] static T &mapped(slot_type &slot) noexcept {
                return slot.second;
            }

            [[nodiscard]] static T const &mapped(slot_type const &slot) noexcept {
                return slot.second;
            }

            [[nodiscard]] static Key const &key_of_value(value_type const &value) noexcept {
                return value.first;
            }
        };

        /**
         * Element access of `sparse_hash` for a set. The elements are stored as `Key`.
         */
        template<typename Key>
        struct set_access {
            static constexpr bool is_map = false;

            using key_type = Key;
            using value_type = Key;
            using slot_type = Key;
            using reference = Key const &;
            using const_reference = Key const &;

            [[nodiscard]] static Key const &key(slot_type const &slot) noexcept {
                return slot;
            }

            [[nodiscard]] static Key const &key_of_value(value_type const &value) noexcept {
                return value;
            }
        };

        /**
         * A group of `bitmap_nb_bits` (64) buckets of the table. Only the occupied buckets have storage: the
         * values are kept densely in `values_`, in bucket order, and a bitmap marks which buckets are occupied.
         * A second bitmap marks the buckets whose value was erased (deleted buckets), so that probing goes on
         * past them.
         *
         * An index is a position in [0, bitmap_nb_bits), like a position in a `std::vector`. An offset is the
         * position of the value of an index in `values_`: the number of occupied buckets before the index.
         *
         * `sparse_array` does not store an allocator, so that groups do not grow with a stateful allocator. Every
         * function that allocates or frees takes the allocator as an argument. `clear(Allocator &)` must be
         * called before a `sparse_array` is destroyed.
         *
         * `T` must be nothrow move constructible and/or copy constructible. The behaviour is undefined if the
         * destructor of `T` throws.
         *
         * `AllocationFailure` decides what a failed allocation of the values does, see `allocate`.
         *
         * See https://smerity.com/articles/2015/google_sparsehash.html for the idea.
         */
        template<typename T, typename Allocator, sh::sparsity sparsity, sh::allocation_failure AllocationFailure>
        struct sparse_array {
            using value_type = T;
            using size_type = std::uint_least8_t;
            using allocator_type = Allocator;
            using allocator_traits = std::allocator_traits<allocator_type>;
            using pointer = typename allocator_traits::pointer;
            using const_pointer = typename allocator_traits::const_pointer;
            using iterator = value_type *;
            using const_iterator = value_type const *;

        private:
            static constexpr size_type capacity_growth_step = (sparsity == sh::sparsity::high)     ? 2
                                                              : (sparsity == sh::sparsity::medium) ? 4
                                                                                                   : 8;

            using bitmap_type = std::uint_least64_t;
            static constexpr std::size_t bitmap_nb_bits = 64;
            static constexpr std::size_t bucket_shift = 6;
            static constexpr std::size_t bucket_mask = bitmap_nb_bits - 1;

            static_assert(std::has_single_bit(bitmap_nb_bits), "bitmap_nb_bits must be a power of two.");
            static_assert(std::numeric_limits<bitmap_type>::digits >= bitmap_nb_bits,
                          "bitmap_type must be able to hold at least bitmap_nb_bits.");
            static_assert((std::size_t{1} << bucket_shift) == bitmap_nb_bits,
                          "(1 << bucket_shift) must be equal to bitmap_nb_bits.");
            static_assert(std::numeric_limits<size_type>::max() >= bitmap_nb_bits,
                          "size_type must be big enough to hold bitmap_nb_bits.");
            static_assert(std::is_unsigned_v<bitmap_type>, "bitmap_type must be unsigned.");
            static_assert((std::numeric_limits<bitmap_type>::max() & bucket_mask) == bitmap_nb_bits - 1);

        public:
            /**
             * @return the index of the `sparse_array` that holds bucket `ibucket` of the table
             */
            [[nodiscard]] static std::size_t sparse_ibucket(std::size_t ibucket) noexcept {
                return ibucket >> bucket_shift;
            }

            /**
             * @return the index of bucket `ibucket` of the table in its `sparse_array`
             */
            [[nodiscard]] static size_type index_in_sparse_bucket(std::size_t ibucket) noexcept {
                return static_cast<size_type>(ibucket & bucket_mask);
            }

            /**
             * @return the number of `sparse_array`s of a table with `bucket_count` buckets
             */
            [[nodiscard]] static std::size_t nb_sparse_buckets(std::size_t bucket_count) noexcept {
                if (bucket_count == 0) {
                    return 0;
                }

                return std::max<std::size_t>(1, sparse_ibucket(std::bit_ceil(bucket_count)));
            }

            sparse_array() noexcept = default;

            /**
             * For uses-allocator construction. The allocator is not stored.
             */
            sparse_array(std::allocator_arg_t, Allocator const & /*alloc*/) noexcept {
            }

            explicit sparse_array(bool last_bucket) noexcept
                : last_array_(last_bucket) {
            }

            /**
             * Allocates storage for `capacity` values. The allocator is const for the MoveInsertable requirement.
             */
            sparse_array(size_type capacity, Allocator const &const_alloc)
                : capacity_(capacity) {
                if (capacity_ > 0) {
                    Allocator alloc(const_alloc);
                    values_ = detail_sparse_hash::allocate<AllocationFailure>(alloc, capacity_);
                    DICE_SPARSE_MAP_ASSERT(values_ != nullptr);
                }
            }

            /**
             * Copies `other` into storage from `const_alloc`.
             */
            sparse_array(sparse_array const &other, Allocator const &const_alloc)
                : bitmap_vals_(other.bitmap_vals_),
                  bitmap_deleted_vals_(other.bitmap_deleted_vals_),
                  capacity_(other.capacity_),
                  last_array_(other.last_array_) {
                DICE_SPARSE_MAP_ASSERT(other.capacity_ >= other.nb_elements_);
                if (capacity_ == 0) {
                    return;
                }

                Allocator alloc(const_alloc);
                values_ = detail_sparse_hash::allocate<AllocationFailure>(alloc, capacity_);
                DICE_SPARSE_MAP_ASSERT(values_ != nullptr);
                try {
                    for (size_type i = 0; i < other.nb_elements_; ++i) {
                        construct_value(alloc, values() + i, other.values()[i]);
                        ++nb_elements_;
                    }
                } catch (...) {
                    clear(alloc);
                    throw;
                }
            }

            sparse_array(sparse_array &&other) noexcept
                : values_(std::exchange(other.values_, nullptr)),
                  bitmap_vals_(std::exchange(other.bitmap_vals_, 0)),
                  bitmap_deleted_vals_(std::exchange(other.bitmap_deleted_vals_, 0)),
                  nb_elements_(std::exchange(other.nb_elements_, 0)),
                  capacity_(std::exchange(other.capacity_, 0)),
                  last_array_(other.last_array_) {
            }

            /**
             * Moves the values of `other` into storage from `const_alloc`, or copies them if their move constructor
             * can throw. `other` keeps its values, the moved ones in a moved-from state.
             */
            sparse_array(sparse_array &&other, Allocator const &const_alloc)
                : bitmap_vals_(other.bitmap_vals_),
                  bitmap_deleted_vals_(other.bitmap_deleted_vals_),
                  capacity_(other.capacity_),
                  last_array_(other.last_array_) {
                DICE_SPARSE_MAP_ASSERT(other.capacity_ >= other.nb_elements_);
                if (capacity_ == 0) {
                    return;
                }

                Allocator alloc(const_alloc);
                values_ = detail_sparse_hash::allocate<AllocationFailure>(alloc, capacity_);
                DICE_SPARSE_MAP_ASSERT(values_ != nullptr);
                try {
                    for (size_type i = 0; i < other.nb_elements_; ++i) {
                        construct_value(alloc, values() + i, std::move_if_noexcept(other.values()[i]));
                        ++nb_elements_;
                    }
                } catch (...) {
                    clear(alloc);
                    throw;
                }
            }

            sparse_array &operator=(sparse_array const &) = delete;

            sparse_array &operator=(sparse_array &&other) noexcept {
                values_ = std::exchange(other.values_, nullptr);
                bitmap_vals_ = std::exchange(other.bitmap_vals_, 0);
                bitmap_deleted_vals_ = std::exchange(other.bitmap_deleted_vals_, 0);
                nb_elements_ = std::exchange(other.nb_elements_, 0);
                capacity_ = std::exchange(other.capacity_, 0);
                last_array_ = other.last_array_;
                return *this;
            }

            ~sparse_array() noexcept {
                // the owner must have called clear(Allocator &) before
                DICE_SPARSE_MAP_ASSERT(capacity_ == 0 && nb_elements_ == 0 && values_ == nullptr);
            }

            [[nodiscard]] iterator begin() noexcept {
                return values();
            }

            [[nodiscard]] iterator end() noexcept {
                return values() + nb_elements_;
            }

            [[nodiscard]] const_iterator begin() const noexcept {
                return cbegin();
            }

            [[nodiscard]] const_iterator end() const noexcept {
                return cend();
            }

            [[nodiscard]] const_iterator cbegin() const noexcept {
                return values();
            }

            [[nodiscard]] const_iterator cend() const noexcept {
                return values() + nb_elements_;
            }

            [[nodiscard]] bool empty() const noexcept {
                return nb_elements_ == 0;
            }

            [[nodiscard]] size_type size() const noexcept {
                return nb_elements_;
            }

            /**
             * Destroys all values and frees the storage.
             */
            void clear(allocator_type &alloc) noexcept {
                destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);

                values_ = nullptr;
                bitmap_vals_ = 0;
                bitmap_deleted_vals_ = 0;
                nb_elements_ = 0;
                capacity_ = 0;
            }

            /**
             * @return true if this is the last `sparse_array` of the table
             */
            [[nodiscard]] bool last() const noexcept {
                return last_array_;
            }

            void set_as_last() noexcept {
                last_array_ = true;
            }

            [[nodiscard]] bool has_value(size_type index) const noexcept {
                DICE_SPARSE_MAP_ASSERT(index < bitmap_nb_bits);
                return (bitmap_vals_ & (bitmap_type{1} << index)) != 0;
            }

            [[nodiscard]] bool has_deleted_value(size_type index) const noexcept {
                DICE_SPARSE_MAP_ASSERT(index < bitmap_nb_bits);
                return (bitmap_deleted_vals_ & (bitmap_type{1} << index)) != 0;
            }

            [[nodiscard]] iterator value(size_type index) noexcept {
                DICE_SPARSE_MAP_ASSERT(has_value(index));
                return values() + index_to_offset(index);
            }

            [[nodiscard]] const_iterator value(size_type index) const noexcept {
                DICE_SPARSE_MAP_ASSERT(has_value(index));
                return values() + index_to_offset(index);
            }

            /**
             * Constructs a value at `index` from `value_args`.
             * @return iterator to the new value
             */
            template<typename... Args>
            iterator set(allocator_type &alloc, size_type index, Args &&...value_args) {
                DICE_SPARSE_MAP_ASSERT(!has_value(index));

                size_type const offset = index_to_offset(index);
                insert_at_offset(alloc, offset, std::forward<Args>(value_args)...);

                bitmap_vals_ = (bitmap_vals_ | (bitmap_type{1} << index));
                bitmap_deleted_vals_ = (bitmap_deleted_vals_ & ~(bitmap_type{1} << index));

                ++nb_elements_;

                DICE_SPARSE_MAP_ASSERT(has_value(index));
                DICE_SPARSE_MAP_ASSERT(!has_deleted_value(index));

                return values() + offset;
            }

            iterator erase(allocator_type &alloc, iterator position) {
                auto const offset = static_cast<size_type>(position - begin());
                return erase(alloc, position, offset_to_index(offset));
            }

            /**
             * Erases the value at `position`, which is at `index`, and marks the bucket as deleted.
             * @return iterator to the next value, or `end()`
             */
            iterator erase(allocator_type &alloc, iterator position, size_type index) {
                DICE_SPARSE_MAP_ASSERT(has_value(index));
                DICE_SPARSE_MAP_ASSERT(!has_deleted_value(index));

                auto const offset = static_cast<size_type>(position - begin());
                erase_at_offset(alloc, offset);

                bitmap_vals_ = (bitmap_vals_ & ~(bitmap_type{1} << index));
                bitmap_deleted_vals_ = (bitmap_deleted_vals_ | (bitmap_type{1} << index));

                --nb_elements_;

                DICE_SPARSE_MAP_ASSERT(!has_value(index));
                DICE_SPARSE_MAP_ASSERT(has_deleted_value(index));

                return values() + offset;
            }

            void swap(sparse_array &other) noexcept {
                using std::swap;

                swap(values_, other.values_);
                swap(bitmap_vals_, other.bitmap_vals_);
                swap(bitmap_deleted_vals_, other.bitmap_deleted_vals_);
                swap(nb_elements_, other.nb_elements_);
                swap(capacity_, other.capacity_);
                swap(last_array_, other.last_array_);
            }

        private:
            [[nodiscard]] value_type *values() noexcept {
                return std::to_address(values_);
            }

            [[nodiscard]] value_type const *values() const noexcept {
                return std::to_address(values_);
            }

            template<typename... Args>
            static void construct_value(allocator_type &alloc, value_type *value, Args &&...value_args) {
                allocator_traits::construct(alloc, value, std::forward<Args>(value_args)...);
            }

            static void destroy_value(allocator_type &alloc, value_type *value) noexcept {
                allocator_traits::destroy(alloc, value);
            }

            static void destroy_and_deallocate_values(allocator_type &alloc, pointer values, size_type nb_values, size_type capacity_values) noexcept {
                if (capacity_values == 0) {
                    // nothing to deallocate, alloc.deallocate(nullptr, 0) is invalid
                    DICE_SPARSE_MAP_ASSERT(nb_values == 0);
                    return;
                }

                value_type *const raw_values = std::to_address(values);
                for (size_type i = 0; i < nb_values; ++i) {
                    destroy_value(alloc, raw_values + i);
                }
                allocator_traits::deallocate(alloc, values, capacity_values);
            }

            [[nodiscard]] static size_type popcount(bitmap_type val) noexcept {
                return static_cast<size_type>(std::popcount(val));
            }

            [[nodiscard]] size_type index_to_offset(size_type index) const noexcept {
                DICE_SPARSE_MAP_ASSERT(index < bitmap_nb_bits);
                return popcount(bitmap_vals_ & ((bitmap_type{1} << index) - bitmap_type{1}));
            }

            /**
             * @return the index of the occupied bucket whose value is at `offset`
             */
            [[nodiscard]] size_type offset_to_index(size_type offset) const noexcept {
                DICE_SPARSE_MAP_ASSERT(offset < nb_elements_);

                bitmap_type bitmap = bitmap_vals_;
                for (size_type i = 0; i < offset; ++i) {
                    bitmap &= bitmap - 1;  // clears the lowest set bit
                }

                return static_cast<size_type>(std::countr_zero(bitmap));
            }

            [[nodiscard]] size_type next_capacity() const noexcept {
                return static_cast<size_type>(capacity_ + capacity_growth_step);
            }

            /**
             * Insertion
             *
             * If `value_type` is nothrow move constructible, `values_` grows by `capacity_growth_step` when it is
             * full, and otherwise the new value is moved into its place. Moving values is safe, so this keeps the
             * strong exception guarantee.
             *
             * Otherwise every insertion allocates new storage for `nb_elements_ + 1` values, copies the values
             * into it and constructs the new value there. On success, `values_` becomes the new storage. This is
             * slower, but it is the only way to keep the strong exception guarantee.
             */
            template<typename... Args>
            void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
                if constexpr (std::is_nothrow_move_constructible_v<value_type>) {
                    if (nb_elements_ < capacity_) {
                        insert_at_offset_no_realloc(alloc, offset, std::forward<Args>(value_args)...);
                    } else {
                        insert_at_offset_realloc(alloc, offset, next_capacity(), std::forward<Args>(value_args)...);
                    }
                } else {
                    insert_at_offset_realloc(alloc, offset, static_cast<size_type>(nb_elements_ + 1), std::forward<Args>(value_args)...);
                }
            }

            template<typename... Args>
            void insert_at_offset_no_realloc(allocator_type &alloc, size_type offset, Args &&...value_args) {
                static_assert(std::is_nothrow_move_constructible_v<value_type>);
                DICE_SPARSE_MAP_ASSERT(offset <= nb_elements_);
                DICE_SPARSE_MAP_ASSERT(nb_elements_ < capacity_);

                value_type *const raw_values = values();
                if (offset == nb_elements_) {
                    construct_value(alloc, raw_values + offset, std::forward<Args>(value_args)...);
                    return;
                }

                // The arguments may refer to a value of this group that the shift below moves, so the new value is
                // constructed before the shift.
                value_holder<value_type, allocator_type> new_value(alloc, std::forward<Args>(value_args)...);

                for (size_type i = nb_elements_; i > offset; --i) {
                    construct_value(alloc, raw_values + i, std::move(raw_values[i - 1]));
                    destroy_value(alloc, raw_values + i - 1);
                }

                try {
                    construct_value(alloc, raw_values + offset, std::move(new_value.get()));
                } catch (...) {
                    for (size_type i = offset; i < nb_elements_; ++i) {
                        construct_value(alloc, raw_values + i, std::move(raw_values[i + 1]));
                        destroy_value(alloc, raw_values + i + 1);
                    }
                    throw;
                }
            }

            template<typename... Args>
            void insert_at_offset_realloc(allocator_type &alloc, size_type offset, size_type new_capacity, Args &&...value_args) {
                DICE_SPARSE_MAP_ASSERT(new_capacity > nb_elements_);

                pointer const new_values = detail_sparse_hash::allocate<AllocationFailure>(alloc, new_capacity);
                DICE_SPARSE_MAP_ASSERT(new_values != nullptr);
                value_type *const raw_new_values = std::to_address(new_values);
                value_type *const raw_values = values();

                if constexpr (std::is_nothrow_move_constructible_v<value_type>) {
                    try {
                        construct_value(alloc, raw_new_values + offset, std::forward<Args>(value_args)...);
                    } catch (...) {
                        allocator_traits::deallocate(alloc, new_values, new_capacity);
                        throw;
                    }

                    // does not throw from here on
                    for (size_type i = 0; i < offset; ++i) {
                        construct_value(alloc, raw_new_values + i, std::move(raw_values[i]));
                    }

                    for (size_type i = offset; i < nb_elements_; ++i) {
                        construct_value(alloc, raw_new_values + i + 1, std::move(raw_values[i]));
                    }
                } else {
                    size_type nb_new_values = 0;
                    try {
                        for (size_type i = 0; i < offset; ++i) {
                            construct_value(alloc, raw_new_values + i, raw_values[i]);
                            ++nb_new_values;
                        }

                        construct_value(alloc, raw_new_values + offset, std::forward<Args>(value_args)...);
                        ++nb_new_values;

                        for (size_type i = offset; i < nb_elements_; ++i) {
                            construct_value(alloc, raw_new_values + i + 1, raw_values[i]);
                            ++nb_new_values;
                        }
                    } catch (...) {
                        destroy_and_deallocate_values(alloc, new_values, nb_new_values, new_capacity);
                        throw;
                    }

                    DICE_SPARSE_MAP_ASSERT(nb_new_values == nb_elements_ + 1);
                }

                destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);

                values_ = new_values;
                capacity_ = new_capacity;
            }

            /**
             * Erasure
             *
             * If `value_type` is nothrow move constructible, the value is destroyed and the values after it are
             * moved one place to the front.
             *
             * Otherwise all values except the erased one are copied into new storage. On success, `values_`
             * becomes the new storage. This is slower, but it is the only way to keep the strong exception
             * guarantee.
             */
            void erase_at_offset(allocator_type &alloc, size_type offset) noexcept(std::is_nothrow_move_constructible_v<value_type>) {
                DICE_SPARSE_MAP_ASSERT(offset < nb_elements_);
                value_type *const raw_values = values();

                if constexpr (std::is_nothrow_move_constructible_v<value_type>) {
                    destroy_value(alloc, raw_values + offset);

                    for (size_type i = offset + 1; i < nb_elements_; ++i) {
                        construct_value(alloc, raw_values + i - 1, std::move(raw_values[i]));
                        destroy_value(alloc, raw_values + i);
                    }
                } else {
                    // the last value is erased without a reallocation, the capacity stays
                    if (offset + 1 == nb_elements_) {
                        destroy_value(alloc, raw_values + offset);
                        return;
                    }

                    DICE_SPARSE_MAP_ASSERT(nb_elements_ > 1);
                    auto const new_capacity = static_cast<size_type>(nb_elements_ - 1);

                    pointer const new_values = detail_sparse_hash::allocate<AllocationFailure>(alloc, new_capacity);
                    DICE_SPARSE_MAP_ASSERT(new_values != nullptr);
                    value_type *const raw_new_values = std::to_address(new_values);

                    size_type nb_new_values = 0;
                    try {
                        for (size_type i = 0; i < nb_elements_; ++i) {
                            if (i != offset) {
                                construct_value(alloc, raw_new_values + nb_new_values, raw_values[i]);
                                ++nb_new_values;
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
            }

            pointer values_ = nullptr;

            bitmap_type bitmap_vals_ = 0;
            bitmap_type bitmap_deleted_vals_ = 0;

            size_type nb_elements_ = 0;
            size_type capacity_ = 0;
            bool last_array_ = false;
        };

        /**
         * The hash table behind `sparse_map` and `sparse_set`.
         *
         * `Access` is `map_access<Key, T>` or `set_access<Key>`. It defines the public element type
         * (`value_type`), the stored element type (`slot_type`) and the reference types of the iterators.
         *
         * A rehash that throws leaves the table unchanged or empty, depending on the type of the elements
         * and on what throws. See `rehash_impl`.
         *
         * The stored elements must be nothrow move constructible and/or copy constructible. The behaviour is
         * undefined if their destructor throws.
         *
         * The buckets are kept in two dimensions. `buckets_` points to an array of `nb_sparse_buckets_`
         * `sparse_array`s, and each `sparse_array` holds `sparse_array::bitmap_nb_bits` buckets. Bucket `ibucket`
         * is at `buckets_[sparse_array::sparse_ibucket(ibucket)]`, position
         * `sparse_array::index_in_sparse_bucket(ibucket)`.
         *
         * The bucket count is 0 or a power of two, and it doubles when the table grows. A hash maps to a bucket
         * with a mask, so `Hash` must be avalanching (see `sh::hash_is_avalanching`). Collisions are resolved with
         * quadratic probing.
         *
         * `AllocationFailure` decides what a failed allocation does, see `allocate`.
         *
         * The hasher, the key equality and the allocator are members, not base classes, and `sparse_hash` has
         * no base class. So `sparse_hash`, and with it `sparse_map` and `sparse_set`, is a standard layout type
         * whenever `Hash`, `KeyEqual`, the allocator and the allocator's pointer type are standard layout.
         * Empty members take no space.
         */
        template<typename Access, typename Hash, typename KeyEqual, typename Allocator, sh::sparsity sparsity, sh::allocation_failure AllocationFailure>
        struct sparse_hash {
        private:
            template<typename, typename, typename, typename, sh::sparsity, sh::allocation_failure>
            friend struct sparse_hash;

        public:
            template<bool is_const>
            struct sparse_iterator;

            using key_type = typename Access::key_type;
            using value_type = typename Access::value_type;
            using hasher = Hash;
            using key_equal = KeyEqual;
            using allocator_type = Allocator;
            using reference = typename Access::reference;
            using const_reference = typename Access::const_reference;
            using size_type = typename std::allocator_traits<allocator_type>::size_type;
            using difference_type = typename std::allocator_traits<allocator_type>::difference_type;
            using pointer = typename std::allocator_traits<allocator_type>::pointer;
            using const_pointer = typename std::allocator_traits<allocator_type>::const_pointer;
            using iterator = sparse_iterator<false>;
            using const_iterator = sparse_iterator<true>;

        private:
            using slot_type = typename Access::slot_type;
            using slot_allocator_type = typename std::allocator_traits<allocator_type>::template rebind_alloc<slot_type>;
            using slot_allocator_traits = std::allocator_traits<slot_allocator_type>;
            using sparse_array = detail_sparse_hash::sparse_array<slot_type, slot_allocator_type, sparsity, AllocationFailure>;
            using array_size_type = typename sparse_array::size_type;
            using bucket_allocator_type = typename std::allocator_traits<allocator_type>::template rebind_alloc<sparse_array>;
            using bucket_allocator_traits = std::allocator_traits<bucket_allocator_type>;
            using bucket_pointer = typename bucket_allocator_traits::pointer;

            static constexpr bool propagate_on_copy_assignment = slot_allocator_traits::propagate_on_container_copy_assignment::value;
            static constexpr bool propagate_on_move_assignment = slot_allocator_traits::propagate_on_container_move_assignment::value;
            static constexpr bool propagate_on_swap = slot_allocator_traits::propagate_on_container_swap::value;
            static constexpr bool allocator_is_always_equal = slot_allocator_traits::is_always_equal::value;

        public:
            /**
             * Forward iterator over the elements.
             *
             * It dereferences to `value_type const &`: a map iterator to `std::pair<Key, T> const &`, a set iterator
             * to `Key const &`. `key()` returns the key, and for a map `value()` returns the mapped value, which is
             * mutable through `iterator`.
             *
             * The iterator holds plain pointers, also with an allocator that uses fancy pointers: the group, the
             * element and the end of the elements of the group. An insertion or an erasure can invalidate it.
             */
            template<bool is_const>
            struct sparse_iterator {
            private:
                friend struct sparse_hash;

                template<typename, typename, typename, typename, sh::sparsity, sh::allocation_failure>
                friend struct sparse_hash;

                friend struct sparse_iterator<!is_const>;

                using bucket_type = std::conditional_t<is_const, sparse_array const, sparse_array>;
                using slot_pointer = std::conditional_t<is_const, slot_type const *, slot_type *>;

                /**
                 * `slot_` is nullptr if `bucket_` is the end of the bucket array.
                 */
                sparse_iterator(bucket_type *bucket, slot_pointer slot) noexcept
                    : bucket_(bucket),
                      slot_(slot),
                      slot_end_(slot == nullptr ? nullptr : bucket->end()) {
                }

                sparse_iterator(bucket_type *bucket, slot_pointer slot, slot_pointer slot_end) noexcept
                    : bucket_(bucket),
                      slot_(slot),
                      slot_end_(slot_end) {
                }

            public:
                using iterator_concept = std::forward_iterator_tag;
                using iterator_category = std::forward_iterator_tag;
                using value_type = typename sparse_hash::value_type;
                using difference_type = std::ptrdiff_t;
                using reference = value_type const &;
                using pointer = value_type const *;

                sparse_iterator() noexcept = default;

                /**
                 * Converts an `iterator` into a `const_iterator`.
                 */
                template<bool other_is_const>
                requires (is_const && !other_is_const)
                sparse_iterator(sparse_iterator<other_is_const> const &other) noexcept  // NOLINT(google-explicit-constructor)
                    : bucket_(other.bucket_),
                      slot_(other.slot_),
                      slot_end_(other.slot_end_) {
                }

                [[nodiscard]] reference operator*() const noexcept {
                    return *slot_;
                }

                [[nodiscard]] pointer operator->() const noexcept {
                    return slot_;
                }

                /**
                 * @return the key of the element
                 */
                [[nodiscard]] key_type const &key() const noexcept {
                    return Access::key(*slot_);
                }

                /**
                 * @return the mapped value of the element of a map, mutable through `iterator`
                 */
                [[nodiscard]] auto &value() const noexcept
                    requires Access::is_map
                {
                    return Access::mapped(*slot_);
                }

                sparse_iterator &operator++() noexcept {
                    DICE_SPARSE_MAP_ASSERT(slot_ != nullptr && slot_end_ == bucket_->end());
                    ++slot_;

                    if (slot_ == slot_end_) [[unlikely]] {
                        to_next_group();
                    }

                    return *this;
                }

                sparse_iterator operator++(int) noexcept {
                    sparse_iterator tmp(*this);
                    ++*this;
                    return tmp;
                }

                /**
                 * Two iterators of the same table are equal if they point to the same element. All end iterators
                 * have `slot_ == nullptr`.
                 */
                friend bool operator==(sparse_iterator const &lhs, sparse_iterator const &rhs) noexcept {
                    return lhs.slot_ == rhs.slot_;
                }

            private:
                /**
                 * Moves to the first element of the next group that is not empty, or to the end.
                 */
                void to_next_group() noexcept {
                    do {
                        if (bucket_->last()) {
                            ++bucket_;
                            slot_ = nullptr;
                            slot_end_ = nullptr;
                            return;
                        }

                        ++bucket_;
                    } while (bucket_->empty());

                    slot_ = bucket_->begin();
                    slot_end_ = bucket_->end();
                }

                bucket_type *bucket_ = nullptr;
                slot_pointer slot_ = nullptr;
                /// the end of the elements of `*bucket_`, nullptr if `slot_` is nullptr
                slot_pointer slot_end_ = nullptr;
            };

            /**
             * Creates a table with at least `bucket_count` buckets. The bucket count is rounded up to a power of two.
             * @throws std::length_error if `bucket_count` is larger than `max_bucket_count()`
             */
            sparse_hash(size_type bucket_count, Hash const &hash, KeyEqual const &equal, slot_allocator_type const &alloc, float max_load_factor)
                : alloc_(alloc),
                  hash_(hash),
                  key_equal_(equal) {
                bucket_count_ = rounded_bucket_count(bucket_count);
                if (bucket_count_ > 0) {
                    allocate_buckets(sparse_array::nb_sparse_buckets(bucket_count_));
                }

                this->max_load_factor(max_load_factor);

                // checked here instead of at class scope, so that value_type may be incomplete there
                static_assert(std::is_nothrow_move_constructible_v<slot_type> || std::is_copy_constructible_v<slot_type>,
                              "Key, and T if present, must be nothrow move constructible and/or copy constructible.");
            }

            ~sparse_hash() {
                destroy_buckets();
            }

            sparse_hash(sparse_hash const &other)
                : sparse_hash(other, slot_allocator_traits::select_on_container_copy_construction(other.alloc_)) {
            }

            /**
             * Copies `other`, with storage from `alloc`.
             */
            sparse_hash(sparse_hash const &other, slot_allocator_type const &alloc)
                : alloc_(alloc),
                  hash_(other.hash_),
                  key_equal_(other.key_equal_),
                  bucket_count_(other.bucket_count_),
                  nb_elements_(other.nb_elements_),
                  nb_deleted_buckets_(other.nb_deleted_buckets_),
                  load_threshold_rehash_(other.load_threshold_rehash_),
                  load_threshold_clear_deleted_(other.load_threshold_clear_deleted_),
                  max_load_factor_(other.max_load_factor_) {
                copy_buckets_from(other);
            }

            sparse_hash(sparse_hash &&other) noexcept(std::is_nothrow_move_constructible_v<slot_allocator_type>
                                                      && std::is_nothrow_move_constructible_v<Hash>
                                                      && std::is_nothrow_move_constructible_v<KeyEqual>)
                : alloc_(std::move(other.alloc_)),
                  hash_(std::move(other.hash_)),
                  key_equal_(std::move(other.key_equal_)),
                  buckets_(std::exchange(other.buckets_, nullptr)),
                  nb_sparse_buckets_(std::exchange(other.nb_sparse_buckets_, 0)),
                  bucket_count_(std::exchange(other.bucket_count_, 0)),
                  nb_elements_(std::exchange(other.nb_elements_, 0)),
                  nb_deleted_buckets_(std::exchange(other.nb_deleted_buckets_, 0)),
                  load_threshold_rehash_(std::exchange(other.load_threshold_rehash_, 0)),
                  load_threshold_clear_deleted_(std::exchange(other.load_threshold_clear_deleted_, 0)),
                  max_load_factor_(other.max_load_factor_) {
            }

            sparse_hash &operator=(sparse_hash const &other) {
                if (this == &other) {
                    return *this;
                }

                destroy_buckets();
                reset_to_empty();

                if constexpr (propagate_on_copy_assignment) {
                    alloc_ = other.alloc_;
                }

                hash_ = other.hash_;
                key_equal_ = other.key_equal_;

                // on an exception *this stays empty
                copy_buckets_from(other);

                bucket_count_ = other.bucket_count_;
                nb_elements_ = other.nb_elements_;
                nb_deleted_buckets_ = other.nb_deleted_buckets_;
                load_threshold_rehash_ = other.load_threshold_rehash_;
                load_threshold_clear_deleted_ = other.load_threshold_clear_deleted_;
                max_load_factor_ = other.max_load_factor_;

                return *this;
            }

            sparse_hash &operator=(sparse_hash &&other) noexcept((propagate_on_move_assignment || allocator_is_always_equal)
                                                                 && std::is_nothrow_move_assignable_v<Hash>
                                                                 && std::is_nothrow_move_assignable_v<KeyEqual>) {
                if (this == &other) {
                    return *this;
                }

                destroy_buckets();
                reset_to_empty();

                // the functors first: if one of them throws, *this is empty and `other` keeps its elements
                hash_ = std::move(other.hash_);
                key_equal_ = std::move(other.key_equal_);

                if constexpr (propagate_on_move_assignment) {
                    alloc_ = std::move(other.alloc_);
                }

                if (propagate_on_move_assignment || allocator_is_always_equal || alloc_ == other.alloc_) {
                    buckets_ = std::exchange(other.buckets_, nullptr);
                    nb_sparse_buckets_ = std::exchange(other.nb_sparse_buckets_, 0);
                } else {
                    // on an exception *this stays empty
                    move_buckets_from(other);
                    other.destroy_buckets();
                }

                bucket_count_ = other.bucket_count_;
                nb_elements_ = other.nb_elements_;
                nb_deleted_buckets_ = other.nb_deleted_buckets_;
                load_threshold_rehash_ = other.load_threshold_rehash_;
                load_threshold_clear_deleted_ = other.load_threshold_clear_deleted_;
                max_load_factor_ = other.max_load_factor_;

                other.reset_to_empty();

                return *this;
            }

            [[nodiscard]] allocator_type get_allocator() const noexcept {
                return allocator_type(alloc_);
            }

            /*
             * Iterators
             */
            [[nodiscard]] iterator begin() noexcept {
                sparse_array *bucket = buckets_begin();
                sparse_array *const last = buckets_end();
                while (bucket != last && bucket->empty()) {
                    ++bucket;
                }

                return iterator(bucket, bucket != last ? bucket->begin() : nullptr);
            }

            [[nodiscard]] const_iterator begin() const noexcept {
                return cbegin();
            }

            [[nodiscard]] const_iterator cbegin() const noexcept {
                sparse_array const *bucket = buckets_begin();
                sparse_array const *const last = buckets_end();
                while (bucket != last && bucket->empty()) {
                    ++bucket;
                }

                return const_iterator(bucket, bucket != last ? bucket->cbegin() : nullptr);
            }

            [[nodiscard]] iterator end() noexcept {
                return iterator(buckets_end(), nullptr);
            }

            [[nodiscard]] const_iterator end() const noexcept {
                return cend();
            }

            [[nodiscard]] const_iterator cend() const noexcept {
                return const_iterator(buckets_end(), nullptr);
            }

            /*
             * Capacity
             */
            [[nodiscard]] bool empty() const noexcept {
                return nb_elements_ == 0;
            }

            [[nodiscard]] size_type size() const noexcept {
                return nb_elements_;
            }

            [[nodiscard]] size_type max_size() const noexcept {
                return std::min<size_type>(slot_allocator_traits::max_size(alloc_), max_bucket_count());
            }

            /*
             * Modifiers
             */
            void clear() noexcept {
                for (sparse_array &bucket : buckets()) {
                    bucket.clear(alloc_);
                }

                nb_elements_ = 0;
                nb_deleted_buckets_ = 0;
            }

            std::pair<iterator, bool> insert(value_type const &value) {
                return insert_impl(Access::key_of_value(value), value);
            }

            std::pair<iterator, bool> insert(value_type &&value) {
                return insert_impl(Access::key_of_value(value), std::move(value));
            }

            template<typename V>
            iterator insert_hint(const_iterator hint, V &&value) {
                if (hint != cend() && compare_keys(key_of(hint), Access::key_of_value(value))) {
                    return mutable_iterator(hint);
                }

                return insert(std::forward<V>(value)).first;
            }

            template<LegacyInputIterator InputIt>
            void insert(InputIt first, InputIt last) {
                if constexpr (std::forward_iterator<InputIt>) {
                    reserve_for_insertion(static_cast<std::size_t>(std::distance(first, last)));
                }

                for (; first != last; ++first) {
                    emplace_value(*first);
                }
            }

            template<typename K, typename M>
            std::pair<iterator, bool> insert_or_assign(K &&key, M &&obj) {
                auto it = try_emplace(std::forward<K>(key), std::forward<M>(obj));
                if (!it.second) {
                    Access::mapped(*it.first.slot_) = std::forward<M>(obj);
                }

                return it;
            }

            template<typename K, typename M>
            iterator insert_or_assign_hint(const_iterator hint, K &&key, M &&obj) {
                if (hint != cend() && compare_keys(key_of(hint), key)) {
                    auto it = mutable_iterator(hint);
                    Access::mapped(*it.slot_) = std::forward<M>(obj);

                    return it;
                }

                return insert_or_assign(std::forward<K>(key), std::forward<M>(obj)).first;
            }

            /**
             * Constructs a `value_type` from `args` and inserts it if its key is not in the table yet.
             */
            template<typename... Args>
            std::pair<iterator, bool> emplace(Args &&...args) {
                return insert(value_type(std::forward<Args>(args)...));
            }

            template<typename... Args>
            iterator emplace_hint(const_iterator hint, Args &&...args) {
                return insert_hint(hint, value_type(std::forward<Args>(args)...));
            }

            /**
             * Inserts an element with key `key` and mapped value constructed from `args`, if `key` is not in the
             * map. Otherwise neither `key` nor `args` are used.
             */
            template<typename K, typename... Args>
            std::pair<iterator, bool> try_emplace(K &&key, Args &&...args) {
                return insert_impl(key, std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<Args>(args)...));
            }

            template<typename K, typename... Args>
            iterator try_emplace_hint(const_iterator hint, K &&key, Args &&...args) {
                if (hint != cend() && compare_keys(key_of(hint), key)) {
                    return mutable_iterator(hint);
                }

                return try_emplace(std::forward<K>(key), std::forward<Args>(args)...).first;
            }

            /**
             * Erases the element at `pos`.
             * @return iterator to the element after the erased one
             */
            iterator erase(iterator pos) {
                DICE_SPARSE_MAP_ASSERT(pos != end() && nb_elements_ > 0);
                sparse_array *bucket = pos.bucket_;
                slot_type *const next_slot = bucket->erase(alloc_, pos.slot_);
                --nb_elements_;
                ++nb_deleted_buckets_;

                if (next_slot != bucket->end()) {
                    return iterator(bucket, next_slot);
                }

                sparse_array *const last = buckets_end();
                do {
                    ++bucket;
                } while (bucket != last && bucket->empty());

                return bucket == last ? end() : iterator(bucket, bucket->begin());
            }

            iterator erase(const_iterator pos) {
                return erase(mutable_iterator(pos));
            }

            iterator erase(const_iterator first, const_iterator last) {
                if (first == last) {
                    return mutable_iterator(first);
                }

                auto const nb_elements_to_erase = static_cast<size_type>(std::distance(first, last));
                auto to_delete = mutable_iterator(first);
                for (size_type i = 0; i < nb_elements_to_erase; ++i) {
                    to_delete = erase(to_delete);
                }

                return to_delete;
            }

            template<typename K>
            size_type erase(K const &key) {
                return erase_impl(key, hash_key(key));
            }

            /**
             * @param hash `hash_function()(key)`
             */
            template<typename K>
            size_type erase(K const &key, std::size_t hash) {
                return erase_impl(key, hash);
            }

            void swap(sparse_hash &other) noexcept(std::is_nothrow_swappable_v<Hash>
                                                   && std::is_nothrow_swappable_v<KeyEqual>) {
                using std::swap;

                // the functors first: if one of them throws, the storage and the allocators are not swapped
                swap(hash_, other.hash_);
                swap(key_equal_, other.key_equal_);

                if constexpr (propagate_on_swap) {
                    swap(alloc_, other.alloc_);
                } else {
                    DICE_SPARSE_MAP_ASSERT(alloc_ == other.alloc_);
                }

                swap(buckets_, other.buckets_);
                swap(nb_sparse_buckets_, other.nb_sparse_buckets_);
                swap(bucket_count_, other.bucket_count_);
                swap(nb_elements_, other.nb_elements_);
                swap(nb_deleted_buckets_, other.nb_deleted_buckets_);
                swap(load_threshold_rehash_, other.load_threshold_rehash_);
                swap(load_threshold_clear_deleted_, other.load_threshold_clear_deleted_);
                swap(max_load_factor_, other.max_load_factor_);
            }

            /*
             * Lookup
             *
             * The overloads with a `hash` parameter take `hash_function()(key)`, so that the caller can hash a key
             * once for several lookups.
             */
            template<typename Self, typename K>
            requires Access::is_map
            auto &at(this Self &self, K const &key) {
                return self.at_hashed(key, self.hash_key(key));
            }

            template<typename Self, typename K>
            requires Access::is_map
            auto &at(this Self &self, K const &key, std::size_t hash) {
                return self.at_hashed(key, hash);
            }

            template<typename K>
            requires Access::is_map
            auto &operator[](K &&key) {
                return Access::mapped(*try_emplace(std::forward<K>(key)).first.slot_);
            }

            template<typename K>
            [[nodiscard]] bool contains(K const &key) const {
                return find_impl(key, hash_key(key)) != cend();
            }

            template<typename K>
            [[nodiscard]] bool contains(K const &key, std::size_t hash) const {
                return find_impl(key, hash) != cend();
            }

            template<typename K>
            [[nodiscard]] size_type count(K const &key) const {
                return contains(key) ? size_type{1} : size_type{0};
            }

            template<typename K>
            [[nodiscard]] size_type count(K const &key, std::size_t hash) const {
                return contains(key, hash) ? size_type{1} : size_type{0};
            }

            /**
             * @return `iterator` for a non-const table, `const_iterator` for a const table
             */
            template<typename Self, typename K>
            [[nodiscard]] auto find(this Self &self, K const &key) {
                return self.find_hashed(key, self.hash_key(key));
            }

            template<typename Self, typename K>
            [[nodiscard]] auto find(this Self &self, K const &key, std::size_t hash) {
                return self.find_hashed(key, hash);
            }

            template<typename Self, typename K>
            [[nodiscard]] auto equal_range(this Self &self, K const &key) {
                return self.equal_range_hashed(key, self.hash_key(key));
            }

            template<typename Self, typename K>
            [[nodiscard]] auto equal_range(this Self &self, K const &key, std::size_t hash) {
                return self.equal_range_hashed(key, hash);
            }

            /*
             * Bucket interface
             */
            [[nodiscard]] size_type bucket_count() const noexcept {
                return bucket_count_;
            }

            /**
             * @return the largest power of two of `std::size_t`, or less if the allocator cannot provide as many
             * `sparse_array`s
             */
            [[nodiscard]] size_type max_bucket_count() const noexcept {
                constexpr auto largest_power_of_two = static_cast<size_type>((std::numeric_limits<std::size_t>::max() / 2) + 1);
                return std::min<size_type>(largest_power_of_two, bucket_allocator_traits::max_size(bucket_allocator_type(alloc_)));
            }

            /*
             * Hash policy
             */
            [[nodiscard]] float load_factor() const noexcept {
                if (bucket_count() == 0) {
                    return 0;
                }

                return static_cast<float>(nb_elements_) / static_cast<float>(bucket_count());
            }

            [[nodiscard]] float max_load_factor() const noexcept {
                return max_load_factor_;
            }

            /**
             * Sets the maximum load factor, clamped to [0.1, 0.8].
             */
            void max_load_factor(float ml) noexcept {
                max_load_factor_ = std::max(min_max_load_factor, std::min(ml, max_max_load_factor));
                load_threshold_rehash_ = static_cast<size_type>(static_cast<float>(bucket_count()) * max_load_factor_);

                float const max_load_factor_with_deleted_buckets = max_load_factor_ + 0.5f * (1.0f - max_load_factor_);
                DICE_SPARSE_MAP_ASSERT(max_load_factor_with_deleted_buckets > 0.0f && max_load_factor_with_deleted_buckets <= 1.0f);
                load_threshold_clear_deleted_ = static_cast<size_type>(static_cast<float>(bucket_count()) * max_load_factor_with_deleted_buckets);
            }

            void rehash(size_type count) {
                count = std::max(count, static_cast<size_type>(ceil_to_size(static_cast<float>(size()) / max_load_factor())));
                rehash_impl(count);
            }

            void reserve(size_type count) {
                rehash(static_cast<size_type>(ceil_to_size(static_cast<float>(count) / max_load_factor())));
            }

            /*
             * Observers
             */
            [[nodiscard]] hasher hash_function() const {
                return hash_;
            }

            [[nodiscard]] key_equal key_eq() const {
                return key_equal_;
            }

            /*
             * Other
             */
            [[nodiscard]] iterator mutable_iterator(const_iterator pos) noexcept {
                return iterator(const_cast<sparse_array *>(pos.bucket_), const_cast<slot_type *>(pos.slot_), const_cast<slot_type *>(pos.slot_end_));
            }

        private:
            template<typename Self, typename K>
            [[nodiscard]] auto find_hashed(this Self &self, K const &key, std::size_t hash) {
                if constexpr (std::is_const_v<Self>) {
                    return self.find_impl(key, hash);
                } else {
                    return self.mutable_iterator(self.find_impl(key, hash));
                }
            }

            template<typename Self, typename K>
            [[nodiscard]] auto &at_hashed(this Self &self, K const &key, std::size_t hash) {
                auto const it = self.find_hashed(key, hash);
                if (it == self.end()) {
                    throw std::out_of_range("Couldn't find key.");
                }

                return Access::mapped(*it.slot_);
            }

            template<typename Self, typename K>
            [[nodiscard]] auto equal_range_hashed(this Self &self, K const &key, std::size_t hash) {
                auto const it = self.find_hashed(key, hash);
                return std::pair{it, (it == self.end()) ? it : std::next(it)};
            }

            [[nodiscard]] sparse_array *buckets_begin() noexcept {
                return std::to_address(buckets_);
            }

            [[nodiscard]] sparse_array const *buckets_begin() const noexcept {
                return std::to_address(buckets_);
            }

            [[nodiscard]] sparse_array *buckets_end() noexcept {
                return buckets_begin() + nb_sparse_buckets_;
            }

            [[nodiscard]] sparse_array const *buckets_end() const noexcept {
                return buckets_begin() + nb_sparse_buckets_;
            }

            [[nodiscard]] std::ranges::subrange<sparse_array *> buckets() noexcept {
                return {buckets_begin(), buckets_end()};
            }

            [[nodiscard]] std::ranges::subrange<sparse_array const *> buckets() const noexcept {
                return {buckets_begin(), buckets_end()};
            }

            template<typename K1, typename K2>
            [[nodiscard]] bool compare_keys(K1 const &key1, K2 const &key2) const {
                return key_equal_(key1, key2);
            }

            [[nodiscard]] static key_type const &key_of(const_iterator it) noexcept {
                return Access::key(*it.slot_);
            }

            /**
             * @return `hash_function()(key)`
             */
            template<typename K>
            [[nodiscard]] std::size_t hash_key(K const &key) const {
                return hash_(key);
            }

            /**
             * @param hash `hash_function()(key)` of a key
             * @return the bucket of `hash`. The table must have buckets.
             */
            [[nodiscard]] std::size_t bucket_for_hash(std::size_t hash) const noexcept {
                DICE_SPARSE_MAP_ASSERT(bucket_count_ > 0);
                return hash & (bucket_count_ - 1);
            }

            /**
             * @return the bucket after `ibucket` on the quadratic probe sequence, at probe number `iprobe`
             */
            [[nodiscard]] std::size_t next_bucket(std::size_t ibucket, std::size_t iprobe) const noexcept {
                return (ibucket + iprobe) & (bucket_count_ - 1);
            }

            /**
             * @return `bucket_count` rounded up to a power of two, or 0 for 0
             * @throws std::length_error if the result is larger than `max_bucket_count()`
             */
            [[nodiscard]] size_type rounded_bucket_count(size_type bucket_count) const {
                if (bucket_count > max_bucket_count()) {
                    throw std::length_error("The hash table exceeds its maximum size.");
                }
                if (bucket_count == 0) {
                    return 0;
                }

                auto const rounded = static_cast<size_type>(std::bit_ceil(static_cast<std::size_t>(bucket_count)));
                if (rounded > max_bucket_count()) {
                    throw std::length_error("The hash table exceeds its maximum size.");
                }
                return rounded;
            }

            /**
             * @return the bucket count after the next growth, twice the current one, at least 2
             * @throws std::length_error if the table cannot grow
             */
            [[nodiscard]] size_type next_bucket_count() const {
                if (bucket_count_ > max_bucket_count() / 2) {
                    throw std::length_error("The hash table exceeds its maximum size.");
                }
                return bucket_count_ == 0 ? 2 : bucket_count_ * 2;
            }

            /**
             * Allocates `nb_sparse_buckets` empty `sparse_array`s and makes them the buckets of the table.
             */
            void allocate_buckets(std::size_t nb_sparse_buckets) {
                DICE_SPARSE_MAP_ASSERT(buckets_ == nullptr && nb_sparse_buckets > 0);
                bucket_allocator_type bucket_alloc(alloc_);
                bucket_pointer const new_buckets = detail_sparse_hash::allocate<AllocationFailure>(bucket_alloc, nb_sparse_buckets);
                sparse_array *const raw_buckets = std::to_address(new_buckets);
                for (std::size_t i = 0; i < nb_sparse_buckets; ++i) {
                    std::construct_at(raw_buckets + i);
                }
                raw_buckets[nb_sparse_buckets - 1].set_as_last();

                buckets_ = new_buckets;
                nb_sparse_buckets_ = nb_sparse_buckets;
            }

            /**
             * Destroys all elements and frees the bucket array. The counters stay as they are.
             */
            void destroy_buckets() noexcept {
                if (nb_sparse_buckets_ == 0) {
                    return;
                }

                for (sparse_array &bucket : buckets()) {
                    bucket.clear(alloc_);
                    std::destroy_at(std::addressof(bucket));
                }

                bucket_allocator_type bucket_alloc(alloc_);
                bucket_allocator_traits::deallocate(bucket_alloc, buckets_, nb_sparse_buckets_);
                buckets_ = nullptr;
                nb_sparse_buckets_ = 0;
            }

            /**
             * Sets the table to the state of a table with a bucket count of 0. Expects that it has no buckets.
             */
            void reset_to_empty() noexcept {
                DICE_SPARSE_MAP_ASSERT(buckets_ == nullptr && nb_sparse_buckets_ == 0);
                bucket_count_ = 0;
                nb_elements_ = 0;
                nb_deleted_buckets_ = 0;
                load_threshold_rehash_ = 0;
                load_threshold_clear_deleted_ = 0;
            }

            /**
             * Builds a bucket array for this table from the buckets of `other`, with `make(other_bucket)`
             * constructing each new bucket in place. On an exception the table has no buckets.
             */
            template<typename OtherHash, typename Make>
            void build_buckets_from(OtherHash &other, Make const &make) {
                DICE_SPARSE_MAP_ASSERT(buckets_ == nullptr && nb_sparse_buckets_ == 0);
                if (other.nb_sparse_buckets_ == 0) {
                    return;
                }

                bucket_allocator_type bucket_alloc(alloc_);
                bucket_pointer const new_buckets = detail_sparse_hash::allocate<AllocationFailure>(bucket_alloc, other.nb_sparse_buckets_);
                sparse_array *const raw_buckets = std::to_address(new_buckets);
                std::size_t nb_constructed = 0;
                try {
                    for (auto &other_bucket : other.buckets()) {
                        make(raw_buckets + nb_constructed, other_bucket);
                        ++nb_constructed;
                    }
                } catch (...) {
                    for (std::size_t i = 0; i < nb_constructed; ++i) {
                        raw_buckets[i].clear(alloc_);
                        std::destroy_at(raw_buckets + i);
                    }
                    bucket_allocator_traits::deallocate(bucket_alloc, new_buckets, other.nb_sparse_buckets_);
                    throw;
                }

                DICE_SPARSE_MAP_ASSERT(raw_buckets[nb_constructed - 1].last());
                buckets_ = new_buckets;
                nb_sparse_buckets_ = other.nb_sparse_buckets_;
            }

            void copy_buckets_from(sparse_hash const &other) {
                build_buckets_from(other, [this](sparse_array *target, sparse_array const &source) {
                    std::construct_at(target, source, alloc_);
                });
            }

            /**
             * Moves the elements of `other` into a new bucket array of this table, or copies the elements whose move
             * constructor can throw. `other` keeps its buckets and its elements, the moved ones in a moved-from
             * state.
             */
            void move_buckets_from(sparse_hash &other) {
                build_buckets_from(other, [this](sparse_array *target, sparse_array &source) {
                    std::construct_at(target, std::move(source), alloc_);
                });
            }

            /**
             * Reserves room for `nb_elements_to_insert` more elements if the table does not have it.
             */
            void reserve_for_insertion(std::size_t nb_elements_to_insert) {
                // a lower max load factor can put the threshold below the size
                size_type const nb_free_buckets = load_threshold_rehash_ > size() ? load_threshold_rehash_ - size() : 0;
                if (nb_elements_to_insert > 0 && nb_free_buckets < nb_elements_to_insert) {
                    reserve(size() + static_cast<size_type>(nb_elements_to_insert));
                }
            }

            /**
             * Inserts an element of a range: `value_type` directly, anything else through `emplace`.
             */
            template<typename V>
            void emplace_value(V &&value) {
                if constexpr (std::is_same_v<std::remove_cvref_t<V>, value_type>) {
                    insert(std::forward<V>(value));
                } else {
                    emplace(std::forward<V>(value));
                }
            }

            /**
             * Looks for `key`. If it is not there, constructs an element from `args` in the first empty or deleted
             * bucket on the probe sequence of `key`.
             *
             * A deleted bucket on the probe sequence is not enough to know that `key` is absent: the search goes
             * on until an empty bucket, and remembers the first deleted bucket for the insertion.
             */
            template<typename K, typename... Args>
            std::pair<iterator, bool> insert_impl(K const &key, Args &&...args) {
                return insert_impl_hashed(key, hash_key(key), std::forward<Args>(args)...);
            }

            /**
             * `insert_impl` with `hash_key(key)` already computed.
             */
            template<typename K, typename... Args>
            std::pair<iterator, bool> insert_impl_hashed(K const &key, std::size_t hash, Args &&...args) {
                if (nb_sparse_buckets_ == 0) {
                    return insert_new(hash, 0, 0, false, std::forward<Args>(args)...);
                }

                std::size_t ibucket = bucket_for_hash(hash);

                // the first deleted bucket on the probe sequence, `bucket_count_` if there is none
                std::size_t ibucket_first_deleted = bucket_count_;

                sparse_array *const raw_buckets = buckets_begin();
                for (std::size_t probe = 0;;) {
                    std::size_t const sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto const index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);
                    sparse_array &bucket = raw_buckets[sparse_ibucket];

                    if (bucket.has_value(index_in_sparse_bucket)) {
                        slot_type *const slot = bucket.value(index_in_sparse_bucket);
                        if (compare_keys(key, Access::key(*slot))) {
                            return {iterator(&bucket, slot), false};
                        }
                    } else if (bucket.has_deleted_value(index_in_sparse_bucket) && probe < bucket_count_) {
                        if (ibucket_first_deleted == bucket_count_) {
                            ibucket_first_deleted = ibucket;
                        }
                    } else if (ibucket_first_deleted != bucket_count_) {
                        return insert_new(hash, sparse_array::sparse_ibucket(ibucket_first_deleted), sparse_array::index_in_sparse_bucket(ibucket_first_deleted), true, std::forward<Args>(args)...);
                    } else {
                        return insert_new(hash, sparse_ibucket, index_in_sparse_bucket, false, std::forward<Args>(args)...);
                    }

                    ++probe;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

            /**
             * Inserts a new element at the given bucket, after checking the load thresholds. If a threshold is
             * reached, the table is rehashed and the insertion starts over. `hash` is `hash_key` of the key of the
             * new element.
             */
            template<typename... Args>
            std::pair<iterator, bool> insert_new(std::size_t hash, std::size_t sparse_ibucket, array_size_type index_in_sparse_bucket, bool reuses_deleted_bucket, Args &&...args) {
                bool const grows = size() >= load_threshold_rehash_;
                if (grows || size() + nb_deleted_buckets_ >= load_threshold_clear_deleted_) {
                    // `key` and the arguments may refer to elements that the rehash moves, so the new element is
                    // constructed before the rehash.
                    value_holder<slot_type, slot_allocator_type> new_slot(alloc_, std::forward<Args>(args)...);
                    if (grows) {
                        rehash_impl(next_bucket_count());
                    } else {
                        clear_deleted_buckets();
                    }
                    return insert_impl_hashed(Access::key(new_slot.get()), hash, std::move(new_slot.get()));
                }

                sparse_array &bucket = buckets_begin()[sparse_ibucket];
                slot_type *const slot = bucket.set(alloc_, index_in_sparse_bucket, std::forward<Args>(args)...);
                ++nb_elements_;
                if (reuses_deleted_bucket) {
                    --nb_deleted_buckets_;
                }

                return {iterator(&bucket, slot), true};
            }

            template<typename K>
            size_type erase_impl(K const &key, std::size_t hash) {
                if (nb_sparse_buckets_ == 0) {
                    return 0;
                }

                sparse_array *const raw_buckets = buckets_begin();
                std::size_t ibucket = bucket_for_hash(hash);
                for (std::size_t probe = 0;;) {
                    std::size_t const sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto const index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);
                    sparse_array &bucket = raw_buckets[sparse_ibucket];

                    if (bucket.has_value(index_in_sparse_bucket)) {
                        slot_type *const slot = bucket.value(index_in_sparse_bucket);
                        if (compare_keys(key, Access::key(*slot))) {
                            bucket.erase(alloc_, slot, index_in_sparse_bucket);
                            --nb_elements_;
                            ++nb_deleted_buckets_;

                            return 1;
                        }
                    } else if (!bucket.has_deleted_value(index_in_sparse_bucket) || probe >= bucket_count_) {
                        return 0;
                    }

                    ++probe;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

            template<typename K>
            [[nodiscard]] const_iterator find_impl(K const &key, std::size_t hash) const {
                if (nb_sparse_buckets_ == 0) {
                    return cend();
                }

                sparse_array const *const raw_buckets = buckets_begin();
                std::size_t ibucket = bucket_for_hash(hash);
                for (std::size_t probe = 0;;) {
                    std::size_t const sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto const index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);
                    sparse_array const &bucket = raw_buckets[sparse_ibucket];

                    if (bucket.has_value(index_in_sparse_bucket)) {
                        slot_type const *const slot = bucket.value(index_in_sparse_bucket);
                        if (compare_keys(key, Access::key(*slot))) {
                            return const_iterator(&bucket, slot);
                        }
                    } else if (!bucket.has_deleted_value(index_in_sparse_bucket) || probe >= bucket_count_) {
                        return cend();
                    }

                    ++probe;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

            /**
             * Removes the deleted-bucket markers with a rehash to the same bucket count.
             */
            void clear_deleted_buckets() {
                rehash_impl(bucket_count_);
                DICE_SPARSE_MAP_ASSERT(nb_deleted_buckets_ == 0);
            }

            /**
             * True if a rehash copies the elements and frees the old groups at the end, see `rehash_impl`.
             */
            static constexpr bool copy_on_rehash = !std::is_nothrow_move_constructible_v<slot_type>;

            /**
             * Moves or copies all elements into a new table with at least `count` buckets. How depends on the type of
             * the elements.
             *
             * An element whose move constructor cannot throw is moved, one old group after the other. Each old group
             * is freed right after its elements are moved. If the hash function throws while the elements are moved, or
             * the allocator with `sh::allocation_failure::throwing`, the table is cleared, so it is empty afterwards.
             * An exception of the allocator before, while the new table allocates its buckets, leaves the table
             * unchanged.
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
                    for (sparse_array const &bucket : buckets()) {
                        for (slot_type const &slot : bucket) {
                            new_table.insert_on_rehash(slot);
                        }
                    }
                } else {
                    try {
                        for (sparse_array &bucket : buckets()) {
                            for (slot_type &slot : bucket) {
                                new_table.insert_on_rehash(std::move(slot));
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
             * equality and maximum load factor. Unlike `swap`, it needs no swappable hash function or key equality.
             */
            void swap_storage(sparse_hash &other) noexcept {
                DICE_SPARSE_MAP_ASSERT(alloc_ == other.alloc_);
                using std::swap;
                swap(buckets_, other.buckets_);
                swap(nb_sparse_buckets_, other.nb_sparse_buckets_);
                swap(bucket_count_, other.bucket_count_);
                swap(nb_elements_, other.nb_elements_);
                swap(nb_deleted_buckets_, other.nb_deleted_buckets_);
                swap(load_threshold_rehash_, other.load_threshold_rehash_);
                swap(load_threshold_clear_deleted_, other.load_threshold_clear_deleted_);
            }

            /**
             * Inserts an element whose key is known not to be in the table. Does not check the load thresholds.
             */
            template<typename S>
            void insert_on_rehash(S &&slot_value) {
                std::size_t ibucket = bucket_for_hash(hash_key(Access::key(slot_value)));
                sparse_array *const raw_buckets = buckets_begin();

                for (std::size_t probe = 0;;) {
                    std::size_t const sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto const index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);
                    sparse_array &bucket = raw_buckets[sparse_ibucket];

                    if (!bucket.has_value(index_in_sparse_bucket)) {
                        bucket.set(alloc_, index_in_sparse_bucket, std::forward<S>(slot_value));
                        ++nb_elements_;

                        return;
                    }

                    DICE_SPARSE_MAP_ASSERT(!compare_keys(Access::key(slot_value), Access::key(*bucket.value(index_in_sparse_bucket))));

                    ++probe;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

        public:
            static constexpr size_type default_init_bucket_count = 0;
            static constexpr float default_max_load_factor = 0.5f;

            /**
             * `max_load_factor(float)` clamps its argument to [`min_max_load_factor`, `max_max_load_factor`].
             */
            static constexpr float min_max_load_factor = 0.1f;
            static constexpr float max_max_load_factor = 0.8f;

        private:
            [[no_unique_address]] slot_allocator_type alloc_;
            [[no_unique_address]] Hash hash_;
            [[no_unique_address]] KeyEqual key_equal_;

            /**
             * Array of `nb_sparse_buckets_` `sparse_array`s, nullptr if the table has no buckets.
             */
            bucket_pointer buckets_ = nullptr;
            size_type nb_sparse_buckets_ = 0;

            /**
             * 0 or a power of two.
             */
            size_type bucket_count_ = 0;
            size_type nb_elements_ = 0;

            /**
             * Number of buckets that are marked as deleted.
             */
            size_type nb_deleted_buckets_ = 0;

            /**
             * Maximum that `nb_elements_` can reach before a rehash grows the table.
             */
            size_type load_threshold_rehash_ = 0;

            /**
             * Maximum that `nb_elements_ + nb_deleted_buckets_` can reach before a rehash removes the
             * deleted-bucket markers.
             */
            size_type load_threshold_clear_deleted_ = 0;

            float max_load_factor_ = default_max_load_factor;
        };

    }  // namespace detail_sparse_hash
}  // namespace dice::sparse_map

#endif
