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
#ifndef DICE_UNORDERED_SPARSE_DETAIL_SPARSE_HASH_HPP
#define DICE_UNORDERED_SPARSE_DETAIL_SPARSE_HASH_HPP

#include <algorithm>
#include <bit>
#include <cassert>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <limits>
#include <memory>
#include <new>
#include <ranges>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>

/**
 * Internal consistency checks. Enabled when `DICE_UNORDERED_SPARSE_DEBUG` or `TSL_DEBUG` is defined.
 */
#if defined(DICE_UNORDERED_SPARSE_DEBUG) || defined(TSL_DEBUG)
#define DICE_UNORDERED_SPARSE_ASSERT(expr) assert(expr)
#else
#define DICE_UNORDERED_SPARSE_ASSERT(expr) (static_cast<void>(0))
#endif

namespace dice::unordered_sparse {

    enum struct sparsity {
        high,
        medium,
        low
    };

    /**
     * A hash function is avalanching if every bit of the input changes about half of the bits of the output.
     * The low bits of such a hash are good enough to pick a bucket. `sparse_map` and `sparse_set` use the hash
     * of an avalanching hash function as it is, and mix the hash of any other hash function with a multiplication
     * first. `std::hash` of integers is the identity in libstdc++ and libc++, so it is not avalanching.
     *
     * A hash function is marked as avalanching by a member type `is_avalanching`
     * (`using is_avalanching = void;`), or by a specialization of this trait. This is the convention of
     * `ankerl::unordered_dense` and of `boost::unordered`.
     */
    template<typename Hash>
        struct hash_is_avalanching : std::bool_constant < requires {
        typename Hash::is_avalanching;
    }>{};

    template<typename Hash>
    inline constexpr bool hash_is_avalanching_v = hash_is_avalanching<Hash>::value;

    namespace detail {

        /**
         * Multiplies `a` and `b` to 128 bits and returns the xor of the two halves.
         */
        [[nodiscard]] constexpr std::uint64_t multiply_fold(std::uint64_t a, std::uint64_t b) noexcept {
#if defined(__SIZEOF_INT128__)
            __extension__ using uint128_t = unsigned __int128;
            auto const product = static_cast<uint128_t>(a) * b;
            return static_cast<std::uint64_t>(product) ^ static_cast<std::uint64_t>(product >> 64U);
#else
            std::uint64_t const a_lo = a & 0xFFFFFFFFU;
            std::uint64_t const a_hi = a >> 32U;
            std::uint64_t const b_lo = b & 0xFFFFFFFFU;
            std::uint64_t const b_hi = b >> 32U;
            std::uint64_t const lo_lo = a_lo * b_lo;
            std::uint64_t const hi_lo = a_hi * b_lo;
            std::uint64_t const lo_hi = a_lo * b_hi;
            std::uint64_t const hi_hi = a_hi * b_hi;
            std::uint64_t const cross = (lo_lo >> 32U) + (hi_lo & 0xFFFFFFFFU) + lo_hi;
            std::uint64_t const hi = hi_hi + (hi_lo >> 32U) + (cross >> 32U);
            std::uint64_t const lo = (cross << 32U) | (lo_lo & 0xFFFFFFFFU);
            return lo ^ hi;
#endif
        }

        /**
         * The hash that picks the bucket: `hash` itself for an avalanching hash function, otherwise `hash`
         * mixed with the golden ratio constant.
         */
        template<typename Hash>
        [[nodiscard]] constexpr std::size_t mixed_hash(std::size_t hash) noexcept {
            if constexpr (hash_is_avalanching_v<Hash>) {
                return hash;
            } else {
                return static_cast<std::size_t>(multiply_fold(hash, UINT64_C(0x9E3779B97F4A7C15)));
            }
        }

        template<typename T>
        concept IsTransparent = requires { typename T::is_transparent; };

        /**
         * `K` is a lookup key that cannot be mistaken for an iterator. Overloads that take a key of any type use
         * it, so that `erase(it)` keeps calling the iterator overload.
         */
        template<typename K, typename Iterator, typename ConstIterator>
        concept NotIterator = !std::is_convertible_v<K, Iterator> && !std::is_convertible_v<K, ConstIterator>;

        /**
         * `Allocator` has its own `construct` or `destroy` for a `T`, so `std::allocator_traits` calls them and not
         * `std::construct_at` and `std::destroy_at`.
         */
        template<typename Allocator, typename T>
        concept HasConstructOrDestroy = requires (Allocator &alloc, T *p, T &value) { alloc.construct(p, std::move(value)); }
                                        || requires (Allocator &alloc, T *p, T const &value) { alloc.construct(p, value); }
                                        || requires (Allocator &alloc, T *p) { alloc.destroy(p); };

        /**
         * A type that meets the parts of the allocator requirements that the deduction guides check.
         */
        template<typename A>
        concept AllocatorLike = requires (A &alloc, std::size_t n) {
            typename A::value_type;
            alloc.allocate(n);
        };

        /**
         * A type with the tuple protocol and two elements, like `std::pair`.
         */
        template<typename P>
        concept PairLike = requires { std::tuple_size<std::remove_cvref_t<P>>::value; }
                           && std::tuple_size<std::remove_cvref_t<P>>::value == 2;

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
         * A range whose elements convert to `T` (the exposition-only concept container-compatible-range of the
         * standard library).
         */
        template<typename R, typename T>
        concept ContainerCompatibleRange = std::ranges::input_range<R> && std::convertible_to<std::ranges::range_reference_t<R>, T>;

        /*
         * Helpers for the deduction guides, as in the standard library.
         */
        template<typename InputIt>
        using iter_value_t = typename std::iterator_traits<InputIt>::value_type;

        template<typename InputIt>
        using iter_key_t = std::remove_const_t<std::tuple_element_t<0, iter_value_t<InputIt>>>;

        template<typename InputIt>
        using iter_mapped_t = std::tuple_element_t<1, iter_value_t<InputIt>>;

        template<typename InputIt>
        using iter_to_alloc_t = std::pair<iter_key_t<InputIt>, iter_mapped_t<InputIt>>;

        template<std::ranges::input_range R>
        using range_key_t = std::remove_const_t<std::tuple_element_t<0, std::ranges::range_value_t<R>>>;

        template<std::ranges::input_range R>
        using range_mapped_t = std::tuple_element_t<1, std::ranges::range_value_t<R>>;

        template<std::ranges::input_range R>
        using range_to_alloc_t = std::pair<range_key_t<R>, range_mapped_t<R>>;

        /**
         * `std::ceil(value)` as `std::size_t`, for a non-negative `value`. Saturates at the maximum of `std::size_t`.
         */
        [[nodiscard]] constexpr std::size_t ceil_to_size(double value) noexcept {
            if (!(value < static_cast<double>(std::numeric_limits<std::size_t>::max()))) {
                return std::numeric_limits<std::size_t>::max();
            }
            auto const truncated = static_cast<std::size_t>(value);
            return static_cast<double>(truncated) < value ? truncated + 1 : truncated;
        }

        /**
         * One `T`, constructed with `allocator_traits<Allocator>::construct` in storage on the stack, and destroyed
         * with `allocator_traits<Allocator>::destroy`. It holds a new element while the table moves its elements,
         * because the arguments of the new element may refer to an element that is moved.
         */
        template<typename T, typename Allocator>
        struct value_holder {
            template<typename... Args>
            constexpr explicit value_holder(Allocator &alloc, Args &&...args)
                : alloc_(alloc) {
                std::allocator_traits<Allocator>::construct(alloc_, std::addressof(storage_.value), std::forward<Args>(args)...);
            }

            value_holder(value_holder const &) = delete;
            value_holder(value_holder &&) = delete;
            value_holder &operator=(value_holder const &) = delete;
            value_holder &operator=(value_holder &&) = delete;

            constexpr ~value_holder() {
                std::allocator_traits<Allocator>::destroy(alloc_, std::addressof(storage_.value));
            }

            [[nodiscard]] constexpr T &get() noexcept {
                return storage_.value;
            }

        private:
            /**
             * Storage for a `T` that is constructed and destroyed by hand.
             */
            union storage_type {
                constexpr storage_type() noexcept {
                }

                constexpr ~storage_type() {
                }

                T value;
            };

            Allocator &alloc_;
            storage_type storage_;
        };

        /**
         * `u` as it is, or as a const lvalue if `T` cannot be constructed from it. A type with a deleted move
         * constructor is then copied, as `std::pair` does.
         */
        template<typename T, typename U>
        [[nodiscard]] constexpr decltype(auto) forward_or_copy(U &&u) noexcept {
            if constexpr (std::is_constructible_v<T, U &&>) {
                return std::forward<U>(u);
            } else {
                return std::as_const(u);
            }
        }

        /**
         * Storage of one element of a `sparse_map`.
         *
         * It is standard layout when `Key` and `T` are, unlike `std::pair`, whose layout the standard does not
         * define. It supports uses-allocator construction: with a scoped or a polymorphic allocator, the key and
         * the mapped value get the allocator as they would in a `std::pair`.
         */
        template<typename Key, typename T>
        struct map_slot {
            Key key;
            T value;

            template<typename... KeyArgs, typename... ValueArgs>
            constexpr map_slot(std::piecewise_construct_t, std::tuple<KeyArgs...> key_args, std::tuple<ValueArgs...> value_args)
                : key(std::make_from_tuple<Key>(std::move(key_args))),
                  value(std::make_from_tuple<T>(std::move(value_args))) {
            }

            /**
             * Constructs the slot from the two elements of a pair-like object, e.g. a `std::pair`.
             */
            template<PairLike P>
            constexpr explicit map_slot(P &&pair)
                : key(forward_or_copy<Key>(std::get<0>(std::forward<P>(pair)))),
                  value(forward_or_copy<T>(std::get<1>(std::forward<P>(pair)))) {
            }

            constexpr map_slot(map_slot const &) = default;
            constexpr map_slot(map_slot &&) = default;
            map_slot &operator=(map_slot const &) = default;
            map_slot &operator=(map_slot &&) = default;
            constexpr ~map_slot() = default;

            template<typename Alloc, typename... KeyArgs, typename... ValueArgs>
            constexpr map_slot(std::allocator_arg_t, Alloc const &alloc, std::piecewise_construct_t, std::tuple<KeyArgs...> key_args, std::tuple<ValueArgs...> value_args)
                : key(std::apply([&alloc](auto &&...args) {
                      return std::make_obj_using_allocator<Key>(alloc, std::forward<decltype(args)>(args)...);
                  },
                                 std::move(key_args))),
                  value(std::apply([&alloc](auto &&...args) {
                      return std::make_obj_using_allocator<T>(alloc, std::forward<decltype(args)>(args)...);
                  },
                                   std::move(value_args))) {
            }

            template<typename Alloc, PairLike P>
            constexpr map_slot(std::allocator_arg_t, Alloc const &alloc, P &&pair)
                : key(std::make_obj_using_allocator<Key>(alloc, std::get<0>(std::forward<P>(pair)))),
                  value(std::make_obj_using_allocator<T>(alloc, std::get<1>(std::forward<P>(pair)))) {
            }

            template<typename Alloc>
            constexpr map_slot(std::allocator_arg_t, Alloc const &alloc, map_slot const &other)
                : key(std::make_obj_using_allocator<Key>(alloc, other.key)),
                  value(std::make_obj_using_allocator<T>(alloc, other.value)) {
            }

            template<typename Alloc>
            constexpr map_slot(std::allocator_arg_t, Alloc const &alloc, map_slot &&other)
                : key(std::make_obj_using_allocator<Key>(alloc, std::move(other.key))),
                  value(std::make_obj_using_allocator<T>(alloc, std::move(other.value))) {
            }
        };

        /**
         * Proxy that `operator->` of a map iterator returns. It holds the reference that `operator*` returns.
         */
        template<typename Reference>
        struct arrow_proxy {
            Reference ref;

            [[nodiscard]] constexpr Reference const *operator->() const noexcept {
                return std::addressof(ref);
            }
        };

        /**
         * The type of a data member that a configuration does not need. With `[[no_unique_address]]` it takes no
         * space.
         */
        struct unused_member {};

        /**
         * Element access of `sparse_hash` for a map. The elements are stored as `map_slot<Key, T>`.
         * The public element type is `std::pair<Key, T>`, the reference type is `std::pair<Key const &, T &>`.
         */
        template<typename Key, typename T>
        struct map_policy {
            static constexpr bool is_map = true;

            using key_type = Key;
            using mapped_type = T;
            using value_type = std::pair<Key, T>;
            using slot_type = map_slot<Key, T>;
            using reference = std::pair<Key const &, T &>;
            using const_reference = std::pair<Key const &, T const &>;

            [[nodiscard]] static constexpr Key const &key(slot_type const &slot) noexcept {
                return slot.key;
            }

            [[nodiscard]] static constexpr T &mapped(slot_type &slot) noexcept {
                return slot.value;
            }

            [[nodiscard]] static constexpr T const &mapped(slot_type const &slot) noexcept {
                return slot.value;
            }

            [[nodiscard]] static constexpr Key const &key_of_value(value_type const &value) noexcept {
                return value.first;
            }
        };

        /**
         * Element access of `sparse_hash` for a set. The elements are stored as `Key`.
         */
        template<typename Key>
        struct set_policy {
            static constexpr bool is_map = false;

            using key_type = Key;
            using value_type = Key;
            using slot_type = Key;
            using reference = Key const &;
            using const_reference = Key const &;

            [[nodiscard]] static constexpr Key const &key(slot_type const &slot) noexcept {
                return slot;
            }

            [[nodiscard]] static constexpr Key const &key_of_value(value_type const &value) noexcept {
                return value;
            }
        };

        /**
         * A group of `bitmap_nb_bits` (64) buckets of the table. Only the occupied buckets have storage: their slots
         * are kept densely in `values_`, in bucket order, and a bitmap (`bitmap_vals_`) marks which buckets are
         * occupied. A second bitmap (`bitmap_deleted_vals_`) marks the buckets whose value was erased (deleted
         * buckets), so that probing goes on past them.
         *
         * An index is a position in [0, bitmap_nb_bits), like a position in a `std::vector`. An offset is the
         * position of the slot of an index in `values_`: the number of occupied buckets before the index.
         *
         * If `T` is nothrow move constructible, every occupied bucket holds a value. An erase removes the value and
         * moves the values after it one slot to the front, and an insertion moves them one slot to the back. The
         * storage grows by `capacity_growth_step` slots when it is full and does not shrink. If `T` is also nothrow
         * move assignable (`shifts_by_assignment`), the values move by move assignment, as in `std::vector`: an
         * insertion constructs one value behind the last one (besides the new value in its holder), and an erase
         * destroys the last one. Otherwise each moved value is constructed at its new place and destroyed at its old
         * place.
         *
         * Otherwise a move could throw halfway and could not be undone, so the values are never moved, and the group
         * has holes (`has_holes`). An erase destroys the value in place and keeps its slot: the bucket becomes a hole,
         * occupied and deleted at once. So an erase moves nothing, allocates nothing and does not throw. An insertion
         * into a hole constructs the value in the slot of the hole. An insertion into any other bucket copies the
         * values into new storage of the exact size, without the holes, and the holes become deleted buckets without
         * a slot. When the last value of a group is erased, the group frees its storage. A copy of a group has no
         * holes.
         *
         * The two bitmaps give four states of a bucket:
         * - not occupied, not deleted: empty.
         * - occupied, not deleted: holds a value.
         * - not occupied, deleted: deleted, without a slot.
         * - occupied and deleted: a hole, a slot without a value (only with `has_holes`).
         *
         * `nb_elements_` counts the values. The number of slots in use is the number of occupied buckets, which is
         * larger by the number of holes.
         *
         * `sparse_array` does not store an allocator, so that groups do not grow with a stateful allocator. Every
         * function that allocates or frees takes the allocator as an argument. `clear(Allocator &)` must be
         * called before a `sparse_array` is destroyed.
         *
         * `T` must be nothrow move constructible and/or copy constructible. The behaviour is undefined if the
         * destructor of `T` throws. A `T` that is nothrow move constructible is moved with
         * `allocator_traits::construct`, and that must not throw either: the moves that shift the values of a group
         * cannot be undone. With a scoped or a polymorphic allocator, `construct` calls the allocator-extended move
         * constructor, which does not throw when the allocators are equal, as they are within a table. `std::vector`
         * makes the same assumption. Inside `values_`, a `T` that is also nothrow move assignable is moved with its
         * move assignment, without the allocator.
         *
         * See https://smerity.com/articles/2015/google_sparsehash.html for the idea.
         */
        template<typename T, typename Allocator, unordered_sparse::sparsity sparsity>
        struct sparse_array {
            using value_type = T;
            using size_type = std::uint_least8_t;
            using allocator_type = Allocator;
            using allocator_traits = std::allocator_traits<allocator_type>;
            using pointer = typename allocator_traits::pointer;
            using const_pointer = typename allocator_traits::const_pointer;
            using iterator = value_type *;
            using const_iterator = value_type const *;

            /**
             * The number of buckets of a `sparse_array`.
             */
            static constexpr std::size_t bitmap_nb_bits = 64;

            /**
             * True if a bucket can be a hole: an erased value whose slot stays in `values_`. That is the case if the
             * move constructor of `value_type` can throw, see the class documentation.
             */
            static constexpr bool has_holes = !std::is_nothrow_move_constructible_v<value_type>;

            /**
             * True if an insertion or an erase moves the values after its position by move assignment, as
             * `std::vector::insert` and `std::vector::erase` do. Then an insertion constructs, besides the new value in
             * its holder, only the value behind the last one through the allocator, and an erase destroys only the
             * last one. That is the case if `value_type` is nothrow move constructible and nothrow move assignable.
             * Otherwise each moved value is constructed at its new place and destroyed at its old place through the
             * allocator.
             */
            static constexpr bool shifts_by_assignment = !has_holes && std::is_nothrow_move_assignable_v<value_type>;

            /**
             * True if a group copies its values as bytes, with `std::memcpy`, instead of one by one through the
             * allocator, when it grows its storage and when it is copied or moved into new storage. That is the case
             * if the group has no holes (`has_holes`), `value_type` is trivially copyable, and the allocator has no
             * `construct` and no `destroy` of its own, so that `std::allocator_traits` constructs with
             * `std::construct_at` and destroys with `std::destroy_at`. A constant evaluation copies one by one. An
             * insertion or an erase in the middle of a group copies no bytes itself: for such a type, the standard
             * library can do the move assignments of `shifts_by_assignment` with one `std::memmove` (libstdc++ does).
             */
            static constexpr bool copies_bytes = !has_holes && std::is_trivially_copyable_v<value_type> && !HasConstructOrDestroy<Allocator, value_type>;

        private:
            static constexpr size_type capacity_growth_step = (sparsity == unordered_sparse::sparsity::high)     ? 2
                                                              : (sparsity == unordered_sparse::sparsity::medium) ? 4
                                                                                                   : 8;

            using bitmap_type = std::uint_least64_t;
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
            [[nodiscard]] static constexpr std::size_t sparse_ibucket(std::size_t ibucket) noexcept {
                return ibucket >> bucket_shift;
            }

            /**
             * @return the index of bucket `ibucket` of the table in its `sparse_array`
             */
            [[nodiscard]] static constexpr size_type index_in_sparse_bucket(std::size_t ibucket) noexcept {
                return static_cast<size_type>(ibucket & bucket_mask);
            }

            /**
             * @return the number of `sparse_array`s of a table with `bucket_count` buckets
             */
            [[nodiscard]] static constexpr std::size_t nb_sparse_buckets(std::size_t bucket_count) noexcept {
                if (bucket_count == 0) {
                    return 0;
                }

                return std::max<std::size_t>(1, sparse_ibucket(std::bit_ceil(bucket_count)));
            }

            constexpr sparse_array() noexcept = default;

            /**
             * For uses-allocator construction. The allocator is not stored.
             */
            constexpr sparse_array(std::allocator_arg_t, Allocator const & /*alloc*/) noexcept {
            }

            constexpr explicit sparse_array(bool last_bucket) noexcept
                : last_array_(last_bucket) {
            }

            /**
             * Allocates storage for `capacity` values. The allocator is const for the MoveInsertable requirement.
             */
            constexpr sparse_array(size_type capacity, Allocator const &const_alloc)
                : capacity_(capacity) {
                if (capacity_ > 0) {
                    Allocator alloc(const_alloc);
                    values_ = allocator_traits::allocate(alloc, capacity_);
                    DICE_UNORDERED_SPARSE_ASSERT(values_ != nullptr);
                }
            }

            /**
             * Copies `other` into storage from `const_alloc`. The holes of `other` become deleted buckets without a
             * slot.
             */
            constexpr sparse_array(sparse_array const &other, Allocator const &const_alloc)
                : bitmap_vals_(other.bitmap_vals_),
                  bitmap_deleted_vals_(other.bitmap_deleted_vals_),
                  capacity_(other.capacity_),
                  last_array_(other.last_array_) {
                DICE_UNORDERED_SPARSE_ASSERT(other.capacity_ >= other.nb_elements_);
                if constexpr (has_holes) {
                    bitmap_vals_ = other.value_bitmap();
                }
                if (capacity_ == 0) {
                    return;
                }

                Allocator alloc(const_alloc);
                values_ = allocator_traits::allocate(alloc, capacity_);
                DICE_UNORDERED_SPARSE_ASSERT(values_ != nullptr);
                if constexpr (copies_bytes) {
                    if !consteval {
                        copy_values(values(), other.values(), other.nb_elements_);
                        nb_elements_ = other.nb_elements_;
                        return;
                    }
                }
                try {
                    if constexpr (has_holes) {
                        other.copy_values_to(alloc, other.value_bitmap(), values(), nb_elements_);
                    } else {
                        for (size_type i = 0; i < other.nb_elements_; ++i) {
                            construct_value(alloc, values() + i, other.values()[i]);
                            ++nb_elements_;
                        }
                    }
                } catch (...) {
                    clear(alloc);
                    throw;
                }
            }

            constexpr sparse_array(sparse_array &&other) noexcept
                : values_(std::exchange(other.values_, nullptr)),
                  bitmap_vals_(std::exchange(other.bitmap_vals_, 0)),
                  bitmap_deleted_vals_(std::exchange(other.bitmap_deleted_vals_, 0)),
                  nb_elements_(std::exchange(other.nb_elements_, 0)),
                  capacity_(std::exchange(other.capacity_, 0)),
                  last_array_(other.last_array_) {
            }

            /**
             * Moves the values of `other` into storage from `const_alloc`, or copies them if their move constructor
             * can throw. `other` keeps its values, the moved ones in a moved-from state. The holes of `other` become
             * deleted buckets without a slot.
             */
            constexpr sparse_array(sparse_array &&other, Allocator const &const_alloc)
                : bitmap_vals_(other.bitmap_vals_),
                  bitmap_deleted_vals_(other.bitmap_deleted_vals_),
                  capacity_(other.capacity_),
                  last_array_(other.last_array_) {
                DICE_UNORDERED_SPARSE_ASSERT(other.capacity_ >= other.nb_elements_);
                if constexpr (has_holes) {
                    bitmap_vals_ = other.value_bitmap();
                }
                if (capacity_ == 0) {
                    return;
                }

                Allocator alloc(const_alloc);
                values_ = allocator_traits::allocate(alloc, capacity_);
                DICE_UNORDERED_SPARSE_ASSERT(values_ != nullptr);
                if constexpr (copies_bytes) {
                    if !consteval {
                        copy_values(values(), other.values(), other.nb_elements_);
                        nb_elements_ = other.nb_elements_;
                        return;
                    }
                }
                try {
                    if constexpr (has_holes) {
                        other.copy_values_to(alloc, other.value_bitmap(), values(), nb_elements_);
                    } else {
                        for (size_type i = 0; i < other.nb_elements_; ++i) {
                            construct_value(alloc, values() + i, std::move_if_noexcept(other.values()[i]));
                            ++nb_elements_;
                        }
                    }
                } catch (...) {
                    clear(alloc);
                    throw;
                }
            }

            /**
             * Not assignable: an assignment could not free the old storage, because the allocator is not stored.
             */
            sparse_array &operator=(sparse_array const &) = delete;
            sparse_array &operator=(sparse_array &&) = delete;

            constexpr ~sparse_array() noexcept {
                // the owner must have called clear(Allocator &) before
                DICE_UNORDERED_SPARSE_ASSERT(capacity_ == 0 && nb_elements_ == 0 && values_ == nullptr);
            }

            /*
             * The values as a range of slots. A group with holes is such a range only while it has no holes.
             */
            [[nodiscard]] constexpr iterator begin() noexcept {
                if constexpr (has_holes) {
                    DICE_UNORDERED_SPARSE_ASSERT(!has_hole());
                }
                return values();
            }

            [[nodiscard]] constexpr iterator end() noexcept {
                if constexpr (has_holes) {
                    DICE_UNORDERED_SPARSE_ASSERT(!has_hole());
                }
                return values() + nb_elements_;
            }

            [[nodiscard]] constexpr const_iterator begin() const noexcept {
                return cbegin();
            }

            [[nodiscard]] constexpr const_iterator end() const noexcept {
                return cend();
            }

            [[nodiscard]] constexpr const_iterator cbegin() const noexcept {
                if constexpr (has_holes) {
                    DICE_UNORDERED_SPARSE_ASSERT(!has_hole());
                }
                return values();
            }

            [[nodiscard]] constexpr const_iterator cend() const noexcept {
                if constexpr (has_holes) {
                    DICE_UNORDERED_SPARSE_ASSERT(!has_hole());
                }
                return values() + nb_elements_;
            }

            /**
             * True if no bucket holds a value.
             */
            [[nodiscard]] constexpr bool empty() const noexcept {
                return nb_elements_ == 0;
            }

            /**
             * @return the number of values, without the holes
             */
            [[nodiscard]] constexpr size_type size() const noexcept {
                return nb_elements_;
            }

            /**
             * Destroys all values and frees the storage.
             */
            constexpr void clear(allocator_type &alloc) noexcept {
                if constexpr (has_holes) {
                    destroy_values_and_deallocate(alloc);
                } else {
                    destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);
                }

                values_ = nullptr;
                bitmap_vals_ = 0;
                bitmap_deleted_vals_ = 0;
                nb_elements_ = 0;
                capacity_ = 0;
            }

            /**
             * @return true if this is the last `sparse_array` of the table
             */
            [[nodiscard]] constexpr bool last() const noexcept {
                return last_array_;
            }

            constexpr void set_as_last() noexcept {
                last_array_ = true;
            }

            /**
             * True if the bucket at `index` holds a value. A hole holds none.
             */
            [[nodiscard]] constexpr bool has_value(size_type index) const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(index < bitmap_nb_bits);
                if constexpr (has_holes) {
                    // the occupied bit first, as without holes, so that a lookup reads the deleted bitmap only for an
                    // occupied bucket
                    bitmap_type const bit = bitmap_type{1} << index;
                    return (bitmap_vals_ & bit) != 0 && (bitmap_deleted_vals_ & bit) == 0;
                }
                return (bitmap_vals_ & (bitmap_type{1} << index)) != 0;
            }

            /**
             * True if the bucket at `index` is deleted, with or without a slot (a hole or not).
             */
            [[nodiscard]] constexpr bool has_deleted_value(size_type index) const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(index < bitmap_nb_bits);
                return (bitmap_deleted_vals_ & (bitmap_type{1} << index)) != 0;
            }

            /**
             * @return the index of the first bucket that holds a value, or `bitmap_nb_bits` if there is none
             */
            [[nodiscard]] constexpr size_type first_value_index() const noexcept {
                bitmap_type const bitmap = value_bitmap();
                return bitmap == 0 ? static_cast<size_type>(bitmap_nb_bits) : static_cast<size_type>(std::countr_zero(bitmap));
            }

            /**
             * @return the index of the first bucket after `index` that holds a value, or `bitmap_nb_bits` if there is
             * none
             */
            [[nodiscard]] constexpr size_type next_value_index(size_type index) const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(index < bitmap_nb_bits);
                bitmap_type const after = value_bitmap() & ((~bitmap_type{0} << index) << 1U);
                return after == 0 ? static_cast<size_type>(bitmap_nb_bits) : static_cast<size_type>(std::countr_zero(after));
            }

            /**
             * True if a bucket of the group is a hole.
             */
            [[nodiscard]] constexpr bool has_hole() const noexcept {
                return (bitmap_vals_ & bitmap_deleted_vals_) != 0;
            }

            /**
             * Values in consecutive slots, without a hole between them, starting at a value of a group with holes.
             * The iterator of a table with holes steps through a run slot by slot, as through a group without holes,
             * and asks the group for the next run at its end.
             */
            struct run_type {
                /// the number of values in the slots right after the current one, up to the next hole
                std::uint32_t nb_following = 0;
                /// the next run starts at the first value after this index, if it is not `bitmap_nb_bits`
                std::uint32_t next_after = 0;
            };

            /**
             * With holes: the slot of the first value at or after `index`, and the run that starts there, as a pair.
             * A group without holes is one run. A value at or after `index` must exist.
             */
            template<typename Self>
            [[nodiscard]] constexpr auto run_from(this Self &self, size_type index) noexcept {
                static_assert(has_holes);
                DICE_UNORDERED_SPARSE_ASSERT(index < bitmap_nb_bits);
                return self.run_of_lowest(self.value_bitmap() & (~bitmap_type{0} << index));
            }

            /**
             * True if a bucket after `index` holds a value, for an `index` below `bitmap_nb_bits`.
             */
            [[nodiscard]] constexpr bool has_value_after(size_type index) const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(index < bitmap_nb_bits);
                return (value_bitmap() & ((~bitmap_type{0} << index) << 1U)) != 0;
            }

            /**
             * With holes: `run_from(index + 1)`, for an `index` below `bitmap_nb_bits`. A value must follow
             * (`has_value_after(index)`).
             */
            template<typename Self>
            [[nodiscard]] constexpr auto run_after(this Self &self, size_type index) noexcept {
                static_assert(has_holes);
                DICE_UNORDERED_SPARSE_ASSERT(index < bitmap_nb_bits);
                return self.run_of_lowest(self.value_bitmap() & ((~bitmap_type{0} << index) << 1U));
            }

            /**
             * With holes.
             * @return the index of the bucket whose value is in the slot `slot`
             */
            [[nodiscard]] constexpr size_type index_of(value_type const *slot) const noexcept {
                static_assert(has_holes);
                auto const offset = static_cast<size_type>(slot - values());
                DICE_UNORDERED_SPARSE_ASSERT(offset < popcount(bitmap_vals_));

                bitmap_type bitmap = bitmap_vals_;
                for (size_type i = 0; i < offset; ++i) {
                    bitmap &= bitmap - 1;  // clears the lowest set bit
                }

                auto const index = static_cast<size_type>(std::countr_zero(bitmap));
                DICE_UNORDERED_SPARSE_ASSERT(has_value(index));
                return index;
            }

            [[nodiscard]] constexpr iterator value(size_type index) noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(has_value(index));
                return values() + index_to_offset(index);
            }

            [[nodiscard]] constexpr const_iterator value(size_type index) const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(has_value(index));
                return values() + index_to_offset(index);
            }

            /**
             * Constructs a value at `index` from `value_args`. With holes, see `set_with_holes`.
             * @return iterator to the new value
             */
            template<typename... Args>
            constexpr iterator set(allocator_type &alloc, size_type index, Args &&...value_args) {
                DICE_UNORDERED_SPARSE_ASSERT(!has_value(index));
                if constexpr (has_holes) {
                    return set_with_holes(alloc, index, std::forward<Args>(value_args)...);
                } else {
                    size_type const offset = index_to_offset(index);
                    insert_at_offset(alloc, offset, std::forward<Args>(value_args)...);

                    bitmap_vals_ = (bitmap_vals_ | (bitmap_type{1} << index));
                    bitmap_deleted_vals_ = (bitmap_deleted_vals_ & ~(bitmap_type{1} << index));

                    ++nb_elements_;

                    DICE_UNORDERED_SPARSE_ASSERT(has_value(index));
                    DICE_UNORDERED_SPARSE_ASSERT(!has_deleted_value(index));

                    return values() + offset;
                }
            }

            /**
             * Erases the value at `index` of a group with holes: destroys the value and keeps its slot as a hole.
             * Moves nothing and allocates nothing. If it was the last value of the group, the group frees its storage,
             * and its holes become deleted buckets without a slot.
             */
            constexpr void erase_in_place(allocator_type &alloc, size_type index) noexcept {
                static_assert(has_holes);
                DICE_UNORDERED_SPARSE_ASSERT(has_value(index));
                DICE_UNORDERED_SPARSE_ASSERT(!has_deleted_value(index));

                destroy_value(alloc, values() + index_to_offset(index));
                bitmap_deleted_vals_ = (bitmap_deleted_vals_ | (bitmap_type{1} << index));
                --nb_elements_;

                if (nb_elements_ == 0) {
                    // every occupied bucket is a hole, so there is no value to destroy
                    DICE_UNORDERED_SPARSE_ASSERT((bitmap_vals_ & ~bitmap_deleted_vals_) == 0);
                    allocator_traits::deallocate(alloc, values_, capacity_);
                    values_ = nullptr;
                    capacity_ = 0;
                    bitmap_vals_ = 0;
                }

                DICE_UNORDERED_SPARSE_ASSERT(!has_value(index));
                DICE_UNORDERED_SPARSE_ASSERT(has_deleted_value(index));
                DICE_UNORDERED_SPARSE_ASSERT(nb_elements_ == popcount(value_bitmap()));
            }

            /**
             * Without holes only.
             */
            constexpr iterator erase(allocator_type &alloc, iterator position) {
                auto const offset = static_cast<size_type>(position - begin());
                return erase(alloc, position, offset_to_index(offset));
            }

            /**
             * Erases the value at `position`, which is at `index`, and marks the bucket as deleted. Without holes
             * only, a group with holes erases with `erase_in_place`.
             * @return iterator to the next value, or `end()`
             */
            constexpr iterator erase(allocator_type &alloc, iterator position, size_type index) {
                DICE_UNORDERED_SPARSE_ASSERT(has_value(index));
                DICE_UNORDERED_SPARSE_ASSERT(!has_deleted_value(index));

                auto const offset = static_cast<size_type>(position - begin());
                erase_at_offset(alloc, offset);

                bitmap_vals_ = (bitmap_vals_ & ~(bitmap_type{1} << index));
                bitmap_deleted_vals_ = (bitmap_deleted_vals_ | (bitmap_type{1} << index));

                --nb_elements_;

                DICE_UNORDERED_SPARSE_ASSERT(!has_value(index));
                DICE_UNORDERED_SPARSE_ASSERT(has_deleted_value(index));

                return values() + offset;
            }

            constexpr void swap(sparse_array &other) noexcept {
                using std::swap;

                swap(values_, other.values_);
                swap(bitmap_vals_, other.bitmap_vals_);
                swap(bitmap_deleted_vals_, other.bitmap_deleted_vals_);
                swap(nb_elements_, other.nb_elements_);
                swap(capacity_, other.capacity_);
                swap(last_array_, other.last_array_);
            }

        private:
            [[nodiscard]] constexpr value_type *values() noexcept {
                return std::to_address(values_);
            }

            [[nodiscard]] constexpr value_type const *values() const noexcept {
                return std::to_address(values_);
            }

            /**
             * Copies `count` values from `source` to `target` with `std::memcpy`. The ranges do not overlap.
             */
            static void copy_values(value_type *target, value_type const *source, size_type count) noexcept {
                static_assert(copies_bytes);
                if (count > 0) {
                    std::memcpy(static_cast<void *>(target), static_cast<void const *>(source), count * sizeof(value_type));
                }
            }

            /**
             * Frees the storage of `capacity_values` values whose values were copied away as bytes. Their destructor
             * is trivial, so nothing is destroyed.
             */
            static constexpr void deallocate_values(allocator_type &alloc, pointer values, size_type capacity_values) noexcept {
                static_assert(copies_bytes);
                if (capacity_values > 0) {
                    allocator_traits::deallocate(alloc, values, capacity_values);
                }
            }

            template<typename... Args>
            static constexpr void construct_value(allocator_type &alloc, value_type *value, Args &&...value_args) {
                allocator_traits::construct(alloc, value, std::forward<Args>(value_args)...);
            }

            static constexpr void destroy_value(allocator_type &alloc, value_type *value) noexcept {
                allocator_traits::destroy(alloc, value);
            }

            static constexpr void destroy_and_deallocate_values(allocator_type &alloc, pointer values, size_type nb_values, size_type capacity_values) noexcept {
                if (capacity_values == 0) {
                    // nothing to deallocate, alloc.deallocate(nullptr, 0) is invalid
                    DICE_UNORDERED_SPARSE_ASSERT(nb_values == 0);
                    return;
                }

                value_type *const raw_values = std::to_address(values);
                for (size_type i = 0; i < nb_values; ++i) {
                    destroy_value(alloc, raw_values + i);
                }
                allocator_traits::deallocate(alloc, values, capacity_values);
            }

            [[nodiscard]] static constexpr size_type popcount(bitmap_type val) noexcept {
                return static_cast<size_type>(std::popcount(val));
            }

            [[nodiscard]] constexpr size_type index_to_offset(size_type index) const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(index < bitmap_nb_bits);
                return popcount(bitmap_vals_ & ((bitmap_type{1} << index) - bitmap_type{1}));
            }

            /**
             * @return the index of the occupied bucket whose value is at `offset`
             */
            [[nodiscard]] constexpr size_type offset_to_index(size_type offset) const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(offset < nb_elements_);

                bitmap_type bitmap = bitmap_vals_;
                for (size_type i = 0; i < offset; ++i) {
                    bitmap &= bitmap - 1;  // clears the lowest set bit
                }

                return static_cast<size_type>(std::countr_zero(bitmap));
            }

            [[nodiscard]] constexpr size_type next_capacity() const noexcept {
                return static_cast<size_type>(capacity_ + capacity_growth_step);
            }

            /**
             * @return the buckets that hold a value: the occupied buckets without the holes
             */
            [[nodiscard]] constexpr bitmap_type value_bitmap() const noexcept {
                if constexpr (has_holes) {
                    return bitmap_vals_ & ~bitmap_deleted_vals_;
                }
                return bitmap_vals_;
            }

            /**
             * With holes: the slot of the value in the lowest bucket of `values_from`, a part of the value bitmap that is
             * not 0, and the run that starts there, as a pair. The masks are derived from `values_from` and from the
             * first hole after the value, so that nothing is shifted by a variable amount.
             */
            template<typename Self>
            [[nodiscard]] constexpr auto run_of_lowest(this Self &self, bitmap_type values_from) noexcept {
                static_assert(has_holes);
                using result_type = std::pair<decltype(self.values()), run_type>;
                DICE_UNORDERED_SPARSE_ASSERT(values_from != 0 && (values_from & ~self.value_bitmap()) == 0);

                bitmap_type const occupied = self.bitmap_vals_;
                bitmap_type const before = (values_from - 1) & ~values_from;
                bitmap_type const after = ~(values_from ^ (values_from - 1));
                size_type const offset = popcount(occupied & before);
                bitmap_type const holes_after = occupied & self.bitmap_deleted_vals_ & after;
                if (holes_after == 0) {
                    // the run ends with the last slot
                    return result_type{self.values() + offset, {static_cast<std::uint32_t>(popcount(occupied) - offset - 1), static_cast<std::uint32_t>(bitmap_nb_bits)}};
                }

                bitmap_type const before_hole = (holes_after - 1) & ~holes_after;
                return result_type{self.values() + offset, {static_cast<std::uint32_t>(popcount(occupied & before_hole) - offset - 1), static_cast<std::uint32_t>(std::countr_zero(holes_after))}};
            }

            /**
             * Copies the values of the buckets in `buckets`, in bucket order, to `target + nb_copied`,
             * `target + nb_copied + 1` and so on. Counts each copy in `nb_copied` right after it, so that the count is
             * right when a copy throws.
             */
            constexpr void copy_values_to(allocator_type &alloc, bitmap_type buckets, value_type *target, size_type &nb_copied) const {
                DICE_UNORDERED_SPARSE_ASSERT((buckets & ~value_bitmap()) == 0);
                value_type const *const raw_values = values();
                for (; buckets != 0; buckets &= buckets - 1) {
                    auto const index = static_cast<size_type>(std::countr_zero(buckets));
                    construct_value(alloc, target + nb_copied, raw_values[index_to_offset(index)]);
                    ++nb_copied;
                }
            }

            /**
             * Destroys the values of a group with holes and frees its storage. Visits the buckets that hold a value in
             * bucket order and stops after `nb_elements_` values. So it also frees storage whose first `nb_elements_`
             * slots hold values and whose other slots hold none, as after a copy that threw. Without a hole these are
             * the first `nb_elements_` slots.
             */
            constexpr void destroy_values_and_deallocate(allocator_type &alloc) noexcept {
                static_assert(has_holes);
                if (!has_hole()) {
                    destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);
                    return;
                }

                value_type *const raw_values = values();
                bitmap_type buckets = value_bitmap();
                for (size_type i = 0; i < nb_elements_; ++i) {
                    DICE_UNORDERED_SPARSE_ASSERT(buckets != 0);
                    destroy_value(alloc, raw_values + index_to_offset(static_cast<size_type>(std::countr_zero(buckets))));
                    buckets &= buckets - 1;
                }
                allocator_traits::deallocate(alloc, values_, capacity_);
            }

            /**
             * `set` for a group with holes. A hole has a slot, so the value is constructed in it, and nothing else
             * moves or allocates. If the construction throws, the hole stays. Any other bucket gets a slot from
             * `insert_without_holes` or, if the group has a hole, from `insert_compacting`.
             */
            template<typename... Args>
            constexpr iterator set_with_holes(allocator_type &alloc, size_type index, Args &&...value_args) {
                static_assert(has_holes);
                bitmap_type const bit = bitmap_type{1} << index;
                if ((bitmap_vals_ & bit) != 0) {
                    DICE_UNORDERED_SPARSE_ASSERT((bitmap_deleted_vals_ & bit) != 0);
                    value_type *const slot = values() + index_to_offset(index);
                    construct_value(alloc, slot, std::forward<Args>(value_args)...);
                    bitmap_deleted_vals_ = (bitmap_deleted_vals_ & ~bit);
                    ++nb_elements_;

                    DICE_UNORDERED_SPARSE_ASSERT(nb_elements_ == popcount(value_bitmap()));
                    return slot;
                }

                value_type *const slot = has_hole() ? insert_compacting(alloc, index, std::forward<Args>(value_args)...)
                                                    : insert_without_holes(alloc, index, std::forward<Args>(value_args)...);

                DICE_UNORDERED_SPARSE_ASSERT(has_value(index) && !has_deleted_value(index));
                DICE_UNORDERED_SPARSE_ASSERT(nb_elements_ == popcount(value_bitmap()) && !has_hole());
                DICE_UNORDERED_SPARSE_ASSERT(slot == values() + index_to_offset(index));
                return slot;
            }

            /**
             * Inserts a value at `index`, a bucket without a slot, into a group that can have holes but has none.
             * Allocates storage for `nb_elements_ + 1` values, copies the slots before the new value, constructs the
             * new value and copies the slots after it. If a copy or the construction throws, the group is unchanged.
             *
             * The arguments may refer to a value of this group: the old storage stays as it is until the new one is
             * complete.
             * @return the slot of the new value
             */
            template<typename... Args>
            constexpr value_type *insert_without_holes(allocator_type &alloc, size_type index, Args &&...value_args) {
                static_assert(has_holes);
                DICE_UNORDERED_SPARSE_ASSERT(!has_hole() && (bitmap_vals_ & (bitmap_type{1} << index)) == 0);

                auto const new_capacity = static_cast<size_type>(nb_elements_ + 1);
                pointer const new_values = allocator_traits::allocate(alloc, new_capacity);
                DICE_UNORDERED_SPARSE_ASSERT(new_values != nullptr);
                value_type *const raw_new_values = std::to_address(new_values);
                value_type const *const raw_values = values();
                size_type const offset = index_to_offset(index);

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
                DICE_UNORDERED_SPARSE_ASSERT(nb_new_values == new_capacity);

                destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);

                bitmap_type const bit = bitmap_type{1} << index;
                values_ = new_values;
                capacity_ = new_capacity;
                bitmap_vals_ = (bitmap_vals_ | bit);
                bitmap_deleted_vals_ = (bitmap_deleted_vals_ & ~bit);
                ++nb_elements_;
                return raw_new_values + offset;
            }

            /**
             * Inserts a value at `index`, a bucket without a slot, into a group with holes. Allocates storage for
             * `nb_elements_ + 1` values, copies the values into it in bucket order and constructs the new value at its
             * place. The new storage has no holes, so the holes become deleted buckets without a slot. If a copy or
             * the construction throws, the group is unchanged.
             *
             * The arguments may refer to a value of this group: the old storage stays as it is until the new one is
             * complete.
             * @return the slot of the new value
             */
            template<typename... Args>
            constexpr value_type *insert_compacting(allocator_type &alloc, size_type index, Args &&...value_args) {
                static_assert(has_holes);
                bitmap_type const bit = bitmap_type{1} << index;
                bitmap_type const before = bit - 1;
                DICE_UNORDERED_SPARSE_ASSERT((bitmap_vals_ & bit) == 0);

                auto const new_capacity = static_cast<size_type>(nb_elements_ + 1);
                pointer const new_values = allocator_traits::allocate(alloc, new_capacity);
                DICE_UNORDERED_SPARSE_ASSERT(new_values != nullptr);
                value_type *const raw_new_values = std::to_address(new_values);

                size_type nb_new_values = 0;
                size_type new_offset = 0;
                try {
                    copy_values_to(alloc, value_bitmap() & before, raw_new_values, nb_new_values);
                    new_offset = nb_new_values;
                    construct_value(alloc, raw_new_values + new_offset, std::forward<Args>(value_args)...);
                    ++nb_new_values;
                    copy_values_to(alloc, value_bitmap() & ~before & ~bit, raw_new_values, nb_new_values);
                } catch (...) {
                    destroy_and_deallocate_values(alloc, new_values, nb_new_values, new_capacity);
                    throw;
                }
                DICE_UNORDERED_SPARSE_ASSERT(nb_new_values == new_capacity);

                destroy_values_and_deallocate(alloc);

                values_ = new_values;
                capacity_ = new_capacity;
                bitmap_vals_ = value_bitmap() | bit;
                bitmap_deleted_vals_ = (bitmap_deleted_vals_ & ~bit);
                ++nb_elements_;
                return raw_new_values + new_offset;
            }

            /**
             * Insertion without holes
             *
             * `values_` grows by `capacity_growth_step` when it is full. Otherwise the values after `offset` move one
             * place to the back (see `shifts_by_assignment`), and the new value is moved into its place. Moving values
             * is safe, so this keeps the strong exception guarantee.
             */
            template<typename... Args>
            constexpr void insert_at_offset(allocator_type &alloc, size_type offset, Args &&...value_args) {
                static_assert(!has_holes);
                if (nb_elements_ < capacity_) {
                    insert_at_offset_no_realloc(alloc, offset, std::forward<Args>(value_args)...);
                } else {
                    insert_at_offset_realloc(alloc, offset, next_capacity(), std::forward<Args>(value_args)...);
                }
            }

            template<typename... Args>
            constexpr void insert_at_offset_no_realloc(allocator_type &alloc, size_type offset, Args &&...value_args) {
                static_assert(std::is_nothrow_move_constructible_v<value_type>);
                DICE_UNORDERED_SPARSE_ASSERT(offset <= nb_elements_);
                DICE_UNORDERED_SPARSE_ASSERT(nb_elements_ < capacity_);

                value_type *const raw_values = values();
                if (offset == nb_elements_) {
                    construct_value(alloc, raw_values + offset, std::forward<Args>(value_args)...);
                    return;
                }

                // The arguments may refer to a value of this group that the shift below moves, so the new value is
                // constructed before the shift.
                value_holder<value_type, allocator_type> new_value(alloc, std::forward<Args>(value_args)...);

                if constexpr (shifts_by_assignment) {
                    // as `std::vector::insert`: the last value is moved into a new slot behind it, the others move one
                    // place to the back by assignment, and the new value is assigned into its place
                    construct_value(alloc, raw_values + nb_elements_, std::move(raw_values[nb_elements_ - 1]));
                    std::move_backward(raw_values + offset, raw_values + nb_elements_ - 1, raw_values + nb_elements_);
                    raw_values[offset] = std::move(new_value.get());
                } else {
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
            }

            template<typename... Args>
            constexpr void insert_at_offset_realloc(allocator_type &alloc, size_type offset, size_type new_capacity, Args &&...value_args) {
                static_assert(!has_holes);
                DICE_UNORDERED_SPARSE_ASSERT(new_capacity > nb_elements_);

                pointer const new_values = allocator_traits::allocate(alloc, new_capacity);
                DICE_UNORDERED_SPARSE_ASSERT(new_values != nullptr);
                value_type *const raw_new_values = std::to_address(new_values);
                value_type *const raw_values = values();

                try {
                    construct_value(alloc, raw_new_values + offset, std::forward<Args>(value_args)...);
                } catch (...) {
                    allocator_traits::deallocate(alloc, new_values, new_capacity);
                    throw;
                }

                // does not throw from here on
                if constexpr (copies_bytes) {
                    if !consteval {
                        copy_values(raw_new_values, raw_values, offset);
                        copy_values(raw_new_values + offset + 1, raw_values + offset, static_cast<size_type>(nb_elements_ - offset));
                        deallocate_values(alloc, values_, capacity_);
                        values_ = new_values;
                        capacity_ = new_capacity;
                        return;
                    }
                }

                for (size_type i = 0; i < offset; ++i) {
                    construct_value(alloc, raw_new_values + i, std::move(raw_values[i]));
                }

                for (size_type i = offset; i < nb_elements_; ++i) {
                    construct_value(alloc, raw_new_values + i + 1, std::move(raw_values[i]));
                }

                destroy_and_deallocate_values(alloc, values_, nb_elements_, capacity_);

                values_ = new_values;
                capacity_ = new_capacity;
            }

            /**
             * Erasure without holes: the value is removed and the values after it move one place to the front (see
             * `shifts_by_assignment`).
             */
            constexpr void erase_at_offset(allocator_type &alloc, size_type offset) noexcept {
                static_assert(!has_holes);
                DICE_UNORDERED_SPARSE_ASSERT(offset < nb_elements_);
                value_type *const raw_values = values();

                if constexpr (shifts_by_assignment) {
                    // as `std::vector::erase`: the values after `offset` move one place to the front by assignment,
                    // and the last value, now moved from, is destroyed
                    std::move(raw_values + offset + 1, raw_values + nb_elements_, raw_values + offset);
                    destroy_value(alloc, raw_values + nb_elements_ - 1);
                } else {
                    destroy_value(alloc, raw_values + offset);

                    for (size_type i = offset + 1; i < nb_elements_; ++i) {
                        construct_value(alloc, raw_values + i - 1, std::move(raw_values[i]));
                        destroy_value(alloc, raw_values + i);
                    }
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
         * `Policy` is `map_policy<Key, T>` or `set_policy<Key>`. It defines the public element type
         * (`value_type`), the stored element type (`slot_type`) and the reference types of the iterators.
         *
         * A rehash that throws leaves the table unchanged or empty, depending on the type of the elements
         * and on what throws. See `rehash_impl`.
         *
         * `erase` does not throw unless the hash function or the key equality throws. If the move constructor of the
         * stored elements can throw, the groups have holes (`has_holes`, see `sparse_array`): an erase destroys the
         * element in place and leaves a hole, which keeps the memory of the element until the group is compacted by
         * an insertion into another bucket of the group or by a rehash, or until its last element is erased. A copy
         * of the table has no holes. A hole counts as a deleted bucket in `nb_deleted_buckets_`, so the clean-up
         * rehash bounds the holes as it bounds the other deleted buckets.
         *
         * `begin()` is constant time and writes nothing. The table keeps the index of its first group that holds an
         * element (`first_nonempty_group_`), and every operation that changes the table keeps it exact. An erase that
         * empties this group reads the headers of the groups after it, up to the next group with an element. So the
         * loop `while (!empty()) { erase(begin()); }` reads every group header once in total.
         *
         * The stored elements must be nothrow move constructible and/or copy constructible. The behaviour is
         * undefined if their destructor throws. See `sparse_array` for what a nothrow move needs from the allocator.
         *
         * The buckets are kept in two dimensions. `buckets_` points to an array of `nb_sparse_buckets_`
         * `sparse_array`s, and each `sparse_array` holds `sparse_array::bitmap_nb_bits` buckets. Bucket `ibucket`
         * is at `buckets_[sparse_array::sparse_ibucket(ibucket)]`, position
         * `sparse_array::index_in_sparse_bucket(ibucket)`.
         *
         * The bucket count is 0 or a power of two, and it doubles when the table grows. A hash maps to a bucket
         * with a mask, after `mixed_hash` (see `hash_is_avalanching`). Collisions are resolved with quadratic
         * probing.
         *
         * The constructors take the allocator of the container or the allocator of the stored elements, and convert
         * it with a direct initialization. So an allocator with an `explicit` converting constructor works, as the
         * allocator requirements allow.
         *
         * The hasher, the key equality and the allocator are members, not base classes, and `sparse_hash` has
         * no base class. So `sparse_hash`, and with it `sparse_map` and `sparse_set`, is a standard layout type
         * whenever `Hash`, `KeyEqual`, the allocator and the allocator's pointer type are standard layout.
         * Empty members take no space.
         */
        template<typename Policy, typename Hash, typename KeyEqual, typename Allocator, unordered_sparse::sparsity sparsity>
        struct sparse_hash {
        private:
            template<typename, typename, typename, typename, unordered_sparse::sparsity>
            friend struct sparse_hash;

        public:
            template<bool is_const>
            struct sparse_iterator;

            using key_type = typename Policy::key_type;
            using value_type = typename Policy::value_type;
            using hasher = Hash;
            using key_equal = KeyEqual;
            using allocator_type = Allocator;
            using reference = typename Policy::reference;
            using const_reference = typename Policy::const_reference;
            using size_type = typename std::allocator_traits<allocator_type>::size_type;
            using difference_type = typename std::allocator_traits<allocator_type>::difference_type;
            using pointer = typename std::allocator_traits<allocator_type>::pointer;
            using const_pointer = typename std::allocator_traits<allocator_type>::const_pointer;
            using iterator = sparse_iterator<false>;
            using const_iterator = sparse_iterator<true>;

        private:
            using slot_type = typename Policy::slot_type;
            using slot_allocator_type = typename std::allocator_traits<allocator_type>::template rebind_alloc<slot_type>;
            using slot_allocator_traits = std::allocator_traits<slot_allocator_type>;
            using value_allocator_type = typename std::allocator_traits<allocator_type>::template rebind_alloc<value_type>;
            using sparse_array = detail::sparse_array<slot_type, slot_allocator_type, sparsity>;
            using array_size_type = typename sparse_array::size_type;
            using run_type = typename sparse_array::run_type;
            using bucket_allocator_type = typename std::allocator_traits<allocator_type>::template rebind_alloc<sparse_array>;
            using bucket_allocator_traits = std::allocator_traits<bucket_allocator_type>;
            using bucket_pointer = typename bucket_allocator_traits::pointer;

            /**
             * True if a group can have holes (see `sparse_array`): the move constructor of the stored elements can
             * throw. Then `erase` destroys an element in place and moves no other element, and a rehash copies the
             * elements (`copy_on_rehash`).
             */
            static constexpr bool has_holes = sparse_array::has_holes;

            static constexpr bool propagate_on_copy_assignment = slot_allocator_traits::propagate_on_container_copy_assignment::value;
            static constexpr bool propagate_on_move_assignment = slot_allocator_traits::propagate_on_container_move_assignment::value;
            static constexpr bool propagate_on_swap = slot_allocator_traits::propagate_on_container_swap::value;
            static constexpr bool allocator_is_always_equal = slot_allocator_traits::is_always_equal::value;

        public:
            /**
             * Forward iterator over the elements.
             *
             * A map iterator dereferences to `std::pair<Key const &, T &>` (`std::pair<Key const &, T const &>` for
             * `const_iterator`): the key is const, the mapped value is mutable through `iterator`. `operator->`
             * returns a proxy, so `it->second` works. A set iterator dereferences to `Key const &`.
             *
             * The iterator holds plain pointers, also with an allocator that uses fancy pointers: the group and the
             * element. The third member depends on the groups. Without holes it is the end of the elements of the
             * group, and `++` steps to the next slot. With holes it is the run of the element (`sparse_array::run_type`):
             * the number of values in the slots right after it, up to the next hole, and the index after which the next
             * run starts. `++` steps to the next slot while the run has values, and otherwise asks the group for the
             * next run. In a group without holes this is one run from the first slot to the last. An iterator that a
             * lookup, an insertion, an erasure or `begin()` returns knows only the index of its element, so its first
             * `++` asks the group. An insertion or an erasure can invalidate it.
             */
            template<bool is_const>
            struct sparse_iterator {
            private:
                friend struct sparse_hash;

                template<typename, typename, typename, typename, unordered_sparse::sparsity>
                friend struct sparse_hash;

                friend struct sparse_iterator<!is_const>;

                using bucket_type = std::conditional_t<is_const, sparse_array const, sparse_array>;
                using slot_pointer = std::conditional_t<is_const, slot_type const *, slot_type *>;

                /**
                 * Without holes. `slot_` is nullptr if `bucket_` is the end of the bucket array.
                 */
                constexpr sparse_iterator(bucket_type *bucket, slot_pointer slot) noexcept
                    : bucket_(bucket),
                      slot_(slot),
                      slot_end_(slot == nullptr ? nullptr : bucket->end()) {
                }

                /**
                 * Without holes.
                 */
                constexpr sparse_iterator(bucket_type *bucket, slot_pointer slot, slot_pointer slot_end) noexcept
                    : bucket_(bucket),
                      slot_(slot),
                      slot_end_(slot_end) {
                }

                /**
                 * With holes. `slot` is the element at `index` of `*bucket`, or nullptr if `bucket` is the end of the
                 * bucket array. The run is not known yet: the first `++` looks for the next value after `index`.
                 */
                constexpr sparse_iterator(bucket_type *bucket, slot_pointer slot, array_size_type index) noexcept
                    : bucket_(bucket),
                      slot_(slot),
                      run_{0, index} {
                }

                /**
                 * With holes. `slot` is an element of `*bucket` and `run` its run. The run is copied field by field, as
                 * everywhere in the iterator, so that the compiler keeps the two counters apart in registers.
                 */
                constexpr sparse_iterator(bucket_type *bucket, slot_pointer slot, run_type run) noexcept
                    : bucket_(bucket),
                      slot_(slot),
                      run_{run.nb_following, run.next_after} {
                }

                /**
                 * With holes. The element at `index` of `*bucket`.
                 */
                constexpr sparse_iterator(bucket_type *bucket, array_size_type index) noexcept
                    : sparse_iterator(bucket, bucket->value(index), index) {
                }

            public:
                using iterator_concept = std::forward_iterator_tag;
                /**
                 * A map iterator returns a proxy reference, so it meets only the Cpp17InputIterator requirements.
                 * It models `std::forward_iterator`.
                 */
                using iterator_category = std::conditional_t<Policy::is_map, std::input_iterator_tag, std::forward_iterator_tag>;
                using value_type = typename sparse_hash::value_type;
                using difference_type = typename sparse_hash::difference_type;
                using reference = std::conditional_t<is_const, typename Policy::const_reference, typename Policy::reference>;
                using pointer = std::conditional_t<Policy::is_map, arrow_proxy<reference>, value_type const *>;

                constexpr sparse_iterator() noexcept = default;

                /**
                 * Converts an `iterator` into a `const_iterator`.
                 */
                template<bool other_is_const>
                requires (is_const && !other_is_const)
                constexpr sparse_iterator(sparse_iterator<other_is_const> const &other) noexcept  // NOLINT(google-explicit-constructor)
                    : bucket_(other.bucket_),
                      slot_(other.slot_),
                      slot_end_(other.slot_end_) {
                    if constexpr (has_holes) {
                        run_.nb_following = other.run_.nb_following;
                        run_.next_after = other.run_.next_after;
                    }
                }

                [[nodiscard]] constexpr reference operator*() const noexcept {
                    if constexpr (Policy::is_map) {
                        return reference{Policy::key(*slot_), Policy::mapped(*slot_)};
                    } else {
                        return *slot_;
                    }
                }

                [[nodiscard]] constexpr pointer operator->() const noexcept {
                    if constexpr (Policy::is_map) {
                        return pointer{**this};
                    } else {
                        return slot_;
                    }
                }

                constexpr sparse_iterator &operator++() noexcept {
                    if constexpr (has_holes) {
                        DICE_UNORDERED_SPARSE_ASSERT(slot_ != nullptr && run_is_plausible());
                        if (run_.nb_following != 0) [[likely]] {
                            --run_.nb_following;
                            ++slot_;
                            return *this;
                        }

                        if (run_.next_after != sparse_array::bitmap_nb_bits && bucket_->has_value_after(static_cast<array_size_type>(run_.next_after))) {
                            auto const [slot, run] = bucket_->run_after(static_cast<array_size_type>(run_.next_after));
                            slot_ = slot;
                            run_.nb_following = run.nb_following;
                            run_.next_after = run.next_after;
                            return *this;
                        }

                        // a new iterator, so that this one stays in registers if the call is not inlined
                        *this = next_group(bucket_);
                    } else {
                        DICE_UNORDERED_SPARSE_ASSERT(slot_ != nullptr && slot_end_ == bucket_->end());
                        ++slot_;

                        if (slot_ == slot_end_) [[unlikely]] {
                            to_next_group();
                        }
                    }

                    return *this;
                }

                constexpr sparse_iterator operator++(int) noexcept {
                    sparse_iterator tmp(*this);
                    ++*this;
                    return tmp;
                }

                /**
                 * Two iterators of the same table are equal if they point to the same element. All end iterators
                 * have `slot_ == nullptr`.
                 */
                friend constexpr bool operator==(sparse_iterator const &lhs, sparse_iterator const &rhs) noexcept {
                    return lhs.slot_ == rhs.slot_;
                }

            private:
                /**
                 * With holes: an iterator to the first value of the group after `*bucket` that is not empty, or the end.
                 */
                [[nodiscard]] static constexpr sparse_iterator next_group(bucket_type *bucket) noexcept {
                    do {
                        if (bucket->last()) {
                            return sparse_iterator(bucket + 1, nullptr, array_size_type{0});
                        }

                        ++bucket;
                    } while (bucket->empty());

                    if (!bucket->has_hole()) {
                        // one run from the first slot to the last
                        return sparse_iterator(bucket, bucket->begin(), run_type{static_cast<std::uint32_t>(bucket->size() - 1), sparse_array::bitmap_nb_bits});
                    }
                    auto const [slot, run] = bucket->run_from(0);
                    return sparse_iterator(bucket, slot, run);
                }

                /**
                 * With holes: true if the iterator holds the run of its element (`sparse_array::run_from`), or if it
                 * knows only the index of the element, as after a lookup. For the debug checks.
                 */
                [[nodiscard]] constexpr bool run_is_plausible() const noexcept {
                    auto const index = bucket_->index_of(slot_);
                    auto const run = bucket_->run_from(index).second;
                    return (run_.nb_following == run.nb_following && run_.next_after == run.next_after)
                           || (run_.nb_following == 0 && run_.next_after == index);
                }

                /**
                 * Without holes: moves to the first element of the next group that is not empty, or to the end.
                 */
                constexpr void to_next_group() noexcept {
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
                /// without holes: the end of the elements of `*bucket_`, nullptr if `slot_` is nullptr
                [[no_unique_address]] std::conditional_t<has_holes, unused_member, slot_pointer> slot_end_ = {};
                /// with holes: the run of the element in `*bucket_`
                [[no_unique_address]] std::conditional_t<has_holes, run_type, unused_member> run_ = {};
            };

            /**
             * Creates a table with at least `bucket_count` buckets. The bucket count is rounded up to a power of two.
             * @throws std::length_error if `bucket_count` is larger than `max_bucket_count()`
             */
            template<typename Alloc>
            requires std::constructible_from<slot_allocator_type, Alloc const &>
            constexpr sparse_hash(size_type bucket_count, Hash const &hash, KeyEqual const &equal, Alloc const &alloc, float max_load_factor)
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

            constexpr ~sparse_hash() {
                destroy_buckets();
            }

            constexpr sparse_hash(sparse_hash const &other)
                : sparse_hash(other, slot_allocator_traits::select_on_container_copy_construction(other.alloc_)) {
            }

            /**
             * Copies `other`, with storage from `alloc`.
             */
            template<typename Alloc>
            requires std::constructible_from<slot_allocator_type, Alloc const &>
            constexpr sparse_hash(sparse_hash const &other, Alloc const &alloc)
                : alloc_(alloc),
                  hash_(other.hash_),
                  key_equal_(other.key_equal_),
                  bucket_count_(other.bucket_count_),
                  nb_elements_(other.nb_elements_),
                  first_nonempty_group_(other.first_nonempty_group_),
                  nb_deleted_buckets_(other.nb_deleted_buckets_),
                  load_threshold_rehash_(other.load_threshold_rehash_),
                  load_threshold_clear_deleted_(other.load_threshold_clear_deleted_),
                  max_load_factor_(other.max_load_factor_) {
                copy_buckets_from(other);
            }

            constexpr sparse_hash(sparse_hash &&other) noexcept(std::is_nothrow_move_constructible_v<slot_allocator_type>
                                                                && std::is_nothrow_move_constructible_v<Hash>
                                                                && std::is_nothrow_move_constructible_v<KeyEqual>)
                : alloc_(std::move(other.alloc_)),
                  hash_(std::move(other.hash_)),
                  key_equal_(std::move(other.key_equal_)),
                  buckets_(std::exchange(other.buckets_, nullptr)),
                  nb_sparse_buckets_(std::exchange(other.nb_sparse_buckets_, 0)),
                  bucket_count_(std::exchange(other.bucket_count_, 0)),
                  nb_elements_(std::exchange(other.nb_elements_, 0)),
                  first_nonempty_group_(std::exchange(other.first_nonempty_group_, 0)),
                  nb_deleted_buckets_(std::exchange(other.nb_deleted_buckets_, 0)),
                  load_threshold_rehash_(std::exchange(other.load_threshold_rehash_, 0)),
                  load_threshold_clear_deleted_(std::exchange(other.load_threshold_clear_deleted_, 0)),
                  max_load_factor_(other.max_load_factor_) {
            }

            /**
             * Moves `other` into a table with storage from `alloc`. If `alloc` is not equal to the allocator of
             * `other`, the elements are moved one by one, or copied if their move constructor can throw, and `other`
             * is left empty. The hash function and the key equality are copied, so that `other` still finds its
             * elements if moving them throws.
             */
            template<typename Alloc>
            requires std::constructible_from<slot_allocator_type, Alloc const &>
            constexpr sparse_hash(sparse_hash &&other, Alloc const &alloc)
                : alloc_(alloc),
                  hash_(other.hash_),
                  key_equal_(other.key_equal_),
                  bucket_count_(other.bucket_count_),
                  nb_elements_(other.nb_elements_),
                  first_nonempty_group_(other.first_nonempty_group_),
                  nb_deleted_buckets_(other.nb_deleted_buckets_),
                  load_threshold_rehash_(other.load_threshold_rehash_),
                  load_threshold_clear_deleted_(other.load_threshold_clear_deleted_),
                  max_load_factor_(other.max_load_factor_) {
                if (allocator_is_always_equal || alloc_ == other.alloc_) {
                    buckets_ = std::exchange(other.buckets_, nullptr);
                    nb_sparse_buckets_ = std::exchange(other.nb_sparse_buckets_, 0);
                } else {
                    move_buckets_from(other);
                    other.destroy_buckets();
                }
                other.reset_to_empty();
            }

            constexpr sparse_hash &operator=(sparse_hash const &other) {
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
                first_nonempty_group_ = other.first_nonempty_group_;
                nb_deleted_buckets_ = other.nb_deleted_buckets_;
                load_threshold_rehash_ = other.load_threshold_rehash_;
                load_threshold_clear_deleted_ = other.load_threshold_clear_deleted_;
                max_load_factor_ = other.max_load_factor_;

                return *this;
            }

            constexpr sparse_hash &operator=(sparse_hash &&other) noexcept((propagate_on_move_assignment || allocator_is_always_equal)
                                                                           && std::is_nothrow_move_assignable_v<Hash>
                                                                           && std::is_nothrow_move_assignable_v<KeyEqual>) {
                if (this == &other) {
                    return *this;
                }

                destroy_buckets();
                reset_to_empty();

                if constexpr (propagate_on_move_assignment || allocator_is_always_equal) {
                    take_storage_from(other);
                } else if (alloc_ == other.alloc_) {
                    take_storage_from(other);
                } else {
                    // The elements first: `other` keeps its functors, so that it still finds its elements if moving
                    // them throws. On an exception *this stays empty. If a functor move throws after the elements
                    // were moved, `other` keeps them in a moved-from state, and with a moved-from hash function if
                    // the move of the key equality throws.
                    move_buckets_from(other);
                    try {
                        hash_ = std::move(other.hash_);
                        key_equal_ = std::move(other.key_equal_);
                    } catch (...) {
                        destroy_buckets();
                        throw;
                    }
                    other.destroy_buckets();
                }

                bucket_count_ = other.bucket_count_;
                nb_elements_ = other.nb_elements_;
                first_nonempty_group_ = other.first_nonempty_group_;
                nb_deleted_buckets_ = other.nb_deleted_buckets_;
                load_threshold_rehash_ = other.load_threshold_rehash_;
                load_threshold_clear_deleted_ = other.load_threshold_clear_deleted_;
                max_load_factor_ = other.max_load_factor_;

                other.reset_to_empty();

                return *this;
            }

            [[nodiscard]] constexpr allocator_type get_allocator() const noexcept {
                return allocator_type(alloc_);
            }

            /*
             * Iterators
             */
            [[nodiscard]] constexpr iterator begin() noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(first_nonempty_group_is_plausible());
                sparse_array *const bucket = buckets_begin() + first_nonempty_group_;
                sparse_array *const last = buckets_end();

                if constexpr (has_holes) {
                    return bucket != last ? iterator(bucket, bucket->first_value_index()) : end();
                } else {
                    return iterator(bucket, bucket != last ? bucket->begin() : nullptr);
                }
            }

            [[nodiscard]] constexpr const_iterator begin() const noexcept {
                return cbegin();
            }

            [[nodiscard]] constexpr const_iterator cbegin() const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(first_nonempty_group_is_plausible());
                sparse_array const *const bucket = buckets_begin() + first_nonempty_group_;
                sparse_array const *const last = buckets_end();

                if constexpr (has_holes) {
                    return bucket != last ? const_iterator(bucket, bucket->first_value_index()) : cend();
                } else {
                    return const_iterator(bucket, bucket != last ? bucket->cbegin() : nullptr);
                }
            }

            [[nodiscard]] constexpr iterator end() noexcept {
                if constexpr (has_holes) {
                    return iterator(buckets_end(), nullptr, array_size_type{0});
                } else {
                    return iterator(buckets_end(), nullptr);
                }
            }

            [[nodiscard]] constexpr const_iterator end() const noexcept {
                return cend();
            }

            [[nodiscard]] constexpr const_iterator cend() const noexcept {
                if constexpr (has_holes) {
                    return const_iterator(buckets_end(), nullptr, array_size_type{0});
                } else {
                    return const_iterator(buckets_end(), nullptr);
                }
            }

            /*
             * Capacity
             */
            [[nodiscard]] constexpr bool empty() const noexcept {
                return nb_elements_ == 0;
            }

            [[nodiscard]] constexpr size_type size() const noexcept {
                return nb_elements_;
            }

            /**
             * @return the number of elements that `max_bucket_count()` buckets hold at the current maximum load factor,
             * or less if the allocator cannot provide as many elements
             */
            [[nodiscard]] constexpr size_type max_size() const noexcept {
                return std::min<size_type>(slot_allocator_traits::max_size(alloc_), rehash_threshold(max_bucket_count()));
            }

            /*
             * Modifiers
             */
            constexpr void clear() noexcept {
                for (sparse_array &bucket : buckets()) {
                    bucket.clear(alloc_);
                }

                nb_elements_ = 0;
                first_nonempty_group_ = nb_sparse_buckets_;
                nb_deleted_buckets_ = 0;
            }

            constexpr std::pair<iterator, bool> insert(value_type const &value) {
                return insert_impl(Policy::key_of_value(value), value);
            }

            constexpr std::pair<iterator, bool> insert(value_type &&value) {
                return insert_impl(Policy::key_of_value(value), std::move(value));
            }

            /**
             * Inserts a set element whose key compares equal to `key`. The element is constructed from `key`
             * only if it is inserted.
             */
            template<typename K>
            constexpr std::pair<iterator, bool> insert_key(K &&key) {
                return insert_impl(key, std::forward<K>(key));
            }

            template<typename V>
            constexpr iterator insert_hint(const_iterator hint, V &&value) {
                if (hint != cend() && compare_keys(key_of(hint), Policy::key_of_value(value))) {
                    return mutable_iterator(hint);
                }

                return insert(std::forward<V>(value)).first;
            }

            template<LegacyInputIterator InputIt>
            constexpr void insert(InputIt first, InputIt last) {
                if constexpr (std::forward_iterator<InputIt>) {
                    reserve_for_insertion(static_cast<std::size_t>(std::distance(first, last)));
                }

                for (; first != last; ++first) {
                    emplace_value(*first);
                }
            }

            /**
             * Inserts every element of `range` whose key is not in the table yet.
             */
            template<std::ranges::input_range R>
            constexpr void insert_range(R &&range) {
                if constexpr (std::ranges::sized_range<R>) {
                    reserve_for_insertion(static_cast<std::size_t>(std::ranges::size(range)));
                } else if constexpr (std::ranges::forward_range<R>) {
                    reserve_for_insertion(static_cast<std::size_t>(std::ranges::distance(range)));
                }

                for (auto &&element : range) {
                    emplace_value(std::forward<decltype(element)>(element));
                }
            }

            template<typename K, typename M>
            constexpr std::pair<iterator, bool> insert_or_assign(K &&key, M &&obj) {
                auto it = try_emplace(std::forward<K>(key), std::forward<M>(obj));
                if (!it.second) {
                    Policy::mapped(*it.first.slot_) = std::forward<M>(obj);
                }

                return it;
            }

            template<typename K, typename M>
            constexpr iterator insert_or_assign_hint(const_iterator hint, K &&key, M &&obj) {
                if (hint != cend() && compare_keys(key_of(hint), key)) {
                    auto it = mutable_iterator(hint);
                    Policy::mapped(*it.slot_) = std::forward<M>(obj);

                    return it;
                }

                return insert_or_assign(std::forward<K>(key), std::forward<M>(obj)).first;
            }

            /**
             * Constructs a `value_type` from `args` and inserts it if its key is not in the table yet. The `value_type`
             * is constructed with the allocator of the table, as the stored element is, so that a scoped or a
             * polymorphic allocator passes itself on to the key and the mapped value.
             */
            template<typename... Args>
            constexpr std::pair<iterator, bool> emplace(Args &&...args) {
                value_allocator_type alloc(alloc_);
                value_holder<value_type, value_allocator_type> value(alloc, std::forward<Args>(args)...);
                return insert(std::move(value.get()));
            }

            template<typename... Args>
            constexpr iterator emplace_hint(const_iterator hint, Args &&...args) {
                value_allocator_type alloc(alloc_);
                value_holder<value_type, value_allocator_type> value(alloc, std::forward<Args>(args)...);
                return insert_hint(hint, std::move(value.get()));
            }

            /**
             * Inserts an element with key `key` and mapped value constructed from `args`, if `key` is not in the
             * map. Otherwise neither `key` nor `args` are used.
             */
            template<typename K, typename... Args>
            constexpr std::pair<iterator, bool> try_emplace(K &&key, Args &&...args) {
                return insert_impl(key, std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<Args>(args)...));
            }

            template<typename K, typename... Args>
            constexpr iterator try_emplace_hint(const_iterator hint, K &&key, Args &&...args) {
                if (hint != cend() && compare_keys(key_of(hint), key)) {
                    return mutable_iterator(hint);
                }

                return try_emplace(std::forward<K>(key), std::forward<Args>(args)...).first;
            }

            /**
             * Erases the element at `pos`. Does not throw: with holes the element is destroyed in place, and otherwise
             * the elements after it in its group are moved, which does not throw.
             * @return iterator to the element after the erased one
             */
            constexpr iterator erase(iterator pos) {
                DICE_UNORDERED_SPARSE_ASSERT(pos != end() && nb_elements_ > 0);
                sparse_array *bucket = pos.bucket_;
                if constexpr (has_holes) {
                    // An iterator that a lookup, an insertion, `begin()` or an erasure returned knows the index of its
                    // element. Otherwise the index is counted from the slot.
                    auto const known = static_cast<array_size_type>(pos.run_.next_after);
                    auto const index = known != sparse_array::bitmap_nb_bits && bucket->has_value(known) && bucket->value(known) == pos.slot_
                                           ? known
                                           : bucket->index_of(pos.slot_);
                    bucket->erase_in_place(alloc_, index);
                    --nb_elements_;
                    ++nb_deleted_buckets_;

                    auto const next_index = bucket->next_value_index(index);
                    if (next_index != sparse_array::bitmap_nb_bits) {
                        return iterator(bucket, next_index);
                    }
                } else {
                    slot_type *const next_slot = bucket->erase(alloc_, pos.slot_);
                    --nb_elements_;
                    ++nb_deleted_buckets_;

                    if (next_slot != bucket->end()) {
                        return iterator(bucket, next_slot);
                    }
                }

                // no element follows the erased one in its group
                if (nb_elements_ == 0) {
                    first_nonempty_group_ = nb_sparse_buckets_;
                    return end();
                }
                bool const emptied_first_group = group_index(bucket) == first_nonempty_group_ && bucket->empty();

                sparse_array *const last = buckets_end();
                do {
                    ++bucket;
                } while (bucket != last && bucket->empty());

                if (emptied_first_group) {
                    first_nonempty_group_ = group_index(bucket);
                }

                if constexpr (has_holes) {
                    return bucket == last ? end() : iterator(bucket, bucket->first_value_index());
                } else {
                    return bucket == last ? end() : iterator(bucket, bucket->begin());
                }
            }

            constexpr iterator erase(const_iterator pos) {
                return erase(mutable_iterator(pos));
            }

            constexpr iterator erase(const_iterator first, const_iterator last) {
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
            constexpr size_type erase(K const &key) {
                return erase_impl(key, hash_key(key));
            }

            /**
             * @param hash `hash_function()(key)`
             */
            template<typename K>
            constexpr size_type erase(K const &key, std::size_t hash) {
                return erase_impl(key, mixed_hash<Hash>(hash));
            }

            /**
             * Moves every element of `source` whose key is not in this table into this table, and erases it from
             * `source`. An element is copied instead if its move constructor can throw. If an exception is thrown,
             * the element that was being merged is still in `source` and not in this table: the erase from `source`
             * comes last and does not throw. A rehash of this table that throws can leave this table empty, see
             * `rehash_impl`.
             */
            template<typename OtherHash>
            constexpr void merge(OtherHash &source) {
                for (auto it = source.begin(); it != source.end();) {
                    auto &slot = *it.slot_;
                    std::size_t const hash = hash_key(Policy::key(slot));
                    if (find_impl(Policy::key(slot), hash) != cend()) {
                        ++it;
                        continue;
                    }

                    // A rehash moves a new element before it moves the others. Here it runs before the element is
                    // touched, so that the element stays in `source` if the rehash throws.
                    make_room_for_one_insertion();
                    insert_impl_hashed(Policy::key(slot), hash, std::move_if_noexcept(slot));
                    it = source.erase(it);
                }
            }

            constexpr void swap(sparse_hash &other) noexcept(std::is_nothrow_swappable_v<Hash>
                                                             && std::is_nothrow_swappable_v<KeyEqual>) {
                using std::swap;

                // The functors first: if one of them throws, the storage and the allocators are not swapped. If the key
                // equality throws, the hash functions are swapped back, so that each table finds its elements again. If
                // that swap throws too, the tables stay inconsistent.
                swap(hash_, other.hash_);
                if constexpr (std::is_nothrow_swappable_v<KeyEqual>) {
                    swap(key_equal_, other.key_equal_);
                } else {
                    try {
                        swap(key_equal_, other.key_equal_);
                    } catch (...) {
                        swap(hash_, other.hash_);
                        throw;
                    }
                }

                if constexpr (propagate_on_swap) {
                    swap(alloc_, other.alloc_);
                } else {
                    DICE_UNORDERED_SPARSE_ASSERT(alloc_ == other.alloc_);
                }

                swap(buckets_, other.buckets_);
                swap(nb_sparse_buckets_, other.nb_sparse_buckets_);
                swap(bucket_count_, other.bucket_count_);
                swap(nb_elements_, other.nb_elements_);
                swap(first_nonempty_group_, other.first_nonempty_group_);
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
            requires Policy::is_map
            constexpr auto &at(this Self &self, K const &key) {
                return self.at_mixed(key, self.hash_key(key));
            }

            template<typename Self, typename K>
            requires Policy::is_map
            constexpr auto &at(this Self &self, K const &key, std::size_t hash) {
                return self.at_mixed(key, mixed_hash<Hash>(hash));
            }

            template<typename K>
            requires Policy::is_map
            constexpr auto &operator[](K &&key) {
                return Policy::mapped(*try_emplace(std::forward<K>(key)).first.slot_);
            }

            template<typename K>
            [[nodiscard]] constexpr bool contains(K const &key) const {
                return find_impl(key, hash_key(key)) != cend();
            }

            template<typename K>
            [[nodiscard]] constexpr bool contains(K const &key, std::size_t hash) const {
                return find_impl(key, mixed_hash<Hash>(hash)) != cend();
            }

            template<typename K>
            [[nodiscard]] constexpr size_type count(K const &key) const {
                return contains(key) ? size_type{1} : size_type{0};
            }

            template<typename K>
            [[nodiscard]] constexpr size_type count(K const &key, std::size_t hash) const {
                return contains(key, hash) ? size_type{1} : size_type{0};
            }

            /**
             * @return `iterator` for a non-const table, `const_iterator` for a const table
             */
            template<typename Self, typename K>
            [[nodiscard]] constexpr auto find(this Self &self, K const &key) {
                return self.find_mixed(key, self.hash_key(key));
            }

            template<typename Self, typename K>
            [[nodiscard]] constexpr auto find(this Self &self, K const &key, std::size_t hash) {
                return self.find_mixed(key, mixed_hash<Hash>(hash));
            }

            template<typename Self, typename K>
            [[nodiscard]] constexpr auto equal_range(this Self &self, K const &key) {
                return self.equal_range_mixed(key, self.hash_key(key));
            }

            template<typename Self, typename K>
            [[nodiscard]] constexpr auto equal_range(this Self &self, K const &key, std::size_t hash) {
                return self.equal_range_mixed(key, mixed_hash<Hash>(hash));
            }

            /*
             * Bucket interface
             */
            [[nodiscard]] constexpr size_type bucket_count() const noexcept {
                return bucket_count_;
            }

            /**
             * @return the largest power of two that `size_type` and `std::size_t` can hold, or less if the allocator
             * cannot provide enough `sparse_array`s of `sparse_array::bitmap_nb_bits` buckets each
             */
            [[nodiscard]] constexpr size_type max_bucket_count() const noexcept {
                constexpr std::uintmax_t largest_count = std::min<std::uintmax_t>(std::numeric_limits<size_type>::max(), std::numeric_limits<std::size_t>::max());
                std::uintmax_t const max_nb_sparse_buckets = bucket_allocator_traits::max_size(bucket_allocator_type(alloc_));
                std::uintmax_t const count = max_nb_sparse_buckets > largest_count / sparse_array::bitmap_nb_bits
                                                 ? largest_count
                                                 : max_nb_sparse_buckets * sparse_array::bitmap_nb_bits;
                return static_cast<size_type>(std::bit_floor(count));
            }

            /*
             * Hash policy
             */
            [[nodiscard]] constexpr float load_factor() const noexcept {
                if (bucket_count() == 0) {
                    return 0;
                }

                return static_cast<float>(nb_elements_) / static_cast<float>(bucket_count());
            }

            [[nodiscard]] constexpr float max_load_factor() const noexcept {
                return max_load_factor_;
            }

            /**
             * Sets the maximum load factor, clamped to [0.1, 0.8].
             */
            constexpr void max_load_factor(float ml) noexcept {
                max_load_factor_ = std::max(min_max_load_factor, std::min(ml, max_max_load_factor));
                load_threshold_rehash_ = rehash_threshold(bucket_count());

                float const max_load_factor_with_deleted_buckets = max_load_factor_ + 0.5f * (1.0f - max_load_factor_);
                DICE_UNORDERED_SPARSE_ASSERT(max_load_factor_with_deleted_buckets > 0.0f && max_load_factor_with_deleted_buckets <= 1.0f);
                load_threshold_clear_deleted_ = static_cast<size_type>(static_cast<float>(bucket_count()) * max_load_factor_with_deleted_buckets);
            }

            /**
             * Rebuilds the table with at least `count` buckets, and with room for its elements. Does nothing if the
             * bucket count stays the same and no bucket is marked as deleted.
             */
            constexpr void rehash(size_type count) {
                count = std::max(count, bucket_count_for(size()));
                if (nb_deleted_buckets_ == 0 && rounded_bucket_count(count) == bucket_count_) {
                    return;
                }
                rehash_impl(count);
            }

            constexpr void reserve(size_type count) {
                rehash(bucket_count_for(count));
            }

            /*
             * Observers
             */
            [[nodiscard]] constexpr hasher hash_function() const {
                return hash_;
            }

            [[nodiscard]] constexpr key_equal key_eq() const {
                return key_equal_;
            }

            /*
             * Other
             */
            [[nodiscard]] constexpr iterator mutable_iterator(const_iterator pos) noexcept {
                if constexpr (has_holes) {
                    return iterator(const_cast<sparse_array *>(pos.bucket_), const_cast<slot_type *>(pos.slot_), pos.run_);
                } else {
                    return iterator(const_cast<sparse_array *>(pos.bucket_), const_cast<slot_type *>(pos.slot_), const_cast<slot_type *>(pos.slot_end_));
                }
            }

        private:
            template<typename Self, typename K>
            [[nodiscard]] constexpr auto find_mixed(this Self &self, K const &key, std::size_t mixed) {
                if constexpr (std::is_const_v<Self>) {
                    return self.find_impl(key, mixed);
                } else {
                    return self.mutable_iterator(self.find_impl(key, mixed));
                }
            }

            template<typename Self, typename K>
            [[nodiscard]] constexpr auto &at_mixed(this Self &self, K const &key, std::size_t mixed) {
                auto const it = self.find_mixed(key, mixed);
                if (it == self.end()) {
                    throw std::out_of_range("Couldn't find key.");
                }

                return Policy::mapped(*it.slot_);
            }

            template<typename Self, typename K>
            [[nodiscard]] constexpr auto equal_range_mixed(this Self &self, K const &key, std::size_t mixed) {
                auto const it = self.find_mixed(key, mixed);
                return std::pair{it, (it == self.end()) ? it : std::next(it)};
            }

            [[nodiscard]] constexpr sparse_array *buckets_begin() noexcept {
                return std::to_address(buckets_);
            }

            [[nodiscard]] constexpr sparse_array const *buckets_begin() const noexcept {
                return std::to_address(buckets_);
            }

            [[nodiscard]] constexpr sparse_array *buckets_end() noexcept {
                return buckets_begin() + nb_sparse_buckets_;
            }

            [[nodiscard]] constexpr sparse_array const *buckets_end() const noexcept {
                return buckets_begin() + nb_sparse_buckets_;
            }

            [[nodiscard]] constexpr std::ranges::subrange<sparse_array *> buckets() noexcept {
                return {buckets_begin(), buckets_end()};
            }

            [[nodiscard]] constexpr std::ranges::subrange<sparse_array const *> buckets() const noexcept {
                return {buckets_begin(), buckets_end()};
            }

            /**
             * @return the index of the group `bucket` in the bucket array
             */
            [[nodiscard]] constexpr size_type group_index(sparse_array const *bucket) const noexcept {
                return static_cast<size_type>(bucket - buckets_begin());
            }

            /**
             * True if `first_nonempty_group_` is `nb_sparse_buckets_` for a table without elements, and otherwise the
             * index of a group that holds an element. The groups before it are not checked, so that the check is
             * constant time like `begin()`.
             */
            [[nodiscard]] constexpr bool first_nonempty_group_is_plausible() const noexcept {
                if (nb_elements_ == 0) {
                    return first_nonempty_group_ == nb_sparse_buckets_;
                }
                return first_nonempty_group_ < nb_sparse_buckets_ && !buckets_begin()[first_nonempty_group_].empty();
            }

            /**
             * Moves `first_nonempty_group_` to the next group that holds an element, after an erase emptied the first
             * one. Reads the headers of the groups in between. Without elements it is `nb_sparse_buckets_` at once.
             */
            constexpr void skip_empty_first_groups() noexcept {
                if (nb_elements_ == 0) {
                    first_nonempty_group_ = nb_sparse_buckets_;
                    return;
                }

                sparse_array const *const raw_buckets = buckets_begin();
                while (raw_buckets[first_nonempty_group_].empty()) {
                    ++first_nonempty_group_;
                }
            }

            template<typename K1, typename K2>
            [[nodiscard]] constexpr bool compare_keys(K1 const &key1, K2 const &key2) const {
                return key_equal_(key1, key2);
            }

            [[nodiscard]] static constexpr key_type const &key_of(const_iterator it) noexcept {
                return Policy::key(*it.slot_);
            }

            /**
             * @return `hash_function()(key)`, mixed (see `hash_is_avalanching`)
             */
            template<typename K>
            [[nodiscard]] constexpr std::size_t hash_key(K const &key) const {
                return mixed_hash<Hash>(hash_(key));
            }

            /**
             * @param hash a mixed hash
             * @return the bucket of `hash`. The table must have buckets.
             */
            [[nodiscard]] constexpr std::size_t bucket_for_hash(std::size_t hash) const noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(bucket_count_ > 0);
                return hash & (bucket_count_ - 1);
            }

            /**
             * @return the bucket after `ibucket` on the quadratic probe sequence, at probe number `iprobe`
             */
            [[nodiscard]] constexpr std::size_t next_bucket(std::size_t ibucket, std::size_t iprobe) const noexcept {
                return (ibucket + iprobe) & (bucket_count_ - 1);
            }

            /**
             * @return `bucket_count` rounded up to a power of two, or 0 for 0
             * @throws std::length_error if the result is larger than `max_bucket_count()`
             */
            [[nodiscard]] constexpr size_type rounded_bucket_count(size_type bucket_count) const {
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
             * @return the maximum that the number of elements can reach in a table with `bucket_count` buckets
             * before a rehash grows the table
             */
            [[nodiscard]] constexpr size_type rehash_threshold(size_type bucket_count) const noexcept {
                return static_cast<size_type>(static_cast<float>(bucket_count) * max_load_factor_);
            }

            /**
             * @return the smallest bucket count that holds `nb_elements` elements without a rehash, or 0 for 0
             * @throws std::length_error if the result is larger than `max_bucket_count()`
             */
            [[nodiscard]] constexpr size_type bucket_count_for(size_type nb_elements) const {
                if (nb_elements == 0) {
                    return 0;
                }

                // checked before the conversion to `size_type`, which can be narrower than `std::size_t`
                std::size_t const min_bucket_count = ceil_to_size(static_cast<double>(nb_elements) / static_cast<double>(max_load_factor_));
                if (std::cmp_greater(min_bucket_count, max_bucket_count())) {
                    throw std::length_error("The hash table exceeds its maximum size.");
                }

                auto count = rounded_bucket_count(static_cast<size_type>(min_bucket_count));
                // the division rounds, so `count` can be one doubling too small
                if (rehash_threshold(count) < nb_elements) {
                    if (count > max_bucket_count() / 2) {
                        throw std::length_error("The hash table exceeds its maximum size.");
                    }
                    count *= 2;
                }
                return count;
            }

            /**
             * @return the bucket count after the next growth, twice the current one, at least 2
             * @throws std::length_error if the table cannot grow
             */
            [[nodiscard]] constexpr size_type next_bucket_count() const {
                if (bucket_count_ > max_bucket_count() / 2) {
                    throw std::length_error("The hash table exceeds its maximum size.");
                }
                return bucket_count_ == 0 ? 2 : bucket_count_ * 2;
            }

            /**
             * Allocates `nb_sparse_buckets` empty `sparse_array`s and makes them the buckets of the table.
             */
            constexpr void allocate_buckets(std::size_t nb_sparse_buckets) {
                DICE_UNORDERED_SPARSE_ASSERT(buckets_ == nullptr && nb_sparse_buckets > 0);
                bucket_allocator_type bucket_alloc(alloc_);
                bucket_pointer const new_buckets = bucket_allocator_traits::allocate(bucket_alloc, nb_sparse_buckets);
                sparse_array *const raw_buckets = std::to_address(new_buckets);
                for (std::size_t i = 0; i < nb_sparse_buckets; ++i) {
                    std::construct_at(raw_buckets + i);
                }
                raw_buckets[nb_sparse_buckets - 1].set_as_last();

                buckets_ = new_buckets;
                nb_sparse_buckets_ = nb_sparse_buckets;
                first_nonempty_group_ = nb_sparse_buckets;
            }

            /**
             * Destroys all elements and frees the bucket array. The counters stay as they are.
             */
            constexpr void destroy_buckets() noexcept {
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
            constexpr void reset_to_empty() noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(buckets_ == nullptr && nb_sparse_buckets_ == 0);
                bucket_count_ = 0;
                nb_elements_ = 0;
                first_nonempty_group_ = 0;
                nb_deleted_buckets_ = 0;
                load_threshold_rehash_ = 0;
                load_threshold_clear_deleted_ = 0;
            }

            /**
             * Builds a bucket array for this table from the buckets of `other`, with `make(other_bucket)`
             * constructing each new bucket in place. On an exception the table has no buckets.
             */
            template<typename OtherHash, typename Make>
            constexpr void build_buckets_from(OtherHash &other, Make const &make) {
                DICE_UNORDERED_SPARSE_ASSERT(buckets_ == nullptr && nb_sparse_buckets_ == 0);
                if (other.nb_sparse_buckets_ == 0) {
                    return;
                }

                bucket_allocator_type bucket_alloc(alloc_);
                bucket_pointer const new_buckets = bucket_allocator_traits::allocate(bucket_alloc, other.nb_sparse_buckets_);
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

                DICE_UNORDERED_SPARSE_ASSERT(raw_buckets[nb_constructed - 1].last());
                buckets_ = new_buckets;
                nb_sparse_buckets_ = other.nb_sparse_buckets_;
            }

            constexpr void copy_buckets_from(sparse_hash const &other) {
                build_buckets_from(other, [this](sparse_array *target, sparse_array const &source) {
                    std::construct_at(target, source, alloc_);
                });
            }

            /**
             * Takes the functors, the buckets and, if it propagates on move assignment, the allocator of `other`.
             * Expects that this table has no buckets and that its allocator can free the buckets of `other`. The
             * functors first: if one of them throws, this table has no buckets and `other` keeps its elements. If the
             * move of the key equality throws, `other` keeps them with a moved-from hash function, which may not find
             * them.
             */
            constexpr void take_storage_from(sparse_hash &other) {
                hash_ = std::move(other.hash_);
                key_equal_ = std::move(other.key_equal_);

                if constexpr (propagate_on_move_assignment) {
                    alloc_ = std::move(other.alloc_);
                }

                buckets_ = std::exchange(other.buckets_, nullptr);
                nb_sparse_buckets_ = std::exchange(other.nb_sparse_buckets_, 0);
            }

            /**
             * Moves the elements of `other` into a new bucket array of this table, or copies the elements whose move
             * constructor can throw. `other` keeps its buckets and its elements, the moved ones in a moved-from
             * state.
             */
            constexpr void move_buckets_from(sparse_hash &other) {
                build_buckets_from(other, [this](sparse_array *target, sparse_array &source) {
                    std::construct_at(target, std::move(source), alloc_);
                });
            }

            /**
             * Reserves room for `nb_elements_to_insert` more elements if the table does not have it. The reservation
             * is a hint, because the elements can have equal keys or keys that are in the table already. So it
             * reserves nothing if `size() + nb_elements_to_insert` exceeds `max_size()`.
             */
            constexpr void reserve_for_insertion(std::size_t nb_elements_to_insert) {
                // a lower max load factor can put the threshold below the size, and the size above `max_size()`
                size_type const nb_free_buckets = load_threshold_rehash_ > size() ? load_threshold_rehash_ - size() : 0;
                if (nb_elements_to_insert == 0 || nb_free_buckets >= nb_elements_to_insert) {
                    return;
                }
                size_type const max_nb_elements = max_size();
                if (std::cmp_less_equal(nb_elements_to_insert, max_nb_elements)
                    && size() <= max_nb_elements - static_cast<size_type>(nb_elements_to_insert)) {
                    reserve(size() + static_cast<size_type>(nb_elements_to_insert));
                }
            }

            /**
             * Grows the table, or removes the deleted-bucket markers, if the insertion of one new element would do
             * it. After it, the insertion of one new element does not rehash.
             */
            constexpr void make_room_for_one_insertion() {
                while (size() >= load_threshold_rehash_) {
                    rehash_impl(next_bucket_count());
                }
                if (size() + nb_deleted_buckets_ >= load_threshold_clear_deleted_) {
                    clear_deleted_buckets();
                }
            }

            /**
             * Inserts an element of a range: `value_type` directly, anything else through `emplace`.
             */
            template<typename V>
            constexpr void emplace_value(V &&value) {
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
            constexpr std::pair<iterator, bool> insert_impl(K const &key, Args &&...args) {
                return insert_impl_hashed(key, hash_key(key), std::forward<Args>(args)...);
            }

            /**
             * `insert_impl` with the mixed hash of `key`.
             */
            template<typename K, typename... Args>
            constexpr std::pair<iterator, bool> insert_impl_hashed(K const &key, std::size_t hash, Args &&...args) {
                if (nb_sparse_buckets_ == 0) {
                    return insert_new(hash, 0, 0, false, std::forward<Args>(args)...);
                }

                std::size_t ibucket = bucket_for_hash(hash);

                bool found_first_deleted_bucket = false;
                std::size_t sparse_ibucket_first_deleted = 0;
                array_size_type index_in_sparse_bucket_first_deleted = 0;

                sparse_array *const raw_buckets = buckets_begin();
                for (std::size_t probe = 0;;) {
                    std::size_t const sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto const index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);
                    sparse_array &bucket = raw_buckets[sparse_ibucket];

                    if (bucket.has_value(index_in_sparse_bucket)) {
                        slot_type *const slot = bucket.value(index_in_sparse_bucket);
                        if (compare_keys(key, Policy::key(*slot))) {
                            if constexpr (has_holes) {
                                return {iterator(&bucket, slot, index_in_sparse_bucket), false};
                            } else {
                                return {iterator(&bucket, slot), false};
                            }
                        }
                    } else if (bucket.has_deleted_value(index_in_sparse_bucket) && probe < bucket_count_) {
                        if (!found_first_deleted_bucket) {
                            found_first_deleted_bucket = true;
                            sparse_ibucket_first_deleted = sparse_ibucket;
                            index_in_sparse_bucket_first_deleted = index_in_sparse_bucket;
                        }
                    } else if (found_first_deleted_bucket) {
                        return insert_new(hash, sparse_ibucket_first_deleted, index_in_sparse_bucket_first_deleted, true, std::forward<Args>(args)...);
                    } else {
                        return insert_new(hash, sparse_ibucket, index_in_sparse_bucket, false, std::forward<Args>(args)...);
                    }

                    ++probe;
                    ibucket = next_bucket(ibucket, probe);
                }
            }

            /**
             * Inserts a new element at the given bucket, after checking the load thresholds. If a threshold is
             * reached, the table is rehashed and the insertion starts over. `hash` is the mixed hash of the key of
             * the new element.
             */
            template<typename... Args>
            constexpr std::pair<iterator, bool> insert_new(std::size_t hash, std::size_t sparse_ibucket, array_size_type index_in_sparse_bucket, bool reuses_deleted_bucket, Args &&...args) {
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
                    return insert_impl_hashed(Policy::key(new_slot.get()), hash, std::move(new_slot.get()));
                }

                sparse_array &bucket = buckets_begin()[sparse_ibucket];
                slot_type *const slot = bucket.set(alloc_, index_in_sparse_bucket, std::forward<Args>(args)...);
                ++nb_elements_;
                if (reuses_deleted_bucket) {
                    --nb_deleted_buckets_;
                }
                if (sparse_ibucket < first_nonempty_group_) {
                    first_nonempty_group_ = static_cast<size_type>(sparse_ibucket);
                }

                if constexpr (has_holes) {
                    return {iterator(&bucket, slot, index_in_sparse_bucket), true};
                } else {
                    return {iterator(&bucket, slot), true};
                }
            }

            template<typename K>
            constexpr size_type erase_impl(K const &key, std::size_t hash) {
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
                        if (compare_keys(key, Policy::key(*slot))) {
                            if constexpr (has_holes) {
                                bucket.erase_in_place(alloc_, index_in_sparse_bucket);
                            } else {
                                bucket.erase(alloc_, slot, index_in_sparse_bucket);
                            }
                            --nb_elements_;
                            ++nb_deleted_buckets_;
                            if (sparse_ibucket == first_nonempty_group_ && bucket.empty()) {
                                skip_empty_first_groups();
                            }

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
            [[nodiscard]] constexpr const_iterator find_impl(K const &key, std::size_t hash) const {
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
                        if (compare_keys(key, Policy::key(*slot))) {
                            if constexpr (has_holes) {
                                return const_iterator(&bucket, slot, index_in_sparse_bucket);
                            } else {
                                return const_iterator(&bucket, slot);
                            }
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
            constexpr void clear_deleted_buckets() {
                rehash_impl(bucket_count_);
                DICE_UNORDERED_SPARSE_ASSERT(nb_deleted_buckets_ == 0);
            }

            /**
             * True if a rehash copies the elements and frees the old groups at the end, see `rehash_impl`. These are
             * the elements whose move constructor can throw, so their groups have holes.
             */
            static constexpr bool copy_on_rehash = has_holes;

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
             * so an exception leaves it unchanged. The new groups have no holes.
             *
             * The table takes the new buckets with `swap_storage`, which cannot throw.
             */
            constexpr void rehash_impl(size_type count) {
                sparse_hash new_table(count, hash_, key_equal_, alloc_, max_load_factor_);

                if constexpr (copy_on_rehash) {
                    for (sparse_array const &bucket : buckets()) {
                        if (!bucket.has_hole()) {
                            for (slot_type const &slot : bucket) {
                                new_table.insert_on_rehash(slot);
                            }
                        } else {
                            for (auto index = bucket.first_value_index(); index != sparse_array::bitmap_nb_bits; index = bucket.next_value_index(index)) {
                                new_table.insert_on_rehash(*bucket.value(index));
                            }
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
            constexpr void swap_storage(sparse_hash &other) noexcept {
                DICE_UNORDERED_SPARSE_ASSERT(alloc_ == other.alloc_);
                using std::swap;
                swap(buckets_, other.buckets_);
                swap(nb_sparse_buckets_, other.nb_sparse_buckets_);
                swap(bucket_count_, other.bucket_count_);
                swap(nb_elements_, other.nb_elements_);
                swap(first_nonempty_group_, other.first_nonempty_group_);
                swap(nb_deleted_buckets_, other.nb_deleted_buckets_);
                swap(load_threshold_rehash_, other.load_threshold_rehash_);
                swap(load_threshold_clear_deleted_, other.load_threshold_clear_deleted_);
            }

            /**
             * Inserts an element whose key is known not to be in the table. Does not check the load thresholds.
             */
            template<typename S>
            constexpr void insert_on_rehash(S &&slot_value) {
                std::size_t ibucket = bucket_for_hash(hash_key(Policy::key(slot_value)));
                sparse_array *const raw_buckets = buckets_begin();

                for (std::size_t probe = 0;;) {
                    std::size_t const sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
                    auto const index_in_sparse_bucket = sparse_array::index_in_sparse_bucket(ibucket);
                    sparse_array &bucket = raw_buckets[sparse_ibucket];

                    if (!bucket.has_value(index_in_sparse_bucket)) {
                        bucket.set(alloc_, index_in_sparse_bucket, std::forward<S>(slot_value));
                        ++nb_elements_;
                        if (sparse_ibucket < first_nonempty_group_) {
                            first_nonempty_group_ = static_cast<size_type>(sparse_ibucket);
                        }

                        return;
                    }

                    DICE_UNORDERED_SPARSE_ASSERT(!compare_keys(Policy::key(slot_value), Policy::key(*bucket.value(index_in_sparse_bucket))));

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
             * The index of the first group that holds an element, or `nb_sparse_buckets_` if there is none. Every
             * operation that changes the table keeps it exact, so that `begin()` is constant time and writes nothing.
             */
            size_type first_nonempty_group_ = 0;

            /**
             * Number of buckets that are marked as deleted, holes included.
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

    }  // namespace detail
}  // namespace dice::unordered_sparse

/**
 * A `map_slot` uses an allocator if its key or its mapped value does.
 */
template<typename Key, typename T, typename Alloc>
struct std::uses_allocator<dice::unordered_sparse::detail::map_slot<Key, T>, Alloc>
    : std::bool_constant<std::uses_allocator_v<Key, Alloc> || std::uses_allocator_v<T, Alloc>> {
};

#endif
