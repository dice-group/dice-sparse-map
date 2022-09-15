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
#include <climits>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <tuple>
#include <utility>
#include <bit> // std::popcount

#include "dice/sparse-map/commons.hpp"
#include "dice/sparse-map/sparse_array.hpp"
#include "dice/sparse-map/vec.hpp"

namespace dice::sparse_map {
namespace detail_sparse_hash {
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
 * The strong exception guarantee only holds if `ExceptionSafety` is set to
 * `dice::sh::exception_safety::strong`.
 *
 * `ValueType` must be nothrow move constructible and/or copy constructible.
 * Behaviour is undefined if the destructor of `ValueType` throws.
 *
 *
 * The class holds its buckets in a 2-dimensional fashion. Instead of having a
 * linear `std::vector<bucket>` for [0, bucket_count) where each bucket stores
 * one value, we have a `std::vector<sparse_array>` (m_sparse_buckets_data)
 * where each `sparse_array` stores multiple values (up to
 * `sparse_array::BITMAP_NB_BITS`). To convert a one dimensional `ibucket`
 * position to a position in `std::vector<sparse_array>` and a position in
 * `sparse_array`, use respectively the methods
 * `sparse_array::sparse_ibucket(ibucket)` and
 * `sparse_array::index_in_sparse_bucket(ibucket)`.
 */
template <class ValueType, class KeySelect, class ValueSelect, class Hash,
          class KeyEqual, class Allocator, class GrowthPolicy,
          dice::sparse_map::sh::exception_safety ExceptionSafety, dice::sparse_map::sh::sparsity Sparsity,
          dice::sparse_map::sh::probing Probing>
class sparse_hash : private Allocator,
                    private Hash,
                    private KeyEqual,
                    private GrowthPolicy {
 private:

  static_assert(
      noexcept(std::declval<GrowthPolicy>().bucket_for_hash(std::size_t(0))),
      "GrowthPolicy::bucket_for_hash must be noexcept.");
  static_assert(noexcept(std::declval<GrowthPolicy>().clear()),
                "GrowthPolicy::clear must be noexcept.");

 public:
  template <bool IsConst>
  class sparse_iterator;

  using key_type = typename KeySelect::key_type;
  using value_type = ValueType;
  using hasher = Hash;
  using key_equal = KeyEqual;
  using allocator_type = Allocator;
  using reference = value_type &;
  using const_reference = const value_type &;
  using size_type = typename std::allocator_traits<allocator_type>::size_type;
  using pointer = typename std::allocator_traits<allocator_type>::pointer;
  using const_pointer = typename std::allocator_traits<allocator_type>::const_pointer;
  using difference_type = typename std::allocator_traits<allocator_type>::difference_type;
  using iterator = sparse_iterator<false>;
  using const_iterator = sparse_iterator<true>;

 private:
  using sparse_array =
      dice::sparse_map::detail_sparse_hash::sparse_array<ValueType, Allocator, Sparsity>;

  using sparse_buckets_allocator = typename std::allocator_traits<
      allocator_type>::template rebind_alloc<sparse_array>;
  using sparse_buckets_container =
      dice::sparse_map::detail_sparse_hash::vec<sparse_array, sparse_buckets_allocator>;
 public:
  /**
   * The `operator*()` and `operator->()` methods return a const reference and
   * const pointer respectively to the stored value type (`Key` for a set,
   * `std::pair<Key, T>` for a map).
   *
   * In case of a map, to get a mutable reference to the value `T` associated to
   * a key (the `.second` in the stored pair), you have to call `value()`.
   */
  template <bool IsConst>
  class sparse_iterator {
    friend class sparse_hash;

   private:
    using sparse_bucket_iterator = typename std::conditional<
        IsConst, typename sparse_buckets_container::const_iterator,
        typename sparse_buckets_container::iterator>::type;

    using sparse_array_iterator =
        typename std::conditional<IsConst,
                                  typename sparse_array::const_iterator,
                                  typename sparse_array::iterator>::type;

    /**
     * sparse_array_it should be nullptr if sparse_bucket_it ==
     * m_sparse_buckets_data.end(). (TODO better way?)
     */
    sparse_iterator(sparse_bucket_iterator sparse_bucket_it,
                    sparse_array_iterator sparse_array_it)
        : m_sparse_buckets_it(sparse_bucket_it),
          m_sparse_array_it(sparse_array_it) {}

   public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = typename std::conditional<IsConst,
                                  const typename sparse_hash::value_type,
                                  typename sparse_hash::value_type>::type;
    using difference_type = std::ptrdiff_t;
    using reference = value_type &;
    using pointer = typename std::conditional<IsConst,
                                  typename sparse_hash::const_pointer,
                                  typename sparse_hash::pointer>::type;

    sparse_iterator() noexcept {}

    // Copy constructor from iterator to const_iterator.
    template <bool TIsConst = IsConst>
    requires TIsConst
    sparse_iterator(const sparse_iterator<!TIsConst> &other) noexcept
        : m_sparse_buckets_it(other.m_sparse_buckets_it),
          m_sparse_array_it(other.m_sparse_array_it) {}

    sparse_iterator(const sparse_iterator &other) = default;
    sparse_iterator(sparse_iterator &&other) = default;
    sparse_iterator &operator=(const sparse_iterator &other) = default;
    sparse_iterator &operator=(sparse_iterator &&other) = default;

    const typename sparse_hash::key_type &key() const {
      return KeySelect()(*m_sparse_array_it);
    }

    template <class U = ValueSelect>
    requires (has_mapped_type<U> && IsConst) 
    const typename U::value_type &value() const {
      return U()(*m_sparse_array_it);
    }

    template <class U = ValueSelect>
    requires (has_mapped_type<U> && !IsConst) 
    typename U::value_type &value() {
      return U()(*m_sparse_array_it);
    }

    reference operator*() const { return *std::to_address(m_sparse_array_it); }

    pointer operator->() const { return std::to_address(m_sparse_array_it); }

    sparse_iterator &operator++() {
      tsl_sh_assert(m_sparse_array_it != nullptr);
      ++m_sparse_array_it;

      //vector iterator with fancy pointers have a problem with ->
      if (m_sparse_array_it == (*m_sparse_buckets_it).end()) {
        do {
          if ((*m_sparse_buckets_it).last()) {
            ++m_sparse_buckets_it;
            m_sparse_array_it = nullptr;
            return *this;
          }

          ++m_sparse_buckets_it;
        } while ((*m_sparse_buckets_it).empty());

        m_sparse_array_it = (*m_sparse_buckets_it).begin();
      }

      return *this;
    }

    sparse_iterator operator++(int) {
      sparse_iterator tmp(*this);
      ++*this;

      return tmp;
    }

    friend bool operator==(const sparse_iterator &lhs,
                           const sparse_iterator &rhs) {
      return lhs.m_sparse_buckets_it == rhs.m_sparse_buckets_it &&
             lhs.m_sparse_array_it == rhs.m_sparse_array_it;
    }

    friend bool operator!=(const sparse_iterator &lhs,
                           const sparse_iterator &rhs) {
      return !(lhs == rhs);
    }

   private:
    sparse_bucket_iterator m_sparse_buckets_it;
    sparse_array_iterator m_sparse_array_it;
  };

 public:
  sparse_hash(size_type bucket_count, const Hash &hash, const KeyEqual &equal,
              const Allocator &alloc, float max_load_factor)
      : Allocator(alloc),
        Hash(hash),
        KeyEqual(equal),
        GrowthPolicy(bucket_count),
        m_sparse_buckets_data(alloc),
        //         m_sparse_buckets_data(std::allocator_traits<Allocator>::rebind_alloc<sparse_buckets_container::Allocator>(alloc)),
        m_sparse_buckets(static_empty_sparse_bucket_ptr()),
        m_bucket_count(bucket_count),
        m_nb_elements(0),
        m_nb_deleted_buckets(0) {
    if (m_bucket_count > max_bucket_count()) {
      throw std::length_error("The map exceeds its maximum size.");
    }

    if (m_bucket_count > 0) {
      /*
       * We can't use the `vector(size_type count, const Allocator& alloc)`
       * constructor as it's only available in C++14 and we need to support
       * C++11. We thus must resize after using the `vector(const Allocator&
       * alloc)` constructor.
       *
       * We can't use `vector(size_type count, const T& value, const Allocator&
       * alloc)` as it requires the value T to be copyable.
       */
      m_sparse_buckets_data.resize(
          sparse_array::nb_sparse_buckets(bucket_count));
      m_sparse_buckets = m_sparse_buckets_data.data();

      tsl_sh_assert(!m_sparse_buckets_data.empty());
      m_sparse_buckets_data.back().set_as_last();
    }

    this->max_load_factor(max_load_factor);

    // Check in the constructor instead of outside of a function to avoid
    // compilation issues when value_type is not complete.
    static_assert(std::is_nothrow_move_constructible<value_type>::value ||
                      std::is_copy_constructible<value_type>::value,
                  "Key, and T if present, must be nothrow move constructible "
                  "and/or copy constructible.");
  }

  ~sparse_hash() { clear(); }

  sparse_hash(const sparse_hash &other)
      : Allocator(std::allocator_traits<
                  Allocator>::select_on_container_copy_construction(other)),
        Hash(other),
        KeyEqual(other),
        GrowthPolicy(other),
        m_sparse_buckets_data(
            std::allocator_traits<
                Allocator>::select_on_container_copy_construction(other)),
        m_bucket_count(other.m_bucket_count),
        m_nb_elements(other.m_nb_elements),
        m_nb_deleted_buckets(other.m_nb_deleted_buckets),
        m_load_threshold_rehash(other.m_load_threshold_rehash),
        m_load_threshold_clear_deleted(other.m_load_threshold_clear_deleted),
        m_max_load_factor(other.m_max_load_factor) {
    copy_buckets_from(other),
        m_sparse_buckets = m_sparse_buckets_data.empty()
                               ? static_empty_sparse_bucket_ptr()
                               : m_sparse_buckets_data.data();
  }

  sparse_hash(sparse_hash &&other) noexcept(
      std::is_nothrow_move_constructible<Allocator>::value
          &&std::is_nothrow_move_constructible<Hash>::value
              &&std::is_nothrow_move_constructible<KeyEqual>::value
                  &&std::is_nothrow_move_constructible<GrowthPolicy>::value
                      &&std::is_nothrow_move_constructible<
                          sparse_buckets_container>::value)
      : Allocator(std::move(other)),
        Hash(std::move(other)),
        KeyEqual(std::move(other)),
        GrowthPolicy(std::move(other)),
        m_sparse_buckets_data(std::move(other.m_sparse_buckets_data)),
        m_sparse_buckets(m_sparse_buckets_data.empty()
                             ? static_empty_sparse_bucket_ptr()
                             : m_sparse_buckets_data.data()),
        m_bucket_count(other.m_bucket_count),
        m_nb_elements(other.m_nb_elements),
        m_nb_deleted_buckets(other.m_nb_deleted_buckets),
        m_load_threshold_rehash(other.m_load_threshold_rehash),
        m_load_threshold_clear_deleted(other.m_load_threshold_clear_deleted),
        m_max_load_factor(other.m_max_load_factor) {
    other.GrowthPolicy::clear();
    other.m_sparse_buckets_data.clear();
    other.m_sparse_buckets = static_empty_sparse_bucket_ptr();
    other.m_bucket_count = 0;
    other.m_nb_elements = 0;
    other.m_nb_deleted_buckets = 0;
    other.m_load_threshold_rehash = 0;
    other.m_load_threshold_clear_deleted = 0;
  }

  sparse_hash &operator=(const sparse_hash &other) {
    if (this != &other) {
      clear();

      if (std::allocator_traits<
              Allocator>::propagate_on_container_copy_assignment::value) {
        Allocator::operator=(other);
      }

      Hash::operator=(other);
      KeyEqual::operator=(other);
      GrowthPolicy::operator=(other);

      if (std::allocator_traits<
              Allocator>::propagate_on_container_copy_assignment::value) {
        m_sparse_buckets_data =
            sparse_buckets_container(static_cast<const Allocator &>(other));
      } else {
        if (m_sparse_buckets_data.size() !=
            other.m_sparse_buckets_data.size()) {
          m_sparse_buckets_data =
              sparse_buckets_container(static_cast<const Allocator &>(*this));
        } else {
          m_sparse_buckets_data.clear();
        }
      }

      copy_buckets_from(other);
      m_sparse_buckets = m_sparse_buckets_data.empty()
                             ? static_empty_sparse_bucket_ptr()
                             : m_sparse_buckets_data.data();

      m_bucket_count = other.m_bucket_count;
      m_nb_elements = other.m_nb_elements;
      m_nb_deleted_buckets = other.m_nb_deleted_buckets;
      m_load_threshold_rehash = other.m_load_threshold_rehash;
      m_load_threshold_clear_deleted = other.m_load_threshold_clear_deleted;
      m_max_load_factor = other.m_max_load_factor;
    }

    return *this;
  }

  sparse_hash &operator=(sparse_hash &&other)  noexcept {
    clear();

    if (not std::allocator_traits<
            Allocator>::propagate_on_container_move_assignment::value
                    and (static_cast<Allocator &>(*this) != static_cast<Allocator &>(other))) {
      move_buckets_from(std::move(other));
    } else {
      static_cast<Allocator &>(*this) = std::move(static_cast<Allocator &>(other));
      m_sparse_buckets_data = std::move(other.m_sparse_buckets_data);
    }

    m_sparse_buckets = m_sparse_buckets_data.empty()
                           ? static_empty_sparse_bucket_ptr()
                           : m_sparse_buckets_data.data();

    static_cast<Hash &>(*this) = std::move(static_cast<Hash &>(other));
    static_cast<KeyEqual &>(*this) = std::move(static_cast<KeyEqual &>(other));
    static_cast<GrowthPolicy &>(*this) =
        std::move(static_cast<GrowthPolicy &>(other));
    m_bucket_count = other.m_bucket_count;
    m_nb_elements = other.m_nb_elements;
    m_nb_deleted_buckets = other.m_nb_deleted_buckets;
    m_load_threshold_rehash = other.m_load_threshold_rehash;
    m_load_threshold_clear_deleted = other.m_load_threshold_clear_deleted;
    m_max_load_factor = other.m_max_load_factor;

    other.GrowthPolicy::clear();
    other.m_sparse_buckets_data.clear();
    other.m_sparse_buckets = static_empty_sparse_bucket_ptr();
    other.m_bucket_count = 0;
    other.m_nb_elements = 0;
    other.m_nb_deleted_buckets = 0;
    other.m_load_threshold_rehash = 0;
    other.m_load_threshold_clear_deleted = 0;

    return *this;
  }

  allocator_type get_allocator() const {
    return static_cast<const Allocator &>(*this);
  }

  /*
   * Iterators
   */
  iterator begin() noexcept {
    auto begin = m_sparse_buckets_data.begin();
    //vector iterator with fancy pointers have a problem with ->
    while (begin != m_sparse_buckets_data.end() && (*begin).empty()) {
      ++begin;
    }

    //vector iterator with fancy pointers have a problem with ->
    return iterator(begin, (begin != m_sparse_buckets_data.end())
                               ? (*begin).begin()
                               : nullptr);
  }

  const_iterator begin() const noexcept { return cbegin(); }

  const_iterator cbegin() const noexcept {
    auto begin = m_sparse_buckets_data.cbegin();
    //vector iterator with fancy pointers have a problem with ->
    while (begin != m_sparse_buckets_data.cend() && (*begin).empty()) {
      ++begin;
    }

    return const_iterator(begin, (begin != m_sparse_buckets_data.cend())
                                     ? (*begin).cbegin()
                                     : nullptr);
  }

  iterator end() noexcept {
    return iterator(m_sparse_buckets_data.end(), nullptr);
  }

  const_iterator end() const noexcept { return cend(); }

  const_iterator cend() const noexcept {
    return const_iterator(m_sparse_buckets_data.cend(), nullptr);
  }

  /*
   * Capacity
   */
  bool empty() const noexcept { return m_nb_elements == 0; }

  size_type size() const noexcept { return m_nb_elements; }

  size_type max_size() const noexcept {
    return std::min(std::allocator_traits<Allocator>::max_size(),
                    m_sparse_buckets_data.max_size());
  }

  /*
   * Modifiers
   */
  void clear() noexcept {
    for (auto &bucket : m_sparse_buckets_data) {
      bucket.clear(*this);
    }

    m_nb_elements = 0;
    m_nb_deleted_buckets = 0;
  }

  template <typename P>
  std::pair<iterator, bool> insert(P &&value) {
    return insert_impl(KeySelect()(value), std::forward<P>(value));
  }

  template <typename P>
  iterator insert_hint(const_iterator hint, P &&value) {
    if (hint != cend() &&
        compare_keys(KeySelect()(*hint), KeySelect()(value))) {
      return mutable_iterator(hint);
    }

    return insert(std::forward<P>(value)).first;
  }

  template <class InputIt>
  void insert(InputIt first, InputIt last) {
    if (std::is_base_of<
            std::forward_iterator_tag,
            typename std::iterator_traits<InputIt>::iterator_category>::value) {
      const auto nb_elements_insert = std::distance(first, last);
      const size_type nb_free_buckets = m_load_threshold_rehash - size();
      tsl_sh_assert(m_load_threshold_rehash >= size());

      if (nb_elements_insert > 0 &&
          nb_free_buckets < size_type(nb_elements_insert)) {
        reserve(size() + size_type(nb_elements_insert));
      }
    }

    for (; first != last; ++first) {
      insert(*first);
    }
  }

  template <class K, class M>
  std::pair<iterator, bool> insert_or_assign(K &&key, M &&obj) {
    auto it = try_emplace(std::forward<K>(key), std::forward<M>(obj));
    if (!it.second) {
      it.first.value() = std::forward<M>(obj);
    }

    return it;
  }

  template <class K, class M>
  iterator insert_or_assign(const_iterator hint, K &&key, M &&obj) {
    if (hint != cend() && compare_keys(KeySelect()(*hint), key)) {
      auto it = mutable_iterator(hint);
      it.value() = std::forward<M>(obj);

      return it;
    }

    return insert_or_assign(std::forward<K>(key), std::forward<M>(obj)).first;
  }

  template <class... Args>
  std::pair<iterator, bool> emplace(Args &&...args) {
    return insert(value_type(std::forward<Args>(args)...));
  }

  template <class... Args>
  iterator emplace_hint(const_iterator hint, Args &&...args) {
    return insert_hint(hint, value_type(std::forward<Args>(args)...));
  }

  template <class K, class... Args>
  std::pair<iterator, bool> try_emplace(K &&key, Args &&...args) {
    return insert_impl(key, std::piecewise_construct,
                       std::forward_as_tuple(std::forward<K>(key)),
                       std::forward_as_tuple(std::forward<Args>(args)...));
  }

  template <class K, class... Args>
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
    tsl_sh_assert(pos != end() && m_nb_elements > 0);
      //vector iterator with fancy pointers have a problem with ->
    auto it_sparse_array_next =
        (*pos.m_sparse_buckets_it).erase(*this, pos.m_sparse_array_it);
    m_nb_elements--;
    m_nb_deleted_buckets++;

    if (it_sparse_array_next == (*pos.m_sparse_buckets_it).end()) {
      auto it_sparse_buckets_next = pos.m_sparse_buckets_it;
      do {
        ++it_sparse_buckets_next;
      } while (it_sparse_buckets_next != m_sparse_buckets_data.end() &&
              (*it_sparse_buckets_next).empty());

      if (it_sparse_buckets_next == m_sparse_buckets_data.end()) {
        return end();
      } else {
        return iterator(it_sparse_buckets_next,
                        (*it_sparse_buckets_next).begin());
      }
    } else {
      return iterator(pos.m_sparse_buckets_it, it_sparse_array_next);
    }
  }

  iterator erase(const_iterator pos) { return erase(mutable_iterator(pos)); }

  iterator erase(const_iterator first, const_iterator last) {
    if (first == last) {
      return mutable_iterator(first);
    }

    // TODO Optimize, could avoid the call to std::distance.
    const size_type nb_elements_to_erase =
        static_cast<size_type>(std::distance(first, last));
    auto to_delete = mutable_iterator(first);
    for (size_type i = 0; i < nb_elements_to_erase; i++) {
      to_delete = erase(to_delete);
    }

    return to_delete;
  }

  template <class K>
  size_type erase(const K &key) {
    return erase(key, hash_key(key));
  }

  template <class K>
  size_type erase(const K &key, std::size_t hash) {
    return erase_impl(key, hash);
  }

  void swap(sparse_hash &other) {
    using std::swap;

    if (std::allocator_traits<Allocator>::propagate_on_container_swap::value) {
      swap(static_cast<Allocator &>(*this), static_cast<Allocator &>(other));
    } else {
      tsl_sh_assert(static_cast<Allocator &>(*this) ==
                    static_cast<Allocator &>(other));
    }

    swap(static_cast<Hash &>(*this), static_cast<Hash &>(other));
    swap(static_cast<KeyEqual &>(*this), static_cast<KeyEqual &>(other));
    swap(static_cast<GrowthPolicy &>(*this),
         static_cast<GrowthPolicy &>(other));
    swap(m_sparse_buckets_data, other.m_sparse_buckets_data);
    swap(m_sparse_buckets, other.m_sparse_buckets);
    swap(m_bucket_count, other.m_bucket_count);
    swap(m_nb_elements, other.m_nb_elements);
    swap(m_nb_deleted_buckets, other.m_nb_deleted_buckets);
    swap(m_load_threshold_rehash, other.m_load_threshold_rehash);
    swap(m_load_threshold_clear_deleted, other.m_load_threshold_clear_deleted);
    swap(m_max_load_factor, other.m_max_load_factor);
  }

  /*
   * Lookup
   */
  template <class K, class U = ValueSelect>
  requires has_mapped_type<U>
  typename U::value_type &at(const K &key) {
    return at(key, hash_key(key));
  }

  template <class K, class U = ValueSelect>
  requires has_mapped_type<U>
  typename U::value_type &at(const K &key, std::size_t hash) {
    auto it = find(key, hash);
    if (it != end()) {
      return it.value();
    } else {
      throw std::out_of_range("Couldn't find key.");
    }
  }

  template <class K, class U = ValueSelect>
  requires has_mapped_type<U>
  const typename U::value_type &at(const K &key) const {
    return at(key, hash_key(key));
  }

  template <class K, class U = ValueSelect>
  requires has_mapped_type<U>
  const typename U::value_type &at(const K &key, std::size_t hash) const {
    auto it = find(key, hash);
    if (it != cend()) {
      return it.value();
    } else {
      throw std::out_of_range("Couldn't find key.");
    }
  }

  template <class K, class U = ValueSelect>
  requires has_mapped_type<U>
  typename U::value_type &operator[](K &&key) {
    return try_emplace(std::forward<K>(key)).first.value();
  }

  template <class K>
  bool contains(const K &key) const {
    return contains(key, hash_key(key));
  }

  template <class K>
  bool contains(const K &key, std::size_t hash) const {
    return count(key, hash) != 0;
  }

  template <class K>
  size_type count(const K &key) const {
    return count(key, hash_key(key));
  }

  template <class K>
  size_type count(const K &key, std::size_t hash) const {
    if (find(key, hash) != cend()) {
      return 1;
    } else {
      return 0;
    }
  }

  template <class K>
  iterator find(const K &key) {
    return find_impl(key, hash_key(key));
  }

  template <class K>
  iterator find(const K &key, std::size_t hash) {
    return find_impl(key, hash);
  }

  template <class K>
  const_iterator find(const K &key) const {
    return find_impl(key, hash_key(key));
  }

  template <class K>
  const_iterator find(const K &key, std::size_t hash) const {
    return find_impl(key, hash);
  }

  template <class K>
  std::pair<iterator, iterator> equal_range(const K &key) {
    return equal_range(key, hash_key(key));
  }

  template <class K>
  std::pair<iterator, iterator> equal_range(const K &key, std::size_t hash) {
    iterator it = find(key, hash);
    return std::make_pair(it, (it == end()) ? it : std::next(it));
  }

  template <class K>
  std::pair<const_iterator, const_iterator> equal_range(const K &key) const {
    return equal_range(key, hash_key(key));
  }

  template <class K>
  std::pair<const_iterator, const_iterator> equal_range(
      const K &key, std::size_t hash) const {
    const_iterator it = find(key, hash);
    return std::make_pair(it, (it == cend()) ? it : std::next(it));
  }

  /*
   * Bucket interface
   */
  size_type bucket_count() const { return m_bucket_count; }

  size_type max_bucket_count() const {
    return m_sparse_buckets_data.max_size();
  }

  /*
   * Hash policy
   */
  float load_factor() const {
    if (bucket_count() == 0) {
      return 0;
    }

    return float(m_nb_elements) / float(bucket_count());
  }

  float max_load_factor() const { return m_max_load_factor; }

  void max_load_factor(float ml) {
    m_max_load_factor = std::max(0.1f, std::min(ml, 0.8f));
    m_load_threshold_rehash =
        size_type(float(bucket_count()) * m_max_load_factor);

    const float max_load_factor_with_deleted_buckets =
        m_max_load_factor + 0.5f * (1.0f - m_max_load_factor);
    tsl_sh_assert(max_load_factor_with_deleted_buckets > 0.0f &&
                  max_load_factor_with_deleted_buckets <= 1.0f);
    m_load_threshold_clear_deleted =
        size_type(float(bucket_count()) * max_load_factor_with_deleted_buckets);
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
  hasher hash_function() const { return static_cast<const Hash &>(*this); }

  key_equal key_eq() const { return static_cast<const KeyEqual &>(*this); }

  /*
   * Other
   */
  iterator mutable_iterator(const_iterator pos) {
    auto it_sparse_buckets =
        m_sparse_buckets_data.begin() +
        std::distance(m_sparse_buckets_data.cbegin(), pos.m_sparse_buckets_it);
  
    if(it_sparse_buckets == m_sparse_buckets_data.end()) {
      return iterator(it_sparse_buckets, nullptr);
    }

    auto it_sparse_array =
      std::to_address(it_sparse_buckets)->begin() +
        std::distance(std::to_address(it_sparse_buckets)->cbegin(), pos.m_sparse_array_it);
      
    return iterator(it_sparse_buckets, it_sparse_array);
  }

 private:
  template <class K>
  std::size_t hash_key(const K &key) const {
    return Hash::operator()(key);
  }

  template <class K1, class K2>
  bool compare_keys(const K1 &key1, const K2 &key2) const {
    return KeyEqual::operator()(key1, key2);
  }

  size_type bucket_for_hash(std::size_t hash) const {
    const std::size_t bucket = GrowthPolicy::bucket_for_hash(hash);
    tsl_sh_assert(sparse_array::sparse_ibucket(bucket) <
                      m_sparse_buckets_data.size() ||
                  (bucket == 0 && m_sparse_buckets_data.empty()));

    return bucket;
  }

  template <class U = GrowthPolicy>
  requires power_of_two_policy<U>
  size_type next_bucket(size_type ibucket, size_type iprobe) const {
    (void)iprobe;
    if (Probing == dice::sparse_map::sh::probing::linear) {
      return (ibucket + 1) & this->m_mask;
    } else {
      tsl_sh_assert(Probing == dice::sparse_map::sh::probing::quadratic);
      return (ibucket + iprobe) & this->m_mask;
    }
  }

  template <class U = GrowthPolicy>
  requires (!power_of_two_policy<U>)
  size_type next_bucket(size_type ibucket, size_type iprobe) const {
    (void)iprobe;
    if (Probing == dice::sparse_map::sh::probing::linear) {
      ibucket++;
      return (ibucket != bucket_count()) ? ibucket : 0;
    } else {
      tsl_sh_assert(Probing == dice::sparse_map::sh::probing::quadratic);
      ibucket += iprobe;
      return (ibucket < bucket_count()) ? ibucket : ibucket % bucket_count();
    }
  }

  // TODO encapsulate m_sparse_buckets_data to avoid the managing the allocator
  void copy_buckets_from(const sparse_hash &other) {
    m_sparse_buckets_data.reserve(other.m_sparse_buckets_data.size());

    try {
      for (const auto &bucket : other.m_sparse_buckets_data) {
        m_sparse_buckets_data.emplace_back(bucket,
                                           static_cast<Allocator &>(*this));
      }
    } catch (...) {
      clear();
      throw;
    }

    tsl_sh_assert(m_sparse_buckets_data.empty() ||
                  m_sparse_buckets_data.back().last());
  }

  void move_buckets_from(sparse_hash &&other) {
    m_sparse_buckets_data.reserve(other.m_sparse_buckets_data.size());

    try {
      for (auto &&bucket : other.m_sparse_buckets_data) {
        m_sparse_buckets_data.emplace_back(std::move(bucket),
                                           static_cast<Allocator &>(*this));
      }
    } catch (...) {
      clear();
      throw;
    }

    tsl_sh_assert(m_sparse_buckets_data.empty() ||
                  m_sparse_buckets_data.back().last());
  }

  template <class K, class... Args>
  std::pair<iterator, bool> insert_impl(const K &key,
                                        Args &&...value_type_args) {
    if (size() >= m_load_threshold_rehash) {
      rehash_impl(GrowthPolicy::next_bucket_count());
    } else if (size() + m_nb_deleted_buckets >=
               m_load_threshold_clear_deleted) {
      clear_deleted_buckets();
    }
    tsl_sh_assert(!m_sparse_buckets_data.empty());

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

    const std::size_t hash = hash_key(key);
    std::size_t ibucket = bucket_for_hash(hash);

    std::size_t probe = 0;
    while (true) {
      std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
      auto index_in_sparse_bucket =
          sparse_array::index_in_sparse_bucket(ibucket);

      if (m_sparse_buckets != static_empty_sparse_bucket_ptr()) {
          if (m_sparse_buckets[sparse_ibucket].has_value(index_in_sparse_bucket)) {
              auto value_it =
                      m_sparse_buckets[sparse_ibucket].value(index_in_sparse_bucket);
              if (compare_keys(key, KeySelect()(*value_it))) {
                  return std::make_pair(
                          iterator(m_sparse_buckets_data.begin() + sparse_ibucket,
                                   value_it),
                          false);
              }
          } else if (m_sparse_buckets[sparse_ibucket].has_deleted_value(
                  index_in_sparse_bucket) &&
                     probe < m_bucket_count) {
              if (!found_first_deleted_bucket) {
                  found_first_deleted_bucket = true;
                  sparse_ibucket_first_deleted = sparse_ibucket;
                  index_in_sparse_bucket_first_deleted = index_in_sparse_bucket;
              }
          } else if (found_first_deleted_bucket) {
              auto it = insert_in_bucket(sparse_ibucket_first_deleted,
                                         index_in_sparse_bucket_first_deleted,
                                         std::forward<Args>(value_type_args)...);
              m_nb_deleted_buckets--;

              return it;
          }
          else {
              return insert_in_bucket(sparse_ibucket, index_in_sparse_bucket,
                                      std::forward<Args>(value_type_args)...);
          }
      }else {
        return insert_in_bucket(sparse_ibucket, index_in_sparse_bucket,
                                std::forward<Args>(value_type_args)...);
      }

      probe++;
      ibucket = next_bucket(ibucket, probe);
    }
  }

  template <class... Args>
  std::pair<iterator, bool> insert_in_bucket(
      std::size_t sparse_ibucket,
      typename sparse_array::size_type index_in_sparse_bucket,
      Args &&...value_type_args) {
      // is not called when empty
    auto value_it = m_sparse_buckets[sparse_ibucket].set(
        *this, index_in_sparse_bucket, std::forward<Args>(value_type_args)...);
    m_nb_elements++;

    return std::make_pair(
        iterator(m_sparse_buckets_data.begin() + sparse_ibucket, value_it),
        true);
  }

  template <class K>
  size_type erase_impl(const K &key, std::size_t hash) {
    std::size_t ibucket = bucket_for_hash(hash);

    std::size_t probe = 0;

    if (m_sparse_buckets == static_empty_sparse_bucket_ptr())
      return 0;
    while (true) {
      const std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
      const auto index_in_sparse_bucket =
          sparse_array::index_in_sparse_bucket(ibucket);

      if (m_sparse_buckets[sparse_ibucket].has_value(index_in_sparse_bucket)) {
        auto value_it =
            m_sparse_buckets[sparse_ibucket].value(index_in_sparse_bucket);
        if (compare_keys(key, KeySelect()(*value_it))) {
          m_sparse_buckets[sparse_ibucket].erase(*this, value_it,
                                                 index_in_sparse_bucket);
          m_nb_elements--;
          m_nb_deleted_buckets++;

          return 1;
        }
      } else if (!m_sparse_buckets[sparse_ibucket].has_deleted_value(
                     index_in_sparse_bucket) ||
                 probe >= m_bucket_count) {
        return 0;
      }

      probe++;
      ibucket = next_bucket(ibucket, probe);
    }
  }

  template <class K>
  iterator find_impl(const K &key, std::size_t hash) {
    return mutable_iterator(
        static_cast<const sparse_hash *>(this)->find(key, hash));
  }

  template <class K>
  const_iterator find_impl(const K &key, std::size_t hash) const {
    std::size_t ibucket = bucket_for_hash(hash);

    std::size_t probe = 0;
    while (true) {
      const std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
      const auto index_in_sparse_bucket =
          sparse_array::index_in_sparse_bucket(ibucket);

      if (m_sparse_buckets == static_empty_sparse_bucket_ptr()) {
          return cend();
      }if (m_sparse_buckets[sparse_ibucket].has_value(index_in_sparse_bucket)) {
        auto value_it =
            m_sparse_buckets[sparse_ibucket].value(index_in_sparse_bucket);
        if (compare_keys(key, KeySelect()(*value_it))) {
          return const_iterator(m_sparse_buckets_data.cbegin() + sparse_ibucket,
                                value_it);
        }
      } else if (!m_sparse_buckets[sparse_ibucket].has_deleted_value(
                     index_in_sparse_bucket) ||
                 probe >= m_bucket_count) {
        return cend();
      }

      probe++;
      ibucket = next_bucket(ibucket, probe);
    }
  }

  void clear_deleted_buckets() {
    // TODO could be optimized, we could do it in-place instead of allocating a
    // new bucket array.
    rehash_impl(m_bucket_count);
    tsl_sh_assert(m_nb_deleted_buckets == 0);
  }

  template <dice::sparse_map::sh::exception_safety U = ExceptionSafety>
  requires (U == dice::sparse_map::sh::exception_safety::basic)
  void rehash_impl(size_type count) {
    sparse_hash new_table(count, static_cast<Hash &>(*this),
                          static_cast<KeyEqual &>(*this),
                          static_cast<Allocator &>(*this), m_max_load_factor);

    for (auto &bucket : m_sparse_buckets_data) {
      for (auto &val : bucket) {
        new_table.insert_on_rehash(std::move(val));
      }

      // TODO try to reuse some of the memory
      bucket.clear(*this);
    }

    new_table.swap(*this);
  }

  /**
   * TODO: For now we copy each element into the new map. We could move
   * them if they are nothrow_move_constructible without triggering
   * any exception if we reserve enough space in the sparse arrays beforehand.
   */
  template <dice::sparse_map::sh::exception_safety U = ExceptionSafety>
  requires (U == dice::sparse_map::sh::exception_safety::strong)
  void rehash_impl(size_type count) {
    sparse_hash new_table(count, static_cast<Hash &>(*this),
                          static_cast<KeyEqual &>(*this),
                          static_cast<Allocator &>(*this), m_max_load_factor);

    for (const auto &bucket : m_sparse_buckets_data) {
      for (const auto &val : bucket) {
        new_table.insert_on_rehash(val);
      }
    }

    new_table.swap(*this);
  }

  template <typename K>
  void  insert_on_rehash(K &&key_value) {
    const key_type &key = KeySelect()(key_value);

    const std::size_t hash = hash_key(key);
    std::size_t ibucket = bucket_for_hash(hash);

    std::size_t probe = 0;
    while (true) {
      std::size_t sparse_ibucket = sparse_array::sparse_ibucket(ibucket);
      auto index_in_sparse_bucket =
          sparse_array::index_in_sparse_bucket(ibucket);

      if (!m_sparse_buckets[sparse_ibucket].has_value(index_in_sparse_bucket)) {
        m_sparse_buckets[sparse_ibucket].set(*this, index_in_sparse_bucket,
                                             std::forward<K>(key_value));
        m_nb_elements++;

        return;
      } else {
        tsl_sh_assert(!compare_keys(
            key, KeySelect()(*m_sparse_buckets[sparse_ibucket].value(
                     index_in_sparse_bucket))));
      }

      probe++;
      ibucket = next_bucket(ibucket, probe);
    }
  }

 public:
  static const size_type DEFAULT_INIT_BUCKET_COUNT = 0;
  static constexpr float DEFAULT_MAX_LOAD_FACTOR = 0.5f;

  using sparse_array_ptr = typename std::allocator_traits<allocator_type>::template rebind_traits<sparse_array>::pointer;
  /**
   * Return an nullptr to indicate an empty bucket
   */
  static sparse_array_ptr static_empty_sparse_bucket_ptr() {
    return {};
  }

 private:
  sparse_buckets_container m_sparse_buckets_data;


  /**
   * Points to m_sparse_buckets_data.data() if !m_sparse_buckets_data.empty()
   * otherwise points to static_empty_sparse_bucket_ptr. This variable is useful
   * to avoid the cost of checking if m_sparse_buckets_data is empty when trying
   * to find an element.
   *
   * TODO Remove m_sparse_buckets_data and only use a pointer instead of a
   * pointer+vector to save some space in the sparse_hash object.
   */

  sparse_array_ptr m_sparse_buckets;

  size_type m_bucket_count;
  size_type m_nb_elements;
  size_type m_nb_deleted_buckets;

  /**
   * Maximum that m_nb_elements can reach before a rehash occurs automatically
   * to grow the hash table.
   */
  size_type m_load_threshold_rehash;

  /**
   * Maximum that m_nb_elements + m_nb_deleted_buckets can reach before cleaning
   * up the buckets marked as deleted.
   */
  size_type m_load_threshold_clear_deleted;
  float m_max_load_factor;
};

}  // namespace detail_sparse_hash
}  // namespace dice

#endif
