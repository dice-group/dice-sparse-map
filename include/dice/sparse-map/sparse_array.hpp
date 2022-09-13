#ifndef SPARSE_ARRAY_HPP
#define SPARSE_ARRAY_HPP
#include "dice/sparse-map/commons.hpp"
#include "dice/sparse-map/sparse_growth_policy.hpp"
#endif

enum class sparsity { high, medium, low };

template <typename T>
  struct Remove_Const {
    template <typename V>
    static T remove(V iter) {
      return const_cast<T>(iter);
  }
};

template <typename U>
struct is_power_of_two_policy : std::false_type {};

template <std::size_t GrowthFactor>
struct is_power_of_two_policy<dice::sparse_map::sh::power_of_two_growth_policy<GrowthFactor>>
    : std::true_type {};

template <typename T>
concept power_of_two_policy = is_power_of_two_policy<T>::value;

template <typename U>
concept has_mapped_type = !std::same_as<U, void>; 

template<typename T>
concept is_nothrow_move_constructible = std::is_nothrow_move_constructible<T>::value;

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

template <typename T, typename Allocator, sparsity Sparsity>
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
  static const size_type CAPACITY_GROWTH_STEP =
          (Sparsity == sparsity::high) ? 2
                                                             : (Sparsity == sparsity::medium)
          ? 4
          : 8;  // (Sparsity == dice::sh::sparsity::low)

  /**
   * Bitmap size configuration.
   * Use 32 bits for the bitmap on 32-bits or less environnement as popcount on
   * 64 bits numbers is slow on these environnement. Use 64 bits bitmap
   * otherwise.
   */
#if SIZE_MAX <= UINT32_MAX
  using bitmap_type = std::uint_least32_t;
  static const std::size_t BITMAP_NB_BITS = 32;
  static const std::size_t BUCKET_SHIFT = 5;
#else
  using bitmap_type = std::uint_least64_t;
  static const std::size_t BITMAP_NB_BITS = 64;
  static const std::size_t BUCKET_SHIFT = 6;
#endif

  static const std::size_t BUCKET_MASK = BITMAP_NB_BITS - 1;

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
  static_assert((std::numeric_limits<bitmap_type>::max() & BUCKET_MASK) ==
                    BITMAP_NB_BITS - 1,
                "");

 public:
  /**
   * Map an ibucket [0, bucket_count) in the hash table to a sparse_ibucket
   * (a sparse_array holds multiple buckets, so there is less sparse_array than
   * bucket_count).
   *
   * The bucket ibucket is in
   * m_sparse_buckets[sparse_ibucket(ibucket)][index_in_sparse_bucket(ibucket)]
   * instead of something like m_buckets[ibucket] in a classical hash table.
   */
  static std::size_t sparse_ibucket(std::size_t ibucket) {
    return ibucket >> BUCKET_SHIFT;
  }

  /**
   * Map an ibucket [0, bucket_count) in the hash table to an index in the
   * sparse_array which corresponds to the bucket.
   *
   * The bucket ibucket is in
   * m_sparse_buckets[sparse_ibucket(ibucket)][index_in_sparse_bucket(ibucket)]
   * instead of something like m_buckets[ibucket] in a classical hash table.
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
        1, sparse_ibucket(round_up_to_power_of_two(
               bucket_count)));
  }

 public:
  sparse_array() noexcept
      : m_values(nullptr),
        m_bitmap_vals(0),
        m_bitmap_deleted_vals(0),
        m_nb_elements(0),
        m_capacity(0),
        m_last_array(false) {}

  //needed for "is_constructible" with no parameters
  sparse_array(std::allocator_arg_t, Allocator const&) noexcept : sparse_array() {}

  explicit sparse_array(bool last_bucket) noexcept
      : m_values(nullptr),
        m_bitmap_vals(0),
        m_bitmap_deleted_vals(0),
        m_nb_elements(0),
        m_capacity(0),
        m_last_array(last_bucket) {}

  //const Allocator needed for MoveInsertable requirement
  sparse_array(size_type capacity, Allocator const &const_alloc)
      : m_values(nullptr),
        m_bitmap_vals(0),
        m_bitmap_deleted_vals(0),
        m_nb_elements(0),
        m_capacity(capacity),
        m_last_array(false) {
    if (m_capacity > 0) {
      auto alloc = const_cast<Allocator&>(const_alloc);
      m_values = alloc.allocate(m_capacity);
      tsl_sh_assert(m_values !=
                    nullptr);  // allocate should throw if there is a failure
    }
  }

  //const Allocator needed for MoveInsertable requirement
  sparse_array(const sparse_array &other, Allocator const &const_alloc)
      : m_values(nullptr),
        m_bitmap_vals(other.m_bitmap_vals),
        m_bitmap_deleted_vals(other.m_bitmap_deleted_vals),
        m_nb_elements(0),
        m_capacity(other.m_capacity),
        m_last_array(other.m_last_array) {
    tsl_sh_assert(other.m_capacity >= other.m_nb_elements);
    if (m_capacity == 0) {
      return;
    }

    auto alloc = const_cast<Allocator&>(const_alloc);
    m_values = alloc.allocate(m_capacity);
    tsl_sh_assert(m_values !=
                  nullptr);  // allocate should throw if there is a failure
    try {
      for (size_type i = 0; i < other.m_nb_elements; i++) {
        construct_value(alloc, m_values + i, other.m_values[i]);
        m_nb_elements++;
      }
    } catch (...) {
      clear(alloc);
      throw;
    }
  }

  sparse_array(sparse_array &&other) noexcept
      : m_values(other.m_values),
        m_bitmap_vals(other.m_bitmap_vals),
        m_bitmap_deleted_vals(other.m_bitmap_deleted_vals),
        m_nb_elements(other.m_nb_elements),
        m_capacity(other.m_capacity),
        m_last_array(other.m_last_array) {
    other.m_values = nullptr;
    other.m_bitmap_vals = 0;
    other.m_bitmap_deleted_vals = 0;
    other.m_nb_elements = 0;
    other.m_capacity = 0;
  }

  //const Allocator needed for MoveInsertable requirement
  sparse_array(sparse_array &&other, Allocator const &const_alloc)
      : m_values(nullptr),
        m_bitmap_vals(other.m_bitmap_vals),
        m_bitmap_deleted_vals(other.m_bitmap_deleted_vals),
        m_nb_elements(0),
        m_capacity(other.m_capacity),
        m_last_array(other.m_last_array) {
    tsl_sh_assert(other.m_capacity >= other.m_nb_elements);
    if (m_capacity == 0) {
      return;
    }

    auto alloc = const_cast<Allocator&>(const_alloc);
    m_values = alloc.allocate(m_capacity);
    tsl_sh_assert(m_values !=
                  nullptr);  // allocate should throw if there is a failure
    try {
      for (size_type i = 0; i < other.m_nb_elements; i++) {
        construct_value(alloc, m_values + i, std::move(other.m_values[i]));
        m_nb_elements++;
      }
    } catch (...) {
      clear(alloc);
      throw;
    }
  }

  sparse_array &operator=(const sparse_array &) = delete;
  sparse_array &operator=(sparse_array &&other) noexcept{
    this->m_values = other.m_values;
    this->m_bitmap_vals = other.m_bitmap_vals;
    this->m_bitmap_deleted_vals = other.m_bitmap_deleted_vals;
    this->m_nb_elements = other.m_nb_elements;
    this->m_capacity = other.m_capacity;
    other.m_values = nullptr;
    other.m_bitmap_vals = 0;
    other.m_bitmap_deleted_vals = 0;
    other.m_nb_elements = 0;
    other.m_capacity = 0;
    return *this;
  }



  ~sparse_array() noexcept {
    // The code that manages the sparse_array must have called clear before
    // destruction. See documentation of sparse_array for more details.
    tsl_sh_assert(m_capacity == 0 && m_nb_elements == 0 && m_values == nullptr);
  }

  iterator begin() noexcept { return m_values; }
  iterator end() noexcept { return m_values + m_nb_elements; }
  const_iterator begin() const noexcept { return cbegin(); }
  const_iterator end() const noexcept { return cend(); }
  const_iterator cbegin() const noexcept { return m_values; }
  const_iterator cend() const noexcept { return m_values + m_nb_elements; }

  bool empty() const noexcept { return m_nb_elements == 0; }

  size_type size() const noexcept { return m_nb_elements; }

  void clear(allocator_type &alloc) noexcept {
    destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

    m_values = nullptr;
    m_bitmap_vals = 0;
    m_bitmap_deleted_vals = 0;
    m_nb_elements = 0;
    m_capacity = 0;
  }

  bool last() const noexcept { return m_last_array; }

  void set_as_last() noexcept { m_last_array = true; }

  bool has_value(size_type index) const noexcept {
    tsl_sh_assert(index < BITMAP_NB_BITS);
    return (m_bitmap_vals & (bitmap_type(1) << index)) != 0;
  }

  bool has_deleted_value(size_type index) const noexcept {
    tsl_sh_assert(index < BITMAP_NB_BITS);
    return (m_bitmap_deleted_vals & (bitmap_type(1) << index)) != 0;
  }

  iterator value(size_type index) noexcept {
    tsl_sh_assert(has_value(index));
    return m_values + index_to_offset(index);
  }

  const_iterator value(size_type index) const noexcept {
    tsl_sh_assert(has_value(index));
    return m_values + index_to_offset(index);
  }

  /**
   * Return iterator to set value.
   */
  template <typename... Args>
  iterator set(allocator_type &alloc, size_type index, Args &&...value_args) {
    tsl_sh_assert(!has_value(index));

    const size_type offset = index_to_offset(index);
    insert_at_offset(alloc, offset, std::forward<Args>(value_args)...);

    m_bitmap_vals = (m_bitmap_vals | (bitmap_type(1) << index));
    m_bitmap_deleted_vals =
        (m_bitmap_deleted_vals & ~(bitmap_type(1) << index));

    m_nb_elements++;

    tsl_sh_assert(has_value(index));
    tsl_sh_assert(!has_deleted_value(index));

    return m_values + offset;
  }

  iterator erase(allocator_type &alloc, iterator position) {
    const size_type offset =
        static_cast<size_type>(std::distance(begin(), position));
    return erase(alloc, position, offset_to_index(offset));
  }

  // Return the next value or end if no next value
  iterator erase(allocator_type &alloc, iterator position, size_type index) {
    tsl_sh_assert(has_value(index));
    tsl_sh_assert(!has_deleted_value(index));

    const size_type offset =
        static_cast<size_type>(std::distance(begin(), position));
    erase_at_offset(alloc, offset);

    m_bitmap_vals = (m_bitmap_vals & ~(bitmap_type(1) << index));
    m_bitmap_deleted_vals = (m_bitmap_deleted_vals | (bitmap_type(1) << index));

    m_nb_elements--;

    tsl_sh_assert(!has_value(index));
    tsl_sh_assert(has_deleted_value(index));

    return m_values + offset;
  }

  void swap(sparse_array &other) {
    using std::swap;

    swap(m_values, other.m_values);
    swap(m_bitmap_vals, other.m_bitmap_vals);
    swap(m_bitmap_deleted_vals, other.m_bitmap_deleted_vals);
    swap(m_nb_elements, other.m_nb_elements);
    swap(m_capacity, other.m_capacity);
    swap(m_last_array, other.m_last_array);
  }

  static iterator mutable_iterator(const_iterator pos) {
    return Remove_Const<iterator>::template remove<const_iterator>(pos);
  }

 private:
  template <typename... Args>
  static void construct_value(allocator_type &alloc, pointer value,
                              Args &&... value_args) {
    std::allocator_traits<allocator_type>::construct(
        alloc, std::to_address(value), std::forward<Args>(value_args)...);
  }

  static void destroy_value(allocator_type &alloc, pointer value) noexcept {
    std::allocator_traits<allocator_type>::destroy(alloc, std::to_address(value));
  }

  static void destroy_and_deallocate_values(
      allocator_type &alloc, pointer values, size_type nb_values,
      size_type capacity_values) noexcept {
    for (size_type i = 0; i < nb_values; i++) {
      destroy_value(alloc, values + i);
    }

    alloc.deallocate(values, capacity_values);
  }

  size_type index_to_offset(size_type index) const noexcept {
    tsl_sh_assert(index < BITMAP_NB_BITS);
    return std::popcount(m_bitmap_vals &
                    ((bitmap_type(1) << index) - bitmap_type(1)));
  }

  // TODO optimize
  size_type offset_to_index(size_type offset) const noexcept {
    tsl_sh_assert(offset < m_nb_elements);

    bitmap_type bitmap_vals = m_bitmap_vals;
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
    return static_cast<size_type>(m_capacity + CAPACITY_GROWTH_STEP);
  }

  /**
   * Insertion
   *
   * Two situations:
   * - Either we are in a situation where
   * std::is_nothrow_move_constructible<value_type>::value is true. In this
   * case, on insertion we just reallocate m_values when we reach its capacity
   * (i.e. m_nb_elements == m_capacity), otherwise we just put the new value at
   * its appropriate place. We can easily keep the strong exception guarantee as
   * moving the values around is safe.
   * - Otherwise we are in a situation where
   * std::is_nothrow_move_constructible<value_type>::value is false. In this
   * case on EACH insertion we allocate a new area of m_nb_elements + 1 where we
   * copy the values of m_values into it and put the new value there. On
   * success, we set m_values to this new area. Even if slower, it's the only
   * way to preserve to strong exception guarantee.
   */
  template <typename... Args>
  requires is_nothrow_move_constructible<value_type>
  void insert_at_offset(allocator_type &alloc, size_type offset,
                        Args &&...value_args) {
    if (m_nb_elements < m_capacity) {
      insert_at_offset_no_realloc(alloc, offset,
                                  std::forward<Args>(value_args)...);
    } else {
      insert_at_offset_realloc(alloc, offset, next_capacity(),
                               std::forward<Args>(value_args)...);
    }
  }

  template <typename... Args>
  requires (!is_nothrow_move_constructible<value_type>)
  void insert_at_offset(allocator_type &alloc, size_type offset,
                        Args &&...value_args) {
    insert_at_offset_realloc(alloc, offset, m_nb_elements + 1,
                             std::forward<Args>(value_args)...);
  }

  template <typename... Args>
  requires is_nothrow_move_constructible<value_type>
  void insert_at_offset_no_realloc(allocator_type &alloc, size_type offset,
                                   Args &&...value_args) {
    tsl_sh_assert(offset <= m_nb_elements);
    tsl_sh_assert(m_nb_elements < m_capacity);

    for (size_type i = m_nb_elements; i > offset; i--) {
      construct_value(alloc, m_values + i, std::move(m_values[i - 1]));
      destroy_value(alloc, m_values + i - 1);
    }

    try {
      construct_value(alloc, m_values + offset,
                      std::forward<Args>(value_args)...);
    } catch (...) {
      for (size_type i = offset; i < m_nb_elements; i++) {
        construct_value(alloc, m_values + i, std::move(m_values[i + 1]));
        destroy_value(alloc, m_values + i + 1);
      }
      throw;
    }
  }

  template <typename... Args>
  requires is_nothrow_move_constructible<value_type>
  void insert_at_offset_realloc(allocator_type &alloc, size_type offset,
                                size_type new_capacity, Args &&...value_args) {
    tsl_sh_assert(new_capacity > m_nb_elements);

    pointer new_values = alloc.allocate(new_capacity);
    // Allocate should throw if there is a failure
    tsl_sh_assert(new_values != nullptr);

    try {
      construct_value(alloc, new_values + offset,
                      std::forward<Args>(value_args)...);
    } catch (...) {
      alloc.deallocate(new_values, new_capacity);
      throw;
    }

    // Should not throw from here
    for (size_type i = 0; i < offset; i++) {
      construct_value(alloc, new_values + i, std::move(m_values[i]));
    }

    for (size_type i = offset; i < m_nb_elements; i++) {
      construct_value(alloc, new_values + i + 1, std::move(m_values[i]));
    }

    destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

    m_values = new_values;
    m_capacity = new_capacity;
  }

  template <typename... Args>
  requires (!is_nothrow_move_constructible<value_type>)
  void insert_at_offset_realloc(allocator_type &alloc, size_type offset,
                                size_type new_capacity, Args &&...value_args) {
    tsl_sh_assert(new_capacity > m_nb_elements);

    value_type *new_values = alloc.allocate(new_capacity);
    // Allocate should throw if there is a failure
    tsl_sh_assert(new_values != nullptr);

    size_type nb_new_values = 0;
    try {
      for (size_type i = 0; i < offset; i++) {
        construct_value(alloc, new_values + i, m_values[i]);
        nb_new_values++;
      }

      construct_value(alloc, new_values + offset,
                      std::forward<Args>(value_args)...);
      nb_new_values++;

      for (size_type i = offset; i < m_nb_elements; i++) {
        construct_value(alloc, new_values + i + 1, m_values[i]);
        nb_new_values++;
      }
    } catch (...) {
      destroy_and_deallocate_values(alloc, new_values, nb_new_values,
                                    new_capacity);
      throw;
    }

    tsl_sh_assert(nb_new_values == m_nb_elements + 1);

    destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

    m_values = new_values;
    m_capacity = new_capacity;
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
   * set m_values to this new area. Even if slower, it's the only way to
   * preserve to strong exception guarantee.
   */
  template <typename... Args>
  requires is_nothrow_move_constructible<value_type>
  void erase_at_offset(allocator_type &alloc, size_type offset) noexcept {
    tsl_sh_assert(offset < m_nb_elements);

    destroy_value(alloc, m_values + offset);

    for (size_type i = offset + 1; i < m_nb_elements; i++) {
      construct_value(alloc, m_values + i - 1, std::move(m_values[i]));
      destroy_value(alloc, m_values + i);
    }
  }

  template <typename... Args>
  requires (!is_nothrow_move_constructible<value_type>)
  void erase_at_offset(allocator_type &alloc, size_type offset) {
    tsl_sh_assert(offset < m_nb_elements);

    // Erasing the last element, don't need to reallocate. We keep the capacity.
    if (offset + 1 == m_nb_elements) {
      destroy_value(alloc, m_values + offset);
      return;
    }

    tsl_sh_assert(m_nb_elements > 1);
    const size_type new_capacity = m_nb_elements - 1;

    value_type *new_values = alloc.allocate(new_capacity);
    // Allocate should throw if there is a failure
    tsl_sh_assert(new_values != nullptr);

    size_type nb_new_values = 0;
    try {
      for (size_type i = 0; i < m_nb_elements; i++) {
        if (i != offset) {
          construct_value(alloc, new_values + nb_new_values, m_values[i]);
          nb_new_values++;
        }
      }
    } catch (...) {
      destroy_and_deallocate_values(alloc, new_values, nb_new_values,
                                    new_capacity);
      throw;
    }

    tsl_sh_assert(nb_new_values == m_nb_elements - 1);

    destroy_and_deallocate_values(alloc, m_values, m_nb_elements, m_capacity);

    m_values = new_values;
    m_capacity = new_capacity;
  }

 private:
  pointer m_values;

  bitmap_type m_bitmap_vals;
  bitmap_type m_bitmap_deleted_vals;

  size_type m_nb_elements;
  size_type m_capacity;
  bool m_last_array;
};