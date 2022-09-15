#ifndef VEC_HPP
#define VEC_HPP
#include "dice/sparse-map/commons.hpp"

namespace dice::sparse_map {
  namespace detail_sparse_hash {

    template<typename T, typename Alloc>
    class vec {
      public:
        using allocator_type = Alloc;
        using value_type = T;
        using allocator_traits = std::allocator_traits<allocator_type>;
        using pointer = typename allocator_traits::pointer;
        using const_pointer = typename allocator_traits::const_pointer;
        using iterator = pointer;
        using const_iterator =  const_pointer;
        using reference = value_type&;
        using const_reference = const value_type&;
        using size_type = typename allocator_traits::size_type;
        using difference_type = typename allocator_traits::difference_type;
        
        static pointer static_empty_sparse_bucket_ptr() {
          return {};
        } 

        vec() = delete;

        explicit vec(const allocator_type& a) noexcept
          : m_size(0),
            m_capacity(0),
            m_sparse_buckets(static_empty_sparse_bucket_ptr()),
            m_allocator(a) {}

        vec(const vec& other) noexcept = default;

        vec(vec&& other) noexcept 
          : m_size(other.m_size), 
            m_capacity(other.m_capacity), 
            m_sparse_buckets(other.m_sparse_buckets),
            m_allocator(std::move(other.m_allocator)) {
              other.m_sparse_buckets = static_empty_sparse_bucket_ptr();
              other.m_size = 0;
              other.m_capacity = 0;
            }

        ~vec() noexcept {
          clear();
        }

        std::size_t size() const noexcept {
          return m_size;
        } 

        std::size_t max_size() const noexcept {
          return std::numeric_limits<std::size_t>::max() / 2;
        } 

        std::size_t capacity() const noexcept {
          return m_capacity;
        }

        iterator begin() noexcept { return m_sparse_buckets; }
        const_iterator begin() const noexcept { return m_sparse_buckets; }
        iterator end() noexcept { return m_sparse_buckets + m_size; }
        const_iterator end() const noexcept { return m_sparse_buckets + m_size; }

        const_iterator cbegin() const noexcept { return m_sparse_buckets; }
        const_iterator cend() const noexcept { return m_sparse_buckets + m_size; }

        reference operator[](std::size_t n) {
          return m_sparse_buckets[n];
        }

        const_reference operator[](std::size_t n) const {
          return m_sparse_buckets[n];
        }

        reference front() {
          return *std::to_address(m_sparse_buckets);
        }

        const_reference front() const {
          return *std::to_address(m_sparse_buckets);
        }

        reference back() {
          return m_sparse_buckets[difference_type(m_size - 1)];
        }
        const_reference back() const {
          return m_sparse_buckets[difference_type(m_size - 1)];
        }

        pointer data() noexcept { return m_sparse_buckets; }
        const_pointer data() const noexcept { return m_sparse_buckets; }

        template <class... Args>
        reference emplace_back(Args&&... args) {
          if (m_size < m_capacity) {
            this->construct(m_sparse_buckets + m_size, std::forward<Args>(args)...);
          } 
          else {
            const std::size_t new_capacity = this->nextCapacity();
            pointer new_sparse_buckets = static_empty_sparse_bucket_ptr();
            try {
              new_sparse_buckets = this->allocate(new_capacity);
            } catch(...) {
              this->clear();
              throw;
            }
            
            this->construct(new_sparse_buckets + m_size, std::forward<Args>(args)...);

            for(std::size_t i = 0; i < m_size; ++i) {
              this->construct(new_sparse_buckets + i, std::move(*(m_sparse_buckets + i)));
              this->destroy(m_sparse_buckets + i);
            }
            
            if(m_capacity != 0) {
              this->deallocate(m_sparse_buckets, m_capacity);
            }

            m_sparse_buckets = new_sparse_buckets;
            m_capacity = new_capacity;
          }
          ++m_size;
          return back();
        }

        void swap(vec& other) {
          std::swap(m_size, other.m_size);
          std::swap(m_capacity, other.m_capacity);
          std::swap(m_sparse_buckets, other.m_sparse_buckets);
          std::swap(m_allocator, other.m_allocator);
        }

        void resize(std::size_t n) {
          if (n <= m_size) {
            return;
          } else {
            if(n > m_capacity) {
              this->reserve(n);
            }
            const auto d = n - m_size;
            for(std::size_t i = 0; i < d; ++i) {
              this->emplace_back();
            }
          }
        }

        bool empty() const noexcept { return m_size == 0; }

        void reserve(std::size_t n) {
          if (n <= m_capacity) {
            return;
          }
          pointer new_sparse_buckets = static_empty_sparse_bucket_ptr();
          try {
            new_sparse_buckets = this->allocate(n);
          } catch(...) {
            this->clear();
            throw;
          }
          for(std::size_t i = 0; i < m_size; ++i) {
            this->construct(new_sparse_buckets + i, std::move(*(m_sparse_buckets + i)));
            this->destroy(m_sparse_buckets + i);
          }
          if(m_capacity != 0) {
            this->deallocate(m_sparse_buckets, m_capacity);
          }

          m_sparse_buckets = new_sparse_buckets;
          m_capacity = n;
        }

        void clear() noexcept { 
          if(m_sparse_buckets == static_empty_sparse_bucket_ptr()) {
            return;
          }
          auto first = m_sparse_buckets;
          auto last = m_sparse_buckets + m_size;

          for (; first != last; ++first) {
            this->destroy(first);
          }
          
          if(m_capacity > 0) {
            this->deallocate(m_sparse_buckets, m_capacity);
          }

          m_size = 0;
          m_capacity = 0;
          m_sparse_buckets = static_empty_sparse_bucket_ptr();
        }

        vec& operator=(vec&& other) {
          if (this == &other) [[unlikely]] {
            return *this;
          }
          std::swap(m_size, other.m_size);
          std::swap(m_capacity, other.m_capacity);
          std::swap(m_sparse_buckets, other.m_sparse_buckets);
          std::swap(m_allocator, other.m_allocator);

          return *this;
        }
      
      private:
        template<typename... Args>
        void construct(pointer p, Args&&... args) {
          allocator_traits::construct(m_allocator, std::to_address(p), std::forward<Args>(args)...);
        }

        void destroy(pointer p) {
          allocator_traits::destroy(m_allocator, std::to_address(p));
        }

        pointer allocate(std::size_t n) {
          const size_type max = allocator_traits::max_size(m_allocator);
          if (max < n) {
            throw std::length_error("reached allocator's max size");
          }

          return allocator_traits::allocate(m_allocator, n);
        }

        void deallocate(pointer p, std::size_t n) {
          allocator_traits::deallocate(m_allocator, std::to_address(p), n);
        }

        std::size_t nextCapacity() const noexcept {
          if (m_capacity == 0) {
            return std::max(64 / sizeof(value_type), std::size_t(1));
          }
          if (m_capacity > 4096 * 32 / sizeof(value_type)) {
            return m_capacity * 2;
          }
          return (m_capacity * 3 + 1) / 2;
        }

      private:
        std::size_t m_size;
        std::size_t m_capacity;
        pointer m_sparse_buckets;
        allocator_type m_allocator;
    };
  } // namespace detail_sparse_hash
} // namespace dice::sparse_map
#endif