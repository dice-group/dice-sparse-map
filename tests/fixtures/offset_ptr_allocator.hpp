#ifndef DICE_SPARSE_MAP_TESTS_FIXTURES_OFFSET_PTR_ALLOCATOR_HPP
#define DICE_SPARSE_MAP_TESTS_FIXTURES_OFFSET_PTR_ALLOCATOR_HPP

#include <boost/interprocess/offset_ptr.hpp>

#include <cstddef>
#include <new>

namespace dice::sparse_map::tests {

    /**
     * Allocator that hands out `boost::interprocess::offset_ptr` pointers to memory from the global heap.
     * It tests that the containers work with fancy pointers. All instances compare equal.
     * @tparam T value type of the allocator
     */
    template<typename T>
    struct offset_ptr_allocator {
        using value_type = T;
        template<typename P>
        using offset_ptr = boost::interprocess::offset_ptr<P>;
        using pointer = offset_ptr<value_type>;
        using const_pointer = offset_ptr<value_type const>;
        using void_pointer = offset_ptr<void>;
        using const_void_pointer = offset_ptr<void const>;
        using difference_type = typename offset_ptr<value_type>::difference_type;
        using size_type = std::size_t;

        offset_ptr_allocator() noexcept = default;

        template<typename U>
        offset_ptr_allocator(
            offset_ptr_allocator<U> const & /*other*/
        ) noexcept { // NOLINT(google-explicit-constructor)
        }

        pointer allocate(size_type n) {
            return pointer{static_cast<T *>(::operator new(n * sizeof(T)))};
        }

        void deallocate(pointer p, size_type /*n*/) noexcept {
            ::operator delete(p.get());
        }

        friend bool operator==(offset_ptr_allocator const & /*lhs*/, offset_ptr_allocator const & /*rhs*/) noexcept {
            return true;
        }
    };

} // namespace dice::sparse_map::tests

#endif // DICE_SPARSE_MAP_TESTS_FIXTURES_OFFSET_PTR_ALLOCATOR_HPP
